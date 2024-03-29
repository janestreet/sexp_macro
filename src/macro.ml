open Core
open Sexplib

exception Include_loop_detected of string [@@deriving sexp]
exception Macro_conv_error of exn * Sexp.t * [ `expanded of Sexp.t ] [@@deriving sexp]

let macro_error err t =
  Of_sexp_error (Failure (sprintf "Error evaluating macros: %s" err), t)
;;

let or_error_of_conv t ~get_sexp =
  match t with
  | Ok x -> Ok x
  | Error (exn, sexp) ->
    (match exn with
     | Macro_conv_error (_, (_ : Sexp.t), _) | Of_sexp_error (_, (_ : Sexp.t)) ->
       Sexplib0.Sexp_conv.sexp_of_exn exn |> Or_error.error_s
     | exn ->
       List [ Sexplib0.Sexp_conv.sexp_of_exn exn; get_sexp sexp ] |> Or_error.error_s)
;;

module Conv = struct
  type 'a t = ('a, exn * Sexp.t) Result.t

  let or_error (t : _ t) = or_error_of_conv t ~get_sexp:Fn.id
end

module Annot_conv = struct
  type 'a t = ('a, exn * Sexp.Annotated.t) Result.t

  let or_error (t : _ t) = or_error_of_conv t ~get_sexp:Sexp.Annotated.get_sexp
end

module Vars = struct
  include Set
  include String.Set

  let add_list set xs = List.fold_left ~f:(fun vars v -> Set.add vars v) ~init:set xs
  let of_list xs = add_list empty xs
end

module Value : sig
  type sexp =
    | Atom of string
    | List of sexp list

  type t = sexp list

  val of_sexp : Sexp.t -> sexp
  val to_sexp : sexp -> Sexp.t
  val of_sexps : Sexp.t list -> t
  val to_sexps : t -> Sexp.t list
end = struct
  type sexp = Sexp.t =
    | Atom of string
    | List of sexp list

  type t = Sexp.t list

  let of_sexp x = x
  let to_sexp x = x
  let of_sexps x = x
  let to_sexps x = x
end

let _ = Value.of_sexp

module Bindings : sig
  type t

  type entry =
    | Value of Value.t
    | Function of
        { args : string list
        ; body : Sexp.t list
        ; (* environment to evaluate the [body] in, to be extended with [args] *)
          environment : t
        }

  val empty : t
  val find : string -> t -> entry option
  val set : string -> entry -> t -> t
  val mem : string -> t -> bool
end = struct
  type t = entry String.Map.t

  and entry =
    | Value of Value.t
    | Function of
        { args : string list
        ; body : Sexp.t list
        ; environment : t
        }

  let empty = String.Map.empty
  let find key m = Map.find m key
  let set key data m = Map.set m ~key ~data
  let mem key m = Map.mem m key
end

(* A physical association list mapping sexps after :include are inlined to sexps
   that they originate from.  This map allows us to recover the original sexp
   that gave rise to an error and to give a precise error location. *)
type trail = (Sexp.t * Sexp.t) list

let rec find_arg result trail =
  match List.Assoc.find trail result ~equal:Core.phys_equal with
  | None -> result
  | Some result -> find_arg result trail
;;

let v_atom : Value.sexp -> string = function
  | Atom str -> str
  | List _ as t -> raise (macro_error "Atom expected" (Value.to_sexp t))
;;

let atom : Sexp.t -> string = function
  | Atom str -> str
  | List _ as t -> raise (macro_error "Atom expected" t)
;;

let atoms : Sexp.t -> string list = function
  | Atom _ as t -> raise (macro_error "Atom list expected" t)
  | List ts -> List.map ~f:atom ts
;;

(* If [~raise_if_any:true], raise an error if a free variable is encountered. *)
let free_variables_gen ~raise_if_any ts =
  (* Tail-recursive w.r.t the number of sexps in a list, but not sexp depth. *)
  let rec free_in_list bound ts acc =
    match ts with
    | Sexp.List (Sexp.Atom ":let" :: v :: vs :: def) :: ts ->
      let acc = free_in_list (Vars.add_list bound (atoms vs)) def acc in
      free_in_list (Vars.add bound (atom v)) ts acc
    | t :: ts ->
      let acc = free bound t acc in
      free_in_list bound ts acc
    | [] -> acc
  and free bound t acc =
    match t with
    | Sexp.List (Sexp.Atom ":use" :: v :: args) ->
      let acc =
        if Vars.mem bound (atom v)
        then acc
        else if raise_if_any
        then (
          let msg =
            "Undefined variable (included files cannot reference variables from outside)"
          in
          raise (macro_error msg v))
        else Vars.add acc (atom v)
      in
      List.fold_left ~f:(fun acc t -> free bound t acc) ~init:acc args
    | Sexp.List ts -> free_in_list bound ts acc
    | Sexp.Atom _ -> acc
  in
  free_in_list Vars.empty ts Vars.empty
;;

let check_no_free_variables ts =
  ignore (free_variables_gen ~raise_if_any:true ts : Vars.t)
;;

let free_variables ts = free_variables_gen ~raise_if_any:false ts

let expand_local_macros_exn ~trail ts =
  let add_result =
    match trail with
    | None -> fun ~arg:_ ~result:_ -> ()
    | Some ref -> fun ~arg ~result -> ref := (Value.to_sexp result, arg) :: !ref
  in
  (* tail-recursive *)
  let rec expand_list defs ts acc : Value.t =
    match ts with
    | (Sexp.List (Sexp.Atom ":let" :: v :: args :: def) as t) :: ts ->
      if List.is_empty def then raise (macro_error "Empty let bodies not allowed" t);
      let v = atom v in
      let args = atoms args in
      let free = free_variables def in
      let args_set = Vars.of_list args in
      let unused = Vars.diff args_set free in
      if not (Vars.is_empty unused)
      then
        raise
          (macro_error
             (sprintf
                "Unused variables: %s"
                (String.concat ~sep:", " (Vars.elements unused)))
             t);
      let undeclared = Vars.diff free args_set in
      (* All the variables should be bound in the environment because we have already
         checked that the file has no free variables. If not, that is a bug. *)
      (match
         Vars.filter undeclared ~f:(fun v -> not (Bindings.mem v defs)) |> Vars.elements
       with
       | [] -> ()
       | _ :: _ as undeclared ->
         raise
           (macro_error
              (sprintf
                 "Undeclared variables in let (bug in sexplib?): %s"
                 (String.concat ~sep:", " undeclared))
              t));
      (match List.find_a_dup args ~compare:String.compare with
       | None -> ()
       | Some dup -> raise (macro_error (sprintf "Duplicated let argument: %s" dup) t));
      expand_list
        (Bindings.set
           v
           (Function
              { args
              ; body = def
              ; environment =
                  defs
                  (* technically we leak memory by using the whole [defs] but
                     it's unlikely that anyone would ever notice. *)
              })
           defs)
        ts
        acc
    | t :: ts -> expand_list defs ts (List.rev_append (expand defs t) acc)
    | [] -> List.rev acc
  and expand defs t =
    match t with
    | Sexp.Atom ((":use" | ":let" | ":include" | ":concat") as s) ->
      raise (macro_error ("Unexpected " ^ s) t)
    | Sexp.Atom _ as t -> Value.of_sexps [ t ]
    | Sexp.List (Sexp.Atom ":use" :: v :: args) ->
      let split_arg = function
        | Sexp.List (Sexp.Atom v :: def) -> v, def
        | arg -> raise (macro_error "Malformed argument" arg)
      in
      let evaluate_and_bind arg_defs (v, def) =
        (* It is important we evaluate with respect to defs here, to avoid one
           argument shadowing the next one. *)
        let def = expand_list defs def [] in
        Bindings.set v (Value def) arg_defs
      in
      let args = List.map ~f:split_arg args in
      let arg_names = List.map ~f:(fun (v, _) -> v) args in
      (match Bindings.find (atom v) defs with
       | None -> raise (macro_error "Undefined variable" v)
       | Some (Value value) ->
         (match arg_names with
          | [] -> value
          | _ :: _ ->
            raise
              (macro_error
                 (sprintf
                    "%s is not a function, but it was applied to arguments"
                    (atom v))
                 t))
       | Some (Function { args = formal_args; body; environment = closure_defs }) ->
         if not ([%compare.equal: string list] arg_names formal_args)
         then
           raise
             (macro_error
                (sprintf
                   ("Formal args of %s differ from supplied args,"
                    ^^ " formal args are [%s]")
                   (atom v)
                   (String.concat ~sep:", " formal_args))
                t);
         let defs = List.fold_left ~f:evaluate_and_bind ~init:closure_defs args in
         expand_list defs body [])
    | Sexp.List (Sexp.Atom ":concat" :: ts) as t ->
      let ts = expand_list defs ts [] in
      let ts =
        try List.map ~f:v_atom ts with
        | _ ->
          let error =
            let appl = Sexp.List (Sexp.Atom ":concat" :: Value.to_sexps ts) in
            sprintf "Malformed concat application: %s" (Sexp.to_string_hum appl)
          in
          raise (macro_error error t)
      in
      let result = Value.Atom (String.concat ~sep:"" ts) in
      add_result ~arg:t ~result;
      [ result ]
    | Sexp.List ts ->
      let ts = expand_list defs ts [] in
      let result = Value.List ts in
      add_result ~arg:t ~result;
      [ result ]
  in
  expand_list Bindings.empty ts []
;;

let expand_local_macros ts =
  try Ok (Value.to_sexps (expand_local_macros_exn ts ~trail:None)) with
  | Of_sexp_error (e, t) -> Conv.or_error (Error (e, t))
;;

module type Sexp_loader = sig
  module Monad : sig
    type 'a t

    val return : 'a -> 'a t

    module Monad_infix : sig
      val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    end

    module List : sig
      val iter : 'a list -> f:('a -> unit t) -> unit t
      val map : 'a list -> f:('a -> 'b t) -> 'b list t
    end
  end

  val load_sexps : string -> Sexp.t list Monad.t
  val load_annotated_sexps : string -> Sexp.Annotated.t list Monad.t
end

module Loader (S : Sexp_loader) = struct
  module M = S.Monad
  open M.Monad_infix

  let ( >>| ) x f = x >>= fun x -> M.return (f x)

  type 'a file_contents = (string * 'a) list

  type mode =
    [ `Fast of Sexp.t list file_contents
    | `Find_error of Sexp.Annotated.t list file_contents
    ]

  let make_absolute_path ~with_respect_to file =
    if Filename.is_relative file
    then Filename.concat (Filename.dirname with_respect_to) file
    else file
  ;;

  let load_all_includes ~allow_includes file : Sexp.t list file_contents M.t =
    let file_contents = ref [] in
    let rec load visited file =
      if List.mem visited file ~equal:String.equal then raise (Include_loop_detected file);
      if List.mem (List.map ~f:fst !file_contents) file ~equal:String.equal
      then M.return ()
      else
        S.load_sexps file
        >>= fun ts ->
        file_contents := (file, ts) :: !file_contents;
        M.List.iter ts ~f:(load_includes (file :: visited) file)
    and load_includes visited file = function
      | Sexp.List [ Sexp.Atom ":include"; Sexp.Atom include_file ] as sexp ->
        if not allow_includes
        then raise (macro_error "include macros are not allowed" sexp);
        let include_file = make_absolute_path ~with_respect_to:file include_file in
        load visited include_file
      | Sexp.List ts -> M.List.iter ts ~f:(load_includes visited file)
      | Sexp.Atom _ -> M.return ()
    in
    load [] file >>= fun () -> M.return !file_contents
  ;;

  let load_all_annotated_includes file_contents : Sexp.Annotated.t list file_contents M.t =
    M.List.map file_contents ~f:(fun (file, _) ->
      S.load_annotated_sexps file >>= fun ts -> M.return (file, ts))
  ;;

  let find_annotated bad_sexp annot_file_contents =
    List.find_map annot_file_contents ~f:(fun (file, annot_sexps) ->
      List.find_map annot_sexps ~f:(fun annot_sexp ->
        match Sexp.Annotated.find_sexp annot_sexp bad_sexp with
        | None -> None
        | Some annot_sexp -> Some (file, annot_sexp)))
  ;;

  (* This function has to compute a transformation trail even though all of the returned
     errors are of the form [Of_sexp_error (_, t)] where [t] is a physical subexpression of
     the input, in the event where an error happens not during macro expansion but during
     conversion to ocaml values. *)
  let expand_and_convert ~multiple (mode : mode) file f =
    let trail = ref ([] : trail) in
    let add_result ~arg ~result =
      match mode with
      | `Fast _ -> ()
      | `Find_error _ -> trail := (result, arg) :: !trail
    in
    let file_contents =
      match mode with
      | `Fast file_contents -> file_contents
      | `Find_error annot_file_contents ->
        List.map
          ~f:(fun (file, annot_sexps) ->
            file, List.map ~f:Sexp.Annotated.get_sexp annot_sexps)
          annot_file_contents
    in
    let rec inline_includes current_file = function
      | Sexp.Atom _ as t -> [ t ]
      (* We expand an :include in list context, because that corresponds to
         the naive string substitution semantics. *)
      | Sexp.List [ Sexp.Atom ":include"; Sexp.Atom include_file ] ->
        load_and_inline (make_absolute_path ~with_respect_to:current_file include_file)
      | Sexp.List ts as t ->
        let ts = List.concat_map ts ~f:(inline_includes current_file) in
        let t' = Sexp.List ts in
        add_result ~arg:t ~result:t';
        [ t' ]
    and load_and_inline file =
      (* The lookup always succeeds, because [file_contents] is a result of
         [load_all_includes]. *)
      let ts =
        List.concat_map
          (List.Assoc.find_exn file_contents file ~equal:String.equal)
          ~f:(inline_includes file)
      in
      (* This checks that, after expanding the includes of file1, file1 doesn't
         have any free variables. So if file1 is included in file2, it won't use
         any of the variable of file2 in scope where file1 is included.
         However, the inclusion of file1 may shadow variables from file2. *)
      check_no_free_variables ts;
      ts
    in
    (* We stop at first error. Finding an error is linear in the size of the
       input due to the way we use [trail], so finding all errors would be
       quadratic. This caused an issue in practice at least once.  *)
    let all_ok_or_first_error ts ~f =
      with_return (fun { return } ->
        Ok
          (List.map ts ~f:(fun t ->
             match f t with
             | Ok x -> x
             | Error e -> return (Error e))))
    in
    let map_results ts ~f =
      if multiple
      then all_ok_or_first_error ~f ts
      else (
        match ts with
        | [ t ] -> Result.map (f t) ~f:(fun t -> [ t ])
        | ts ->
          failwith
            (sprintf
               "wrong number of sexps in %s, expecting 1, got %d"
               file
               (List.length ts)))
    in
    match mode with
    | `Fast _ ->
      let ts = expand_local_macros_exn ~trail:None (load_and_inline file) in
      map_results ts ~f:(fun t -> Ok (f t))
    | `Find_error annot_file_contents ->
      let locate_error f =
        try Ok (f ()) with
        | Of_sexp_error (exc, bad_sexp) as e ->
          (* Find the original sexp that caused the error. *)
          let unexpanded_bad_sexp = find_arg bad_sexp !trail in
          (match find_annotated unexpanded_bad_sexp annot_file_contents with
           | Some (file, unexpanded_bad_annot_sexp) ->
             let exc =
               match Sexp.Annotated.get_conv_exn ~file ~exc unexpanded_bad_annot_sexp with
               | Of_sexp_error (inner_exc, unexpanded_bad_sexp) as exc ->
                 if [%compare.equal: Sexp.t] bad_sexp unexpanded_bad_sexp
                 then exc
                 else Macro_conv_error (inner_exc, unexpanded_bad_sexp, `expanded bad_sexp)
               | exc -> exc
             in
             Error (exc, unexpanded_bad_annot_sexp)
           (* This case should never happen. *)
           | None -> raise e)
      in
      let inline_and_expand () =
        expand_local_macros_exn ~trail:(Some trail) (load_and_inline file)
      in
      (match locate_error inline_and_expand with
       | Error _ as e -> e
       | Ok ts -> map_results ts ~f:(fun t -> locate_error (fun () -> f t)))
  ;;

  let load ?(allow_includes = true) ~multiple file f =
    load_all_includes ~allow_includes file
    >>= fun file_contents ->
    try M.return (expand_and_convert ~multiple (`Fast file_contents) file f) with
    | Of_sexp_error _ as original_exn ->
      load_all_annotated_includes file_contents
      >>| fun annotated_file_contents ->
      (match
         expand_and_convert ~multiple (`Find_error annotated_file_contents) file f
       with
       | Error _ as e -> e
       | Ok _ ->
         (* Avoid returning success in the case there was an error.
            This can be bad e.g. when reading the input from a pipe. *)
         raise original_exn)
  ;;

  let load_sexps_conv ?allow_includes file f =
    load ?allow_includes ~multiple:true file (fun v -> f (Value.to_sexp v))
    >>| Annot_conv.or_error
  ;;

  let load_sexp_conv ?allow_includes file f =
    load ?allow_includes ~multiple:false file (fun v -> f (Value.to_sexp v))
    >>| fun result ->
    Result.map result ~f:(function
      | [ a ] -> a
      | _ -> assert false)
    |> Annot_conv.or_error
  ;;

  let included_files file =
    load_all_includes ~allow_includes:true file >>| List.map ~f:fst
  ;;
end

exception Error_in_file of string * exn

let () =
  Sexplib.Conv.Exn_converter.add
    ~finalise:false
    [%extension_constructor Error_in_file]
    (function
    | Error_in_file (file, exn) ->
      Sexp.List [ Sexp.Atom ("Error in file " ^ file); Sexplib.Conv.sexp_of_exn exn ]
    | _ -> assert false)
;;

let add_error_location file = function
  | Sexp.Parse_error e ->
    let err_msg = sprintf "%s: %s" file e.Sexp.err_msg in
    Sexp.Parse_error { e with Sexp.err_msg }
  | Failure e -> Failure (sprintf "%s: %s" file e)
  | error -> Error_in_file (file, error)
;;
