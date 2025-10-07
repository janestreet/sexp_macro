open! Core
open Async

type ('a, 'b) load = ?allow_includes:bool -> string -> (Sexp.t -> 'a) -> 'b Deferred.t

module Macro_loader = Macro.Loader (struct
    module Monad = struct
      type 'a t = 'a Deferred.t

      let return = return

      module Monad_infix = Deferred.Monad_infix

      module List = struct
        let iter xs ~f = Deferred.List.iter ~how:`Sequential xs ~f
        let map xs ~f = Deferred.List.map ~how:`Sequential xs ~f
      end
    end

    let load_sexps file =
      match%map
        Monitor.try_with ~run:`Schedule ~rest:`Log ~extract_exn:true (fun () ->
          Reader.with_file file ~f:(fun t -> Pipe.to_list (Reader.read_sexps t)))
      with
      | Ok sexps -> sexps
      | Error e -> raise (Macro.add_error_location file e)
    ;;

    let load_annotated_sexps file =
      match%map
        Monitor.try_with ~run:`Schedule ~rest:`Log ~extract_exn:true (fun () ->
          Reader.with_file file ~f:(fun t -> Pipe.to_list (Reader.read_annotated_sexps t)))
      with
      | Ok sexps -> sexps
      | Error e -> raise (Macro.add_error_location file e)
    ;;
  end)

let gen_load_sexp_exn (type a) ~allow_includes ~file ~(a_of_sexp : Sexp.t -> a) =
  Macro_loader.load_sexp_conv ?allow_includes file a_of_sexp >>| ok_exn
;;

let load_sexp_exn ?allow_includes file a_of_sexp =
  gen_load_sexp_exn ~allow_includes ~file ~a_of_sexp
;;

let[@warning "-16"] gen_load_sexp ?allow_includes ~file ~a_of_sexp =
  Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log ~extract_exn:true (fun () ->
    gen_load_sexp_exn ~allow_includes ~file ~a_of_sexp)
;;

let load_sexp ?allow_includes file a_of_sexp =
  gen_load_sexp ?allow_includes ~file ~a_of_sexp
;;

let[@warning "-16"] gen_load_sexps_exn
  (type a)
  ?allow_includes
  ~file
  ~(a_of_sexp : Sexp.t -> a)
  =
  Macro_loader.load_sexps_conv ?allow_includes file a_of_sexp >>| ok_exn
;;

let load_sexps_exn ?allow_includes file a_of_sexp =
  gen_load_sexps_exn ?allow_includes ~file ~a_of_sexp
;;

let[@warning "-16"] gen_load_sexps ?allow_includes ~file ~a_of_sexp =
  Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log ~extract_exn:true (fun () ->
    gen_load_sexps_exn ?allow_includes ~file ~a_of_sexp)
;;

let load_sexps ?allow_includes file a_of_sexp =
  gen_load_sexps ?allow_includes ~file ~a_of_sexp
;;

let included_files file =
  Deferred.Or_error.try_with ~run:`Schedule ~rest:`Log (fun () ->
    Macro_loader.included_files file)
;;
