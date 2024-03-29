open Core
open Sexplib

module type Load = sig
  val load_sexp_conv_exn : ?allow_includes:bool -> string -> (Sexp.t -> 'a) -> 'a

  val load_sexps_conv
    :  ?allow_includes:bool
    -> string
    -> (Sexp.t -> 'a)
    -> 'a list Or_error.t

  val included_files : string -> string list
end

(** [make (module Load)] runs a bunch of tests on [Load] functions and prints the output.
    If [reference] is supplied, [Load]'s output is compared against [reference]'s output,
    and the output is only printed if they differ. *)
val make : ?reference:(module Load) -> (module Load) -> unit
