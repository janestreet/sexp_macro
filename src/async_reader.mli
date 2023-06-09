open! Core
open Async

type ('a, 'b) load = ?allow_includes:bool -> string -> (Sexp.t -> 'a) -> 'b Deferred.t

val load_sexp : ('a, 'a Or_error.t) load
val load_sexp_exn : ('a, 'a) load
val load_sexps : ('a, 'a list Or_error.t) load
val load_sexps_exn : ('a, 'a list) load
val included_files : string -> string list Or_error.t Deferred.t

module Macro_loader : sig
  val load_sexps_conv
    :  ?allow_includes:bool
    -> string
    -> (Sexp.t -> 'a)
    -> 'a list Or_error.t Deferred.t

  val included_files : string -> string list Deferred.t
end
