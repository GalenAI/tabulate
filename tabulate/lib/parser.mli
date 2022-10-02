open Core
open Async

type t = {
  side : [ `Left | `Right ] option;
  sites : string list;
  medications : string list;
}
[@@deriving sexp, yojson_of]

val parse : Openai.t -> string -> t Or_error.t Deferred.t
