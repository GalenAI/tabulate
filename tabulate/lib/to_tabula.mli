open! Core

module Control : sig
  type t = Proceed | Dismiss [@@deriving sexp]
end

type t =
  | Transcription of string
  | Control of Control.t
  | Mistakes of Mistakes.t list
[@@deriving sexp]

val to_jsonaf : t -> Jsonaf.t
