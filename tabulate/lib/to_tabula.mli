open! Core

module Control : sig
  type t = Proceed | Dismiss [@@deriving sexp, yojson]
end

type t =
  | Transcription of string
  | Control of Control.t
  | Mistakes of Mistakes.t list
[@@deriving sexp, yojson]
