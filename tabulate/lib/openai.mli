open Core
open Async

type t [@@deriving sexp]

val create : key:string -> t

val completion :
  t ->
  model:string ->
  max_tokens:int ->
  temperature:float ->
  prompt:string ->
  string Or_error.t Deferred.t
