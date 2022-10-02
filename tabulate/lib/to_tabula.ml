open Core
open Async

module Control = struct
  type t = Proceed | Dismiss [@@deriving sexp, yojson]
end

type t =
  | Transcription of string
  | Control of Control.t
  | Mistakes of Mistakes.t list
[@@deriving sexp, yojson]

let%expect_test "transcription" =
  [%yojson_of: t] (Transcription "testing 1 2 3")
  |> Yojson.Safe.to_string |> print_endline;
  [%expect {| ["Transcription","testing 1 2 3"] |}];
  Deferred.unit

let%expect_test "control" =
  [%yojson_of: t] (Control Proceed) |> Yojson.Safe.to_string |> print_endline;
  [%expect {| ["Control",["Proceed"]] |}];
  [%yojson_of: t] (Control Dismiss) |> Yojson.Safe.to_string |> print_endline;
  [%expect {| ["Control",["Dismiss"]] |}];
  Deferred.unit

let%expect_test "mistakes" =
  [%yojson_of: t]
    (Mistakes [ Mistakes.Drug_to_drug { first = "a"; second = "b" } ])
  |> Yojson.Safe.to_string |> print_endline;
  [%expect {| ["Mistakes",[["Drug_to_drug",{"first":"a","second":"b"}]]] |}];
  Deferred.unit
