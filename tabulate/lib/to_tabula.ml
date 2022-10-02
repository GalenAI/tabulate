open Core

module Control = struct
  type t = Proceed | Dismiss [@@deriving sexp]

  let to_jsonaf t =
    match t with Proceed -> `String "proceed" | Dismiss -> `String "dismiss"
end

type t =
  | Transcription of string
  | Control of Control.t
  | Mistakes of Mistakes.t list
[@@deriving sexp]

let to_jsonaf t =
  match t with
  | Transcription trans ->
      `Object [ ("mt", `String "transcription"); ("m", `String trans) ]
  | Control cont ->
      `Object [ ("mt", `String "control"); ("m", Control.to_jsonaf cont) ]
  | Mistakes m ->
      `Object
        [
          ("mt", `String "mistake");
          ( "m",
            `String
              (List.map m ~f:(fun m -> Mistakes.to_str m)
              |> String.concat ~sep:", ") );
        ]

let%expect_test "transcription" =
  to_jsonaf (Transcription "testing 1 2 3") |> Jsonaf.to_string |> print_endline;
  [%expect {| {"mt":"transcription","m":"testing 1 2 3"} |}]

let%expect_test "control" =
  to_jsonaf (Control Proceed) |> Jsonaf.to_string |> print_endline;
  [%expect {| {"mt":"control","m":"proceed"} |}];
  to_jsonaf (Control Dismiss) |> Jsonaf.to_string |> print_endline;
  [%expect {| {"mt":"control","m":"dismiss"} |}]

let%expect_test "mistakes" =
  to_jsonaf (Mistakes [ Mistakes.Drug_to_drug { first = "a"; second = "b" } ])
  |> Jsonaf.to_string |> print_endline;
  [%expect
    {| {"mt":"mistake","m":"Drug-to-drug contraindication between a and b"} |}]
