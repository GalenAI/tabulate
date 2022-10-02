open Core
open Async

let pre_prompt = ""

type t = {
  side : [ `Left | `Right ] option;
  site : string option;
  medications : string list;
}
[@@deriving sexp, yojson_of]

let%expect_test "empty" =
  [%yojson_of: t] { side = None; site = None; medications = [] }
  |> Yojson.Safe.to_string |> print_endline;
  [%expect {| {"side":null,"site":null,"medications":[]} |}]

let%expect_test "normal" =
  [%yojson_of: t]
    { side = Some `Right; site = Some "abdomen"; medications = [ "adderall" ] }
  |> Yojson.Safe.to_string |> print_endline;
  [%expect {| {"side":["Right"],"site":"abdomen","medications":["adderall"]} |}]

let find_line_with_prefix item prefix lines =
  List.filter_map lines ~f:(String.chop_prefix ~prefix)
  |> List.hd
  |> Result.of_option
       ~error:
         (Error.create_s [%message "Could not find" item (lines : string list)])

let get_side lines =
  let%bind.Or_error side_raw = find_line_with_prefix "side" "Side: " lines in
  match String.uppercase side_raw with
  | "LEFT" -> Ok `Left
  | "RIGHT" -> Ok `Right
  | _ -> Or_error.error_s [%message "Invalid side" side_raw]

let get_site lines =
  let%map.Or_error site_raw = find_line_with_prefix "site" "Site: " lines in
  String.lowercase site_raw

let get_medications lines =
  let%map.Or_error medications_raw =
    find_line_with_prefix "medications" "Medications: " lines
  in
  String.split medications_raw ~on:','
  |> List.map ~f:String.strip
  |> List.map ~f:String.lowercase

let parse_completion completion =
  let lines = String.split_lines completion in
  let side = get_side lines |> Or_error.ok in
  let site = get_site lines |> Or_error.ok in
  let medications =
    match get_medications lines with
    | Ok medications -> medications
    | Error _ -> []
  in
  Ok { side; site; medications }

let%expect_test "test completion" =
  let parsed =
    parse_completion "Side: Left\nSite: abdomen\nMedications: a, b, c,d"
    |> Or_error.ok_exn
  in
  print_s [%sexp (parsed : t)];
  [%expect {| ((side (Left)) (site (abdomen)) (medications (a b c d))) |}]

let parse openai transcription =
  let open Deferred.Or_error.Let_syntax in
  let prompt = pre_prompt ^ transcription in
  let%bind completion =
    Openai.completion openai ~model:"text-davinci-002" ~max_tokens:100
      ~temperature:0.0 ~prompt
  in
  let%bind completion =
    String.chop_prefix completion ~prefix:prompt
    |> Result.of_option
         ~error:(Error.create_s [%message "Invalid completion" completion])
    |> Deferred.return
  in
  parse_completion completion |> Deferred.return
