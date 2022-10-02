open Core
open Async

type t = { key : string }

let create ~key = { key }

let completion { key } ~model ~max_tokens ~temperature ~prompt =
  let args =
    [
      "-k";
      key;
      "-m";
      model;
      "-M";
      Int.to_string max_tokens;
      "-t";
      Float.to_string temperature;
      "-p";
      prompt;
    ]
  in
  Process.run ~prog:"ocaml" ~args ()
