open! Core
open! Async

(* CR eddieli: Add good logging *)

let new_word_thresh = 10
let time_span_thresh = Time_ns.Span.of_sec 10.0

module Mistakes = struct
  module T = struct
    type t =
      | Drug_to_drug of { first : string; second : string }
      | Drug_allergy of string
      | Wrong_side
      | Wrong_site of { actual : string; wrong : string }
    [@@deriving sexp, compare, yojson]
  end

  include T
  include Comparable.Make (T)

  let%expect_test "empty list" =
    [%yojson_of: t list] [] |> Yojson.Safe.to_string |> print_endline;
    [%expect {| [] |}]

  let%expect_test "all possible" =
    [%yojson_of: t list]
      [
        Drug_to_drug { first = "a"; second = "b" };
        Drug_allergy "test";
        Wrong_side;
        Wrong_site { actual = "a"; wrong = "b" };
      ]
    |> Yojson.Safe.to_string |> print_endline;
    [%expect
      {| [["Drug_to_drug",{"first":"a","second":"b"}],["Drug_allergy","test"],["Wrong_side"],["Wrong_site",{"actual":"a","wrong":"b"}]] |}]
end

type t = {
  mutable transcription : string;
  mutable existing_transcription : string;
  (* CR eddieli: We might want to make sure that GPT requests can never be made
     concurrently. *)
  mutable last_gpt_time : Time_ns.t option;
  mutable existing_mistakes : Mistakes.Set.t;
  openai : Openai.t;
  const_out_socket : ([ `Pub ] Zmq_async.Socket.t[@sexp.opaque]);
  mistake_out_socket : ([ `Pub ] Zmq_async.Socket.t[@sexp.opaque]);
}
[@@deriving sexp]

let create_empty ~openai ~const_out_socket ~mistake_out_socket =
  {
    transcription = "";
    existing_transcription = "";
    last_gpt_time = None;
    existing_mistakes = Mistakes.Set.empty;
    openai;
    const_out_socket;
    mistake_out_socket;
  }

let zmq_sub_socket ctx ~addr =
  let sync_socket = Zmq.Socket.create ctx Zmq.Socket.sub in
  let socket = Zmq_async.Socket.of_socket sync_socket in
  Zmq.Socket.connect sync_socket addr;
  Zmq.Socket.subscribe sync_socket "";
  socket

let zmq_pub_socket ctx ~addr =
  let sync_socket = Zmq.Socket.create ctx Zmq.Socket.pub in
  let socket = Zmq_async.Socket.of_socket sync_socket in
  Zmq.Socket.bind sync_socket addr;
  socket

let handle_parsed t parsed =
  let parsed_json = [%yojson_of: Parser.t] parsed |> Yojson.Safe.to_string in
  Zmq_async.Socket.send t.const_out_socket parsed_json

let count_new_words { transcription; existing_transcription; _ } =
  String.chop_prefix_exn transcription ~prefix:existing_transcription
  |> String.split ~on:' ' |> List.length

let ok_process t =
  let new_words = count_new_words t in
  match t.last_gpt_time with
  | None -> true
  | Some time ->
      let time_diff = Time_ns.diff (Time_ns.now ()) time in
      new_words > new_word_thresh && Time_ns.Span.(time_diff > time_span_thresh)

let handle_transcription t text =
  (* CR eddieli: make sure this doesn't include chunk times *)
  t.transcription <- t.transcription ^ text;
  match ok_process t with
  | false -> Deferred.unit
  | true -> (
      t.last_gpt_time <- Time_ns.now () |> Some;
      t.existing_transcription <- t.transcription;
      match%bind Parser.parse t.openai t.transcription with
      | Ok parsed -> handle_parsed t parsed
      | Error err ->
          print_s [%message "Parse error" (err : Error.t)];
          Deferred.unit)

let handle_mistake t text =
  let _handle_or_error () =
    let%bind.Deferred.Or_error mistakes =
      Or_error.try_with (fun () ->
          Yojson.Safe.from_string text
          |> [%of_yojson: Mistakes.t list] |> Mistakes.Set.of_list)
      |> return
    in
    let new_mistakes = Set.diff t.existing_mistakes mistakes in
    t.existing_mistakes <- Set.union t.existing_mistakes mistakes;
    match Set.is_empty new_mistakes with
    | true -> Deferred.Or_error.ok_unit
    | false ->
        let new_mistakes = Set.to_list new_mistakes in
        let new_mistakes_json =
          [%yojson_of: Mistakes.t list] new_mistakes |> Yojson.Safe.to_string
        in
        let%bind () =
          Zmq_async.Socket.send t.mistake_out_socket new_mistakes_json
        in
        Deferred.Or_error.ok_unit
  in
  match%bind _handle_or_error () with
  | Ok () -> Deferred.unit
  | Error err ->
      print_s [%message "Mistake error" (err : Error.t)];
      Deferred.unit

let command =
  Command.async ~summary:"run tabulate server"
    [%map_open.Command
      let stt_ipc = flag "-stt-ipc" (required string) ~doc:"FILE ipc for stt"
      and const_out_ipc =
        flag "-const-out-ipc" (required string)
          ~doc:"FILE out ipc for constraints"
      and const_in_ipc =
        flag "-const-in-ipc" (required string)
          ~doc:"FILE in ipc for constraints"
      and mistake_out_addr =
        flag "-mistake-out-addr" (required string)
          ~doc:"ADDR address for output to tabula"
      and key = flag "-key" (required string) ~doc:"KEY api key for OpenAI" in
      fun () ->
        (* ZMQ initialization *)
        let context = Zmq.Context.create () in
        let stt_socket = zmq_sub_socket context ~addr:stt_ipc in
        let const_out_socket = zmq_pub_socket context ~addr:const_out_ipc in
        let const_in_socket = zmq_sub_socket context ~addr:const_in_ipc in
        let mistake_out_socket =
          zmq_pub_socket context ~addr:mistake_out_addr
        in
        (* Init state *)
        let openai = Openai.create ~key in
        let curr_state =
          create_empty ~openai ~const_out_socket ~mistake_out_socket
        in
        (* Pipes *)
        let stt_reader =
          Pipe.unfold ~init:() ~f:(fun () ->
              let%bind text = Zmq_async.Socket.recv stt_socket in
              Some (text, ()) |> return)
        in
        let mistake_reader =
          Pipe.unfold ~init:() ~f:(fun () ->
              let%bind text = Zmq_async.Socket.recv const_in_socket in
              Some (text, ()) |> return)
        in
        Clock_ns.every' time_span_thresh (fun () ->
            handle_transcription curr_state "");
        let%bind () = Pipe.iter stt_reader ~f:(handle_transcription curr_state)
        and () = Pipe.iter mistake_reader ~f:(handle_mistake curr_state) in
        Deferred.unit]
