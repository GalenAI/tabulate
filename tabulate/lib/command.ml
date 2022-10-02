open! Core
open! Async

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
end

type t = {
  mutable transcription : string;
  mutable existing_transcription : string;
  (* CR eddieli: We might want to make sure that GPT requests can never be made
     concurrently. *)
  mutable last_gpt_time : Time_ns.t option;
  existing_mistakes : Mistakes.Set.t;
}
[@@deriving sexp]

let make_empty () =
  {
    transcription = "";
    existing_transcription = "";
    last_gpt_time = None;
    existing_mistakes = Mistakes.Set.empty;
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

let command =
  Command.async ~summary:"run tabulate server"
    [%map_open.Command
      let stt_ipc = flag "--stt-ipc" (required string) ~doc:"FILE ipc for stt"
      and const_out_ipc =
        flag "--const-out-ipc" (required string)
          ~doc:"FILE out ipc for constraints"
      and const_in_ipc =
        flag "--const-in-ipc" (required string)
          ~doc:"FILE in ipc for constraints"
      and key = flag "--key" (required string) ~doc:"KEY api key for OpenAI" in
      fun () ->
        (* ZMQ initialization *)
        let context = Zmq.Context.create () in
        let stt_socket = zmq_sub_socket context ~addr:stt_ipc in
        let const_out_socket = zmq_pub_socket context ~addr:const_out_ipc in
        let const_in_socket = zmq_sub_socket context ~addr:const_in_ipc in
        (* Init state *)
        let curr_state = make_empty () in
        let openai = Openai.create ~key in
        Deferred.unit]
