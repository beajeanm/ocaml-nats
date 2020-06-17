open Lwt.Infix

type t = {output: Lwt_io.output_channel}

type message_processor = (Messages.t, string) result -> unit Lwt.t

type reader_state = {in_chan: Lwt_io.input_channel; leftover: Bigstringaf.t}

let create_connection host port =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let host = Unix.gethostbyname host in
  let addr = Lwt_unix.ADDR_INET (host.h_addr_list.(0), port) in
  Lwt_unix.connect socket addr
  >|= fun _ ->
  let ic = Lwt_io.of_fd ~mode:Input socket in
  let oc = Lwt_io.of_fd ~mode:Output socket in
  (ic, oc)

let read_next_message client =
  let state = Angstrom.Buffered.parse Protocol.parser in
  let updated_state =
    Angstrom.Buffered.feed state (`Bigstring client.leftover)
  in
  Angstrom_lwt_unix.with_buffered_parse_state updated_state client.in_chan
  >|= fun (unconsumed, result) ->
  let leftover =
    Bigstringaf.sub unconsumed.buf ~off:unconsumed.off ~len:unconsumed.len
  in
  ({client with leftover}, result)

let create_stream client =
  let state = ref client in
  let f () =
    read_next_message !state
    >|= fun (new_client, result) ->
    state := new_client ;
    Some result
  in
  Lwt_stream.from f

let process_message client push = function
  | Ok Messages.Ping ->
      Lwt.async (fun () -> Lwt_io.write client.output "Pong\r\n")
  | Ok Messages.Ok | Ok (Messages.Info _) ->
      ()
  | Ok (Messages.Err msg) ->
      Lwt.async (fun () -> push (Result.Error msg))
  | _ as msg ->
      Lwt.async (fun () -> push msg)

let pub ~msg ~subject client =
  let formatted_msg =
    Printf.sprintf "pub %s %i\r\n%s\r\n" subject (String.length msg) msg
  in
  Lwt_io.write client.output formatted_msg

let sub ~subject ?(queue = "") ~sid client =
  let formatted_msg = Printf.sprintf "sub %s %s %s\r\n" subject queue sid in
  Lwt_io.write client.output formatted_msg

let start ~host ~port push =
  let rec read_loop stream client : unit Lwt.t =
    Lwt_stream.next stream
    >>= fun msg ->
    process_message client push msg ;
    read_loop stream client
  in
  create_connection host port
  >|= fun (ic, oc) ->
  let client = {output= oc} in
  let reader_state = {in_chan= ic; leftover= Bigstringaf.create 0} in
  Lwt.async (fun () -> read_loop (create_stream reader_state) client) ;
  client
