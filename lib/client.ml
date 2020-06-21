open Lwt.Infix

type t = {output: Lwt_io.output_channel}

let create_connection host port =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let host = Unix.gethostbyname host in
  let addr = Lwt_unix.ADDR_INET (host.h_addr_list.(0), port) in
  Lwt_unix.connect socket addr
  >|= fun _ ->
  let ic = Lwt_io.of_fd ~mode:Input socket in
  let oc = Lwt_io.of_fd ~mode:Output socket in
  (ic, oc)

let process_message client push = function
  | Messages.Ping ->
      Lwt_io.write client.output "Pong\r\n"
  | Messages.Ok | Messages.Info _ ->
      Lwt.return_unit
  | _ as msg ->
      push msg

let pub ~msg ~subject client =
  let formatted_msg =
    Printf.sprintf "pub %s %i\r\n%s\r\n" subject (String.length msg) msg
  in
  Lwt_io.write client.output formatted_msg

let sub ~subject ?(queue = "") ~sid client =
  let formatted_msg = Printf.sprintf "sub %s %s %s\r\n" subject queue sid in
  Lwt_io.write client.output formatted_msg

let start ~host ~port push =
  create_connection host port
  >|= fun (ic, oc) ->
  let client = {output= oc} in
  let filtered_push = process_message client push in
  let (promise, resolver) = Lwt.wait () in
  Lwt.async (fun () ->
      Angstrom_lwt_unix.parse_many Protocol.parser filtered_push ic
      >|= fun _ -> Lwt.wakeup resolver ()) ;
  (client, promise)
