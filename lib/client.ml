open Lwt.Infix

type t =
  { out_chan: Lwt_io.output_channel
  ; completion: unit Lwt.t
  ; subscriptions: Subscriptions.t }


let create_connection host port =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let host = Unix.gethostbyname host in
  let addr = Lwt_unix.ADDR_INET (host.h_addr_list.(0), port) in
  Lwt_unix.connect socket addr
  >|= fun _ ->
  let ic = Lwt_io.of_fd ~mode:Input socket in
  let oc = Lwt_io.of_fd ~mode:Output socket in
  (ic, oc)

let process_message out_chan subscriptions = function
  | Messages.Ping ->
      Lwt_io.write out_chan "Pong\r\n"
  | Messages.Msg msg ->
      let sid = msg.sid in
      Lwt.return @@ Subscriptions.pub sid msg subscriptions
  | _ ->
      Lwt.return_unit

let start ~host ~port =
  create_connection host port
  >|= fun (in_chan, out_chan) ->
  let subscriptions = Subscriptions.init () in
  let k = process_message out_chan subscriptions in
  let promise, resolver = Lwt.wait () in
  Lwt.async (fun () ->
      Angstrom_lwt_unix.parse_many Protocol.parser k in_chan
      >|= fun _ -> Lwt.wakeup resolver ()) ;
  {out_chan; completion= promise; subscriptions}

let pub ~msg ~subject client =
  let formatted_msg =
    Printf.sprintf "pub %s %i\r\n%s\r\n" subject (String.length msg) msg
  in
  Lwt_io.write client.out_chan formatted_msg

let sub ~subject ?(queue = "") client =
  let (sid, stream) = Subscriptions.sub client.subscriptions in
  let formatted_msg = Printf.sprintf "sub %s %s %s\r\n" subject queue sid in
  Lwt_io.printl formatted_msg
  >>= fun _ -> Lwt_io.write client.out_chan formatted_msg >|= fun () -> stream
