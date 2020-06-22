type t =
  { out_chan: Lwt_io.output_channel
  ; in_chan: Lwt_io.input_channel
  ; completion: unit Lwt.t
  ; subscriptions: Subscriptions.t }

let create_connection host port =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let host = Unix.gethostbyname host in
  let addr = Lwt_unix.ADDR_INET (host.h_addr_list.(0), port) in
  let%lwt _ = Lwt_unix.connect socket addr in
  let ic = Lwt_io.of_fd ~mode:Input socket in
  let oc = Lwt_io.of_fd ~mode:Output socket in
  Lwt.return (ic, oc)

let process_message out_chan subscriptions = function
  | Messages.Ping ->
      Lwt_io.write out_chan "Pong\r\n"
  | Messages.Msg msg ->
      let sid = msg.sid in
      Lwt.return @@ Subscriptions.pub sid msg subscriptions
  | _ ->
      Lwt.return_unit

let parse_all k in_chan resolver () =
  try%lwt
    let%lwt _ = Angstrom_lwt_unix.parse_many Protocol.parser k in_chan in
    Lwt.return @@ Lwt.wakeup resolver ()
  with _ -> Lwt.return @@ Lwt.wakeup resolver ()

let start ~host ~port =
  let%lwt in_chan, out_chan = create_connection host port in
  let subscriptions = Subscriptions.init () in
  let k = process_message out_chan subscriptions in
  let promise, resolver = Lwt.wait () in
  Lwt.async @@ parse_all k in_chan resolver ;
  Lwt.return {out_chan; in_chan; completion= promise; subscriptions}

let pub ~msg ~subject client =
  let formatted_msg =
    Printf.sprintf "pub %s %i\r\n%s\r\n" subject (String.length msg) msg
  in
  Lwt_io.write client.out_chan formatted_msg

let sub ~subject ?(queue = "") client =
  let sid, stream = Subscriptions.sub client.subscriptions in
  let formatted_msg = Printf.sprintf "sub %s %s %s\r\n" subject queue sid in
  let%lwt _ = Lwt_io.write client.out_chan formatted_msg in
  Lwt.return stream

let safe_close channel =
  try%lwt Lwt_io.close channel with _ -> Lwt.return_unit

let shutdown client =
  let _ = Subscriptions.shutdown client.subscriptions in
  let%lwt _ = safe_close client.out_chan in
  safe_close client.in_chan
