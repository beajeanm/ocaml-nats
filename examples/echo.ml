open Nats

let wait_and_close client () =
  let%lwt _ = Lwt_unix.sleep 30. in
  Client.shutdown client

let print_and_reply client msg =
  let open Messages in
  let reply msg client =
    Client.pub ~subject:msg.subject ~msg:msg.payload client
  in
  let%lwt _ = Lwt_io.printlf "[%s] %s" msg.sid msg.payload in
  reply msg client

let main () =
  let host = ref "demo.nats.io" in
  let port = ref 4222 in
  let spec =
    Arg.
      [ ("--host", String (fun h -> host := h), "hostname")
      ; ("--port", Int (fun p -> port := p), "port") ]
  in
  Arg.parse spec ignore "echo --host host --port port" ;
  let%lwt client = Client.start ~host:!host ~port:!port in
  let%lwt messages = Client.sub ~subject:"foo.*" client in
  Lwt.async @@ wait_and_close client ;
  Lwt_stream.iter_s (print_and_reply client) messages

let () = Lwt_main.run @@ main ()
