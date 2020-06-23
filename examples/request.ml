open Nats

let wait_and_close client () =
  let%lwt _ = Lwt_unix.sleep 30. in
  Client.close client

let print msg =
  let open Messages in
  Lwt_io.printlf "[%s] %s" msg.sid msg.payload

let main () =
  let host = ref "demo.nats.io" in
  let port = ref 4222 in
  let spec =
    Arg.
      [ ("--host", String (fun h -> host := h), "hostname")
      ; ("--port", Int (fun p -> port := p), "port") ]
  in
  Arg.parse spec ignore "request --host host --port port" ;
  let%lwt client = Client.start ~host:!host ~port:!port in
  let%lwt message = Client.req ~msg:"Hello" ~subject:"foo.reply" client in
  print message

let () = Lwt_main.run @@ main ()
