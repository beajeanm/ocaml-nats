open Nats
open Lwt.Infix

let reply msg client =
 let open Messages in
 Client.pub ~subject:msg.subject ~msg:msg.payload client

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
  Lwt_stream.iter_s Messages.(fun msg -> Lwt_io.printlf "[%s] %s" msg.sid msg.payload >>= fun _ -> reply msg client) messages

let () = Lwt_main.run @@ main ()
