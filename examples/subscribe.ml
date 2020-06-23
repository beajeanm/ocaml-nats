open Nats

let wait_and_close client wait () =
  let%lwt _ = Lwt_unix.sleep (float_of_int wait) in
  Client.close client

let print msg =
  let open Messages in
  Lwt_io.printlf "[%s] %s" msg.sid msg.payload

let main () =
  let host = ref "demo.nats.io" in
  let port = ref 4222 in
  let wait = ref 30 in
  let spec =
    Arg.
      [ ("--host", String (fun h -> host := h), "hostname")
      ; ("--port", Int (fun p -> port := p), "port")
      ; ("--wait", Int (fun w -> wait := w), "wait time in seconds") ]
  in
  Arg.parse spec ignore "subscribe --host host --port port --wait wait" ;
  let%lwt client = Client.start ~host:!host ~port:!port in
  let%lwt messages = Client.sub ~subject:"foo.*" client in
  Lwt.async @@ wait_and_close client !wait ;
  Lwt_stream.iter_s print messages

let () = Lwt_main.run @@ main ()
