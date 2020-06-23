open Nats
open Lwt.Infix

let main () =
  let host = ref "demo.nats.io" in
  let port = ref 4222 in
  let nb_msgs = ref 10 in
  let spec =
    Arg.
      [ ("--host", String (fun h -> host := h), "hostname")
      ; ("--port", Int (fun p -> port := p), "port")
      ; ("--nbmsgs", Int (fun n -> nb_msgs := n), "number of messages") ]
  in
  Arg.parse spec ignore "publish --host host --port port --nbmsgs nb" ;
  let%lwt client = Client.start ~host:!host ~port:!port in
  let rec loop counter subject =
    if counter == !nb_msgs then Lwt.return_unit
    else
      Client.pub ~msg:("Hello " ^ string_of_int counter) ~subject client
      >>= fun () -> loop (counter + 1) subject
  in
  loop 0 "foo.bar"

let () = Lwt_main.run @@ main ()
