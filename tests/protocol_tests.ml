(*---------------------------------------------------------------------------
  Copyright (c) 2020 Jean-Michel Bea

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
  REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
  AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
  INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
  LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
  OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
  PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

open Nats

let parse = Angstrom.(parse_string ~consume:Consume.All Protocol.parser)

let t_msg = Alcotest.testable Messages.pp Messages.equal

let t_result = Alcotest.(result t_msg string)

let test_ping () =
  let pings = [parse "pIng\r\n"; parse "PING\r\n"] in
  List.iter
    (fun ping_msg -> Alcotest.check t_result "parsed ping" (Ok Ping) ping_msg)
    pings

let test_ok () =
  let ok_msg = parse "+OK\r\n" in
  Alcotest.check t_result "parsed Ok" (Ok Ok) ok_msg

let test_err () =
  let error = "Unknown Protocol Operations" in
  let err_msg = parse ("-err    '" ^ error ^ "'\r\n") in
  Alcotest.check t_result "parsed error message" (Ok (Err error)) err_msg

let test_info () =
  let info_raw_msg =
    String.concat ""
      [ {|INFO {"server_id":"1ec445b504f4edfb4cf7927c707dd717","version":"0.6.6","go":"go1.4.2","host":"0.0.0.0",|}
      ; {|"port":4222,"auth_required":false,"ssl_required":true,"max_payload":1048576}|}
      ; " \r\n" ]
  in
  let opts =
    let open Messages in
    [ ("server_id", String "1ec445b504f4edfb4cf7927c707dd717")
    ; ("version", String "0.6.6")
    ; ("go", String "go1.4.2")
    ; ("host", String "0.0.0.0")
    ; ("port", Number 4222)
    ; ("auth_required", False)
    ; ("ssl_required", True)
    ; ("max_payload", Number 1048576) ]
  in
  let expected = Messages.Info opts in
  Alcotest.check t_result "parsed info" (Ok expected) (parse info_raw_msg)

let test_msg_1 () =
  let msg_raw = "MSG FOO.BAR 9 11\r\nHello World\r\n" in
  let expected =
    let open Messages in
    Msg {subject= "FOO.BAR"; sid= "9"; reply_to= None; payload= "Hello World"}
  in
  Alcotest.check t_result "parsed hello world message" (Ok expected)
    (parse msg_raw)

let test_msg_2 () =
  let msg_raw = "MSG\tFOO.BAR 9 INBOX.34 11\r\nHello World\r\n" in
  let expected =
    let open Messages in
    Msg
      { subject= "FOO.BAR"
      ; sid= "9"
      ; reply_to= Some "INBOX.34"
      ; payload= "Hello World" }
  in
  Alcotest.check t_result "parsed hello world message" (Ok expected)
    (parse msg_raw)

let test_wildcard_subject () =
  let msg_raw = "MSG > fOo9   INBOX.34 11\r\nHello World\r\n" in
  let expected =
    let open Messages in
    Msg
      { subject= ">"
      ; sid= "fOo9"
      ; reply_to= Some "INBOX.34"
      ; payload= "Hello World" }
  in
  Alcotest.check t_result "parsed hello world message" (Ok expected)
    (parse msg_raw)

let test_wildcard_termination () =
  let msg_raw = "MSG BAR.> fOo9   INBOX.34 11\r\nHello World\r\n" in
  let expected =
    let open Messages in
    Msg
      { subject= "BAR.>"
      ; sid= "fOo9"
      ; reply_to= Some "INBOX.34"
      ; payload= "Hello World" }
  in
  Alcotest.check t_result "parsed hello world message" (Ok expected)
    (parse msg_raw)

let test_invalid_subject () =
  let msg_raw = "MSG FOO..BAR 9 INBOX.34 11\r\nHello World\r\n" in
  let expected = ": Invalid subject FOO.." in
  Alcotest.check t_result "invalid subject" (Error expected) (parse msg_raw)

let test_invalid_subject2 () =
  let msg_raw = "MSG * 9 INBOX.34 11\r\nHello World\r\n" in
  let expected = ": Invalid subject *" in
  Alcotest.check t_result "invalid subject" (Error expected) (parse msg_raw)

let test_invalid_input () =
  Alcotest.check t_result "Invalid input" (Error ": Invalid input")
    (parse "random string")
