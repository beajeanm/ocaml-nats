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

open Angstrom
open Messages

let is_ws = function ' ' | '\t' -> true | _ -> false

let ws = skip_while is_ws

let eol = string "\r\n"

let quoted quote_mark parser = char quote_mark *> parser <* char quote_mark

let quoted_string quote_mark =
  quoted quote_mark (take_while (fun c -> c != quote_mark))

let ping = string_ci "ping" *> eol *> return Ping

let ok = char '+' *> string_ci "ok" *> eol *> return Ok

let err =
  let err_prefix = char '-' *> string_ci "err" *> ws in
  let err_msg = err_prefix *> quoted_string '\'' <* eol in
  lift (fun s -> Err s) err_msg

let _false = string "false" *> return False

let _true = string "true" *> return True

let str = lift (fun s -> String s) (quoted_string '"')

let integer =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

let option_value =
  let num = lift (fun i -> Number i) integer in
  peek_char_fail
  >>= function 'f' -> _false | 't' -> _true | '"' -> str | _ -> num

let info =
  let pair x y = (x, y) in
  let option_name = quoted_string '"' in
  let option_ = lift2 pair (option_name <* char ':') option_value in
  let options = char '{' *> sep_by (char ',') option_ <* char '}' in
  lift (fun i -> Info i) (string_ci "info" *> ws *> options <* ws <* eol)

let is_alphanum c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

let alphanums = take_while1 is_alphanum

type state =
  | Start
  | Token of Buffer.t
  | Dot of Buffer.t
  | Success of Buffer.t
  | Error of string

let subject =
  let is_token_sep c = c = '.' in
  let is_wildcard c = c = '>' in
  let token_extractor state char =
    match state with
    | Start ->
        let buffer = Buffer.create 256 in
        let _ = Buffer.add_char buffer char in
        if is_wildcard char then Some (Success buffer)
        else if is_alphanum char then Some (Token buffer)
        else Some (Error ("Invalid subject " ^ Buffer.contents buffer))
    | Token buffer ->
        if is_ws char then Some (Success buffer)
        else (
          Buffer.add_char buffer char ;
          if is_token_sep char then Some (Dot buffer)
          else if is_alphanum char then Some (Token buffer)
          else Some (Error ("Invalid subject " ^ Buffer.contents buffer)) )
    | Dot buffer ->
        let _ = Buffer.add_char buffer char in
        if is_wildcard char then Some (Success buffer)
        else if is_alphanum char then Some (Token buffer)
        else Some (Error ("Invalid subject " ^ Buffer.contents buffer))
    | Error _ ->
        None
    | Success _ ->
        None
  in
  Angstrom.scan_state Start token_extractor
  >>= function
  | Success buffer ->
      Angstrom.return (Buffer.contents buffer)
  | Error msg ->
      Angstrom.fail msg
  | _ ->
      Angstrom.fail "Invalid subject"

let msg =
  let pair x y = (x, y) in
  let msg_prefix = string_ci "msg" *> ws in
  let subject_parser = msg_prefix *> subject <* ws in
  let subject_and_sid_parser = lift2 pair subject_parser (alphanums <* ws) in
  let reply_to_and_length_parser =
    lift2 pair (lift (fun s -> Some s) (subject <* ws)) (integer <* eol)
    <|> lift (fun i -> (None, i)) (integer <* eol)
  in
  let reply_to_and_payload_parser =
    reply_to_and_length_parser
    >>= fun (reply_to, length) ->
    lift (fun payload -> (reply_to, payload)) (take length <* eol)
  in
  lift2
    (fun (subject, sid) (reply_to, payload) ->
      Msg {subject; sid; reply_to; payload})
    subject_and_sid_parser reply_to_and_payload_parser

let parser =
  peek_char_fail
  >>= function
  | '+' ->
      ok
  | '-' ->
      err
  | 'P' | 'p' ->
      ping
  | 'I' ->
      info
  | 'M' ->
      msg
  | _ ->
      fail "Invalid input"
