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

type info_t = False | True | String of string | Number of int
[@@deriving show, eq]

type msg_t =
  {subject: string; sid: string; reply_to: string option; payload: string}
[@@deriving show, eq]

type t =
  | Ping
  | Ok
  | Err of string
  | Info of (string * info_t) list
  | Msg of msg_t
[@@deriving show, eq]
