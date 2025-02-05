(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

type t =
  | Red
  | Yellow
[@@deriving eq]

let show = function
  | Red -> "blood"
  | Yellow -> "sand"

let pp ppf suite =
  match suite with
  | Red -> Format.fprintf ppf "%s" (show suite)
  | Yellow -> Format.fprintf ppf "%s" (show suite)
