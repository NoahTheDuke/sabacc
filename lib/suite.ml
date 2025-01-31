(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

type suite =
  | Red
  | Yellow

let show_suite = function
  | Red -> "blood"
  | Yellow -> "sand"

let pp_suite ppf suite =
  match suite with
  | Red -> Format.fprintf ppf "%s" (show_suite suite)
  | Yellow -> Format.fprintf ppf "%s" (show_suite suite)
