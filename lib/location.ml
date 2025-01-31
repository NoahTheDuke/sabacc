(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

type location =
  | Deck
  | Discard

let show_location = function
  | Deck -> "deck"
  | Discard -> "discard"

let pp_location ppf location =
  match location with
  | Deck -> Format.fprintf ppf "%s" (show_location location)
  | Discard -> Format.fprintf ppf "%s" (show_location location)
