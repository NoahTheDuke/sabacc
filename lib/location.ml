(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

module Location = struct
  type t =
    | Deck
    | Discard
    | Drawn
    | Hand
  [@@deriving eq]

  let show = function
    | Deck -> "deck"
    | Discard -> "discard"
    | Drawn -> "drawn"
    | Hand -> "hand"

  let pp ppf (location : t) =
    match location with
    | Deck -> Format.fprintf ppf "%s" (show location)
    | Discard -> Format.fprintf ppf "%s" (show location)
    | Drawn -> Format.fprintf ppf "%s" (show location)
    | Hand -> Format.fprintf ppf "%s" (show location)
end
