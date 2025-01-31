(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Card

type hand = {
  red : card;
  yellow : card;
}

let show_hand (hand : hand) : string =
  Printf.sprintf "%s, %s" (show_card hand.red) (show_card hand.yellow)

let pp_hand ppf (hand : hand) =
  Format.fprintf ppf "%s, %s" (show_card hand.red) (show_card hand.yellow);
  ()
