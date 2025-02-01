(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Card
open Suite

module Hand = struct
  type t = {
    red : card;
    yellow : card;
  }
  [@@deriving eq]

  let show (hand : t) : string =
    Printf.sprintf "%s, %s" (show_card hand.red) (show_card hand.yellow)

  let pp ppf (hand : t) =
    Format.fprintf ppf "%s, %s" (show_card hand.red) (show_card hand.yellow);
    ()

  let by_suite (suite : Suite.t) (hand : t) : card =
    match suite with
    | Red -> hand.red
    | Yellow -> hand.yellow
end
