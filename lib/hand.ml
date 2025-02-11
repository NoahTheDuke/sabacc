(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Card
open Suite

type hand_value =
  | PureSabacc
  | PairSabacc of int
  | Other of int
[@@deriving eq, show]

type t = {
  red : Card.t;
  yellow : Card.t;
  value : hand_value option;
}
[@@deriving eq]

let create (red : Card.t) (yellow : Card.t) : t =
  assert (Card.is_red red);
  assert (Card.is_yellow yellow);
  { red; yellow; value = None }

let of_pair (red : Rank.t) (yellow : Rank.t) : t =
  create (Card.create Red red) (Card.create Yellow yellow)

let show (hand : t) : string =
  Printf.sprintf "%s, %s" (Card.show hand.red) (Card.show hand.yellow)

let pp ppf (hand : t) =
  Format.fprintf ppf "%s, %s" (Card.show hand.red) (Card.show hand.yellow);
  ()

let by_suite (suite : Suite.t) (hand : t) : Card.t =
  match suite with
  | Red -> hand.red
  | Yellow -> hand.yellow
