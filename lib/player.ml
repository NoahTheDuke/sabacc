(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Card
open Hand

type status =
  | In
  | Standing
[@@deriving show, eq]

type state =
  | Waiting
  | Action
  | Decision
[@@deriving show, eq]

type t = {
  name : string;
  drawn : Card.t option;
  hand : Hand.t option;
  chips : int;
  invested_chips : int;
  status : status;
  took_action : bool;
  state : state;
  place : int;
}
[@@deriving show, eq]

let create (name : string) : t =
  {
    name;
    drawn = None;
    hand = None;
    chips = 0;
    invested_chips = 0;
    status = In;
    took_action = false;
    state = Waiting;
    place = 1;
  }

let create_with_hand (name : string) (hand : Hand.t) : t =
  let player = create name in
  { player with hand = Some hand }
