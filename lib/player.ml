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

let setinel = Hand.of_pair (Value 0) (Value 0)

let create ?(hand = setinel) (name : string) : t =
  let player =
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
  in
  if Hand.equal hand setinel then player else { player with hand = Some hand }
