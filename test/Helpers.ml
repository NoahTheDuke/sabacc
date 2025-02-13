(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Sabacc

let a_rank = Alcotest.testable Rank.pp Rank.equal

let a_suite = Alcotest.testable Suite.pp Suite.equal

let a_card = Alcotest.testable Card.pp Card.equal

let a_hand = Alcotest.testable Hand.pp Hand.equal

let a_deck = Alcotest.testable Deck.pp Deck.equal

let a_player = Alcotest.testable Player.pp Player.equal

let a_players = Alcotest.testable Players.pp Players.equal
