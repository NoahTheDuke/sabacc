open Sabacc

let a_rank = Alcotest.testable Rank.pp Rank.equal

let a_suite = Alcotest.testable Suite.pp Suite.equal

let a_card = Alcotest.testable Card.pp Card.equal

let a_hand = Alcotest.testable Hand.pp Hand.equal

let a_deck = Alcotest.testable Deck.pp Deck.equal

let a_player = Alcotest.testable Player.pp Player.equal

let a_players = Alcotest.testable Players.pp Players.equal
