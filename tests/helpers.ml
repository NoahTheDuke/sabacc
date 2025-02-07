open Sabacc

let rank = Alcotest.testable Rank.pp Rank.equal

let suite = Alcotest.testable Suite.pp Suite.equal

let card = Alcotest.testable Card.pp Card.equal

let deck = Alcotest.testable Deck.pp Deck.equal

let player = Alcotest.testable Player.pp Player.equal

let players = Alcotest.testable Players.pp Players.equal
