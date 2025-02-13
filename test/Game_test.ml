(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Sabacc
open Helpers
open Alcotest

let () =
  Testo.test "roll_imposter" (fun () ->
      let card = Card.create Red (Value 1) in
      let card' = Game.roll_imposter card in
      check a_card ~pos:__POS__ "Card is unchanged" card card';

      let card = Card.create Red (Imposter (Some 1)) in
      let card' = Game.roll_imposter card in
      check a_card ~pos:__POS__ "Card is unchanged" card card';

      let card = Card.create Red (Imposter None) in
      let card = Game.roll_imposter card in
      check bool ~pos:__POS__ "" true
        (match card.rank with
        | Imposter i -> Option.is_some i
        | _ -> false);
      check bool ~pos:__POS__ "" true
        (match card.rank with
        | Imposter (Some i) -> i > 0 && i < 7
        | _ -> false);

      ())

let () =
  Testo.test "calculate_hand" (fun () ->
      let a_hand_value = testable Hand.pp_hand_value Hand.equal_hand_value in

      let hand = Hand.create (Card.create Red Sylop) (Card.create Yellow Sylop) in
      check a_hand_value ~pos:__POS__ "heck" Hand.PureSabacc (Game.calculate_hand hand);

      Range.from 1 6
      |> Range.iter ~f:(fun i ->
             List.iter
               (fun (rank : Rank.t) ->
                 let hand =
                   Hand.create (Card.create Red rank) (Card.create Yellow Sylop)
                 in
                 check a_hand_value ~pos:__POS__ "heck" (Hand.PairSabacc i)
                   (Game.calculate_hand hand))
               [ Value i; Imposter (Some i) ]);
      ();

      Range.from 1 6
      |> Range.iter ~f:(fun i1 ->
             Range.from 1 6
             |> Range.iter ~f:(fun i2 ->
                    if i1 != i2 then (
                      let hand =
                        Hand.create (Card.create Red (Value i1))
                          (Card.create Yellow (Value i2))
                      in
                      check a_hand_value ~pos:__POS__
                        (Printf.sprintf "value %i, value %i" i1 i2)
                        (Hand.Other (abs (i1 - i2)))
                        (Game.calculate_hand hand);

                      let hand =
                        Hand.create (Card.create Red (Value i1))
                          (Card.create Yellow (Rank.imp_of_int i2))
                      in
                      check a_hand_value ~pos:__POS__
                        (Printf.sprintf "value %i, imposter %i" i1 i2)
                        (Hand.Other (abs (i1 - i2)))
                        (Game.calculate_hand hand)))))

let () =
  Testo.test "compare_hands" (fun () ->
      let player1 = Player.create "noah" ~hand:(Hand.of_pair (Value 1) (Value 1)) in
      let player2 = Player.create "may" ~hand:(Hand.of_pair (Value 3) (Value 4)) in
      let player3 = Player.create "lavender" ~hand:(Hand.of_pair (Value 3) (Value 1)) in
      let player4 =
        Player.create "asterius" ~hand:(Hand.of_pair (Rank.imp_of_int 4) Sylop)
      in
      let players =
        [ player1; player2; player3; player4 ]
        |> List.map (fun (player : Player.t) ->
               let hand = Option.get player.hand in
               let hand = Some { hand with value = Some (Game.calculate_hand hand) } in
               let player = { player with hand } in
               player)
      in
      let sorted =
        players
        |> List.stable_sort (fun (p1 : Player.t) (p2 : Player.t) ->
               Game.compare_hands (Option.get p1.hand).value (Option.get p2.hand).value)
      in
      check (list string) ~pos:__POS__ "Players should be sorted"
        [ "noah"; "asterius"; "may"; "lavender" ]
        (sorted |> List.map (fun (player : Player.t) -> player.name));
      ())

let () =
  Testo.test "calculate_round" (fun () ->
      let player1 = Player.create "noah" ~hand:(Hand.of_pair (Value 1) (Value 1)) in
      let player2 = Player.create "may" ~hand:(Hand.of_pair (Value 2) (Value 2)) in
      let player3 = Player.create "lav" ~hand:(Hand.of_pair (Value 3) (Value 3)) in
      let player4 = Player.create "aster" ~hand:(Hand.of_pair (Value 4) (Value 4)) in
      let players = [ player1; player2; player3; player4 ] in
      let game = Game.create { players; starting_chips = 6 } in
      let game = { game with players = Players.create players } in
      let game = Game.calculate_round game in
      check (list string) ~pos:__POS__ "one winner" [ "noah" ]
        (Option.get game.winners |> List.sort String.compare);

      let player4 = Player.create "aster" ~hand:(Hand.of_pair (Value 1) Sylop) in
      let players = [ player1; player2; player3; player4 ] in
      let game = Game.create { players; starting_chips = 6 } in
      let game = { game with players = Players.create players } in
      let game = Game.calculate_round game in
      check (list string) ~pos:__POS__ "two winners" [ "aster"; "noah" ]
        (Option.get game.winners |> List.sort String.compare);

      ())
