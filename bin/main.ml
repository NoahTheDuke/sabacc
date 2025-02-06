(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Sabacc

let () =
  let options : Game.new_game_options =
    {
      players = [ "noah"; "may"; "lavender"; "asterius" ] |> List.map Game.create_player;
      starting_chips = 6;
    }
  in
  let game = Game.create options in
  let game = { game with running = true } in
  Game.game_loop game
