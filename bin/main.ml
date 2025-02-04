(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Sabacc.Game

let () =
  let options =
    { players = [ "noah"; "may"; "lavender"; "asterius" ]; starting_chips = 6 }
  in
  let game = Game.create options in
  let game = { game with running = true } in
  Game.game_loop game

(*let game = game |> Game.handle_action { a_type = Draw (Red, Deck); player = "noah" }*)
(**)
(*let () = Game.display game*)
