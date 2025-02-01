(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Sabacc.Game

let options =
  { players = [ "noah"; "may"; "lavender"; "asterius" ]; starting_chips = 6 }

let game = Game.create options
let () = Game.display game

let game = game |> Game.handle_action { a_type = Draw (Red, Deck); player = "noah" }

(*let () = Game.display game*)
