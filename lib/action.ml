(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Card
open Location
open Suite

type 'variant action_type =
  | Draw : Suite.t * Location.t -> [> `Draw ] action_type
  | ChooseDrawn : Location.t -> [> `ChooseDrawn ] action_type
  | Stand : [> `Stand ] action_type
  | Shift : [> `Shift ] action_type

let show_action_type a_t =
  match a_t with
  | Draw (suite, location) ->
      Printf.sprintf "`Draw (%s, %s)" (Suite.show suite) (Location.show location)
  | ChooseDrawn location -> Printf.sprintf "`ChooseDrawn %s" (Location.show location)
  | Stand -> "`Stand"
  | Shift -> "`Shift"

let pp_action_type ppf a_t =
  match a_t with
  | Draw (suite, location) ->
      Format.fprintf ppf "`Draw (%s, %s)" (Suite.show suite) (Location.show location)
  | ChooseDrawn location ->
      Format.fprintf ppf "`ChooseDrawn %s" (Location.show location)
  | Stand -> Format.fprintf ppf "%s" "`Stand"
  | Shift -> Format.fprintf ppf "%s" "`Shift"

type 'variant action = {
  a_type : 'variant action_type;
  player : string;
}

let show_action action =
  Printf.printf "{a_type: %s, player: \"%s\"}"
    (show_action_type action.a_type)
    action.player

let pp_action ppf action =
  Format.fprintf ppf "{a_type: %s, player \"%s\"}"
    (show_action_type action.a_type)
    action.player;
  ()
