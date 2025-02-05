(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Card
open Hand
open Player
module StringMap = Map.Make (String)

type player_map = Player.t StringMap.t

type t = player_map

let pp ppf (players : t) =
  Format.pp_print_string ppf "{";
  StringMap.iter
    (fun k v -> Format.fprintf ppf "\"%s\" -> %s; " k (Player.show v))
    players;
  Format.pp_print_string ppf "|]}"

let show (players : t) =
  "{ players = {\n"
  ^ String.concat ""
      (List.map
         (fun (k, v) -> Printf.sprintf "  \"%s\" -> %s;\n" k (Player.show v))
         (StringMap.to_list players))
  ^ "|]}"

let to_seq (players : t) : Player.t Seq.t =
  players |> StringMap.to_seq |> Seq.map (fun (_, v) -> v)

let create (players : Player.t list) : t =
  let players =
    players |> List.to_seq
    |> Seq.map (fun (p : Player.t) -> (p.name, p))
    |> StringMap.of_seq
  in
  players

let all_took_turns (players : t) : bool =
  StringMap.for_all (fun name (player : Player.t) -> player.took_turn) players

let reset (players : t) : t =
  let players =
    StringMap.map (fun (player : Player.t) -> { player with took_turn = false }) players
  in
  players
