(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Player
module StringMap = Map.Make (String)

type player_map = Player.t StringMap.t [@@deriving eq]

type t = player_map [@@deriving eq]

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
  players |> StringMap.to_seq |> Seq.map (fun (_, player) -> player)

let active_to_seq (players : t) : Player.t Seq.t =
  players |> StringMap.to_seq
  |> Seq.filter_map (fun (_, player) -> if player.active then Some player else None)

let to_list (players : t) : Player.t list =
  players |> StringMap.to_list |> List.map (fun (_, player) -> player)

let active_to_list (players : t) : Player.t list =
  players |> StringMap.to_list
  |> List.filter_map (fun (_, player) -> if player.active then Some player else None)

let create (players : Player.t list) : t =
  players |> List.to_seq
  |> Seq.map (fun (p : Player.t) -> (p.name, p))
  |> StringMap.of_seq

let all_took_action (players : t) : bool =
  players |> active_to_list |> List.for_all (fun player -> player.took_action)

let reset_action (players : t) : t =
  StringMap.map (fun player -> { player with took_action = false }) players

let set_active (players : t) : t =
  StringMap.map
    (fun player ->
      match player.chips with
      | 0 -> { player with active = false }
      | _ -> player)
    players
