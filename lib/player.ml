(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Card
open Hand

module Player = struct
  type status =
    | In
    | Standing
  [@@deriving show, eq]

  type state =
    | Waiting
    | Action
    | Decision
  [@@deriving show, eq]

  type t = {
    name : string;
    drawn : Card.t option;
    hand : Hand.t;
    chips : int;
    invested_chips : int;
    status : status;
    took_turn : bool;
    state : state;
  }
  [@@deriving show, eq]

  let create (name : string) (hand : Hand.t) (chips : int) : t =
    {
      name;
      drawn = None;
      hand;
      chips;
      invested_chips = 0;
      status = In;
      took_turn = false;
      state = Waiting;
    }
end

module StringMap = Map.Make (String)

type player_map = Player.t StringMap.t

module Players = struct
  type t = {
    pm : player_map;
    turn_order : string array;
  }

  let pp ppf (m : t) =
    Format.pp_print_string ppf "{ players = {";
    StringMap.iter
      (fun k v -> Format.fprintf ppf "\"%s\" -> %s; " k (Player.show v))
      m.pm;
    Format.pp_print_string ppf "}; order = [|";
    List.iter (Format.fprintf ppf "\"%s\";") (Array.to_list m.turn_order);
    Format.pp_print_string ppf "|]}"

  let show (m : t) =
    "{ players = {\n"
    ^ String.concat ""
        (List.map
           (fun (k, v) -> Printf.sprintf "  \"%s\" -> %s;\n" k (Player.show v))
           (StringMap.to_list m.pm))
    ^ "}; order = [|"
    ^ String.concat " "
        (List.map (Printf.sprintf "\"%s\";") (Array.to_list m.turn_order))
    ^ "|]}"

  let create (players : Player.t list) : t =
    let turn_order =
      players |> List.map (fun (p : Player.t) -> p.name) |> Array.of_list
    in
    let pm =
      players |> List.to_seq
      |> Seq.map (fun (p : Player.t) -> (p.name, p))
      |> StringMap.of_seq
    in
    { pm; turn_order }

  let int_to_player (index : int) (players : t) : Player.t =
    let name = Array.get players.turn_order index in
    StringMap.find name players.pm

  let all_took_turns (players : t) : bool =
    StringMap.for_all (fun name (player : Player.t) -> player.took_turn) players.pm

  let none_took_turns (players : t) : bool =
    players.pm
    |> StringMap.exists (fun name (player : Player.t) -> player.took_turn)
    |> not

  let reset (players : t) : t =
    let pm =
      StringMap.map
        (fun (player : Player.t) -> { player with took_turn = false })
        players.pm
    in
    { players with pm }
end
