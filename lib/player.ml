(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Card
open Hand

module Player = struct
  type status =
    | In
    | Standing
  [@@deriving show]

  type t = {
    name : string;
    drawn : card option;
    hand : hand;
    chips : int;
    invested_chips : int;
    status : status;
  }
  [@@deriving show]

  let create (name : string) (hand : hand) (chips : int) : t =
    { name; drawn = None; hand; chips; invested_chips = 0; status = In }
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
end
