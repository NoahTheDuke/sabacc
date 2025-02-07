(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Card
open Location
open Rank
open Suite

let () = Random.self_init ()

type t = {
  suite : Suite.t;
  deck_pile : Card.t list;
  discard_pile : Card.t list;
}
[@@deriving show, eq]

let create (suite : Suite.t) : t =
  let deck : Card.t array =
    [|
      { suite; Card.rank = Value 1 };
      { suite; Card.rank = Value 1 };
      { suite; Card.rank = Value 1 };
      { suite; Card.rank = Value 2 };
      { suite; Card.rank = Value 2 };
      { suite; Card.rank = Value 2 };
      { suite; Card.rank = Value 3 };
      { suite; Card.rank = Value 3 };
      { suite; Card.rank = Value 3 };
      { suite; Card.rank = Value 4 };
      { suite; Card.rank = Value 4 };
      { suite; Card.rank = Value 4 };
      { suite; Card.rank = Value 5 };
      { suite; Card.rank = Value 5 };
      { suite; Card.rank = Value 5 };
      { suite; Card.rank = Value 6 };
      { suite; Card.rank = Value 6 };
      { suite; Card.rank = Value 6 };
      { suite; Card.rank = Imposter None };
      { suite; Card.rank = Imposter None };
      { suite; Card.rank = Imposter None };
      { suite; Card.rank = Sylop };
    |]
  in
  Array.shuffle ~rand:Random.int deck;
  let discard = deck.(0) in
  { suite; deck_pile = deck |> Array.to_list |> List.tl; discard_pile = [ discard ] }

exception Empty_pile of string

exception DrawDrawn of string

let draw (location : Location.t) (deck : t) =
  match location with
  | Deck -> (
      match deck.deck_pile with
      | [] -> raise (Empty_pile ("Empty " ^ Suite.show deck.suite ^ " deck"))
      | c :: d -> ({ deck with deck_pile = d }, c))
  | Discard -> (
      match deck.discard_pile with
      | [] -> raise (Empty_pile ("Empty " ^ Suite.show deck.suite ^ " discard"))
      | c :: d -> ({ deck with discard_pile = d }, c))
  | l -> raise (DrawDrawn (Printf.sprintf "Can't draw from %s" (Location.show l)))

let update_discard (card : Card.t) (deck : t) : t =
  { deck with discard_pile = card :: deck.discard_pile }

let length (deck : t) = List.length deck.deck_pile

let available (deck : t) : Card.t option = List.nth_opt deck.discard_pile 0

let discard_str (deck : t) =
  match available deck with
  | Some c -> Card.show c
  | None -> Printf.sprintf "%s [Empty]" (Suite.show deck.suite)
