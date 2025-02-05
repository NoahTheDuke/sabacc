(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Card
open Location
open Rank
open Suite

let knuth_shuffle a =
  let n = Array.length a in
  let a = Array.copy a in
  for i = n - 1 downto 1 do
    let k = Random.int (i + 1) in
    let x = a.(k) in
    a.(k) <- a.(i);
    a.(i) <- x
  done;
  a

module Deck = struct
  type t = {
    suite : Suite.t;
    deck_pile : Card.t list;
    discard_pile : Card.t list;
  }
  [@@deriving show, eq]

  let create (suite : Suite.t) : t =
    let deck : Card.t array =
      knuth_shuffle
        [|
          { suite; Card.rank = One };
          { suite; Card.rank = One };
          { suite; Card.rank = One };
          { suite; Card.rank = Two };
          { suite; Card.rank = Two };
          { suite; Card.rank = Two };
          { suite; Card.rank = Three };
          { suite; Card.rank = Three };
          { suite; Card.rank = Three };
          { suite; Card.rank = Four };
          { suite; Card.rank = Four };
          { suite; Card.rank = Four };
          { suite; Card.rank = Five };
          { suite; Card.rank = Five };
          { suite; Card.rank = Five };
          { suite; Card.rank = Six };
          { suite; Card.rank = Six };
          { suite; Card.rank = Six };
          { suite; Card.rank = Imposter };
          { suite; Card.rank = Imposter };
          { suite; Card.rank = Imposter };
          { suite; Card.rank = Sylop };
        |]
    in
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

  let display (deck : t) =
    let suite = Suite.show deck.suite in
    Printf.printf "  %s deck: %i cards remaining, discard: %s" suite (length deck)
      (match available deck with
      | Some c -> Card.show c
      | None -> "[Empty]")
end
