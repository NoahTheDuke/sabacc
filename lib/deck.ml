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
    suite : suite;
    deck_pile : card list;
    discard_pile : card list;
  }
  [@@deriving show]

  let create (suite : suite) : t =
    let deck =
      knuth_shuffle
        [|
          { suite; rank = One };
          { suite; rank = One };
          { suite; rank = One };
          { suite; rank = Two };
          { suite; rank = Two };
          { suite; rank = Two };
          { suite; rank = Three };
          { suite; rank = Three };
          { suite; rank = Three };
          { suite; rank = Four };
          { suite; rank = Four };
          { suite; rank = Four };
          { suite; rank = Five };
          { suite; rank = Five };
          { suite; rank = Five };
          { suite; rank = Six };
          { suite; rank = Six };
          { suite; rank = Six };
          { suite; rank = Imposter };
          { suite; rank = Imposter };
          { suite; rank = Imposter };
          { suite; rank = Sylop };
        |]
    in
    { suite; deck_pile = Array.to_list deck; discard_pile = [] }

  exception Empty_pile of string

  (** [draw deck location] returns the specified pile and the top card *)
  let draw (location : location) (deck : t) =
    match location with
    | Deck -> (
        match deck.deck_pile with
        | [] -> raise (Empty_pile ("Empty " ^ show_suite deck.suite ^ " deck"))
        | c :: d -> ({ deck with deck_pile = d }, c))
    | Discard -> (
        match deck.discard_pile with
        | [] -> raise (Empty_pile ("Empty " ^ show_suite deck.suite ^ " discard"))
        | c :: d -> ({ deck with discard_pile = d }, c))

  let discard (card : card) (deck : t) : t =
    { deck with discard_pile = card :: deck.discard_pile }

  let length (deck : t) = List.length deck.deck_pile

  let available (deck : t) : card option =
    match deck.discard_pile with
    | [] -> None
    | x :: xs -> Some x

  let display (deck : t) =
    let suite = show_suite deck.suite in
    Printf.printf "  %s deck: %i cards remaining, discard: %s" suite (length deck)
      (match available deck with
      | None -> "[Empty]"
      | Some c -> Printf.sprintf "%s" (show_card c))
end
