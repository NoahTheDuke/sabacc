(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Rank
open Suite

type t = {
  suite : Suite.t;
  rank : Rank.t;
}
[@@deriving eq]

let create (suite : Suite.t) (rank : Rank.t) : t = { rank; suite }

let is_red (card : t) : bool =
  match card.suite with
  | Red -> true
  | _ -> false

let is_yellow (card : t) : bool =
  match card.suite with
  | Yellow -> true
  | _ -> false

let show (card : t) : string =
  Printf.sprintf "%s of %s" (Rank.show card.rank) (Suite.show card.suite)

let pp ppf (card : t) =
  Format.fprintf ppf "%s of %s" (Rank.show card.rank) (Suite.show card.suite);
  ()
