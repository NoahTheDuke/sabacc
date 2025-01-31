(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Rank
open Suite

type card = {
  suite : suite;
  rank : rank;
}

let show_card (card : card) : string =
  Printf.sprintf "%s of %s" (show_rank card.rank) (show_suite card.suite)

let pp_card ppf (card : card) =
  Format.fprintf ppf "%s of %s" (show_rank card.rank) (show_suite card.suite);
  ()
