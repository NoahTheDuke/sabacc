(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Rank
open Suite

module Card = struct
  type t = {
    suite : Suite.t;
    rank : rank;
  }
  [@@deriving eq]

  let show (card : t) : string =
    Printf.sprintf "%s of %s" (show_rank card.rank) (Suite.show card.suite)

  let pp ppf (card : t) =
    Format.fprintf ppf "%s of %s" (show_rank card.rank) (Suite.show card.suite);
    ()
end
