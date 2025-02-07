(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

type t =
  | Value of int
  | Imposter of int option
  | Sylop
[@@deriving eq]

let show = function
  | Value n -> Printf.sprintf "%i" n
  | Imposter n ->
      Printf.sprintf "Imposter%s"
        (match n with
        | Some n -> Printf.sprintf " %i" n
        | None -> "")
  | Sylop -> "Sylop"

let pp ppf rank =
  match rank with
  | Value n -> Format.fprintf ppf "%i" n
  | Imposter _ -> Format.fprintf ppf "%s" (show rank)
  | Sylop -> Format.fprintf ppf "%s" "Sylop"

let imposter_of_int (n : int) : t =
  assert (n > 0);
  assert (n < 7);
  Imposter (Some n)
