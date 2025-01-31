(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

type rank =
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Imposter
  | Sylop

let show_rank = function
  | One -> "1"
  | Two -> "2"
  | Three -> "3"
  | Four -> "4"
  | Five -> "5"
  | Six -> "6"
  | Imposter -> "Imposter"
  | Sylop -> "Sylop"

let pp_rank ppf suite =
  match suite with
  | One -> Format.fprintf ppf "%i" 1
  | Two -> Format.fprintf ppf "%i" 2
  | Three -> Format.fprintf ppf "%i" 3
  | Four -> Format.fprintf ppf "%i" 4
  | Five -> Format.fprintf ppf "%i" 5
  | Six -> Format.fprintf ppf "%i" 6
  | Imposter -> Format.fprintf ppf "%s" "Imposter"
  | Sylop -> Format.fprintf ppf "%s" "Sylop"
