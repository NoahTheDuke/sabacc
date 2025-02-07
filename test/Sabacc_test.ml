(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Sabacc

let pos = __POS__

let tests _env = List.concat [ Game_test.tests () ]

let () = Testo.interpret_argv ~project_name:"sabacc" tests
