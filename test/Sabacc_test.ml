(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

(* open all other test files here so their Testo.test calls are executed *)

open Game_test

let () =
  Testo.interpret_argv ~project_name:"sabacc" (fun _env ->
      Testo.get_registered_tests ())
