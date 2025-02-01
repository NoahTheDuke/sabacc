(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  Printf.printf "Execution time: %f secondsn"
                (Unix.gettimeofday () -. t);
  flush stdout;
  res
