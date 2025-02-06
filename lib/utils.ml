(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  Printf.printf "Execution time: %f secondsn" (Unix.gettimeofday () -. t);
  flush stdout;
  res

let rec cat = function
  | [], y -> y
  | x :: xs, y -> x :: cat (xs, y)

let rec skip = function
  | [], _ -> []
  | (_ :: xs as xs1), c -> if c > 0 then skip (xs, c - 1) else xs1

let rec take = function
  | [], _ -> []
  | x :: xs, c -> if c > 0 then x :: take (xs, c - 1) else []

let cycle l i = cat (skip (l, i), take (l, i))
