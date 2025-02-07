open Sabacc

let pos = __POS__

let tests _env =
  [
    Testo.create "roll_imposter" (fun () ->
        let card = Card.create Red (Value 1) in
        let card' = Game.roll_imposter card in
        Alcotest.check Helpers.card ~pos "Card is unchanged" card card';

        let card = Card.create Red (Imposter (Some 1)) in
        let card' = Game.roll_imposter card in
        Alcotest.check Helpers.card ~pos "Card is unchanged" card card';

        let card = Card.create Red (Imposter None) in
        let card = Game.roll_imposter card in
        Alcotest.check Alcotest.bool ~pos "" true
          (match card.rank with
          | Imposter i -> Option.is_some i
          | _ -> false);

        ());
  ]

let () = Testo.interpret_argv ~project_name:"sabacc" tests
