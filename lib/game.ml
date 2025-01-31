(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Action
open Card
open Deck
open Hand
open Player
open Suite

type new_game_options = {
  starting_chips : int;
  players : string list;
}

type game_step =
  | Setup
  | StartOfRound
  | Action
  | Decision
  | PassTurn
  | EndOfRound
  | Results
[@@deriving show]

module Game = struct
  type t = {
    red : Deck.t;
    yellow : Deck.t;
    starting_chips : int;
    players : Players.t;
    round : int;
    turn : int;
    starting_player : int;
    current_player : string;
    step : game_step;
  }
  [@@deriving show]

  let get_player (name : string) (game : t) : Player.t =
    StringMap.find name game.players.pm

  let display (game : t) : unit =
    Printf.printf "Turn: %i, Round: %i, Current player: %s\n" game.turn game.round
      (Array.get game.players.turn_order game.starting_player);
    print_endline "Decks:";
    Deck.display game.red;
    print_newline ();
    Deck.display game.yellow;
    print_newline ();
    print_endline "Players:";
    Array.iter
      (fun name ->
        let player = get_player name game in
        Printf.printf "  %s - Chips: %i, Invested: %i\n" player.name player.chips
          player.invested_chips)
      game.players.turn_order;
    print_endline "Your hand (noah):";
    let primary =
      StringMap.find (Array.get game.players.turn_order 0) game.players.pm
    in
    Printf.printf "  %s\n" (show_hand primary.hand);
    (match primary.drawn with
    | Some card -> Printf.printf "  Drawn: %s\n" (show_card card)
    | None -> ());
    flush stdout

  let update_player (player : Player.t) (game : t) : t =
    let pm = StringMap.add player.name player game.players.pm in
    let players = { game.players with pm } in
    { game with players }

  let update_players (players : player_map) (game : t) : t =
    { game with players = { game.players with pm = players } }

  let update_deck (deck : Deck.t) (game : t) : t =
    match deck.suite with
    | Red -> { game with red = deck }
    | Yellow -> { game with yellow = deck }

  let suite_to_deck (suite : suite) (game : t) : Deck.t =
    match suite with
    | Red -> game.red
    | Yellow -> game.yellow

  let rec create (options : new_game_options) : t =
    assert (List.length options.players > 0);
    assert (List.length options.players <= 4);
    let red = Deck.create Red in
    let yellow = Deck.create Yellow in
    let red, yellow, players =
      List.fold_left
        (fun (red, yellow, players) player_name ->
          let red, red_card = Deck.draw Deck red in
          let yellow, yellow_card = Deck.draw Deck yellow in
          let player =
            Player.create player_name
              { red = red_card; yellow = yellow_card }
              options.starting_chips
          in
          (red, yellow, players @ [ player ]))
        (red, yellow, []) options.players
    in
    let red, red_discard = Deck.draw Deck red in
    let yellow, yellow_discard = Deck.draw Deck yellow in
    let game =
      {
        red;
        yellow;
        starting_chips = options.starting_chips;
        players = Players.create players;
        round = 0;
        turn = 0;
        starting_player = 0;
        current_player = List.hd options.players;
        step = Setup;
      }
    in
    game |> discard red_discard |> discard yellow_discard

  and discard (card : card) (game : t) : t =
    let deck = suite_to_deck card.suite game in
    game |> update_deck (Deck.discard card deck)

  let draw (action : [ `Draw ] action) (game : t) : t =
    let player = get_player action.player game in
    match action.a_type with
    | Draw (suite, location) ->
        let deck = suite_to_deck suite game in
        let deck, new_card = Deck.draw location deck in
        game |> update_deck deck |> update_player { player with drawn = Some new_card }

  let choose_drawn (action : [ `ChooseDrawn ] action) (game : t) : t =
    let player = get_player action.player game in
    match action.a_type with
    | ChooseDrawn card ->
        let hand = player.hand in
        let game, player =
          match card.suite with
          | Red ->
              ( discard hand.red game,
                { player with drawn = None; hand = { hand with red = card } } )
          | Yellow ->
              ( discard hand.yellow game,
                { player with drawn = None; hand = { hand with red = hand.red } } )
        in
        game |> update_player player

  let stand (action : [ `Stand ] action) (game : t) : t =
    let player = get_player action.player game in
    game |> update_player { player with status = Standing }

  let shift (action : [ `Shift ] action) (game : t) : t = game

  let handle_action action (game : t) : t =
    match action with
    | { a_type = Draw _; _ } as action -> draw action game
    | { a_type = ChooseDrawn _; _ } as action -> choose_drawn action game
    | { a_type = Stand; _ } as action -> stand action game
    | { a_type = Shift; _ } as action -> shift action game

  let get_current_player (game : t) : Player.t =
    StringMap.find game.current_player game.players.pm

  let available_actions (game : t) : string list =
    let _current_player = get_current_player game in
    [ "a" ]
end
