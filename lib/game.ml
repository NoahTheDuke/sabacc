(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

(* internal *)
open Action
open Card
open Deck
open Hand
open Location
open Player
open Suite
open Utils

(* external *)
open Lwt.Infix

type new_game_options = {
  starting_chips : int;
  players : Player.t list;
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

type t = {
  running : bool;
  red_deck : Deck.t;
  yellow_deck : Deck.t;
  starting_chips : int;
  players : Players.t;
  turn_order : string array;
  round : int;
  turn : int;
  starting_player : int;
  current_player : int;
  step : game_step;
}
[@@deriving show]

let get_player (name : string) (game : t) : Player.t = StringMap.find name game.players

let update_player (player : Player.t) (game : t) : t =
  let players = StringMap.add player.name player game.players in
  { game with players }

let update_players (players : player_map) (game : t) : t = { game with players }

let create_player (player_name : string) : Player.t = Player.create player_name

let update_deck (deck : Deck.t) (game : t) : t =
  match deck.suite with
  | Red -> { game with red_deck = deck }
  | Yellow -> { game with yellow_deck = deck }

let suite_to_deck (suite : Suite.t) (game : t) : Deck.t =
  match suite with
  | Red -> game.red_deck
  | Yellow -> game.yellow_deck

type redeal = {
  red_deck : Deck.t;
  yellow_deck : Deck.t;
  players : Players.t;
}

let deal (game : t) : t =
  let red_deck, yellow_deck, players =
    Seq.fold_left
      (fun (red_deck, yellow_deck, players) (player : Player.t) ->
        let red_deck, red_card = Deck.draw Deck red_deck in
        let yellow_deck, yellow_card = Deck.draw Deck yellow_deck in
        let player =
          { player with hand = Some { red = red_card; yellow = yellow_card } }
        in
        (red_deck, yellow_deck, players @ [ player ]))
      (game.red_deck, game.yellow_deck, [])
      (Players.to_seq game.players)
  in
  { game with red_deck; yellow_deck; players = Players.create players }

let create (options : new_game_options) : t =
  assert (List.length options.players > 0);
  assert (List.length options.players <= 4);
  let red_deck = Deck.create Red in
  let yellow_deck = Deck.create Yellow in
  let game =
    {
      running = false;
      red_deck;
      yellow_deck;
      starting_chips = options.starting_chips;
      players = Players.create options.players;
      turn_order =
        options.players |> List.map (fun (p : Player.t) -> p.name) |> Array.of_list;
      round = 0;
      turn = 0;
      starting_player = -1;
      current_player = -1;
      step = Setup;
    }
  in
  game |> deal

let get_current_player (game : t) : Player.t =
  let name = Array.get game.turn_order game.current_player in
  StringMap.find name game.players

let get_current_player_name (game : t) : string = (get_current_player game).name

let discard (card : Card.t) (game : t) : t =
  let deck = suite_to_deck card.suite game in
  game |> update_deck (Deck.update_discard card deck)

let set_step (step : game_step) (game : t) : t = { game with step }

let draw_from_deck (location : Location.t) (suite : Suite.t) (game : t) : t * Card.t =
  let deck = suite_to_deck suite game in
  let deck, new_card = Deck.draw location deck in
  (update_deck deck game, new_card)

let draw_action (action : [ `Draw ] action) (game : t) : t =
  let player = get_player action.player game in
  match action.a_type with
  | Draw (suite, location) ->
      let game, new_card = draw_from_deck location suite game in
      game
      |> update_player
           {
             player with
             drawn = Some new_card;
             chips = player.chips - 1;
             invested_chips = player.invested_chips + 1;
           }
      |> set_step Decision

exception WrongLocation of string

let choose_drawn_action (action : [ `ChooseDrawn ] action) (game : t) : t =
  let player = get_player action.player game in
  let hand = player.hand in
  let drawn = Option.get player.drawn in
  let game, player =
    match action.a_type with
    | ChooseDrawn Drawn ->
        let existing =
          (drawn |> fun c -> c.suite) |> fun s -> Hand.by_suite s (Option.get hand)
        in
        let hand =
          Option.map
            (fun (h : Hand.t) ->
              match drawn.suite with
              | Red -> { h with red = drawn }
              | Yellow -> { h with yellow = drawn })
            hand
        in
        let player = { player with drawn = None; hand } in
        (discard existing game, player)
    | ChooseDrawn Hand ->
        let player = { player with drawn = None } in
        (discard drawn game, player)
  in
  game |> update_player player |> set_step PassTurn

let stand_action (action : [ `Stand ] action) (game : t) : t =
  let player = get_player action.player game in
  game |> update_player { player with status = Standing } |> set_step PassTurn

let shift_action (action : [ `Shift ] action) (game : t) : t = game

let available_actions (game : t) =
  let current_player = get_current_player game in
  match current_player.drawn with
  | Some _ -> [ ChooseDrawn Drawn; ChooseDrawn Hand ]
  | None ->
      let actions = ref (Pvec.empty ()) in
      if current_player.chips > 0 then begin
        (match Deck.available game.red_deck with
        | Some _ -> actions := Pvec.append (Draw (Red, Discard)) !actions
        | None -> ());
        actions := Pvec.append (Draw (Red, Deck)) !actions;
        (match Deck.available game.yellow_deck with
        | Some _ -> actions := Pvec.append (Draw (Yellow, Discard)) !actions
        | None -> ());
        actions := Pvec.append (Draw (Yellow, Deck)) !actions
      end;
      actions := Pvec.append Stand !actions;
      (*actions := Pvec.append Shift !actions;*)
      Pvec.to_list !actions

let verify_action (action : 'a action) (game : t) : bool =
  let actions = available_actions game in
  List.exists
    (fun a ->
      match (a, action.a_type) with
      | Draw (s1, l1), Draw (s2, l2) -> Suite.equal s1 s2 && Location.equal l1 l2
      | ChooseDrawn l1, ChooseDrawn l2 -> choose_location_equal l1 l2
      | Stand, Stand -> true
      | Shift, Shift -> true
      | _ -> false)
    actions

let handle_action (action : 'a action) (game : t) : t =
  if verify_action action game then
    match action with
    | { a_type = Draw _; _ } as action -> draw_action action game
    | { a_type = ChooseDrawn _; _ } as action -> choose_drawn_action action game
    | { a_type = Stand; _ } as action -> stand_action action game
    | { a_type = Shift; _ } as action -> shift_action action game
  else (
    Printf.printf "Can't use %s now" (show_action action);
    game)

let show_action_type a_t (game : t) : string =
  match a_t with
  | Draw (suite, Discard) ->
      Printf.sprintf "Draw from %s discard: %s" (Suite.show suite)
        ( game |> suite_to_deck suite |> Deck.available |> fun c ->
          match c with
          | Some c -> Card.show c
          | None -> "[Empty]" )
  | Draw (suite, location) -> Printf.sprintf "Draw from %s deck" (Suite.show suite)
  | ChooseDrawn Drawn ->
      let player = get_current_player game in
      let drawn = Option.get player.drawn in
      Printf.sprintf "Choose %s from discard" (Card.show drawn)
  | ChooseDrawn Hand ->
      let player = get_current_player game in
      let card =
        Hand.by_suite (Option.get player.drawn).suite (Option.get player.hand)
      in
      Printf.sprintf "Choose %s from hand" (Card.show card)
  | Stand -> "Stand"
  | Shift -> "Use shift token"

let display (game : t) : unit =
  let current_player_name = get_current_player_name game in
  Printf.printf "Turn: %i, Round: %i, Current player: %s\n" game.turn game.round
    current_player_name;
  print_endline "Decks:";
  Deck.display game.red_deck;
  print_newline ();
  Deck.display game.yellow_deck;
  print_newline ();
  print_endline "Players:";
  Array.iter
    (fun name ->
      let player = get_player name game in
      Printf.printf "  %s - Chips: %i, Invested: %i\n" player.name player.chips
        player.invested_chips)
    game.turn_order;
  Printf.printf "Your hand (%s):" current_player_name;
  let primary = StringMap.find (Array.get game.turn_order 0) game.players in
  (match primary.hand with
  | Some h -> Printf.printf "  %s\n" (Hand.show h)
  | None -> ());
  (match primary.drawn with
  | Some card -> Printf.printf "  Drawn: %s\n" (Card.show card)
  | None -> ());
  Printf.printf "Available actions:\n%s"
    (available_actions game
    |> List.mapi (fun i a -> Printf.sprintf "  %i) %s" i (show_action_type a game))
    |> String.concat "\n");
  print_newline ();
  flush stdout

let start_of_round (game : t) : t =
  let starting_player = succ game.starting_player mod 3 in
  let current_player = starting_player in
  let game =
    { game with turn = 1; round = succ game.round; starting_player; current_player }
  in
  let name = get_current_player_name game in
  Printf.printf "New round: %i\n Starting player is %s, current player is %s\n"
    game.round name name;
  game |> set_step Action

let action_step (game : t) : t =
  let actions = available_actions game in
  let choice = read_int () in
  match List.nth_opt actions choice with
  | Some a_t ->
      handle_action { a_type = a_t; player = get_current_player_name game } game
  | None ->
      print_endline "Wrong choice, try again";
      game

let pass_turn_step (game : t) : t =
  print_endline "pass_turn_step";
  let player = { (get_current_player game) with took_turn = true } in
  let next_player = succ game.current_player in
  let game = update_player player game in
  let game =
    { game with current_player = next_player mod Array.length game.turn_order }
  in
  match Players.all_took_turns game.players with
  | true ->
      let turn = succ game.turn mod 3 in
      let starting_player =
        succ game.starting_player mod Array.length game.turn_order
      in
      let players = Players.reset game.players in
      { game with turn; starting_player; players } |> set_step StartOfRound
  | false -> game |> set_step Action

let end_of_round_step (game : t) : t =
  (* reset_players_and_decks game in *)
  match game.round with
  | 3 -> game |> set_step Results
  | n -> { game with round = succ n } |> set_step StartOfRound

let step_game game =
  match game.step with
  | Setup -> game |> set_step StartOfRound
  | StartOfRound -> game |> start_of_round
  | Action
  | Decision ->
      game |> action_step
  | PassTurn -> game |> pass_turn_step
  | EndOfRound -> game |> end_of_round_step
  | Results -> { game with running = false }

let rec game_loop (game : t) =
  if game.running then (
    let game = step_game game in
    (match game.step with
    | Action -> display game
    | Decision -> display game
    | _ -> ());
    game_loop game)
