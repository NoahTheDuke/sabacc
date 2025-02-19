(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

open Action
open Card
open Deck
open Hand
open Location
open Player
open Players
open Suite
open Utils

type new_game_options = {
  starting_chips : int;
  players : Player.t list;
}

type game_step =
  | Setup
  | StartOfRound
  | Action
  | Decision
  | CheckTurn
  | EndOfRound
  | Results
[@@deriving show]

type t = {
  running : bool;
  red_deck : Deck.t;
  yellow_deck : Deck.t;
  starting_chips : int;
  players : Players.t;
  turn_order : string list;
  round : int;
  turn : int;
  current_player : int;
  step : game_step;
  winners : string list option;
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
    List.fold_left
      (fun (red_deck, yellow_deck, players) (player : Player.t) ->
        match player.active with
        | true ->
            let red_deck, red_card = Deck.draw Deck red_deck in
            let yellow_deck, yellow_card = Deck.draw Deck yellow_deck in
            let player =
              {
                player with
                place = 0;
                hand = Some { red = red_card; yellow = yellow_card; value = None };
              }
            in
            (red_deck, yellow_deck, players @ [ player ])
        | false -> (red_deck, yellow_deck, players @ [ player ]))
      (game.red_deck, game.yellow_deck, [])
      (Players.to_list game.players)
  in
  { game with red_deck; yellow_deck; players = Players.create players }

let create (options : new_game_options) : t =
  assert (List.length options.players > 0);
  assert (List.length options.players <= 4);
  let red_deck = Deck.create Red in
  let yellow_deck = Deck.create Yellow in
  {
    running = true;
    red_deck;
    yellow_deck;
    starting_chips = options.starting_chips;
    players = Players.create options.players;
    turn_order = options.players |> List.map (fun (p : Player.t) -> p.name);
    round = 1;
    turn = 1;
    current_player = -1;
    step = Setup;
    winners = None;
  }

let get_current_player (game : t) : Player.t =
  let name = List.nth game.turn_order game.current_player in
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
        let suite = drawn.suite in
        let existing = Hand.by_suite suite (Option.get hand) in
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
  game |> update_player player |> set_step CheckTurn

let stand_action (action : [ `Stand ] action) (game : t) : t =
  let player = get_player action.player game in
  game |> update_player { player with status = Standing } |> set_step CheckTurn

let shift_action (_action : [ `Shift ] action) (game : t) : t = game

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
  | Draw (suite, _location) -> Printf.sprintf "Draw from %s deck" (Suite.show suite)
  | ChooseDrawn Drawn ->
      let player = get_current_player game in
      let drawn = Option.get player.drawn in
      Printf.sprintf "Choose to keep %s" (Card.show drawn)
  | ChooseDrawn Hand ->
      let player = get_current_player game in
      let card =
        Hand.by_suite (Option.get player.drawn).suite (Option.get player.hand)
      in
      Printf.sprintf "Choose to keep %s" (Card.show card)
  | Stand -> "Stand"
  | Shift -> "Use shift token"

let display (game : t) : unit =
  print_newline ();
  let current_player_name = get_current_player_name game in
  Printf.printf "Round: %i, Turn: %i, Current player: %s\n" game.round game.turn
    current_player_name;
  Printf.printf "Discards: %s, %s"
    (Deck.discard_str game.red_deck)
    (Deck.discard_str game.yellow_deck);
  print_newline ();
  print_endline "Players:";
  List.iter
    (fun player ->
      match player.active with
      | true ->
          Printf.printf "  %s - Chips: %i, Invested: %i\n" player.name player.chips
            player.invested_chips
      | false -> Printf.printf "  %s - Out of the game\n" player.name)
    (Players.to_list game.players);
  Printf.printf "Your hand (%s):" current_player_name;
  let primary = StringMap.find current_player_name game.players in
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

let setup_step (game : t) : t =
  let game =
    List.fold_left
      (fun game player ->
        update_player { player with chips = game.starting_chips } game)
      game
      (Players.to_list game.players)
  in
  game |> deal |> set_step StartOfRound

let start_of_round (game : t) : t =
  let game = { game with turn = 1; current_player = 0 } in
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

let check_turn_step (game : t) : t =
  let player = { (get_current_player game) with took_action = true } in
  let game = update_player player game in
  let next_player = succ game.current_player in
  let game =
    { game with current_player = next_player mod List.length game.turn_order }
  in
  if Players.all_took_action game.players then
    let new_turn = succ game.turn in
    let players = Players.reset_action game.players in
    let turn = if new_turn = 4 then 1 else new_turn in
    let game = { game with turn; players } in
    match new_turn with
    | 4 -> game |> set_step EndOfRound
    | _ -> game |> set_step Action
  else game |> set_step Action

(** [roll_imposter card] will change card's rank to [Imposter (Some n)] if it's
    [Imposter None], and otherwise will return the card unchanged. *)
let roll_imposter (card : Card.t) : Card.t =
  match card.rank with
  | Imposter None ->
      let val1 = Random.int_in_range ~min:1 ~max:6 in
      let val2 = Random.int_in_range ~min:1 ~max:6 in
      let new_val = if val1 < val2 then val1 else val2 in
      { card with rank = Rank.imp_of_int new_val }
  | _ -> card

let calculate_hand (hand : Hand.t) =
  match (hand.red.rank, hand.yellow.rank) with
  (* pure sabacc: two sylops *)
  | Sylop, Sylop -> PureSabacc
  (* regular sabacc: a pair or a pair with imposter or a pair with sylop *)
  | Sylop, Value n
  | Value n, Sylop
  | Sylop, Imposter (Some n)
  | Imposter (Some n), Sylop ->
      PairSabacc n
  (* non-sabacc: two cards *)
  | Value n1, Value n2
  | Value n1, Imposter (Some n2)
  | Imposter (Some n1), Value n2
  | Imposter (Some n1), Imposter (Some n2) ->
      if n1 = n2 then PairSabacc n1 else Other (abs (n1 - n2))
  (* Imposter None, shouldn't be possible *)
  | _, _ -> raise (Failure "calculate_hand received Imposter None")

let compare_hands hand1 hand2 : int =
  match (hand1, hand2) with
  | Some v1, Some v2 -> (
      match (v1, v2) with
      | PureSabacc, _ -> -1
      | _, PureSabacc -> 1
      | PairSabacc n1, PairSabacc n2 -> n1 - n2
      | PairSabacc _n1, Other _n2 -> -1
      | Other _n1, PairSabacc _n2 -> 1
      | Other n1, Other n2 -> n1 - n2)
  | _, _ -> raise (Failure "compare_hands received a None hand.value")

let calculate_round (game : t) : t =
  (* update player hands with imposter values and hand values *)
  let game =
    game.players |> Players.active_to_list
    |> List.fold_left
         (fun game player ->
           let hand = Option.get player.hand in
           (* roll for each imposter *)
           let hand =
             {
               hand with
               red = roll_imposter hand.red;
               yellow = roll_imposter hand.yellow;
             }
           in
           (* then calculate the hand's value *)
           let hand = Some { hand with value = Some (calculate_hand hand) } in
           let player = { player with hand } in
           update_player player game)
         game
  in
  (* determine everyone's place by sorting per hand value *)
  let players_by_hand =
    game.players |> Players.active_to_list
    |> group_by (fun player -> (Option.get player.hand).value)
  in
  let game =
    players_by_hand |> Hashtbl.to_seq_keys |> List.of_seq
    |> List.stable_sort compare_hands
    |> List.mapi (fun index hand_value -> (index + 1, hand_value))
    |> List.fold_left
         (fun game (index, hand_value) ->
           let players = Hashtbl.find players_by_hand hand_value in
           List.fold_left
             (fun game player -> update_player { player with place = index } game)
             game players)
         game
  in
  (* find the only player with place 1 *)
  let winners =
    game.players
    |> StringMap.filter (fun _ player -> player.place = 1)
    |> StringMap.to_list
    |> List.map (fun (player_name, _) -> player_name)
  in
  let game = { game with winners = Some winners } in
  game

(** [tax_hand player] returns player with chips and invested_chips as such:

    * if round winner (place = 1), get invested chips back. if a pair sabacc (losing
    sabacc hand), taxed 1 chip. if non-sabacc hand, taxed diff of hand (Other). *)
let tax_hand (player : Player.t) : Player.t =
  match player.place with
  | 1 ->
      if player.invested_chips > 0 then
        Printf.printf "%s gets %i invested chips back into their stock.\n" player.name
          player.invested_chips;
      { player with chips = player.chips + player.invested_chips; invested_chips = 0 }
  | _ -> (
      match Option.get (Option.get player.hand).value with
      | PairSabacc n
      | Other n ->
          let tax = if player.chips - n < 0 then player.chips else n in
          let cs = if tax = 1 then "chip" else "chips" in
          Printf.printf "%s is taxed %i %s.\n" player.name tax cs;
          {
            player with
            chips = player.chips - tax;
            invested_chips = player.invested_chips + tax;
          }
      | _ -> player)

let lose_invested_chips (game : t) : t =
  game.players |> Players.active_to_list
  |> List.fold_left
       (fun game player ->
         let player = tax_hand player in
         if player.invested_chips > 0 then
           Printf.printf "%s loses %i invested %s.\n" player.name player.invested_chips
             (match player.invested_chips with
             | 1 -> "chip"
             | _ -> "chips")
         else Printf.printf "%s is not taxed.\n" player.name;
         game |> update_player { player with invested_chips = 0 })
       game

let print_end_of_round (game : t) : unit =
  print_endline "\nRound hands:";
  game.players |> Players.active_to_list
  |> List.stable_sort (fun p1 p2 -> p1.place - p2.place)
  |> List.iter (fun player ->
         Printf.printf " %i) %s: %s\n" player.place player.name
           (player.hand |> Option.get |> Hand.show));
  print_newline ();
  Printf.printf "Round winner: %s"
    (game.winners |> Option.value ~default:[ "No winner" ] |> String.concat ", ");
  print_newline ();
  print_newline ()

let rotate_turn_order (game : t) : t =
  let game = { game with players = Players.set_active game.players } in
  let turn_order =
    game.turn_order
    |> List.filter_map (fun player_name ->
           let player = get_player player_name game in
           match player.active with
           | true -> Some player.name
           | false -> None)
    |> fun t_o -> cycle t_o 1
  in
  { game with turn_order }

let end_of_round_step (game : t) : t =
  let game = game |> calculate_round in
  print_end_of_round game;
  let game = game |> lose_invested_chips in
  let () =
    print_newline ();
    print_endline "Press any key to continue to next round.";
    let _ = read_line () in
    print_newline ();
    print_newline ()
  in
  let game = game |> rotate_turn_order |> deal in
  match Players.active_to_list game.players with
  | [] ->
      print_endline "No one wins! fuck you!";
      raise (Failure "all done")
  | [ player ] -> { game with winners = Some [ player.name ] } |> set_step Results
  | _ -> { game with round = succ game.round } |> set_step StartOfRound

let results_step (game : t) : t =
  Printf.printf "%s has won the game! congrats!\n"
    (game.winners |> Option.get |> List.hd);
  let game = { game with running = false } in
  game

let step_game game =
  match game.step with
  | Setup -> game |> setup_step
  | StartOfRound -> game |> start_of_round
  | Action
  | Decision ->
      game |> action_step
  | CheckTurn -> game |> check_turn_step
  | EndOfRound -> game |> end_of_round_step
  | Results -> game |> results_step

let rec game_loop (game : t) =
  if game.running then (
    let game = step_game game in
    (match game.step with
    | Action -> display game
    | Decision -> display game
    | _ -> ());
    game_loop game)
  else game

let start (game : t) = game_loop game
