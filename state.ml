(** Implementation of State in UNO Date: 03/28/21

    @author Samuel Rodriguez, Yohanes Kidane (sar325, ysk27) *)

(* Abstraction Function: TODO
 *
 *  
 * ROUGH DRAFT: 
 *    - t.players is a list [player_1; player_2; ...; player_n] all of type 
 *      Player.t that represents [player_1, player_2, ..., player_n] such that 
 *      turn-order's presedence is in ascending order. Namely, it is player_1's 
 *      current turn.
 *    - I wanna say a state always has a non-empty deck?
 *
 * Representation Invariant: 
 *  
 * ROUGH DRAFT:
 *    - t.stack_penalty must be positive
 *    - I wanna say t.deck cannot be empty in state before 
 *      anything is done to state?
 *)

(** [NoPlayersFound] is an exc that is raised when no players are found
    when searching for the current player. *)
exception NoPlayersFound

(** [NoMoreCards] is an exc that is raised when accessing a card in
    [deck] when [deck] is empty. *)
exception NoMoreCards

open Card
open Player

type deck = Card.t list

type t = {
  deck : deck;
  starting_deck : deck;
  stack_penalty : int;
  top_card : Card.t;
  players : Player.t list;
}

type result =
  | Legal of t
  | GameOver of Player.t
  | Illegal

(** [top_card g] is the top card of the played pile.*)
let top_card g = g.top_card

(** [stack_penalty g] is the current draw penalty from action cards.*)
let stack_penalty g = g.stack_penalty

(** [current_player g] is the player who's turn is next in game [g].
    Raises NoPlayersFound if [g.players] is empty.*)
let current_player g =
  match g.players with [] -> raise NoPlayersFound | h :: t -> h

(** [shuffle lst] is the same as [lst] except its elements are in a
    different order. The new order is randomized. *)
let shuffle lst =
  List.map (fun x -> (Random.bits (), x)) lst
  |> List.fast_sort compare |> List.map snd

(** [init_deck cards] is the initial deck comprised of [cards] for an
    UNO game such that the first card in it is a non-action card. Raises
    NoMoreCards if [cards] is empty.*)
let rec init_deck (cards : Card.t list) =
  match cards with
  | [] -> raise NoMoreCards
  | h :: _ ->
      let actions = Card.actions h in
      if
        actions.skip || actions.reverse || fst actions.swap
        || actions.change_color
      then init_deck (shuffle cards)
      else cards

(** [remove_card deck] is [deck] with the top card removed. If doing so
    results in an empty deck, then this will be a fresh set of
    reshuffled cards from the starting deck. *)
let remove_card start_deck = function
  | [] -> raise NoMoreCards
  | h :: t -> if t = [] then init_deck start_deck else t

(** [draw_cards deck starting_deck acc n] is a tuple where the first
    item in the tuple is a list of cards drawn and the second item is
    the resulting deck after drawing [n] cards from the top of deck.*)
let rec draw_cards deck starting_deck acc = function
  | 0 -> (acc, deck)
  | n -> (
      match deck with
      | [] -> raise NoMoreCards
      | h :: t ->
          draw_cards
            (remove_card starting_deck deck)
            starting_deck (h :: acc) (n - 1))

(** [rotate_players players] is [players] with the head of [players]
    moved to the end of the list.*)
let rec rotate_players = function
  | [] -> raise NoPlayersFound
  | h :: t -> t @ [ h ]

(** applies swap, rotate, and skip effects from card [c] to players,
    respectively.*)
let swap_rotate_players players c =
  let swapped_hands =
    match players with
    | [] -> raise NoPlayersFound
    | h :: _ as no_swap ->
        if fst (actions c).swap then
          swap_hands (id h) (snd (actions c).swap) players
        else no_swap
  in
  let apply_rev =
    if (actions c).reverse then List.rev swapped_hands
    else rotate_players swapped_hands
  in
  if (actions c).skip then rotate_players apply_rev else apply_rev

(** [legal_play c1 c2] is true if playing c1 is valid on c2.*)
let legal_play (c1 : Card.t) (c2 : Card.t) =
  let effects_c1 = actions c1 in
  color c1 = color c2
  || effects_c1.change_color
  || (draw_penalty c1 = draw_penalty c2 && draw_penalty c1 > 0)
  || (digit c1 = digit c2 && digit c1 <> None)
  || effects_c1 = actions c2
     && (effects_c1.skip || effects_c1.reverse || fst effects_c1.swap)

(** [add_to_hand player cards] is [player] with [cards] added to their
    hand*)
let rec add_to_hand player cards = List.fold_left add_card player cards

(** [players_draw players deck starting_deck] is the tuple [(ps, cards)]
    where [ps] is a list of players with 7 cards in their hands and
    [cards] is the deck of cards after each player has drawn their
    cards.*)
let players_draw players deck starting_deck =
  let rec players_draw_help ps cards = function
    | 0 -> (ps, cards)
    | n -> (
        match ps with
        | [] -> raise NoPlayersFound
        | h :: t ->
            let tup = draw_cards cards starting_deck [] 7 in
            players_draw_help
              (t @ [ add_to_hand h (fst tup) ])
              (snd tup) (n - 1))
  in
  players_draw_help players deck (List.length players)

let init_state (cards : Card.t list) (players : Player.t list) =
  match init_deck cards with
  | [] -> raise NoMoreCards
  | h :: t as full_deck ->
      let players_draw_tup = players_draw players t full_deck in
      {
        deck = snd players_draw_tup;
        top_card = h;
        starting_deck = full_deck;
        stack_penalty = 0;
        players = fst players_draw_tup;
      }

(** [penalize g] is the new [g] after penalizing the current player in
    [g] with stack penalty if stack_penalty is not 0, otherwise the
    player is penalized by drawing a single card as their turn. *)
let penalize g =
  let penalty = if g.stack_penalty = 0 then 1 else g.stack_penalty in
  let card_split = draw_cards g.deck g.starting_deck [] penalty in
  let player' = add_to_hand (current_player g) (fst card_split) in
  {
    g with
    deck = snd card_split;
    stack_penalty = 0;
    players =
      (match g.players with
      | [] -> raise NoPlayersFound
      | h :: t -> rotate_players (player' :: t));
  }

(** [play_card c g] is the result of attempting to play card [c] in the
    state [g]. Legal st' where st' is the state after playing the [c],
    Gameover player' if player' has no more cards left, and Illegal
    otherwise . *)
let play_card c g =
  if
    List.mem c (Player.player_hand (current_player g))
    && legal_play c g.top_card
  then
    let player' = Player.remove_card (current_player g) c in
    if List.length (player_hand player') <> 0 then
      let stack_penalty' = g.stack_penalty + draw_penalty c in
      let players' =
        match g.players with
        | [] -> raise NoPlayersFound
        | h :: t -> swap_rotate_players (player' :: t) c
      in
      Legal
        {
          g with
          stack_penalty = stack_penalty';
          top_card = c;
          players = players';
        }
    else GameOver player'
  else Illegal

let play c g =
  match c with
  | Some card -> play_card card g
  | None -> Legal (penalize g)

let print g = g
