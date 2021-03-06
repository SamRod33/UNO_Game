(** Implementation of State in UNO Date: 03/28/21

    @author Samuel Rodriguez, Yohanes Kidane (sar325, ysk27) *)

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

let top_card g = g.top_card

let stack_penalty g = g.stack_penalty

let current_player g =
  match g.players with [] -> raise NoPlayersFound | h :: t -> h

let players g = g.players

(** [shuffle lst] is the same as [lst] except its elements are in a
    different order. The new order is randomized. *)
let shuffle lst =
  Random.self_init ();
  List.map (fun x -> (Random.bits (), x)) lst
  |> List.fast_sort compare |> List.map snd

(** [init_deck cards] is the initial deck comprised of [cards] for an
    UNO game such that the first card in it is a non-action card. Raises
    NoMoreCards if [cards] is empty.*)
let rec init_deck (cards : Card.t list) =
  match shuffle cards with
  | [] -> raise NoMoreCards
  | h :: _ as shuffled ->
      let card_actions = Card.actions h in
      if
        card_actions.skip || card_actions.reverse
        || fst card_actions.swap || card_actions.change_color
        || draw_penalty h > 0
        || color h = ANY
      then init_deck shuffled
      else shuffled

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

(** [swap_rotate_players players c] applies swap, rotate, and skip
    effects from card [c] to players [players], respectively.*)
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
  if (actions c).skip || ((actions c).reverse && List.length players = 2)
  then rotate_players apply_rev
  else apply_rev

(** [legal_play c1 c2] is true if playing c1 is valid on c2.*)
let legal_play (c1 : Card.t) (c2 : Card.t) penalty =
  let effects_c1 = actions c1 in
  if penalty > 0 && draw_penalty c1 <> draw_penalty c2 then false
  else
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

(** [new_state_aux g stack_penalty' c players] creates a new gamestate
    given originla gamestate [g] with a new stack penalty
    [stack_penalty'], new top card [c], and new players [players']. *)
let new_state_aux g stack_penalty' c players' =
  {
    g with
    stack_penalty = stack_penalty';
    top_card = c;
    players = players';
  }

(** [play_card c g] is the result of attempting to play card [c] in the
    state [g]. Legal st' where st' is the state after playing the [c],
    Gameover player' if player' has no more cards left, and Illegal
    otherwise. *)
let play_card c g =
  if
    List.mem c (Player.player_hand (current_player g))
    && legal_play c g.top_card g.stack_penalty
  then
    let player' = Player.remove_card (current_player g) c in
    if List.length (player_hand player') <> 0 then
      let stack_penalty' = g.stack_penalty + draw_penalty c in
      let players' =
        match g.players with
        | [] -> raise NoPlayersFound
        | h :: t -> swap_rotate_players (player' :: t) c
      in
      Legal (new_state_aux g stack_penalty' c players')
    else GameOver player'
  else Illegal

let play c g =
  match c with
  | Some card -> play_card card g
  | None -> Legal (penalize g)

let change_current_players_hand remc addc g =
  match g.players with
  | [] -> raise NoPlayersFound
  | h :: t ->
      let remve_card = Player.remove_card h remc in
      let added_card = Player.add_card remve_card addc in
      { g with players = added_card :: t }

let t_test (d : Card.t list) (s : Card.t list) sp tc p =
  {
    deck = d;
    starting_deck = s;
    stack_penalty = sp;
    top_card = tc;
    players = p;
  }
