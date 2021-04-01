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
        actions.skip || actions.reverse || actions.swap
        || actions.change_color
      then init_deck (shuffle cards)
      else cards

(** [remove_card deck] is [deck] without its top card. If doing so
    results in an empty [deck], then this will be an entirely new
    reshuffled deck. EDIT: Might need to have access to the initial
    collection of cards for the entire game? *)
let remove_card_from_deck starting_deck = function
  | [] -> raise NoMoreCards
  | h :: t -> if t = [] then init_deck starting_deck else t

let init_state (cards : Card.t list) (players : Player.t list) =
  match init_deck cards with
  | [] -> raise NoMoreCards
  | h :: t as full_deck ->
      {
        deck = t;
        top_card = h;
        starting_deck = full_deck;
        stack_penalty = 0;
        players;
      }

(** [rotate_players p players] is [players] with the player at the front
    of [players] removed and [p] put at the end of [players] Raises
    NoPlayersFound if [players] is empty.*)
let rotate_players p = function
  | [] -> raise NoPlayersFound
  | h :: t -> t @ [ p ]

(** [legal_play c1 c2] is true if playing c1 is valid on c2.*)
let legal_play (c1 : Card.t) (c2 : Card.t) =
  color c1 = color c2
  || (actions c1).change_color
  || draw_penalty c1 = draw_penalty c2
  || amount c1 = amount c2
  || actions c1 = actions c2

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
            (remove_card_from_deck starting_deck deck)
            starting_deck (h :: acc) (n - 1))

(** [add_cards_to_player player cards] is player with [cards] added to
    their hand*)
let rec add_cards_to_player player cards =
  List.fold_left add_card player cards

let play (c : Card.t option) (g : t) =
  let player = current_player g in
  match c with
  | Some card ->
      if List.mem card (Player.player_hand player) = false then Illegal
      else if legal_play card g.top_card then
        let updated_player = Player.remove_card player card in
        if List.length (player_hand updated_player) = 0 then
          GameOver updated_player
        else
          let updated_player_list =
            rotate_players updated_player g.players
          in
          let new_stack_penalty = g.stack_penalty + draw_penalty card in
          Legal
            {
              g with
              stack_penalty = new_stack_penalty;
              top_card = card;
              players = updated_player_list;
            }
      else Illegal
  | None ->
      let num_draw =
        if g.stack_penalty = 0 then 1 else g.stack_penalty
      in
      let cards_tup = draw_cards g.deck g.starting_deck [] num_draw in
      let cards_drawn = fst cards_tup in
      let new_deck = snd cards_tup in
      let updated_player = add_cards_to_player player cards_drawn in
      Legal
        {
          g with
          deck = new_deck;
          stack_penalty = 0;
          players = rotate_players updated_player g.players;
        }
