(** Implementation of State in UNO

    @author Samuel Rodriguez, Yohanes Kidane (sar325, ysk27)
    Date: 03/28/21
*)

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

(** [NoPlayersFound] is an exc that is raised when no players 
    are found when searching for the current player. *)
 exception NoPlayersFound

 (** [NoMoreCards] is an exc that is raised when 
    accessing a card in [deck] when [deck] is empty. *)
 exception NoMoreCards

 type deck = Card.t list

 type t = {
  deck : deck;
  stack_penalty : int;
  top_card : Card.t;
  players : Player.t list;
}

 type result = 
  | Legal of t
  | Illegal

  (** [top_card g] is the top card of the played pile.*)
  let top_card g = g.top_card

  (** [stack_penalty g] is the current draw penalty from action cards.*)
  let stack_penalty g = g.stack_penalty

  (** [current_player g] is the player who's turn is next in game [g]. Raises
  NoPlayersFound if [g.players] is empty.*)
  let current_player g =
    match g.players with
    | [] -> raise NoPlayersFound
    | h :: t -> h

  (** [shuffle lst] is the same as [lst] except its 
      elements are in a different order. The new order is randomized. *)
  let shuffle lst = 
    List.map (fun x -> (Random.bits (), x)) lst
    |> List.fast_sort compare |> List.map snd

  (** [init_deck cards] is the initial deck comprised of [cards] for 
      an UNO game such that the first card in it is a non-action card. Raises
      NoMoreCards if [cards] is empty.*)
  let rec init_deck (cards : Card.t List) = 
    match cards with
    | [] -> raise NoMoreCards
    | h :: _ -> begin
      match h with 
      | None -> cards
      | _ -> init_deck (shuffle cards)
    end

  (** [remove_card deck] is [deck] without its top card. If doing so results
    in an empty [deck], then this will be an entirely new reshuffled deck. 
    EDIT: Might need to have access to the initial
    collection of cards for the entire game? *)
  let remove_card deck = failwith("TODO")
 
  (** [init_state c p_list] is the inital state of the game with cards [cards] 
  and players [players] *)
  let init_state (cards : Card.t list) (players : Player.t list) = 
    { 
      deck = init_deck cards; 
      stack_penalty = 0;
      players = players
    }

  (** [rotate_players p] is [p] with the player at the front of [p] moved to 
  the end of the list. Raises NoPlayersFound if [p] is empty.*)
  let rotate_players = function
  | [] -> NoPlayersFound
  | h :: t -> t @ [h]

  (** [play c g] is the state of the game after the current player plays 
  card option [c] in game state [g]. If [c] is None, then the player will draw 
  cards. If a legal move is played, a legal game state is returned. Otherwise, 
  Illegal is returned. *)
  let play (c : Card.t option) (g : t) = 
    match c with
    (** If [c] is Some card, then check if playing the card is legal. If playing
      the card is legal, then the card will be placed as the new top_card and
      the effects of the card will be applied. Then the player order will be
      rotated.*)
    | Some card -> begin
    end
    (** if [c] is None, then the current player will draw cards according to
      the current draw penalty. Then the player order will be rotated.*)
    | None ->
