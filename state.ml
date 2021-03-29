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

 open Queue

 type deck = Card.t list


 type t = {
  deck : deck;
  stack_penalty : int;
  top_card : Card.t;
  players : Player.t Queue.t;
}

 type result = 
  | Legal of t
  | Illegal

  (** [top_card g] is the top card of the played pile.*)
  let top_card g = g.top_card

  (** [stack_penalty g] is the current draw penalty from action cards.*)
  let stack_penalty g = g.stack_penalty

  (** [current_player g] is the player who's turn is next in game [g].*)
  let current_player g = 
    if is_empty g.players then raise NoPlayersFound
    else peek g.players

  (** [shuffle lst] is the same as [lst] except its 
      elements are in a different order. The new order is randomized. *)
  let shuffle lst = 
    List.map (fun x -> (Random.bits (), x)) lst
    |> List.fast_sort compare |> List.map snd

  (** [init_deck cards] is the initial deck comprised of [cards] for 
      an UNO game such that the first card in it is a non-action card.*)
  let rec init_deck cards = 
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
 
  let init_state c p_list = 
    { 
      deck = init_deck c; 
      stack_penalty = 0;
      players = p_list
    }

  (** [play p c g] is the state of the game after player [p] plays card [c] in 
    game state [g]. If a legal move is played, a legal game state is returned.
    Otherwise, Illegal is returned. *)
  let play p c g = 
    match c with
    | Some card -> begin
    end
    | None ->
