(** Implementation of State in UNO

    @author Samuel Rodriguez (sar325)
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
  players : Player.t list;
}

 type result = 
  | Legal of t
  | Illegal

  let top_card g = 
    match g.deck with
    | [] -> raise NoMoreCards
    | h :: _ -> h

  let stack_penalty g = g.stack_penalty

  let current_player g = 
    match g.players with 
    | [] -> raise NoPlayersFound
    | h :: _ -> h

  (** [init_deck cards] is the initial deck comprised of [cards] for 
      an UNO game such that the first card in it is a non-action card.*)
  let init_deck cards = failwith("TODO")

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

  let play p c g = failwith("TODO")
