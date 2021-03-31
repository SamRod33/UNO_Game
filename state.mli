(** Representation of the game state.
    This module represents the data stored in game state.
    @author Yohanes K. (ysk27), Tise Alatise (ofa2)
    Date: 03/25/21
    
*)
open GameSettings

(** The abstract type of values representing the game state *)
type t

(** The type of representing a deck of cards *)
type deck 
(* = Card.t list *)

(** The type representing the result of an attempted card played. *)
type result =
  | Legal of t
  | Illegal

(** [top_card g] is the card that is on top of the played cards pile of 
    game [g]. *)
val top_card : t -> Card.t

(** [stack_penalty g] is the current stack penalty of game [g]. 
	Example: A number card has a stack penalty of 0 but a +2 card has stack
	penalty of 2. *)
val stack_penalty : t -> int

(** [current_player g] is the person whose turn it is in game [g]. *)
val current_player : t -> Player.t

(** [init_state c p_list] is the initial state of the game with customizable 
    cards [c] and players [p_list]
    Requires: top_card (init_state c p_list)] cannot be an effect card *)
val init_state : Card.t list -> Player.t list -> t

(** [play p c g] is the result of an attempted play by player [p] with card [c] 
    in game [g]: whether it's legal state or illegal state *)
val play : Player.t -> Card.t option -> t -> result