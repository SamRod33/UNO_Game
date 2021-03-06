(** Representation of the game state. Date: 03/25/21 This module
    represents the data stored in game state.

    @author Yohanes K. (ysk27), Tise Alatise (ofa2) *)

(** The abstract type of values representing the game state *)
type t

(** The type of representing a deck of cards *)
type deck

(* = Card.t list *)

(** The type representing the result of an attempted card played. *)
type result =
  | Legal of t
  | GameOver of Player.t
  | Illegal

(** [top_card g] is the card that is on top of the played cards pile of
    game [g]. *)
val top_card : t -> Card.t

(** [stack_penalty g] is the current stack penalty of game [g]. Example:
    A number card has a stack penalty of 0 but a +2 card has stack
    penalty of 2. *)
val stack_penalty : t -> int

(** [current_player g] is the person whose turn it is in game [g]. *)
val current_player : t -> Player.t

(** [players g] is the list of players in game [g]. *)
val players : t -> Player.t list

(** [init_state c p_list] is the initial state of the game with
    customizable cards [c] and players [p_list] Requires:
    [top_card(init_state c p_list)] cannot be an effect card *)
val init_state : Card.t list -> Player.t list -> t

(** [play c g] is the state of the game after the current player plays
    card option [c] in game state [g]. If [c] is None, then the player
    will draw cards. If c is Some card, then [color card] must not be
    ANY (the color that the card will take must already be decided. If a
    legal move is played, a legal game state is returned. If the current
    player in [g] has an empty player hand then Gameover player is
    returned. If Otherwise, Illegal is returned. *)
val play : Card.t option -> t -> result

(** [change_player_hand remc addc g] is [g] with the current player's
    hand in [g.players] with [remc] removed and [addc] added. Requires:
    [remc] is in the hand of the current player.*)
val change_current_players_hand : Card.t -> Card.t -> t -> t

(** [t_test d s sp tc p] is a state that is created from the given deck
    [d], starting deck [s], stack penalty [sp], top card [tc], and
    player list [p]. *)
val t_test :
  Card.t list -> Card.t list -> int -> Card.t -> Player.t list -> t
