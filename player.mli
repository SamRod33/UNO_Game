(** Representation of static player data.

    This module represents the data stored in player files.

    Date: 04/14/21

    @author Keri T. (kmt225) Samuel Rodriguez (sar325) *)

(** The abstract type of values representing the player. *)
type t

(** Raised when player does not have the card in its hand. *)
exception CardNotInHand of Card.t

(** [create name] is a new player with playername [name] with an empty
    hand*)
val create : string -> t

(** [name p] is the name of player [p] *)
val name : t -> string

(** [player_hand p] is player [p]'s hand of cards *)
val player_hand : t -> Card.t list

(** [is_uno p] is true when a player's hand has 1 card or less, false
    otherwise. *)
val is_uno : t -> bool

(** [add_card p c] is the player [p] after drawing the card [c]. *)
val add_card : t -> Card.t -> t

(** [remove_card p c] is the player [p]'s hand after remvoing card [c]
    from player [p]'s hand. Throws [CardNotInHand] if the card is not in
    the hand. *)
val remove_card : t -> Card.t -> t

(** [id p] is the id of player [p] *)
val id : t -> int
