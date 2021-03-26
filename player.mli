(** Representation of static player data.

    This module represents the data stored in player files.
    @author Keri T. (kmt225)
    Date: 03/25/21
    
*)
open Card

(** The abstract type of values representing the player. *)
type t

(** Raised when player does not have the card in its hand. *)
exception Card_Not_In_Hand

(** [name p] is the name of player [p] *)
val name : t -> string

(** [player_hand p] is player [p]'s hand of cards *)
val player_hand : t -> t.hand

(** [is_uno p] is true when a player's hand has 1 card or less, 
    false otherwise. *)
val is_uno : t -> bool

(** [add_card p c] is the player [p] after drawing the card [c]. *)
val add_card : t -> Card.t -> t

(** [remove_card p c] is the player [p]'s hand after remvoing card [c] 
    from player [p]'s hand.
    Throws [Card_Not_In_Hand] if the card is not in the hand. *)
val remove_card : t -> Card.t -> t
