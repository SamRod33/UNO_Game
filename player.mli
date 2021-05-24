(** Representation of static player data.

    This module represents the data stored in player files.

    Date: 04/14/21

    @author Keri T. (kmt225) Samuel Rodriguez (sar325) *)

(** The abstract type of values representing the player. *)
type t

(** Raised when player does not have the card in its hand. *)
exception CardNotInHand of Card.t

(** [create name is_cpu id_num] is a new player with playername [name]
    and id [id_num] with an empty hand. The player is a computer iff
    [is_cpu] is true. *)
val create : string -> bool -> int -> t

(** [create_test name h is_cpu] is a new player with playername [name]
    and hand [h]. The player is a computer iff [is_cpu] is true. *)
val create_test : string -> Card.t list -> bool -> t

(** [name p] is the name of player [p] *)
val name : t -> string

(** [player_hand p] is player [p]'s hand of cards *)
val player_hand : t -> Card.t list

(** [swap_hands p1_id p2_id players] is [players] with p1 and p2's hands
    swapped where [p1_id] and [p2_id] are the id's of p1 and p2,
    respectively. Fails if p1 and p2 are not in [players].*)
val swap_hands : int -> int -> t list -> t list

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

(** [is_cpu] is true when the player is a computer, false otherwise. *)
val is_cpu : t -> bool
