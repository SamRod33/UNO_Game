(** Representation of static card data. This module represents the data
    stored in card files. It handles loading of that data from JSON as
    well as querying the data.

    @author: Samuel Rodriguez (sar325) Date: 03/26/21 *)

(** The abstract type of values representing a card. *)
type t

(** The type of card's color. *)
type color =
  | R
  | G
  | B
  | Y
  | ANY

(** The type of card's draw penalty. *)
type draw_penalty = int

(** The type of card's action. *)
type actions = {
  skip : bool;
  reverse : bool;
  swap : bool;
  change_color : bool;
}

(** [create j] is the card that [j] represents. Requires: [j] is a valid
    JSON adventure representation. *)
val create : Yojson.Basic.t -> t

(** [draw_penalty c] is the identifier of the draw penalty in card [c]. *)
val draw_penalty : t -> draw_penalty

(** [color c] is the identifier of the color of card [c]. *)
val color : t -> color

(** [action c] is the identifier of the action of card [c]. *)
val actions : t -> actions

(** [amount c] is the amount of card [c] in the standard deck of a game. *)
val amount : t -> int

(** [digit c] is the number on the face of the card. *)
val digit : t -> int option

(** [standard_cards] is the Card.t list of cards in a standard UNO deck. *)
val standard_cards : t list
