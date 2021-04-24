(** Representation of the computer logic. Date: 04/22/21.

    @author Keri Tenerowicz (kmt225) *)

(** [basic_action g] is the first playable from the left in the
    computer's hand in state [g]. None is returned if the cpu wants to
    draw a card. *)
val basic_action : State.t -> Card.t option

(** [action g] is the card the computer will play based on a give state
    [g]. None is returned if the cpu wants to draw a card. *)
val action : State.t -> Card.t option
