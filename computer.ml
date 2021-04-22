open State
open Player
open Card

(** [is_valid_card g c] is a true when a card [c] s a legal move in game
    [g] and false otherwise. *)
let is_valid_card g card =
  let s' = play (Some card) g in
  match s' with Legal s' -> true | _ -> false

(** [find_valid_card g h] is the first valid card option in a player's
    hand [h] from the left in a given state [g]. None is returned if the
    hand is empty. *)
let rec find_valid_card g hand =
  match hand with
  | [] -> None
  | h :: t -> if is_valid_card g h then Some h else find_valid_card g t

(** [action g] is the card the computer will play based on a give state
    [g]. None is returned if the cpu wants to draw a card. *)
let action g = find_valid_card g (player_hand (current_player g))

(*****************************************************************************
  All functions below are for determining a better move based on the
  current state ("AI").
  ****************************************************************************)

(** [next_player g c] is the player after the current player in game [g]
    after playing card [c]. *)
let next_player g c =
  let s' = play (Some c) g in
  match s' with Legal s' -> current_player s' | _ -> current_player g

(** [add_weight g c] is the list of tuples which each have the card [c]
    then the weight of the card based on its value in the current state
    of game [g]. *)
let add_weight g c =
  if
    (stack_penalty g > 0 || is_uno (next_player g c))
    && draw_penalty c > 0
  then (c, 0)
  else
    match actions c with
    | {
     skip = false;
     reverse = false;
     swap = false, _;
     change_color = false;
    } ->
        (c, 5)
    | _ -> (c, 10)

(** [comp a b] is -1 if the weight of the first card is greater, 0 if
    equal, and 1 otherwise. *)
let comp (a : Card.t * int) (b : Card.t * int) =
  Stdlib.compare (snd b) (snd a)

(** [lw_card g] is the lowest weighted card from the list of valid
    (playable) cards of the current player's hand in game [g]. *)
let lw_card g =
  let rec valid_hand acc hand =
    match hand with
    | [] -> acc
    | h :: t ->
        if is_valid_card g h then h :: acc else valid_hand hand acc
  in
  List.map (add_weight g)
    (g |> current_player |> player_hand |> valid_hand [])
  |> List.sort comp |> List.hd |> fst

(** [action g] is the card the computer will play based on a give state
    [g]. None is returned if the cpu wants to draw a card. *)
(*let action g = lw_card g*)
