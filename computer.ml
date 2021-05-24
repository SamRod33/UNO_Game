open State
open Player
open Card

(** [max r g b y largest] is the largest integer in of the numbers [r],
    [g], [b], [y]. *)
let max r g b y largest =
  let rec max_int (lst : int list) (largest, index) i =
    match lst with
    | [] -> (largest, index)
    | h :: t ->
        if h > largest then max_int t (h, i) (i + 1)
        else max_int t (largest, index) (i + 1)
  in
  match snd (max_int [ r; g; b; y ] (0, 1) 1) with
  | 1 -> R
  | 2 -> G
  | 3 -> B
  | _ -> Y

(** [most_color h] is the color that occurs most in the player's hand
    [h]. *)
let most_color h =
  let rec mc h r g b y =
    match h with
    | [] -> max r g b y 0
    | h :: t -> (
        match color h with
        | R -> mc t (r + 1) g b y
        | G -> mc t r (g + 1) b y
        | B -> mc t r g (b + 1) y
        | Y -> mc t r g b (y + 1)
        | ANY -> mc t r g b y)
  in
  mc h 0 0 0 0

(** [least_cards g] is the player with the least amount of cards in game
    [g] excluding the current player. *)
let least_cards g =
  let rec lc plyrs (p, c) =
    match plyrs with
    | [] -> id p
    | h :: t ->
        let h_length = player_hand h |> List.length in
        if h_length < c then lc t (h, h_length) else lc t (p, c)
  in
  match players g with
  | [] -> current_player g |> id
  | h :: t -> lc t (h, 100)

(** [card c g] is the card [c] with the most optimal action and/or color
    applied based on the current game [g]. *)
let card c g =
  match actions c with
  | { skip = _; reverse = _; swap = true, _; change_color = true } ->
      change_color
        (set_swap_id c (least_cards g))
        (most_color (g |> current_player |> player_hand))
  | { skip = _; reverse = _; swap = _, _; change_color = true } ->
      change_color c (most_color (g |> current_player |> player_hand))
  | _ -> c

(** [play_new_card g c] is Legal if the card [c] is playable in game
    [g], Gameover if playing that card results in a game over, and
    Illegal if the card cannot be played. *)
let play_new_card g c =
  let new_card = card c g in
  let new_g = change_current_players_hand c new_card g in
  play (Some new_card) new_g

(** [is_valid_card g c] is a true when a card [c] s a legal move in game
    [g] and false otherwise. *)
let is_valid_card g c =
  match play_new_card g c with Illegal -> false | _ -> true

(** [find_valid_card g h] is the first valid card option in a player's
    hand [h] from the left in a given state [g]. None is returned if the
    hand is empty. *)
let rec first_valid_card g hand =
  match hand with
  | [] -> None
  | h :: t -> if is_valid_card g h then Some h else first_valid_card g t

(** [valid_hand g acc h] is a list of cards from a player's hand [h]
    that would be valid to play in game state [g]. *)
let rec valid_hand g acc hand =
  match hand with
  | [] -> List.rev acc
  | h :: t ->
      if is_valid_card g h then valid_hand g (h :: acc) t
      else valid_hand g acc t

(** [next_player g c] is the player after the current player in game [g]
    after playing card [c]. *)
let next_player g c =
  let s' = play_new_card g c in
  match s' with Legal s' -> current_player s' | _ -> current_player g

(** [add_weight g c] is the list of tuples which each have the card [c]
    then the weight of the card based on its value in the current state
    of game [g]. *)
let add_weight g c =
  if draw_penalty c = 2 && top_card g |> draw_penalty = 2 then (c, 0)
  else if draw_penalty c = 4 && top_card g |> draw_penalty = 4 then
    (c, 0)
  else if is_uno (next_player g c) && draw_penalty c = 2 then (c, 0)
  else if is_uno (next_player g c) && draw_penalty c = 4 then (c, 1)
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
  Stdlib.compare (snd a) (snd b)

let action g : 'a option * 'a option =
  let uw_cards =
    g |> current_player |> player_hand |> valid_hand g []
  in
  let w_cards = List.map (add_weight g) uw_cards in
  match List.fast_sort comp w_cards with
  | [] -> (None, None)
  | h :: t -> (
      match actions (fst h) with
      | { skip = _; reverse = _; swap = true, _; change_color = _ } ->
          ( Some (fst h),
            Some
              (change_color
                 (set_swap_id (fst h) (least_cards g))
                 (most_color uw_cards)) )
      | { skip = _; reverse = _; swap = _, _; change_color = true } ->
          ( Some (fst h),
            Some (change_color (fst h) (most_color uw_cards)) )
      | _ -> (Some (fst h), Some (fst h)))

(** [action_test g] is a list of weighted card tuples for the current
    player's hand in game [g]. *)
let action_test_cpu g =
  let uw_cards =
    g |> current_player |> player_hand |> valid_hand g []
  in
  let w_cards = List.map (add_weight g) uw_cards in
  w_cards

(** [computer_swap_id c] is the swap id of the card [c]. *)
let computer_swap_id c = swap_id_of_card c

(** [computer_color c] is the color of the card c. *)
let computer_color c = color c
