open Yojson.Basic.Util

(********************************************************************)
(* Card types *)
(********************************************************************)

(* number of cards to print per row. *)
let c_per_row = 7

(* number of lines and columns a card consists of. *)
let card_dim = (9, 11)

type color =
  | R
  | G
  | B
  | Y
  | ANY

type actions = {
  skip : bool;
  reverse : bool;
  swap : bool * int;
  change_color : bool;
}

type draw_penalty = int

type t = {
  color : color;
  digit : int option;
  actions : actions;
  penalty : draw_penalty;
  amount : int;
}

(********************************************************************)
(* Creating custom cards *)
(********************************************************************)

(* [j json] is the Yoson Basic representation of type t from the json
   file [j] *)
let j json = Yojson.Basic.from_file json

(* [to_color s] is [s] as a color. *)
let to_color s =
  match String.lowercase_ascii s with
  | "red" -> R
  | "green" -> G
  | "blue" -> B
  | "yellow" -> Y
  | "any" -> ANY
  | _ -> raise (Invalid_argument s)

(* [make_swap j] is the swap tuple from [j]. *)
let make_swap j =
  let swap = j |> member "swap" |> to_bool in
  let swap_p2 = j |> member "swap_p2" |> to_int in
  (swap, swap_p2)

(* [to_actions j] is [j] as an actions list. *)
let to_actions j =
  {
    skip = j |> member "skip" |> to_bool;
    reverse = j |> member "reverse" |> to_bool;
    swap = make_swap j;
    change_color = j |> member "change color" |> to_bool;
  }

(* [to_digit j] is [j] as a digit. *)
let to_digit j = try Some (to_int j) with _ -> None

let create j =
  {
    color = j |> member "color" |> to_string |> to_color;
    actions = j |> member "actions" |> to_actions;
    penalty = j |> member "draw penalty" |> to_int;
    amount = j |> member "amount" |> to_int;
    digit = j |> member "digit" |> to_digit;
  }

let create_cards lst = List.map create lst

let standard_cards =
  j "standard_cards.json" |> member "standard deck" |> to_list
  |> create_cards

let custom_cards =
  j "custom_card.json" |> member "standard deck" |> to_list
  |> create_cards

let full_deck = custom_cards @ standard_cards

(** [print_color c] is the color for ANSITerminal to print. *)
let print_color = function
  | R -> ANSITerminal.red
  | G -> ANSITerminal.green
  | Y -> ANSITerminal.yellow
  | B -> ANSITerminal.blue
  | ANY -> ANSITerminal.white

(** [print_face c] is the face card representation of cards in UNO. Each
    line of the face card is split into each element. *)
let print_face c =
  if c.digit <> None then
    match Option.get c.digit with
    | 0 -> Facecards.zero
    | 1 -> Facecards.one
    | 2 -> Facecards.two
    | 3 -> Facecards.three
    | 4 -> Facecards.four
    | 5 -> Facecards.five
    | 6 -> Facecards.six
    | 7 -> Facecards.seven
    | 8 -> Facecards.eight
    | 9 -> Facecards.nine
    | x when x > 9 || x < 0 -> failwith "digit not between 0-9"
    | _ -> failwith "Impossible pattern match: Invalid digit card"
  else if c.penalty <> 0 then
    match c.penalty with
    | 2 -> Facecards.plus_2
    | 4 -> Facecards.plus_4
    | x when x <> 2 || x <> 4 -> failwith "Stack card not +2 nor +4"
    | _ ->
        failwith
          "Impossible pattern match: Invalid stack penalty face card"
  else if c.actions.reverse then Facecards.reverse
  else if fst c.actions.swap then Facecards.swap
  else if c.actions.skip then Facecards.skip
  else if c.actions.change_color then Facecards.wild
  else failwith "Impossible pattern match: Invalid card"

(** [print_ord c] is the list order to print [c] along with the color to
    print each line. *)
let print_ord c =
  let card_ord = Queue.create () in
  let rec helper lst =
    match lst with
    | [] -> ()
    | h :: t ->
        Queue.add (print_color c.color, h) card_ord;
        helper t
  in
  helper (print_face c);
  card_ord

(** [print_card_row q_lst] prints the first line from every card in
    [q_lst]. *)
let rec print_card_row q_lst =
  match q_lst with
  | [] -> ()
  | h :: t ->
      let cline = Queue.pop h in
      ANSITerminal.print_string [ fst cline ] (snd cline ^ "  ");
      print_card_row t

(** [num_digits n acc] is the number of digits in [n]. Implements tail
    recursion. *)
let rec num_digits n acc =
  match n with
  | n when n < 10 -> acc
  | n -> num_digits (n / 10) (acc + 1)

(** [idx_spacing n] is the amount of spaces to separate each card. *)
let idx_spacing n = String.make (snd card_dim + 2 - num_digits n 1) ' '

(** [idx_label start stop acc] is a string sequence from start to stop
    such that each digit in the sequence is separated by idx_spacing. *)
let rec idx_label start stop acc =
  match stop with
  | n when n >= start ->
      idx_label start (n - 1) (string_of_int n ^ idx_spacing n ^ acc)
  | n when n < start -> acc
  | _ -> failwith "Impossible pattern match: idx label"

(** [print_cards cards start] prints out [cards] and labels each pp_card
    starting at [start] to [start] \+ min(c_per_row, length of cards -
    1). *)
let print_cards cards start =
  print_endline
    ("     "
    ^ idx_label start (start + min c_per_row (List.length cards - 1)) ""
    );
  let pp_orders = List.map (fun c -> print_ord c) cards in
  let rec pp_cards_aux = function
    | 0 -> ()
    | n ->
        print_card_row pp_orders;
        print_string "\n";
        pp_cards_aux (n - 1)
  in
  pp_cards_aux (fst card_dim)

let rec print_per_row acc n cards =
  match cards with
  | [] -> print_cards acc n
  | h :: t ->
      if List.length acc >= c_per_row then
        print_cards acc (n - List.length acc)
      else ();
      if List.length acc >= c_per_row then print_per_row [] n t
      else print_per_row (h :: acc) (n + 1) t

let pp_cards cards = print_per_row [] 0 cards

(********************************************************************)
(* Alpha Demo Card functions *)
(********************************************************************)

let draw_penalty c = c.penalty

let color c = c.color

let actions c = c.actions

let amount c = c.amount

let digit c = c.digit
