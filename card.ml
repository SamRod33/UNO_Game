open Yojson.Basic.Util

(********************************************************************)
(* Card types *)
(********************************************************************)

type color =
  | R
  | G
  | B
  | Y
  | ANY

type actions = {
  skip : bool;
  reverse : bool;
  swap : bool;
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

(* [to_color j] is [j] as an actions. *)
let to_actions j =
  {
    skip = j |> member "skip" |> to_bool;
    reverse = j |> member "reverse" |> to_bool;
    swap = j |> member "swap" |> to_bool;
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

(** [create_cards lst] is the parsed list of JSONs containing Cards. *)
let rec create_cards lst = List.map create lst

(*** [create_std_lst] is the list of JSONs of cards in a standard deck. *)
let rec create_std_lst =
  let f (nm, j) = j in
  List.map f (to_assoc (Yojson.Basic.from_file "standard_cards.json"))

(********************************************************************)
(* Alpha Demo Card functions *)
(********************************************************************)

let draw_penalty c = c.penalty

let color c = c.color

let actions c = c.actions

let amount c = c.amount

let digit c = c.digit

(********************************************************************)
(* Standard cards *)
(********************************************************************)

let no_actions =
  { skip = false; reverse = false; swap = false; change_color = false }

let num_card col dig =
  {
    color = col;
    digit = Some dig;
    actions = no_actions;
    penalty = 0;
    amount = 1;
  }

let skip_actions =
  { skip = true; reverse = false; swap = false; change_color = false }

let skip_card col =
  {
    color = col;
    digit = None;
    actions = skip_actions;
    penalty = 0;
    amount = 1;
  }

let reverse_actions =
  { skip = false; reverse = true; swap = false; change_color = false }

let reverse_card col =
  {
    color = col;
    digit = None;
    actions = reverse_actions;
    penalty = 0;
    amount = 1;
  }

let swap_actions =
  { skip = false; reverse = false; swap = true; change_color = false }

let swap_card col =
  {
    color = col;
    digit = None;
    actions = swap_actions;
    penalty = 0;
    amount = 1;
  }

let cc_actions =
  { skip = false; reverse = false; swap = false; change_color = true }

let change_color =
  {
    color = ANY;
    digit = None;
    actions = cc_actions;
    penalty = 0;
    amount = 1;
  }

let draw_2 col =
  {
    color = col;
    digit = None;
    actions = no_actions;
    penalty = 2;
    amount = 1;
  }

let draw_4 =
  {
    color = ANY;
    digit = None;
    actions = cc_actions;
    penalty = 4;
    amount = 1;
  }

let color_cards c =
  [
    num_card c 0;
    num_card c 1;
    num_card c 2;
    num_card c 3;
    num_card c 4;
    num_card c 5;
    num_card c 6;
    num_card c 7;
    num_card c 8;
    num_card c 9;
    skip_card c;
    reverse_card c;
    swap_card c;
    draw_2 c;
  ]

let red_cards = color_cards R

let blue_cards = color_cards B

let green_cards = color_cards G

let yellow_cards = color_cards Y

let any_color_cards = [ change_color; draw_4 ]

let standard_cards =
  List.flatten
    [
      red_cards; blue_cards; green_cards; yellow_cards; any_color_cards;
    ]
