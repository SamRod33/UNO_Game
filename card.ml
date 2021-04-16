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

(********************************************************************)
(* Alpha Demo Card functions *)
(********************************************************************)

let draw_penalty c = c.penalty

let color c = c.color

let actions c = c.actions

let amount c = c.amount

let digit c = c.digit
