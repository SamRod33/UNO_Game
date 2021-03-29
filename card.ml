open Yojson.Basic.Util

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
  actions : actions;
  penalty : draw_penalty;
}

(**************************************************************************)

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

let create j =
  {
    color = j |> member "color" |> to_string |> to_color;
    actions = j |> member "actions" |> to_actions;
    penalty = j |> member "draw penalty" |> to_int;
  }

(**************************************************************************)

let draw_penalty c = c.penalty

let color c = c.color

let actions c = c.actions
