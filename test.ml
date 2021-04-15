open OUnit2
open Card
open Player

(** [deck] is the list of cards from [custom_card.json] used in testing. *)
let deck =
  let open Yojson.Basic in
  let open Yojson.Basic.Util in
  "custom_card.json" |> from_file |> member "standard deck" |> to_list
  |> create_cards

(** [string_of_color] is the string representation of color from a card*)
let string_of_color = function
  | R -> "red"
  | G -> "green"
  | B -> "blue"
  | Y -> "yellow"
  | ANY -> "any"

(** [string_of_digit] is the string representation of digit from a card. *)
let string_of_digit = function
  | None -> "None"
  | Some x -> string_of_int x

(** [string_of_actions c] is the string representation of actions from
    [c]*)
let string_of_actions c =
  "skip: " ^ string_of_bool c.skip ^ " reverse: "
  ^ string_of_bool c.reverse
  ^ " swap: " ^ string_of_bool c.swap ^ " change color: "
  ^ string_of_bool c.change_color

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(** [pp_card c] pretty prints [c]*)
let pp_card c =
  Format.asprintf "(%s, %s, %d, %s)"
    (string_of_color (color c))
    (string_of_digit (digit c))
    (draw_penalty c)
    (string_of_actions (actions c))

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [f_test name f c expected to_str] creates an OUnit test case called
    [name] that checks [expected] equals [f c]. [to_str] dictates how
    the test case should print out the actual value of [f c] if
    otherwise. *)
let f_test name f c expected to_str =
  name >:: fun _ -> assert_equal expected (f c) ~printer:to_str

(** [f2_test name f x k exp printer] creates an OUnit test case that
    asserts [f x k] is equivalent to [exp]. [printer] tells this test
    case how to print [exp] and [f x k] *)
let f2_test name f x k exp printer =
  name >:: fun _ -> assert_equal exp (f x k) ~printer

(*********************************************************)
(**********END OF HELPER FUNCTIONS************************)
(*********************************************************)
