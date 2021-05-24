open Graphics
open Constants
open Images
open Png
open WindowGui

(* [selections] is the selection list to display for player and computer
   selections. *)
let selections =
  [ "Green-0"; "Blue-1"; "Green-2"; "Red-3"; "Yellow-4"; "Blue-5" ]

(* [max_players] current cap on the number of players that can player. *)
let max_players = 5

(* [select_x, select_y] is the starting position for selections. *)
let select_x, select_y = (90, 225)

let outline_pos_x, outline_pos_y =
  (ref (select_x - 10), ref (select_y - 10))

let init_outline_x, init_ouline_y = (!outline_pos_x, !outline_pos_y)

let outline_width, outline_height = (130, 180)

(* [p_txt_x, p_txt_y] is the starting position for player select text. *)
let p_txt_x, p_txt_y = (select_x, select_y + 145 + 180)

(* [comp_txt_x, comp_txt_y] is the starting position for computer select
   text. *)
let comp_txt_x, comp_txt_y = (select_x + 90, select_y + 145 + 180)

let p_select_idx = ref 0

let comp_select_idx = ref 0

let draw_change_color_card c pos =
  let c_x, c_y = pos in
  upload_img _CARD_DIR c c_x c_y

(** [draw_cards cards pos] draws [cards] starting at [pos]. Ensures
    spacing between each card. *)
let rec draw_cards cards pos =
  let c_x, c_y = pos in
  let c_space_x, c_space_y = card_space in
  match cards with
  | [] -> ()
  | c :: t ->
      draw_change_color_card c pos;
      draw_cards t (c_x + c_space_x, c_y + c_space_y)

(** [move op limit space color_a color_b] draws the new selection. *)
let move op space color_a color_b =
  highlight_selection color_a color_b (op 0 space) !outline_pos_x
    !outline_pos_y outline_width outline_height;
  outline_pos_x := op !outline_pos_x space

(** [fst_n] gives the first [n] in [lst]. Is tail-recursive*)
let rec fst_n n acc lst =
  match (n, lst) with
  | 0, lst -> List.rev acc
  | n, h :: t -> fst_n (n - 1) (h :: acc) t
  | _ -> failwith "Something bad happened in fst_n"

(** [draw_selections] displays [prompt] at ([x], [y]) and [selections]
    between 0 - [num_select]. *)
let draw_selections prompt x y num_select selections =
  let selections = fst_n num_select [] selections in
  set_background _BLACK;
  draw_cards selections (select_x, select_y);
  upload_img _TEXT_DIR prompt x y

(* TODO: Spec *)
let select st p_selection select_idx =
  if st.key = _RIGHT_KEY then
    if
      !outline_pos_x
      >= ((List.length selections - p_selection) * fst card_space) - 50
    then ()
    else (
      move ( + ) (fst card_space) _GOLD _BLACK;
      select_idx := !select_idx + 1)
  else if st.key = _LEFT_KEY then
    if !outline_pos_x <= select_x then ()
    else (
      move ( - ) (fst card_space) _GOLD _BLACK;
      select_idx := !select_idx - 1)
  else if st.key = _CONFIRM_KEY then raise Exit

(* TODO: spec *)
let run_select_win p_selection select_idx x y msg =
  if p_selection >= 5 then 0
  else (
    draw_selections msg x y
      (List.length selections - p_selection)
      selections;
    highlight_selection _GOLD _BLACK 0 !outline_pos_x !outline_pos_y
      outline_width outline_height;
    (try
       while running do
         let st = wait_next_event [ Key_pressed ] in
         synchronize ();
         if st.key = _QUIT_KEY then exit 0
         else select st p_selection select_idx
       done
     with Exit -> ());
    !select_idx)

(** [select_p_win] is the main player select window. *)
let select_p_win () = run_select_win 0 p_select_idx p_txt_x p_txt_y

(** [select_p_win] is the main computer select window. *)
let select_comp_win p_selection =
  run_select_win p_selection comp_select_idx comp_txt_x comp_txt_y

(** [select_win ()] is the tuple: (number of players, number of
    computers). *)
let select_win () =
  let p_selection =
    select_p_win ()
      "select_the_number_of_people_that_want_to_play_the_game"
  in
  outline_pos_x := init_outline_x;
  outline_pos_y := init_ouline_y;
  ( p_selection,
    select_comp_win p_selection
      "select_the_number_of_computer_opponents" )

;;
open_window;
let p_selection =
  select_p_win ()
    "select_the_number_of_people_that_want_to_play_the_game"
in
print_endline (string_of_int p_selection);
outline_pos_x := init_outline_x;
outline_pos_y := init_ouline_y;
let comp_selection =
  select_comp_win p_selection "select_the_number_of_computer_opponents"
in
print_endline (string_of_int comp_selection)
(* let comp_selection = select_comp_win 5 in print_endline
   (string_of_int comp_selection) *)
(* select_seq () *)
