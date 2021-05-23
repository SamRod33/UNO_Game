open Graphics
open Constants
open Window_gui
open State
open Player

let players = ()

(* [create_player_card_tuple ply_lst acc] is a list of tuples that
   contains the each player in [ply_lst] and number of cards in that
   player's hand. *)
let rec create_player_card_tuple ply_lst acc =
  (* TODO: change player id to num *)
  match ply_lst with
  | [] -> acc
  | p :: t ->
      create_player_card_tuple t
        ((p, p |> player_hand |> List.length) :: acc)

(* [outline_width, outline_height] is the dimensions of the outline. *)
let outline_width, outline_height = (160, 210)

(* [swap_txt_pos_x, swap_txt_pos_y] is the position of the change color
   prompt, as inspired by the mockup. *)
let swap_txt_pos_x, swap_txt_pos_y = (logo_pos_x - 200, logo_pos_y - 100)

(* [cards_start_pos_x, cards_start_pos_y] is the starting position of
   the color changing cards, as inspired by the mockup. *)
let cards_start_pos_x, cards_start_pos_y =
  (swap_txt_pos_x, swap_txt_pos_y - 300)

(* [outline_pos_x, outline_pos_y] is the starting position of the
   selection outline. *)
let outline_pos_x, outline_pos_y =
  (ref (cards_start_pos_x - 20), ref (cards_start_pos_y - 20))

let player_selected_idx = ref 0

let swap_player_cards =
  [ "player_hand_1"; "player_hand_2"; "player_hand_3"; "player_hand_4" ]

(** [draw_change_color_card c pos] draws [c] at [pos]. *)
let draw_player_hand_card c pos =
  let p_x, p_y = pos in
  upload_img _ASSET_DIR c p_x p_y

(** [draw_cards cards pos] draws [cards] starting at [pos]. Ensures
    spacing between each card. *)
let rec draw_cards cards pos =
  let p_x, p_y = pos in
  let p_space_x, p_space_y = swap_space in
  match cards with
  | [] -> ()
  | c :: t ->
      draw_player_hand_card c pos;
      draw_cards t (p_x + p_space_x, p_y + p_space_y)

let draw_swap_player_screen g =
  (* TODO: will need to omit the open_window when integrating. *)
  open_window;
  set_background _BLACK;
  draw_logo ();
  upload_img _TEXT_DIR "Choose a player" swap_txt_pos_x swap_txt_pos_y;
  draw_cards swap_player_cards
    (cards_swap_start_pos_x, cards_swap_start_pos_y);
  add_nums g

(* [swap_player_phase st] Launches the change color window phase. *)
let swap_player_phase st =
  if st.key = _QUIT_KEY then raise Exit
  else if st.key = _CONFIRM_KEY then
    failwith
      ("TODO: return card_selected: "
      ^ string_of_int !player_selected_idx)
  else if st.key = _RIGHT_KEY then
    if !outline_pos_x >= List.length swap_player_cards * fst swap_space
    then ()
    else (
      highlight_selection _GOLD _BLACK (fst swap_space) !outline_pos_x
        !outline_pos_y outline_width outline_height;
      outline_pos_x := !outline_pos_x + fst swap_space;
      player_selected_idx := !player_selected_idx + 1)
  else if st.key = _LEFT_KEY then
    if !outline_pos_x <= cards_start_pos_x - 10 then ()
    else (
      highlight_selection _GOLD _BLACK
        ~-(fst swap_space)
        !outline_pos_x !outline_pos_y outline_width outline_height;
      outline_pos_x := !outline_pos_x + ~-(fst swap_space);
      player_selected_idx := !player_selected_idx - 1)

;;
open_window;
draw_swap_player_screen g;
highlight_selection _GOLD _BLACK 0 !outline_pos_x !outline_pos_y
  outline_width outline_height;

try
  while running do
    let st = wait_next_event [ Key_pressed ] in
    synchronize ();
    swap_player_phase st
  done
with Exit -> ()
