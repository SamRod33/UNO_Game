open Graphics
open Constants
open Window_gui
open State
open Player

(* [outline_width, outline_height] is the dimensions of the outline. *)
let outline_width, outline_height = (160, 210)

(* [swap_txt_pos_x, swap_txt_pos_y] is the position of the change color
   prompt, as inspired by the mockup. *)
let swap_txt_pos_x, swap_txt_pos_y = (logo_pos_x - 200, logo_pos_y - 100)

(* [swap_start_pos_x, swap_start_pos_y] is the starting position of the
   swap player info cards, as inspired by the mockup. *)
let swap_start_pos_x, swap_start_pos_y =
  (swap_txt_pos_x, swap_txt_pos_y - 300)

(* [outline_pos_x, outline_pos_y] is the starting position of the
   selection outline. *)
let outline_pos_x, outline_pos_y =
  (ref (swap_start_pos_x - 20), ref (swap_start_pos_y - 20))

(* [num_start_x, num_start_y] is the starting position of the number of
   cards each opponent has. *)
let num_start_x, num_start_y =
  (swap_start_pos_x + 120, swap_start_pos_y + 170)

(* [player_selected_idx] is the index of the player selected. *)
let player_selected_idx = ref 0

(* [swap_player_cards] is the list of cards to display in the window. *)
let swap_player_cards =
  [ "player_hand_1"; "player_hand_2"; "player_hand_3"; "player_hand_4" ]

(* [create_player_card_tuple ply_lst acc] is a list of integers that
   contains the number of cards in each player's hand in [ply_lst]. *)
let rec create_player_card_nums ply_lst (acc : int list) =
  match ply_lst with
  | [] -> acc
  | p :: t ->
      create_player_card_nums t
        ((p |> player_hand |> List.length) :: acc)

(* [split_num nc acc] is the lsit of digits in number [nc]. *)
let rec split_num nc acc =
  if nc < 10 then nc :: acc
  else split_num (nc / 10) ((nc mod 10) :: acc)

(* [draw_specific_num lst_digs x y] is the number [lst_digs] drawn using
   GUI images at position [x], [y]. *)
let rec draw_specific_num lst_digs x y =
  match lst_digs with
  | [] -> ()
  | h :: t -> (
      match h with
      | 0 ->
          upload_img _TEXT_DIR "0" x y;
          draw_specific_num t (x + 30) (y + 0)
      | 1 ->
          upload_img _TEXT_DIR "1" x y;
          draw_specific_num t (x + 30) (y + 0)
      | 2 ->
          upload_img _TEXT_DIR "2" x y;
          draw_specific_num t (x + 30) (y + 0)
      | 3 ->
          upload_img _TEXT_DIR "3" x y;
          draw_specific_num t (x + 30) (y + 0)
      | 4 ->
          upload_img _TEXT_DIR "4" x y;
          draw_specific_num t (x + 30) (y + 0)
      | 5 ->
          upload_img _TEXT_DIR "5" x y;
          draw_specific_num t (x + 30) (y + 0)
      | 6 ->
          upload_img _TEXT_DIR "6" x y;
          draw_specific_num t (x + 30) (y + 0)
      | 7 ->
          upload_img _TEXT_DIR "7" x y;
          draw_specific_num t (x + 30) (y + 0)
      | 8 ->
          upload_img _TEXT_DIR "8" x y;
          draw_specific_num t (x + 30) (y + 0)
      | 9 ->
          upload_img _TEXT_DIR "9" x y;
          draw_specific_num t (x + 30) (y + 0)
      | _ -> failwith "impossible: number not composed of digits")

(* [draw_nums] is the numbers [nums] for each opponent player drawn in
   the window starting at position [x], [y]. *)
let rec draw_nums nums x y =
  let p_space_x, p_space_y = swap_space in
  match nums with
  | [] -> ()
  | num :: t ->
      (let lst_digs = split_num num [] in
       draw_specific_num lst_digs x y);
      draw_nums t (x + p_space_x) (y + p_space_y)

(* [add_nums g x y] gets the number of cards for each opponent player in
   game [g] then starts drawing them at position [x], [y]. *)
let add_nums g x y =
  let players =
    List.filter (fun a -> a <> current_player g) (players g)
  in
  let ply_nums = create_player_card_nums players [] in
  draw_nums ply_nums num_start_x num_start_y

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

(* [draw_swap_player_screen g] is the swap player screen with the card
   and players displayed from game [g]. *)
let draw_swap_player_screen () =
  (* TODO: will need to omit the open_window when integrating. *)
  open_window;
  set_background _BLACK;
  draw_logo ();
  upload_img _TEXT_DIR "Choose a player" swap_txt_pos_x swap_txt_pos_y;
  draw_cards swap_player_cards
    (cards_swap_start_pos_x, cards_swap_start_pos_y)
(*; add_nums g num_start_x num_start_y*)

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
    if !outline_pos_x <= swap_start_pos_x - 10 then ()
    else (
      highlight_selection _GOLD _BLACK
        ~-(fst swap_space)
        !outline_pos_x !outline_pos_y outline_width outline_height;
      outline_pos_x := !outline_pos_x + ~-(fst swap_space);
      player_selected_idx := !player_selected_idx - 1)

;;
open_window;
draw_swap_player_screen ();
highlight_selection _GOLD _BLACK 0 !outline_pos_x !outline_pos_y
  outline_width outline_height;

try
  while running do
    let st = wait_next_event [ Key_pressed ] in
    synchronize ();
    swap_player_phase st
  done
with Exit -> ()
