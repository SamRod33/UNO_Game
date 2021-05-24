open Graphics
open Constants
open Images
open Png
open WindowGui

(**********************)
let p_id = 5

let penalty_num = 78

let dummy_data = [ (5, 20); (2, 30); (3, 55); (4, 50) ]

(*dummy data is id x card hand*)

(**********************)
let player_cards = Card.standard_cards

let outline_width, outline_height = (130, 180)

let player_num_pos = (223 + 84, 54 + 553)

let penalty_num_pos = (448 + 35, 380)

let selected_spacing = 161

let card_selected_idx = ref 0

let card_init_pos = (40, 35)

let card_start_pos = ref card_init_pos

let card_end_pos = ref (List.length player_cards * (9 + fst card_space))

let outline_pos_x = ref 30

let outline_pos_y = ref 25

let draw_top_card card = upload_img _CARD_DIR card 272 330

let draw_card_deck () = upload_img _CARD_DIR "back" 84 330

let set_player_hand_background () =
  set_color _GREEN;
  fill_rect 0 0 (int_of_string _WIDTH) 275

let draw_game_frames () =
  upload_img _ASSET_DIR "game_keys" 632 553;
  upload_img _ASSET_DIR "player_turn_frame" 84 553;
  upload_img _ASSET_DIR "penalty_frame" 448 330;
  upload_img _ASSET_DIR "player_hand_frame" 624 330

let rec display_hand_amt i (x, y) =
  if i < 10 then upload_img _TEXT_DIR (string_of_int i ^ "_mini") x y
  else if i < 100 then (
    let fst_dig = i / 10 in
    let snd_dig = i mod 10 in
    upload_img _TEXT_DIR (string_of_int fst_dig ^ "_mini") x y;
    display_hand_amt snd_dig (x + 8, y) )

let draw_one_player_hand_amount c (x, y) =
  let id, amt = c in
  upload_img _ASSET_DIR ("frame_hand_" ^ string_of_int id) x y;
  display_hand_amt amt (x + 40, y + 65);
  ()

let rec draw_player_hand_amounts l (x, y) =
  match l with
  | [] -> ()
  | h :: t ->
      draw_one_player_hand_amount h (x, y);
      draw_player_hand_amounts t (x + 70, y)

(** [move op1 op2 limit bound] draws the selection and if the player
    selection satisfies [limit] and [bound] then the cards are shifted
    by [op1]. *)
let move op1 op2 limit bound =
  if bound then
    if limit !outline_pos_x then (
      card_start_pos :=
        (op1 (fst !card_start_pos) selected_spacing, snd !card_start_pos);
      card_end_pos := op1 !card_end_pos selected_spacing )
    else (
      highlight_selection _GOLD _GREEN
        (op2 0 selected_spacing)
        !outline_pos_x !outline_pos_y outline_width outline_height;
      outline_pos_x := op2 !outline_pos_x selected_spacing;
      card_selected_idx := !card_selected_idx + 1 )
  else ()

(* [draw_main_screen] draws all parts of the main screen except player
   hand cards. *)
let draw_main_screen p_id penalty other_player_info =
  open_window;
  set_background _BLACK;
  draw_logo ();
  draw_game_frames ();
  (*tise difference*)
  display_player_num p_id player_num_pos;
  display_num penalty penalty_num_pos;
  draw_player_hand_amounts other_player_info (645, 330 + 46);
  (*difference*)
  draw_top_card "Draw4";
  draw_card_deck ();
  set_player_hand_background ();
  highlight_selection _GOLD _GREEN 0 !outline_pos_x !outline_pos_y
    outline_width outline_height

;;
draw_main_screen p_id penalty_num dummy_data;
try
  while true do
    draw_cards player_cards !card_start_pos (selected_spacing, 0);
    let st = wait_next_event [ Key_pressed ] in
    synchronize ();
    if st.key = _QUIT_KEY then raise Exit
    else if st.key = _RIGHT_KEY then
      move ( - ) ( + )
        (fun x -> x >= int_of_string _WIDTH - (2 * fst card_space))
        (!card_end_pos >= !outline_pos_x)
    else if st.key = _LEFT_KEY then
      move ( + ) ( - )
        (fun x -> x <= fst card_init_pos)
        (fst !card_start_pos <= !outline_pos_x)
    else if st.key = _CONFIRM_KEY then
      failwith
        ("player selected a card: " ^ string_of_int !card_selected_idx)
  done
with Exit -> ()
