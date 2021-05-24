open Graphics
open Constants
open Images
open Png
open WindowGui
open WindowHelp

let player_cards = Card.standard_cards

let outline_width, outline_height = (130, 180)

let selected_spacing = 161

let player_num_pos = (223 + 84, 54 + 553)

let penalty_num_pos = (448 + 35, 380)

let card_init_pos = (40, 35)

let card_start_pos = ref card_init_pos

let out_init_x, out_init_y = (30, 25)

let outline_pos_x = ref 30

let outline_pos_y = ref 25

let init_idx =
  ((!outline_pos_x - 30) / selected_spacing)
  - ((fst !card_start_pos - fst card_init_pos) / selected_spacing)

let card_selected_idx = ref init_idx

(** [draw_top_card card] draws the top card on the main screen*)
let draw_top_card card =
  match Card.img card with
  | "Draw4" | "Wild" -> (
      match Card.color card with
      | Card.R -> upload_img _ASSET_DIR "red_color" 272 330
      | Card.Y -> upload_img _ASSET_DIR "yellow_color" 272 330
      | Card.B -> upload_img _ASSET_DIR "blue_color" 272 330
      | Card.G -> upload_img _ASSET_DIR "green_color" 272 330
      | Card.ANY -> failwith "ANY is invalid in draw_top_card")
  | _ -> upload_img _CARD_DIR (Card.img card) 272 330

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
    display_hand_amt snd_dig (x + 8, y))

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
let move op1 op2 limit bound card_end_pos =
  if bound then (
    if limit !outline_pos_x then (
      card_start_pos :=
        (op1 (fst !card_start_pos) selected_spacing, snd !card_start_pos);
      card_end_pos := op1 !card_end_pos selected_spacing)
    else (
      highlight_selection _GOLD _GREEN
        (op2 0 selected_spacing)
        !outline_pos_x !outline_pos_y outline_width outline_height;
      outline_pos_x := op2 !outline_pos_x selected_spacing);
    card_selected_idx := op2 !card_selected_idx 1)
  else ()

(* [draw_main_screen] draws all parts of the main screen except player
   hand cards. *)
let draw_main_screen top p_id penalty other_player_info =
  set_background _BLACK;
  draw_logo ();
  draw_game_frames ();
  display_player_num p_id player_num_pos;
  display_num penalty penalty_num_pos;
  draw_player_hand_amounts other_player_info (645, 330 + 46);
  draw_top_card top;
  draw_card_deck ();
  set_player_hand_background ();
  highlight_selection _GOLD _GREEN 0 !outline_pos_x !outline_pos_y
    outline_width outline_height

let run_main st card_end_pos top p_id penalty other_player_info =
  if st.key = _QUIT_KEY then exit 0
  else if st.key = _HELP_KEY then (
    help_win ();
    draw_main_screen top p_id penalty other_player_info)
  else if st.key = _CONFIRM_KEY then raise Exit
  else if st.key = _RIGHT_KEY then
    move ( - ) ( + )
      (fun x -> x >= int_of_string _WIDTH - (2 * fst card_space))
      (!card_end_pos >= !outline_pos_x + 180)
      card_end_pos
  else if st.key = _LEFT_KEY then
    move ( + ) ( - )
      (fun x -> x <= fst card_init_pos)
      (fst !card_start_pos <= !outline_pos_x)
      card_end_pos
  else if st.key = _DRAW_KEY then (
    card_selected_idx := -1;
    raise Exit)

(** [main_win top p_id penalty other_player_info player_cards] run the
    main window game loop. i.e. show the top card [top], who the current
    player is [p_id], current [penalty], how many cards each player has
    in [other_player_info], and allow the current player to select any
    of their [player_cards]. This function returns Some Card.t if the
    player selected a card, or None if they wanted to quit the game. *)
let main_win top p_id penalty other_player_info player_cards =
  card_start_pos := card_init_pos;
  outline_pos_x := out_init_x;
  outline_pos_y := out_init_y;
  let card_end_pos =
    ref
      ((List.length player_cards * (9 + fst card_space))
      + fst !card_start_pos)
  in
  card_selected_idx :=
    ((!outline_pos_x - 30) / selected_spacing)
    - ((fst !card_start_pos - fst card_init_pos) / selected_spacing);
  draw_main_screen top p_id penalty other_player_info;
  (try
     while true do
       draw_cards player_cards !card_start_pos (selected_spacing, 0);
       let st = wait_next_event [ Key_pressed ] in
       synchronize ();
       run_main st card_end_pos top p_id penalty other_player_info
     done
   with Exit -> ());
  if !card_selected_idx < 0 then None
  else Some (List.nth player_cards !card_selected_idx)

(* ;; open_window; let s = main_win (List.hd player_cards) 1 0 [ (5,
   20); (2, 30); (3, 55); (4, 50) ] player_cards in Card.pp_cards [
   Option.get s ] false *)
