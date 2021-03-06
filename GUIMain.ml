open State
open Card
open Player
open Computer
open MainFunctions
open WindowMain
open WindowSwapPlayer
open WindowChangeColor
open WindowIntermission
open WindowIntro
open WinSelect
open WindowEndGame

let color_change () =
  match change_color_win () with
  | None -> exit 0
  | Some "red_color" -> R
  | Some "blue_color" -> B
  | Some "green_color" -> G
  | Some "yellow_color" -> Y
  | _ -> failwith "change color window gave an incorrect color"

let rec gui_game_loop g recent_cards =
  let cur_player = current_player g in
  if is_cpu cur_player then gui_cpu_play g cur_player recent_cards
  else gui_player_play g cur_player recent_cards

and gui_player_play g cur_player recent_cards =
  let cur_player_hand = player_hand cur_player in
  let other_player =
    (List.filter (fun p -> id p <> id cur_player)) (players g)
  in
  let other_players_info =
    List.map (fun p -> (id p, List.length (player_hand p))) other_player
  in
  let old_played_card =
    main_win (top_card g)
      (id (current_player g))
      (stack_penalty g) other_players_info cur_player_hand
  in
  gui_player_play_2 g recent_cards old_played_card

and gui_player_play_2 g recent_cards old_played_card =
  let played_card =
    match old_played_card with
    | None -> None
    | Some c ->
        if fst (actions c).swap then
          let swapping_player_id = int_of_string (swap_player_win g) in
          Some (set_swap_id c swapping_player_id)
        else Some c
  in
  let g =
    match played_card with
    | None -> g
    | Some c ->
        if fst (actions c).swap then
          change_current_players_hand (Option.get old_played_card) c g
        else g
  in
  gui_player_play_3 g recent_cards old_played_card played_card

and gui_player_play_3 g recent_cards old_played_card played_card =
  match play played_card g with
  | Illegal -> gui_game_loop g recent_cards
  | Legal next_g -> (
      let next_p_id = id (current_player next_g) in
      match played_card with
      | Some c ->
          gui_player_play_card g recent_cards played_card next_p_id c
            next_g
      | None ->
          intermit_win next_p_id recent_cards;
          gui_game_loop next_g recent_cards)
  | GameOver winner -> end_game_win (Player.id winner) ()

and gui_player_play_card g recent_cards played_card next_p_id c next_g =
  if color c = ANY then
    let new_color = color_change () in
    let new_c = change_color c new_color in
    let recent_cards =
      update_five_most_recent_card new_c recent_cards
    in
    colored_card_play g recent_cards next_p_id new_c c
  else normal_card_play g recent_cards next_p_id c next_g

and normal_card_play g recent_cards next_p_id c next_g =
  let recent_cards = update_five_most_recent_card c recent_cards in
  intermit_win next_p_id recent_cards;
  gui_game_loop next_g recent_cards

and colored_card_play g recent_cards next_p_id new_c c =
  match play (Some new_c) (change_current_players_hand c new_c g) with
  | Legal next ->
      intermit_win next_p_id recent_cards;
      gui_game_loop next recent_cards
  | _ -> failwith "Illegal game state.\n"

and gui_cpu_play_help
    g
    cur_player
    recent_cards
    played_action
    hand_card
    cpu_card
    changed_gst =
  match play cpu_card changed_gst with
  | Legal new_gst ->
      let next_p_id = id (current_player new_gst) in
      let recent_cards =
        match cpu_card with
        | None -> recent_cards
        | Some c -> update_five_most_recent_card c recent_cards
      in
      intermit_win next_p_id recent_cards;
      gui_game_loop new_gst recent_cards
  | Illegal -> failwith "The computer made an error!\n\n"
  | GameOver winner -> end_game_win (Player.id winner) ()

and gui_cpu_play g cur_player recent_cards =
  let played_action = action g in
  let hand_card = fst played_action in
  let cpu_card = snd played_action in
  let changed_gst =
    if Option.is_some cpu_card then
      change_current_players_hand (Option.get hand_card)
        (Option.get cpu_card) g
    else g
  in
  gui_cpu_play_help g cur_player recent_cards played_action hand_card
    cpu_card changed_gst

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  intro_win;
  let players_tup = select_win () in
  let players = create_players [] (fst players_tup) (snd players_tup) in
  if fst players_tup + snd players_tup < 2 then (
    intro_win;
    main ())
  else
    let start_state = init_state standard_cards players in
    gui_game_loop start_state []

(* Execute the game engine. *)
let () = main ()
