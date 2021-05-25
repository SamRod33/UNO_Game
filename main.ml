open State
open Card
open Player
open Computer
open MainFunctions

(** [player_hand_info player] is a string with infomation about the hand
    of [player].*)
let player_hand_info player =
  name player ^ " has "
  ^ string_of_int (List.length (player_hand player))
  ^ " cards. "

(** [player_hands_info player g] is a string with information about the
    hands of every player in state [g] besides [player].*)
let player_hands_info player g =
  List.fold_left
    (fun str p -> str ^ player_hand_info p)
    ""
    (all_players_but_one player g)

(** [quit_str] is the string displayed when the user quits the game. *)
let quit_str = "Thanks for playing!\n\n"

(** [check_quit ()] reads player input from the terminal and either ends
    the game, is [None], or is [Some n] for some integer [n].*)
let check_quit () =
  match read_line () with
  | "Quit" ->
      print_string quit_str;
      exit 0
  | a -> int_of_string_opt a

(** [clear ()] is the command for clearing the terminal. *)
let clear () = ignore (Sys.command "clear")

(** [buffer next_gst recent] buffers the game in the next gamestate
    [next_gst] in-between each player's turn after card [recent] was
    played.*)
let buffer next_gst recent =
  clear ();
  print_string "\nThe most recently played cards are:\n";
  pp_cards recent false;
  print_string
    ("It is "
    ^ name (current_player next_gst)
    ^ "'s turn. Enter anything to continue.\n\n");
  match read_line () with
  | "Quit" ->
      print_string quit_str;
      exit 0
  | _ -> clear ()

(** [fail_str] is the string used for when an action has failed. *)
let fail_str = "Try again.\n"

(** [illegal_card] is the string used when a play with an illegal card
    is attempted. *)
let illegal_card = "You can't play that card. Try another.\n"

(** [select_color ()] is the terminal prompt for selecting the color of
    a wild card. *)
let rec select_color () =
  print_string "Type in R, G, B, or Y to select the color.\n\n";
  match read_line () with
  | "Quit" ->
      print_string "Thanks for playing!\n\n";
      exit 0
  | "R" -> R
  | "G" -> G
  | "B" -> B
  | "Y" -> Y
  | _ ->
      print_string "Choose a valid color.\n\n";
      select_color ()

(** [failed ()] clears the terminal and displays a fail message. *)
let failed () =
  clear ();
  print_string fail_str

(** [select_swap_player gst] handles the logic for handling when a
    players plays a swap card. *)
let rec select_swap_player gst =
  clear ();
  let other_players = all_players_but_one (current_player gst) gst in
  print_string (player_hands_info (current_player gst) gst);
  print_string
    "\n\
     Type in the index of the player you want to swap hands with. The \
     index of the leftmost player is 0.\n";
  match check_quit () with
  | None ->
      failed ();
      select_swap_player gst
  | Some n ->
      if n > -1 && n < List.length other_players then
        get_nth_player_id n other_players
      else (
        failed ();
        select_swap_player gst)

(** [game_loop g recent_cards] runs the game loop for game [g] with
    recently played cards [recent_cards]. *)
let rec game_loop g recent_cards =
  let cur_player = current_player g in
  if is_cpu cur_player then cpu_play g cur_player recent_cards
  else player_play g cur_player recent_cards

(** [format_card_buffer_aux next_gst recent_cards] is the buffer for
    when the transition to [next_gst] needs a buffer when there are
    recently played cards [recent_cards]. *)
and format_card_buffer_aux next_gst recent_cards =
  buffer next_gst recent_cards;
  game_loop next_gst recent_cards

(** [format_card gst next_gst recent_cards] buffers the game and handles
    changing colors from a wild card for human players (computer already
    handles changing colors) in gamestate [gst] with recently played
    cards [recent_cards] for next gamestate [next_gst]. *)
and format_card gst next_gst recent_cards = function
  | None -> format_card_buffer_aux next_gst recent_cards
  | Some c ->
      if color c = ANY then
        let new_c = change_color c (select_color ()) in
        let recent_cards =
          update_five_most_recent_card new_c recent_cards
        in
        match
          play (Some new_c) (change_current_players_hand c new_c gst)
        with
        | Legal next ->
            buffer next_gst recent_cards;
            game_loop next recent_cards
        | _ -> print_string "Illegal game state.\n"
      else
        let recent_cards =
          update_five_most_recent_card c recent_cards
        in
        format_card_buffer_aux next_gst recent_cards

(** [player_play g cur_player recent_cards] handles the logic for human
    players taking their turn in game [g] with current player
    [cur_player] and recently played cards [recent_cards]. *)
and player_play g cur_player recent_cards =
  let cur_player_hand = player_hand cur_player in
  print_string ("It is " ^ name cur_player ^ "'s turn.\n");
  print_string "The top card is:\n\n";
  pp_cards [ top_card g ] false;
  print_string
    ("\nThe current stack penalty is "
    ^ string_of_int (stack_penalty g)
    ^ ".\n");
  print_string (player_hands_info cur_player g);
  print_string "\nYour cards are:\n\n";
  pp_cards cur_player_hand true;
  print_endline
    "\n\
     Type in the index of the card you wish to play (starting from 0). \
     If you do not have a card to play, type -1.\n";
  print_string "> ";
  end_of_game_loop g cur_player cur_player_hand recent_cards

(** [cpu_play g cur_player recent_cards] handles the logic for computer
    players taking their turn in game [g] with current player
    [cur_player] and recently played cards [recent_cards]. *)
and cpu_play g cur_player recent_cards =
  let played_action = action g in
  let hand_card = fst played_action in
  let cpu_card = snd played_action in
  let changed_gst =
    if Option.is_some cpu_card then
      change_current_players_hand (Option.get hand_card)
        (Option.get cpu_card) g
    else g
  in
  match play cpu_card changed_gst with
  | Legal new_gst ->
      format_card changed_gst new_gst recent_cards cpu_card
  | Illegal -> print_string "The computer made an error!\n\n"
  | GameOver _ ->
      print_string "The computer wins...\n\n";
      exit 0

(** [end_of_game_loop g cur_player cur_player_hand recent_cards] handles
    the game logic in game [g] with recently played cards [recent_cards]
    after the current player [cur_player] with hand [cur_player_hand]
    decides which card to play. *)
and end_of_game_loop g cur_player cur_player_hand recent_cards =
  match check_quit () with
  | None ->
      failed ();
      game_loop g recent_cards
  | Some n ->
      if n > -2 && n < List.length cur_player_hand then
        let index_card =
          if n = -1 then None else Some (List.nth cur_player_hand n)
        in
        let is_swap =
          n <> -1 && fst (actions (Option.get index_card)).swap
        in
        end_of_game_loop_2 g cur_player cur_player_hand index_card
          is_swap recent_cards
      else failed ();
      game_loop g recent_cards

(** [end_of_game_loop_2] continues the logic of the above function. *)
and end_of_game_loop_2
    g
    cur_player
    cur_player_hand
    index_card
    is_swap
    recent_cards =
  let play_card =
    if is_swap then
      Some (set_swap_id (Option.get index_card) (select_swap_player g))
    else index_card
  in
  let g =
    if is_swap then
      change_current_players_hand (Option.get index_card)
        (Option.get play_card) g
    else g
  in
  end_of_game_loop_3 g cur_player cur_player_hand index_card is_swap
    play_card recent_cards

(** [end_of_game_loop_3] continues the logic of the above function. *)
and end_of_game_loop_3
    g
    cur_player
    cur_player_hand
    index_card
    is_swap
    play_card
    recent_cards =
  match play play_card g with
  | Illegal ->
      clear ();
      print_string illegal_card;
      game_loop g recent_cards
  | Legal next_g ->
      clear ();
      format_card g next_g recent_cards play_card
  | GameOver winner ->
      print_string ("\n" ^ name winner ^ " wins!\n\n");
      exit 0

(** [play_game players] begins the game loop with players [players]. *)
let play_game players =
  let start_state = init_state standard_cards players in
  game_loop start_state []

(** [computer_opponents_prompt n restart] is a helper that takes input
    on computer opponents given some integer [n]. *)
let computer_opponents_prompt n restart =
  print_endline
    "How many computer opponents do you want? (In addition to the \
     earlier number).\n";
  print_string "> ";
  match check_quit () with
  | None -> restart ()
  | Some c ->
      if n + c >= 2 && n > 0 && c >= 0 then
        c |> create_players [] n |> play_game
      else restart ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nUno \nType 'Quit' at any time to quit the game.\n\n";
  let rec getPlayers u =
    let restart () =
      clear ();
      print_string fail_str;
      getPlayers ()
    in
    print_endline
      "Type in the number of people that want to play the game.\n";
    print_string "> ";
    match check_quit () with
    | None -> restart ()
    | Some n -> computer_opponents_prompt n restart
  in
  getPlayers ()

(* Executes the game engine. *)
let () = main ()
