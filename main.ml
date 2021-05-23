open State
open Card
open Player
open Computer

let five_most_recent_card = ref []

let update_five_most_recent_card c () =
  if List.length !five_most_recent_card <= 5 then (
    five_most_recent_card := c :: !five_most_recent_card;
    () )
  else
    match !five_most_recent_card with
    | c1 :: c2 :: c3 :: c4 :: t ->
        five_most_recent_card := [ c; c1; c2; c3; c4 ];
        ()
    | _ -> failwith "Impossible case b/c we can only have length of 5"

(** [create_players num_real num_computer] is a list of
    [num_real + num_computer] players, where [num_computer] are
    computers.*)
let create_players players num_real num_computer =
  let rec help lst cpus = function
    | 0 -> lst
    | n ->
        let player =
          if cpus then create ("Computer " ^ string_of_int n) true
          else create ("Player " ^ string_of_int n) false
        in
        help (player :: lst) cpus (n - 1)
  in
  let people = help [] true num_computer in
  help people false num_real

(** [all_players_but_one player g] is a list of all the players but
    [player] in state [g].*)
let all_players_but_one player g =
  List.filter (fun p -> id p <> id player) (players g)

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

let quit_str = "Thanks for playing!\n\n"

(** reads player input from the terminal and either ends the game, is
    [None], or is [Some n] for some integer [n].*)
let check_quit () =
  match read_line () with
  | "Quit" ->
      print_string quit_str;
      exit 0
  | a -> int_of_string_opt a

let clear () = ignore (Sys.command "clear")

(** buffers the game in-between each player's turn.*)
let buffer next_gst =
  clear ();
  print_string
    ( "It is "
    ^ name (current_player next_gst)
    ^ "'s turn. Enter anything to continue.\n" );
  match read_line () with
  | "Quit" ->
      print_string quit_str;
      exit 0
  | _ -> clear ()

let fail_str = "Try again.\n"

let illegal_card = "You can't play that card. Try another.\n"

(** terminal prompt for selecting the color of a wild card.*)
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

let failed () =
  clear ();
  print_string fail_str

(** [get_nth_player_id n players] is the id of the (n+1)th player in
    [players] starting from the leftmost player.*)
let rec get_nth_player_id n players =
  match players with
  | [] -> failwith "not enough players"
  | h :: t -> if n = 0 then id h else get_nth_player_id (n - 1) t

(* handles the logic for handling when a players plays a swap card.*)
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
        select_swap_player gst )

let rec game_loop g =
  let cur_player = current_player g in
  if is_cpu cur_player then cpu_play g cur_player
  else player_play g cur_player

(** buffers the game and handles changing colors from a wild card for
    human players (computer already handles changing colors).*)
and format_card gst next_gst = function
  | None ->
      buffer next_gst;
      game_loop next_gst
  | Some c ->
      if color c = ANY then
        let new_c = change_color c (select_color ()) in
        match
          play (Some new_c) (change_current_players_hand c new_c gst)
        with
        | Legal next ->
            buffer next_gst;
            game_loop next
        | _ -> print_string "Illegal game state.\n"
      else buffer next_gst;
      game_loop next_gst

(** handles logic for human players taking their turn.*)
and player_play g cur_player =
  let cur_player_hand = player_hand cur_player in
  print_string ("It is " ^ name cur_player ^ "'s turn.\n");
  print_string "The top card is:\n\n";
  pp_cards [ top_card g ] false;
  print_string
    ( "\nThe current stack penalty is "
    ^ string_of_int (stack_penalty g)
    ^ ".\n" );
  print_string (player_hands_info cur_player g);
  print_string "\nYour cards are:\n\n";
  pp_cards cur_player_hand true;
  print_endline
    "\n\
     Type in the index of the card you wish to play (starting from 0). \
     If you do not have a card to play, type -1.\n";
  print_string "> ";
  end_of_game_loop g cur_player cur_player_hand

(** handles logic for computer players taking their turn.*)
and cpu_play g cur_player =
  let cpu_card = action g in
  let changed_gst =
    if
      Option.is_some cpu_card
      && (actions (Option.get cpu_card)).change_color
    then
      change_current_players_hand
        (change_color (Option.get cpu_card) ANY)
        (Option.get cpu_card) g
    else g
  in
  match play cpu_card changed_gst with
  | Legal new_gst -> format_card changed_gst new_gst cpu_card
  | Illegal -> print_string "The computer made an error!\n\n"
  | GameOver _ ->
      print_string "The computer wins...\n\n";
      exit 0

(** handles game logic after player decides which card to play.*)
and end_of_game_loop g cur_player cur_player_hand =
  match check_quit () with
  | None ->
      failed ();
      game_loop g
  | Some n ->
      if n > -2 && n < List.length cur_player_hand then (
        let index_card =
          if n = -1 then None else Some (List.nth cur_player_hand n)
        in
        let is_swap =
          n <> -1 && fst (actions (Option.get index_card)).swap
        in
        update_five_most_recent_card index_card ();
        end_of_game_loop_2 g cur_player cur_player_hand index_card
          is_swap )
      else failed ();
      game_loop g

(** continuation of above function*)
and end_of_game_loop_2 g cur_player cur_player_hand index_card is_swap =
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
    play_card

(** continuation of above function*)
and end_of_game_loop_3
    g
    cur_player
    cur_player_hand
    index_card
    is_swap
    play_card =
  match play play_card g with
  | Illegal ->
      clear ();
      print_string illegal_card;
      game_loop g
  | Legal next_g ->
      clear ();
      format_card g next_g play_card
  | GameOver winner ->
      print_string ("\n" ^ name winner ^ " wins!\n\n");
      exit 0

let play_game players =
  let start_state = init_state standard_cards players in
  game_loop start_state

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
    | Some n -> (
        print_endline
          "How many computer opponents do you want? (In addition to \
           the earlier number).\n";
        print_string "> ";
        match check_quit () with
        | None -> restart ()
        | Some c ->
            if n + c >= 2 && n > 0 && c >= 0 then
              c |> create_players [] n |> play_game
            else restart () )
  in
  getPlayers ()

(* Execute the game engine. *)
let () = main ()
