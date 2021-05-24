open State
open Card
open Player
open Computer

(** [update_five_most_recent_card c cards] is [c :: cards] if
    [List.length cards < 5] and [cards] with the last element in the
    list removed and [c] added to the beginning of the list if
    [List.length cards = 5]. Requires: [List.length cards <= 5]*)
let update_five_most_recent_card card = function
  | [ a; b; c; d; e ] -> [ card; a; b; c; d ]
  | a -> card :: a

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

(** [get_nth_player_id n players] is the id of the (n+1)th player in
    [players] starting from the leftmost player.*)
let rec get_nth_player_id n players =
  match players with
  | [] -> failwith "not enough players"
  | h :: t -> if n = 0 then id h else get_nth_player_id (n - 1) t
