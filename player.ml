open Card

exception CardNotInHand of Card.t

type t = {
  name : string;
  hand : Card.t list;
  id : int;
  is_computer : bool;
}

let swap_hands p1_id p2_id players =
  let rec find_hand p_id = function
    | [] ->
        failwith
          ("player not found. id attempted = " ^ string_of_int p_id)
    | h :: t -> if h.id = p_id then h.hand else find_hand p_id t
  in
  let p1_hand = find_hand p1_id players in
  let p2_hand = find_hand p2_id players in
  List.map
    (fun p ->
      if p.id = p1_id then { p with hand = p2_hand }
      else if p.id = p2_id then { p with hand = p1_hand }
      else p)
    players

let is_cpu p = p.is_computer

let create name is_cpu id_num =
  { name; hand = []; id = id_num; is_computer = is_cpu }

let create_test name h is_cpu id =
  { name; hand = h; id; is_computer = is_cpu }

let name p = p.name

let player_hand p = p.hand

let is_uno p =
  match p.hand with [] -> true | [ h ] -> true | _ -> false

let add_card player card = { player with hand = card :: player.hand }

(* [remove_first_of_dup hand card acc] is the player's hand after
   removing the first of any given card form the player's original hand
   [hand] if the card [card] is in the hand.*)
let rec remove_first_of_dup hand card acc =
  match hand with
  | [] -> hand
  | h :: t ->
      if h = card then acc @ t
      else remove_first_of_dup t card (h :: acc)

let remove_card player card =
  if not (List.mem card player.hand) then raise (CardNotInHand card)
  else
    let new_hand = remove_first_of_dup player.hand card [] in
    { player with hand = new_hand }

let id p = p.id
