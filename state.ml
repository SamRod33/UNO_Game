(** Implementation of State in UNO

    @author Samuel Rodriguez (sar325)
    Date: 03/28/21
*)

(* Abstraction Function: TODO
 *
 *
 * Representation Invariant: TODO
 *)

 type t = () (* TODO *)

 type deck = () (* TODO *)

 type result = 
  | Legal of t
  | Illegal

  let top_card g = failwith("TODO")

  let stack_penalty g = failwith("TODO")

  let current_player g = failwith("TODO")

  let init_state c p_list = failwith("TODO")

  let play p c g = failwith("TODO")
