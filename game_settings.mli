(*The abstract type of values representing the game settings in play *)
type t

open Yojson.Basic.Util

(** [from_json j] is the game settings that [j] represents. Requires: [j] is
    a valid JSON game setting representation. *)
val from_json : Yojson.Basic.t -> GameSettings.t

(*returns true if rule is in play*)
val stack_2s_and_4s : bool
val 
val rules3