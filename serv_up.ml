open Unix

(* do not know how to implement *)
let connect i o = failwith "TODO"

(* I guess its the address of this device? *)
let addr =
  Unix.getsockname (Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0)

let actually_make = false

(* trying to create a server here, not making much progress *)
let my_server =
  if actually_make then establish_server connect addr else ()

let run () = print_endline "done!"

let () = run ()
