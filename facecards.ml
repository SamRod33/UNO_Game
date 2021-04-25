(* [top_face] is the top part of a face card *)
let top_face = ("***********", "*         *")

(* [bot_face] is the bottom part of a face card *)
let bot_face = ("*         *", "***********")

(*** FACE CARDS PP ***)

let zero =
  [
    fst top_face;
    snd top_face;
    "*  @@@@@  *";
    "*  @   @  *";
    "*  @   @  *";
    "*  @   @  *";
    "*  @@@@@  *";
    fst bot_face;
    snd bot_face;
  ]

let one =
  [
    fst top_face;
    snd top_face;
    "*  @@@    *";
    "*    @    *";
    "*    @    *";
    "*    @    *";
    "*  @@@@@  *";
    fst bot_face;
    snd bot_face;
  ]

let two =
  [
    fst top_face;
    snd top_face;
    "*  @@@@@  *";
    "*      @  *";
    "*  @@@@@  *";
    "*  @      *";
    "*  @@@@@  *";
    fst bot_face;
    snd bot_face;
  ]

let three =
  [
    fst top_face;
    snd top_face;
    "*  @@@@@  *";
    "*      @  *";
    "*   @@@@  *";
    "*      @  *";
    "*  @@@@@  *";
    fst bot_face;
    snd bot_face;
  ]

let four =
  [
    fst top_face;
    snd top_face;
    "*  @   @  *";
    "*  @   @  *";
    "*  @@@@@  *";
    "*      @  *";
    "*      @  *";
    fst bot_face;
    snd bot_face;
  ]

let five =
  [
    fst top_face;
    snd top_face;
    "*  @@@@@  *";
    "*  @      *";
    "*  @@@@@  *";
    "*      @  *";
    "*  @@@@@  *";
    fst bot_face;
    snd bot_face;
  ]

let six =
  [
    fst top_face;
    snd top_face;
    "*  @@@@@  *";
    "*  @      *";
    "*  @@@@@  *";
    "*  @   @  *";
    "*  @@@@@  *";
    fst bot_face;
    snd bot_face;
  ]

let seven =
  [
    fst top_face;
    snd top_face;
    "*  @@@@@  *";
    "*      @  *";
    "*      @  *";
    "*      @  *";
    "*      @  *";
    fst bot_face;
    snd bot_face;
  ]

let eight =
  [
    fst top_face;
    snd top_face;
    "*  @@@@@  *";
    "*  @   @  *";
    "*  @@@@@  *";
    "*  @   @  *";
    "*  @@@@@  *";
    fst bot_face;
    snd bot_face;
  ]

let nine =
  [
    fst top_face;
    snd top_face;
    "*  @@@@@  *";
    "*  @   @  *";
    "*  @@@@@  *";
    "*      @  *";
    "*  @@@@@  *";
    fst bot_face;
    snd bot_face;
  ]

let skip =
  [
    fst top_face;
    snd top_face;
    "*  @@@@@  *";
    "* @   @ @ *";
    "* @  @  @ *";
    "* @ @   @ *";
    "*  @@@@@  *";
    fst bot_face;
    snd bot_face;
  ]

let reverse =
  [
    fst top_face;
    snd top_face;
    "*    @@@  *";
    "*     @@  *";
    "*  @ @ @  *";
    "*  @@     *";
    "*  @@@    *";
    fst bot_face;
    snd bot_face;
  ]

let swap =
  [
    fst top_face;
    snd top_face;
    "* @@@  /\\ *";
    "* @@@  || *";
    "* @### || *";
    "*  ### || *";
    "*  ### \\/ *";
    fst bot_face;
    snd bot_face;
  ]

let plus_2 =
  [
    fst top_face;
    snd top_face;
    "*  @@@ +2 *";
    "*  @@@    *";
    "*  @@###  *";
    "*    ###  *";
    "* +2 ###  *";
    fst bot_face;
    snd bot_face;
  ]

let plus_4 =
  [
    fst top_face;
    snd top_face;
    "*  @@ @@  *";
    "*  @@ @@  *";
    "*   +4    *";
    "*  @@ @@  *";
    "*  @@ @@  *";
    fst bot_face;
    snd bot_face;
  ]

let wild =
  [
    fst top_face;
    snd top_face;
    "*  @@#@@  *";
    "*  @@#@@  *";
    "*  #####  *";
    "*  @@#@@  *";
    "*  @@#@@  *";
    fst bot_face;
    snd bot_face;
  ]

let empty =
  [
    fst top_face;
    snd top_face;
    "";
    "";
    "";
    "";
    "";
    fst bot_face;
    snd bot_face;
  ]
