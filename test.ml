(** Testing Comment Explanation:

    **********************************************************************
    Which parts of the system were automatically tested by OUnit vs.
    manually tested:

    We automatically tested much of the game logic using OUnit2 tests
    and bisect as we did for other assignments. This was done for the
    player, card, state, computer, and test helper modules. We wrote
    test cases that had an coverage of 87.63% across all ten files
    tested. These files are listed below:

    [card.ml - card_test.ml - computer.ml - computer_test.ml - player.ml
    - player_test.ml - state.ml - state_test.ml - test.ml -
    test_helper.ml]

    We manually tested the GUI windows. This was done by looking at the
    output of the screen each time an individual GUI window was run and
    checking to make sure all images and objects were drawn in the
    correct places on the window. In doing this, we also discovered a
    few bugs in the logic before they were automatically tested.

    **********************************************************************
    What modules were tested by OUnit and how test cases were developed
    (black box, glass box, randomized, etc.):

    The modules tested by OUnit are listed below (same list as above):
    the player, card, state, computer, and test helper modules. For
    player, card, state, and computer, tests were initially created with
    black box testing. As errors were detected, more tests were created
    with glass box testing to ensure these errors did not occur again.
    FOr the test and test_helper files, we used glass box testing.

    **********************************************************************
    Argument for why the testing approach demonstrates the correctness
    of the system:

    Our testing approach demonstraes correctness of the system because
    we have verified the visuals and logic of the game in most ways that
    a user could interact with the program. While we tested the GUI main
    manually, it's logic is based on a system

    The main for terminal - that was automatically system. Since both
    mains share logic, the correctness of the terminal main implies the
    correctness of the GUI main. *)

include CardTest
include PlayerTest
include ComputerTest
include StateTest
open OUnit2

(** [test_suites] is the list combining all tests in the program. *)
let test_suites =
  List.flatten
    [
      CardTest.card_suite;
      PlayerTest.player_suite;
      ComputerTest.computer_suite;
      StateTest.state_suite;
    ]

let full_suite = "Testing entire suite" >::: test_suites

let _ = run_test_tt_main full_suite
