open OUnit2
open Card

let suite = "test suite for Card" >::: List.flatten []

let _ = run_test_tt_main suite
