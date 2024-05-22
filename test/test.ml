open OUnit2
open Gui
module C1 = CatWalk

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. T*)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let pp_bool b = match b with false -> "false" | true -> "true"

let select_string_test (name : string) (expected_output : string)
    (input : Gui.t) : test =
  name >:: fun _ -> assert_equal expected_output (select_string input)

let select_string_testing =
  [
    select_string_test "testing work with not 1 cycle"
      "Click to start a work session!" work_obj;
    select_string_test "testing short break" "Click to take a short break!"
      break_obj;
    select_string_test "testing long break" "Click to take a long break!"
      long_break_obj;
  ]

let emptysafe_test (name : string) (input : string) (expected_output : int) :
    test =
  name >:: fun _ -> assert_equal expected_output (emptysafe_int_of_string input)

let emptysafe_testing =
  [
    emptysafe_test "testing emptysafe 0 is 0" "0" 0;
    emptysafe_test "testing emptysafe 1 is 1" "1" 1;
    emptysafe_test "testing non-number is 0" "a" 0;
    emptysafe_test "testing long string is 0" "abc" 0;
    emptysafe_test "testing long integer is the int" "1394812" 1394812;
  ]

let sec_triple_test (name : string) (input : string * string * string)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (seconds_from_triple input)

let sec_triple_testing =
  [
    sec_triple_test "she/her" ("she", "her", "hers") 0;
    sec_triple_test "they/them" ("they", "them", "theirs") 0;
    sec_triple_test "he/him" ("he", "him", "his") 0;
    sec_triple_test "she/in" ("she", "in", "it") 0;
    sec_triple_test "slay/slem/0" ("slay", "slem", "0") 0;
    sec_triple_test "slem/0/slay" ("slem", "0", "slay") 0;
    sec_triple_test "0/slay/slem" ("0", "slay", "slem") 0;
    sec_triple_test "01:11:11" ("01", "11", "11") 4271;
    sec_triple_test "12:11:11" ("12", "11", "11") 43871;
    sec_triple_test "10:00:00" ("12", "00", "00") 43200;
  ]

let time_string_test (name : string) input expected_output : test =
  name >:: fun _ -> assert_equal expected_output (time_string input)

let a = ref 3600
let b = ref 3000
let c = ref 60
let d = ref 20
let e = ref 1
let f = ref 0

let time_string_testing =
  [
    time_string_test "ref 3600 is 0" a "01:00:00";
    time_string_test "ref 3600 is 0" b "00:50:00";
    time_string_test "ref 3600 is 0" c "00:01:00";
    time_string_test "ref 3600 is 0" d "00:00:20";
    time_string_test "ref 3600 is 0" e "00:00:01";
    time_string_test "ref 3600 is 0" f "00:00:00";
  ]

let in_rectangle_test (name : string) (expected_output : bool) (x : int)
    (y : int) (rect_x : int) (rect_y : int) (rect_w : int) (rect_h : int) : test
    =
  name >:: fun _ ->
  print_string (pp_bool (in_rectangle x y rect_x rect_y rect_w rect_h));
  print_endline "";
  assert_equal expected_output (in_rectangle x y rect_x rect_y rect_w rect_h)

let in_rectangle_testing =
  [
    (*
       if x > rect_x && x < rect_x + rect_w && y > rect_y && y < rect_y + rect_h then
         true
       else false

        (x)(y) (rect_x) (rect_y ) (rect_w) (rect_h) *)
    in_rectangle_test "all the same values is false" false 10 10 10 10 10 10;
    in_rectangle_test "all 0 is false" false 0 0 0 0 0 0;
    in_rectangle_test "all the same negative ints is false" false (-10) (-10)
      (-10) (-10) (-10) (-10);
    in_rectangle_test "y > rect_y is violated" false 100 60 60 60 60 60;
    in_rectangle_test "y < rect_y + rect_h is violatad" false 100 100 60 60 60
      10;
    in_rectangle_test "x > rect_x is violated" false 60 100 60 60 60 60;
    in_rectangle_test "x < rect_x + rect_w is violatad" false 100 100 60 60 10
      60;
    in_rectangle_test "true value with positive ints only" true 100 100 60 60 60
      60;
    in_rectangle_test "true value with x and y ints only - width can't be <0"
      true (-60) (-60) (-100) (-100) 50 50;
  ]

let newcatpos_test (name : string) (direction : int) (oldx : int) (oldy : int)
    (expected_output : int * int) : test =
  name >:: fun _ -> assert_equal expected_output (newcatpos direction oldx oldy)

let speed = 2

let newcatpos_testing =
  [
    newcatpos_test "testing 0 with x, y zero" 0 0 0 (0 + speed, 0);
    newcatpos_test "testing 0 with x nonzero y zero" 0 1 0 (1 + speed, 0);
    newcatpos_test "testing 0 with x zero y nonzero" 0 0 1 (0 + speed, 1);
    newcatpos_test "testing 1 with x, y zero" 1 0 0 (0, 0 + speed);
    newcatpos_test "testing 1 with x nonzero y zero" 1 1 0 (1, 0 + speed);
    newcatpos_test "testing 1 with x zero y nonzero" 1 0 1 (0, 1 + speed);
    newcatpos_test "testing 2 with x, y zero" 2 0 0 (0 - speed, 0);
    newcatpos_test "testing 2 with x nonzero y zero" 2 1 0 (1 - speed, 0);
    newcatpos_test "testing 2 with x zero y nonzero" 2 0 1 (0 - speed, 1);
    newcatpos_test "testing 3 with x, y zero" 3 0 0 (0, 0 - speed);
    newcatpos_test "testing 3 with x nonzero y zero" 3 1 0 (1, 0 - speed);
    newcatpos_test "testing 3 with x zero y nonzero" 3 0 1 (0, 1 - speed);
  ]

let gui_timer_tests =
  [
    "test suites for gui.ml"
    >::: List.flatten
           [
             select_string_testing;
             emptysafe_testing;
             sec_triple_testing;
             time_string_testing;
             in_rectangle_testing;
           ];
  ]

let cat_timer_tests =
  [ "test suites for cat.ml" >::: List.flatten [ newcatpos_testing ] ]

let suite =
  "test suites for whole project "
  >::: List.flatten [ gui_timer_tests; cat_timer_tests ]

let _ = run_test_tt_main suite
