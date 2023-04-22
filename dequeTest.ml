(*****************************************************************************)
(* PROBLEM 7: WRITING TEST CASES                                             *)
(*****************************************************************************)

;; open Assert
;; open Deque

(* `DequeTest` is used to test the deque implementation from deque.ml. 

   Read through the module, then write your test cases in the space
   provided below.  Your TAs will be grading the completeness of your
   tests.  *)

;; print_endline ("\n--- Running tests for Deque ---")

(* Here is a test to help get you started. *)

let test () : bool =
  is_empty (create ())
;; run_test "is_empty: call on empty returns true" test


(* Now, it's your turn! Make sure to comprehensively test all the other
   functions you implemented in deque.ml. It will probably be helpful to
   have the files deque.ml/mli open as you work.

   We provide many test cases for you, so your job here is to finish writing
   tests for `remove_head`, `remove_tail`, `delete_last`, `delete_first`, and
   `reverse`.

   Your TAs will be manually grading the completeness of your test cases.

   Note: Remember the difference between structural and reference
   equality; think about why you shouldn't be directly comparing 
   deques with the '=' of structural equality. *)

(* ---------- Write your own test cases below. ---------- *)
(* INSERT_HEAD TESTS *)
let test () : bool =
  let d = create () in
  insert_head 1 d;
  valid d && peek_head d = 1 && peek_tail d = 1
;; run_test "insert_head into empty" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  insert_head 2 d;
  valid d && peek_head d = 2 && peek_tail d = 1
;; run_test "insert_head into singleton" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  insert_head 2 d;
  insert_head 3 d;
  valid d && peek_head d = 3 && peek_tail d = 1
;; run_test "insert_head into non-empty, multi-element" test

(* INSERT_TAIL TESTS *)
let test () : bool =
  let d = create () in
  insert_tail 1 d;
  valid d && peek_head d = 1 && peek_tail d = 1
;; run_test "insert_tail into empty" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  valid d && peek_head d = 1 && peek_tail d = 2
;; run_test "insert_tail into singleton" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  insert_tail 3 d;
  valid d && peek_head d = 1 && peek_tail d = 3
;; run_test "insert_tail into non-empty, multi-element" test

(* REMOVE_HEAD *)
let test () : bool =
  let d = create () in
  remove_head d = failwith "empty deque"
;; run_test "remove_head failwith empty should be reported" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  remove_head d = 1 && is_empty d
;; run_test "remove_head singleton" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  insert_head 2 d;
  insert_head 3 d;
  valid d && remove_head d = 3 && peek_head d = 2
;; run_test "remove_head into non-empty, multi-element" test

(* REMOVE_TAIL *)
let test () : bool =
  let d = create () in
  remove_head d = failwith "empty deque"
;; run_test "remove_tail failwith empty should be reported" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  remove_tail d = 1
;; run_test "remove_tail singleton" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  insert_tail 3 d;
  valid d && remove_tail d = 3 && peek_tail d = 2
;; run_test "remove_tail into non-empty, multi-element" test

(* DELETE_LAST *)
let test () : bool =
  let d = create () in
  delete_last 1 d;
  is_empty d
;; run_test "delete first empty" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  delete_last 1 d;
  is_empty d
;; run_test "delete last singleton" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  insert_tail 3 d;
  delete_last 1 d;
  valid d && peek_head d = 2 && peek_tail d = 3
;; run_test "delete last head" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  insert_tail 3 d;
  insert_tail 2 d;
  delete_last 2 d;
  valid d && remove_head d = 1 && peek_head d = 2 && peek_tail d = 3
;; run_test "delete last tail" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  insert_tail 3 d;
  insert_tail 4 d;
  insert_tail 5 d;
  insert_tail 6 d;
  insert_tail 7 d;
  delete_last 8 d;
  to_list d = [1; 2; 3; 4; 5; 6; 7]
;; run_test "delete last no occurrences" test

let test () : bool =
  let d = create () in
  insert_tail 2 d;
  insert_tail 2 d;
  insert_tail 2 d;
  insert_tail 2 d;
  insert_tail 2 d;
  insert_tail 2 d;
  insert_tail 2 d;
  delete_first 2 d;
  to_list d = [2; 2; 2; 2; 2; 2]
;; run_test "delete last all occurrences" test

(* DELETE_FIRST *)
let test () : bool =
  let d = create () in
  delete_first 1 d;
  is_empty d
;; run_test "delete first empty" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  delete_first 1 d;
  is_empty d
;; run_test "delete first singleton" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  insert_tail 3 d;
  delete_first 1 d;
  valid d && peek_head d = 2 && peek_tail d = 3
;; run_test "delete first head" test

let test () : bool =
  let d = create () in
  insert_tail 3 d;
  insert_tail 2 d;
  insert_tail 1 d;
  delete_first 1 d;
  valid d && peek_head d = 3 && peek_tail d = 2
;; run_test "delete first tail" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  insert_tail 3 d;
  insert_tail 2 d;
  delete_first 2 d;
  to_list d = [1; 3; 2]
;; run_test "delete first multiple occurrence" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  insert_tail 3 d;
  insert_tail 4 d;
  insert_tail 5 d;
  insert_tail 6 d;
  insert_tail 7 d;
  delete_first 8 d;
  to_list d = [1; 2; 3; 4; 5; 6; 7]
;; run_test "delete first no occurrences" test

let test () : bool =
  let d = create () in
  insert_tail 2 d;
  insert_tail 2 d;
  insert_tail 2 d;
  insert_tail 2 d;
  insert_tail 2 d;
  insert_tail 2 d;
  insert_tail 2 d;
  delete_first 2 d;
  to_list d = [2; 2; 2; 2; 2; 2]
;; run_test "delete first all occurrences" test

(* REVERSE *)
let test () : bool =
  let d = create () in
  reverse d;
  valid d && is_empty d
;; run_test "reverse empty deque" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  reverse d;
  valid d && peek_head d = 1 && peek_tail d = 1
;; run_test "reverse singleton" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  insert_tail 3 d;
  reverse d;
  to_list d = [3; 2; 1]
;; run_test "reverse non-empty, multi-element" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  insert_tail 3 d;
  insert_tail 4 d;
  insert_tail 5 d;
  insert_tail 6 d;
  insert_tail 7 d;
  reverse d;
  to_list d = [7; 6; 5; 4; 3; 2; 1]
;; run_test "reverse non-empty, multi-element list odd" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  insert_tail 3 d;
  insert_tail 4 d;
  insert_tail 5 d;
  insert_tail 6 d;
  reverse d;
  to_list d = [6; 5; 4; 3; 2; 1]
;; run_test "reverse non-empty, multi-element list even" test


(* ---------- Write your own test cases above. ---------- *)
