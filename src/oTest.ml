(********************************************************************)
(*                                                                  *)
(* OTest: an OCaml unit test framework                              *)
(* Copyright (C) 2009  Alex Leighton                                *)
(*                                                                  *)
(* OTest is free software: you can redistribute it and/or modify it *)
(* under the terms of the GNU General Public License as published   *)
(* by the Free Software Foundation, either version 3 of the         *)
(* License, or (at your option) any later version.                  *)
(*                                                                  *)
(* This program is distributed in the hope that it will be useful,  *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of   *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *)
(* GNU General Public License for more details.                     *)
(*                                                                  *)
(********************************************************************)

(** An OCaml unit testing framework. *)

(** Exception indicating failure of a test. *)
exception Fail of string

(** Verbosity type. *)
type verboseness = Off | On

(** Current level of verbosity. *)
let verbosity_level = ref Off

(** Set the current level of verbosity. *)
let set_verbosity v = verbosity_level := v

(** The signature of a test collection. *)
module type TestCollection = sig

  type input
  type output

  (** A test case consists of the name of the test, the input to the test,
      the test itself, and the expected output of the test. *)
  type testcase = string * input * (input -> output) * output

  val name : string
    (** The name of the collection of tests. *)

  val tests : testcase list
    (** The tests. *)

end

(** Test Harness functor which creates a test harness given a collection of
    tests. *)
module TestHarness (T : TestCollection) =
struct

  (** Test results = Total Tests * Tests Passed * Result list *)
  type testresults = int * int * (bool * T.output option) list

  (**/**)
  let test_or_tests n = if n = 1 then " test" else " tests"

  let print_msg s newline =
    match !verbosity_level with
        Off -> ()
      | On  -> if newline then print_endline s else print_string s

  let runtime start stop n =
    let runtime = stop -. start in
      Printf.sprintf "Ran %d%s in %2.3fs" n (test_or_tests n) runtime

  let passed_message np nt =
      Printf.sprintf "%d of %d%s passed" np nt (test_or_tests nt)
  (**/**)

  let runtest (name,input,test,eoutput) =
    try
      print_msg (Printf.sprintf "Running %s test %s ... " T.name name) false;
      let aoutput = test input in
        if aoutput = eoutput
        then ( print_msg "ok" true;
               true,(Some aoutput),"passed" )
        else ( print_msg "failed" true;
               print_msg "    reason: expected output not received\n" true;
               false,(Some aoutput),"expected output not received." )
    with
      | Fail msg ->
          print_msg "failed" true;
          print_msg (Printf.sprintf "    reason: %s\n" msg) true;
          false,None,msg

  let run_tests () =
    let start = Sys.time () in
    let results = List.map runtest T.tests in
    let stop = Sys.time () in
    let numtests = List.length results in
    let passed,failed = List.partition (fun (x,y,z) -> x) results in
    let numpassed = List.length passed in
      print_msg "--------------------------------------------------" true;
      print_endline (runtime start stop numtests);
      print_endline (passed_message numpassed numtests);
      numtests,numpassed,results

end

(** {2 Assertions} *)

(** Fails the current test immediately. *)
let fail ?(msg="failure") = raise (Fail msg)

(**/**)
let pass = ()

let make_message m f = if m = "" then f else (f ^ ": " ^ m)

let pass_or_fail v f msg =
  if v then pass else fail ~msg:msg

(**/**)

(** {3 Asserting Truth}

    Asserting that the given value is true. *)

(**/**)
let __assert ?(msg="") a f =
  let m = make_message msg f in pass_or_fail a f m
(**/**)

(** Fails the current test if [v] is not true.
    @param msg The message to use on failure.
    @param v   The value asserted to be true. *)
let assert_true ?(msg="") v = __assert v "assert_true" ~msg:msg

(** See {!assert_true}. *)
let assert_ ?(msg="") v = __assert v "assert_" ~msg:msg

(** See {!assert_true}. *)
let fail_unless ?(msg="") v = __assert v "fail_unless" ~msg:msg


(** {3 Asserting Falsehood}

    Asserting that the given value is false. *)

(**/**)
let __failif ?(msg="") a f =
  let m = make_message msg f in pass_or_fail (not a) f m
(**/**)

(** Fails the current test if [v] is true.
    @param msg The message to use on failure.
    @param v   The value asserted to be false. *)
let assert_false ?(msg="") v = __failif v "assert_false" ~msg:msg

(** See {!assert_false}. *)
let fail_if ?(msg="") a = __failif a "fail_if" ~msg:msg


(** {3 Asserting Equality}

    Assert that the given values are equal. *)

(**/**)
let __assertequal ?(msg="") a b f =
  let m = make_message msg f in pass_or_fail (a = b) f m
(**/**)

(** Fails the current test if [a != b].
    @param msg The message to use on failure.
    @param a   The first value to compare.
    @param b   The second value to compare. *)
let assert_equal ?(msg="") a b =
  __assertequal a b "assert_equal" ~msg:msg

(** See {!assert_equal}. *)
let fail_unless_equal ?(msg="") a b  =
  __assertequal a b "fail_unless_equal" ~msg:msg


(** {3 Asserting Inequality}

    Asserting that the given values are not equal. *)

(**/**)
let __assertnotequal ?(msg="") a b f =
  let m = make_message msg f in pass_or_fail (a != b) f m
(**/**)

(** Fails the current test if [a = b].
    @param msg The message to use on failure.
    @param a   The first value to compare.
    @param b   The second value to compare. *)
let assert_not_equal ?(msg="") a b =
  __assertnotequal a b "assert_not_equal" ~msg:msg

(** See {!assert_not_equal}. *)
let fail_if_equal ?(msg="") a b =
  __assertnotequal a b "fail_if_equal" ~msg:msg


(** {3 Asserting Exceptions}

    Assert that the given exception is raised when the given function
    is evaluated. *)

(**/**)
let __assertraises ?(msg="") e fn f =
  let m = make_message msg f in
    try fn (); fail ~msg:m
    with exn ->
      if e = exn then pass else raise exn
(**/**)

(** Fails the current test if the given exception is not raised.
    @param msg The message to use on failure.
    @param e   The exception that should be raised.
    @param fn  The function to evaluate. *)
let assert_raises ?(msg="") e fn =
  __assertraises e fn "assert_raises" ~msg:msg

(** See {!assert_raises}. *)
let fail_unless_raises ?(msg="") e fn =
  __assertraises e fn "fail_unless_raises" ~msg:msg
