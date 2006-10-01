open Ppl_ocaml
open Printf
open Gmp

let rec print_linear_expression  = function
    Variable v ->
      print_string "V(";
      print_int v;
      print_string ")";
  | Coefficient c ->
      print_int(Z.to_int c)
  | Unary_Minus e ->
      print_string "-(";
      print_linear_expression e;
      print_string ")";
  | Unary_Plus e ->
      print_linear_expression e
  | Plus (e1, e2) ->
      print_string "(";
      print_linear_expression e1;
      print_string " + ";
      print_linear_expression e2;
      print_string ")";
  | Minus (e1, e2) ->
      print_string "(";
      print_linear_expression e1;
      print_string " - ";
      print_linear_expression e2;
      print_string ")";
  | Times (c, e) ->
      print_int(Z.to_int c);
      print_string "*(";
      print_linear_expression e;
      print_string ")";
;;

(* Build linear expressions the hard way. *)

print_string "Build linear expressions manually:\n" ;;

let rec a = Variable 0
and b = Variable 1
and c = Variable 2
and n = Coefficient (Z.from_int 3)
and e1 = Plus (c, c)
and e2 = Times ((Z.from_int 7), a)
;;

print_linear_expression a; print_string "\n" ;;
print_linear_expression b; print_string "\n" ;;
print_linear_expression c; print_string "\n" ;;
print_linear_expression n; print_string "\n" ;;
print_linear_expression e1; print_string "\n" ;;
print_linear_expression e2; print_string "\n" ;;

(* See whether operators can make life better. *)

print_string "Build linear expressions with operators:\n" ;;

let linear_expression_of_int n = Coefficient (Z.of_int n) ;;
let linear_expression_plus e1 e2 = Plus (e1, e2) ;;
let linear_expression_minus e1 e2 = Minus (e1, e2) ;;
let linear_expression_times c e = Times (c, e) ;;
let linear_constraint_eq e1 e2 = Equal (e1, e2) ;;
let linear_constraint_lt e1 e2 = Less_Than (e1, e2) ;;
let linear_constraint_gt e1 e2 = Greater_Than (e1, e2) ;;
let linear_constraint_le e1 e2 = Less_Than_Or_Equal (e1, e2) ;;
let linear_constraint_ge e1 e2 = Greater_Than_Or_Equal (e1, e2) ;;

let ( +/ ) = linear_expression_plus
let ( -/ ) = linear_expression_minus
let ( */ ) = linear_expression_times
let ( =/ ) = linear_constraint_eq
let ( </ ) = linear_constraint_lt
let ( >/ ) = linear_constraint_gt
let ( <=/ ) = linear_constraint_le
let ( >=/ ) = linear_constraint_ge

let e3 =
  (Z.of_int 3) */ a
  +/
  (Z.of_int 4) */ b
  -/
  (linear_expression_of_int 7)
;;

print_linear_expression e3; print_string "\n" ;;

(* Probably the most convenient thing for the user will be to use the
   the Camlp4 preprocessor: see
   http://caml.inria.fr/pub/docs/manual-ocaml/manual003.html#htoc10 *)

(* Build a PPL::Linear_Expression out of an OCaml linear_expression. *)

print_string "Build and print the corresponding PPL::Linear_Expression:\n" ;;

test_linear_expression e3 ;;

(* Build a PPL::Constraint out of an OCaml linear_constraint. *)

print_string "Build and print a PPL::Constraint:\n" ;;

test_linear_constraint (e3 >/ e1) ;;

(* Build a PPL::Generator out of an OCaml linear_generator. *)

print_string "Build and print a PPL::Generator:\n" ;;

test_linear_generator (Ray e3) ;;

(* Build a PPL::Constraint_System out of an OCaml constraint_system. *)

print_string "Build and print a PPL::Constraint_System:\n" ;;

test_constraint_system [e3 >/ e1; e1 >/ e2; e1 <=/ e2 -/ n] ;;

(* Build a PPL::Generator_System out of an OCaml generator_system. *)

print_string "Build and print a PPL::Generator_System:\n" ;;

test_generator_system [(Ray e3); (Line e1); (Closure_Point (e2, (Z.from_int 5)))] ;;

print_string "Bye!\n"

