(**Interfaces file to define new excpeptions, new types and interfaces function to PPL*)

open Gmp

type linear_expression =
    Variable of int
  | Coefficient of Z.t
  | Unary_Plus of linear_expression
  | Unary_Minus of linear_expression
  | Plus of linear_expression * linear_expression
  | Minus of linear_expression * linear_expression
  | Times of Z.t * linear_expression

type linear_constraint =
    Less_Than of linear_expression * linear_expression
  | Less_Than_Or_Equal of linear_expression * linear_expression
  | Equal of linear_expression * linear_expression
  | Greater_Than of linear_expression * linear_expression
  | Greater_Than_Or_Equal of linear_expression * linear_expression

type linear_generator =
    Line of linear_expression
  | Ray of linear_expression
  | Point of linear_expression * Z.t
  | Closure_Point of linear_expression * Z.t

type constraint_system = linear_constraint list

type generator_system = linear_generator list

external test_linear_expression:
  linear_expression -> unit = "test_linear_expression"

external test_linear_constraint:
  linear_constraint -> unit = "test_linear_constraint"

external test_linear_generator:
  linear_generator -> unit = "test_linear_generator"

(*
val add_contraint_to_consys :
  linear_constraint list -> linear_constraint -> linear_constraint list

val add_generator_to_gensys :
    linear_generator list -> linear_generator -> linear_generator list

external test_con_sys:
  linear_constraint list -> unit = "test_con_sys"

external test_gen_sys:
  linear_generator list -> unit = "test_con_sys"
*)
