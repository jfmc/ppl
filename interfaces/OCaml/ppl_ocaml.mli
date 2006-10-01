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

type polyhedron

external ppl_new_C_Polyhedron_from_space_dimension:
  int -> polyhedron = "ppl_new_C_Polyhedron_from_space_dimension"

external ppl_Polyhedron_space_dimension:
  polyhedron -> int = "ppl_Polyhedron_space_dimension"

external test_linear_expression:
  linear_expression -> unit = "test_linear_expression"

external test_linear_constraint:
  linear_constraint -> unit = "test_linear_constraint"

external test_linear_generator:
  linear_generator -> unit = "test_linear_generator"

external test_constraint_system:
  constraint_system -> unit = "test_constraint_system"

external test_generator_system:
  generator_system -> unit = "test_generator_system"
