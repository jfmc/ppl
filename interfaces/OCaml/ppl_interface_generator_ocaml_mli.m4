m4_define(`dnl', `m4_dnl')
dnl Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>
dnl
dnl This file is part of the Parma Polyhedra Library (PPL).
dnl
dnl The PPL is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by the
dnl Free Software Foundation; either version 2 of the License, or (at your
dnl option) any later version.
dnl
dnl The PPL is distributed in the hope that it will be useful, but WITHOUT
dnl ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
dnl FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
dnl for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software Foundation,
dnl Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.
dnl
dnl For the most up-to-date information see the Parma Polyhedra Library
dnl site: http://www.cs.unipr.it/ppl/ .

dnl This file generates ppl_prolog.icc.
dnl
dnl Include files defining macros that generate the non-fixed part.
m4_include(`ppl_interface_generator_ocaml_mli_code.m4')dnl
m4_include(`ppl_interface_generator_common.m4')dnl
m4_include(`ppl_interface_generator_common_dat.m4')dnl
m4_include(`ppl_interface_generator_ocaml_dat.m4')dnl
dnl
m4_divert(-1)dnl

dnl m4_pre_all_classes_code
dnl
dnl Definition for converting a term to a class handle code for all
dnl classes must be placed before all the generated code so that one class
dnl can be copied from another.
m4_define(`m4_pre_all_classes_code', `')


dnl m4_pre_extra_class_code(Class_Counter)
dnl Prefix extra code for each class.
m4_define(`m4_pre_extra_class_code', `dnl
m4_ifelse(m4_interface_class$1, Polyhedron,
type c_`'m4_downcase(m4_interface_class$1)
type nnc_`'m4_downcase(m4_interface_class$1),
type m4_downcase(m4_interface_class$1))

')

m4_divert`'dnl
dnl
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

type linear_grid_generator =
    Grid_Line of linear_expression
  | Grid_Parameter of linear_expression * Z.t
  | Grid_Point of linear_expression * Z.t

type poly_gen_relation =
    Subsumes

(* type relation_with_grid_generator = *)
(*     Subsumes *)

type poly_con_relation =
    Is_Disjoint
  | Strictly_Intersects
  | Is_Included
  | Saturates

type relation_with_congruence =
    Is_Disjoint
  | Strictly_Intersects
  | Is_Included

type linear_congruence = linear_expression * linear_expression * Z.t

type constraint_system = linear_constraint list

type generator_system = linear_generator list

type grid_generator_system = linear_grid_generator list

type congruence_system = linear_congruence list

(* Declared temporarily in this way to avoid name clashes. *)
type relation_symbol = Less_Than_RS | Less_Than_Or_Equal_RS | Equal_RS
                       | Greater_Than_RS | Greater_Than_Or_Equal_RS

type optimization_mode = Minimization | Maximization

type mip_problem_status = Unfeasible_Mip_Problem | Unbounded_Mip_Problem
                        | Optimized_Mip_Problem


type mip_problem

 val ppl_new_MIP_Problem_from_space_dimension:
 int -> mip_problem

val ppl_new_MIP_Problem:
      int -> constraint_system -> linear_expression
	-> optimization_mode -> mip_problem

val ppl_MIP_Problem_space_dimension:
  mip_problem -> int

val ppl_MIP_Problem_constraints:
  mip_problem -> constraint_system

val ppl_MIP_Problem_add_space_dimensions_and_embed:
  mip_problem -> int -> unit

val ppl_MIP_Problem_add_to_integer_space_dimensions:
  mip_problem -> int list -> unit

val ppl_MIP_Problem_add_constraint:
  mip_problem -> linear_constraint -> unit

val ppl_MIP_Problem_add_constraints:
  mip_problem -> constraint_system -> unit

val ppl_MIP_Problem_set_objective_function:
  mip_problem -> linear_expression -> unit

val ppl_MIP_Problem_is_satisfiable:
  mip_problem -> bool

val ppl_MIP_Problem_solve:
  mip_problem -> mip_problem_status

val ppl_MIP_Problem_optimization_mode:
  mip_problem -> optimization_mode

val ppl_MIP_Problem_feasible_point:
  mip_problem -> linear_generator

val ppl_MIP_Problem_optimizing_point:
  mip_problem -> linear_generator

val ppl_MIP_Problem_objective_function:
  mip_problem -> linear_expression

val ppl_MIP_Problem_optimal_value:
  mip_problem -> Z.t * Z.t

val ppl_MIP_Problem_evaluate_objective_function:
  mip_problem -> linear_generator  -> Z.t * Z.t

val ppl_MIP_Problem_OK:
  mip_problem -> bool

val ppl_MIP_Problem_clear:
  mip_problem -> unit

val ppl_MIP_Problem_set_optimization_mode:
  mip_problem -> optimization_mode -> unit

val ppl_MIP_Problem_swap:
  mip_problem -> mip_problem -> unit

val test_linear_expression:
  linear_expression -> unit

val test_linear_constraint:
  linear_constraint -> unit

val test_linear_generator:
  linear_generator -> unit

val test_constraint_system:
  constraint_system -> unit

val test_generator_system:
  generator_system -> unit

val test_linear_constraint:
  linear_constraint -> unit

val test_linear_generator:
  linear_generator -> unit

val test_constraint_system:
  constraint_system -> unit

val test_generator_system:
  generator_system -> unit




dnl
dnl Generate the non-fixed part of the file.
m4_all_code`'dnl
dnl
