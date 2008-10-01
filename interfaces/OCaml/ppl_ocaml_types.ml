(* OCaml interface: type declarations.
   Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . *)

open Gmp

type degenerate_element =
    Universe
  | Empty

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
  | Less_Or_Equal of linear_expression * linear_expression
  | Equal of linear_expression * linear_expression
  | Greater_Than of linear_expression * linear_expression
  | Greater_Or_Equal of linear_expression * linear_expression

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
type relation_symbol = Less_Than_RS | Less_Or_Equal_RS | Equal_RS
                       | Greater_Than_RS | Greater_Or_Equal_RS

type complexity_class = Polynomial_Complexity
                        | Simplex_Complexity
                        | Any_Complexity

type optimization_mode = Minimization | Maximization

type mip_problem_status = Unfeasible_Mip_Problem | Unbounded_Mip_Problem
                        | Optimized_Mip_Problem

type control_parameter_name = Pricing

type control_parameter_value = Pricing_Steepest_Edge_Float
                               | Pricing_Steepest_Edge_Exact
                               | Pricing_Textbook
