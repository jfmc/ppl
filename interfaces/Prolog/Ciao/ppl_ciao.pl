
/* Ciao Prolog interface: Ciao Prolog part.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ .

*/

:- module(ppl_ciao,
[

	  ppl_version_major/1,
	  ppl_version_minor/1,
	  ppl_version_revision/1,
	  ppl_version_beta/1,
	  ppl_version/1,
	  ppl_banner/1,
	  ppl_max_space_dimension/1,
	  ppl_Coefficient_is_bounded/0,
	  ppl_Coefficient_max/1,
	  ppl_Coefficient_min/1,
	  ppl_initialize/0,
	  ppl_finalize/0,
	  ppl_set_timeout_exception_atom/1,
	  ppl_timeout_exception_atom/1,
	  ppl_set_timeout/1,
	  ppl_reset_timeout/0,
	  ppl_new_LP_Problem_trivial/1,
	  ppl_new_LP_Problem/4,
	  ppl_new_LP_Problem_from_LP_Problem/2,
	  ppl_LP_Problem_swap/2,
	  ppl_delete_LP_Problem/1,
	  ppl_LP_Problem_space_dimension/2,
	  ppl_LP_Problem_constraints/2,
	  ppl_LP_Problem_objective_function/2,
	  ppl_LP_Problem_optimization_mode/2,
	  ppl_LP_Problem_clear/1,
	  ppl_LP_Problem_add_constraint/2,
	  ppl_LP_Problem_add_constraints/2,
	  ppl_LP_Problem_set_objective_function/2,
	  ppl_LP_Problem_set_optimization_mode/2,
	  ppl_LP_Problem_is_satisfiable/1,
	  ppl_LP_Problem_solve/2,
	  ppl_LP_Problem_feasible_point/2,
	  ppl_LP_Problem_optimizing_point/2,
	  ppl_LP_Problem_optimal_value/3,
	  ppl_LP_Problem_evaluate_objective_function/4,
	  ppl_LP_Problem_OK/1,
	  ppl_new_C_Polyhedron_from_space_dimension/3,
	  ppl_new_NNC_Polyhedron_from_space_dimension/3,
	  ppl_new_C_Polyhedron_from_C_Polyhedron/2,
	  ppl_new_C_Polyhedron_from_NNC_Polyhedron/2,
	  ppl_new_NNC_Polyhedron_from_C_Polyhedron/2,
	  ppl_new_NNC_Polyhedron_from_NNC_Polyhedron/2,
	  ppl_new_C_Polyhedron_from_constraints/2,
	  ppl_new_NNC_Polyhedron_from_constraints/2,
	  ppl_new_C_Polyhedron_from_generators/2,
	  ppl_new_NNC_Polyhedron_from_generators/2,
	  ppl_new_C_Polyhedron_from_bounding_box/2,
	  ppl_new_NNC_Polyhedron_from_bounding_box/2,
	  ppl_Polyhedron_swap/2,
	  ppl_delete_Polyhedron/1,
	  ppl_Polyhedron_space_dimension/2,
	  ppl_Polyhedron_affine_dimension/2,
	  ppl_Polyhedron_get_constraints/2,
	  ppl_Polyhedron_get_generators/2,
	  ppl_Polyhedron_get_minimized_constraints/2,
	  ppl_Polyhedron_get_minimized_generators/2,
	  ppl_Polyhedron_relation_with_constraint/3,
	  ppl_Polyhedron_relation_with_generator/3,
	  ppl_Polyhedron_get_bounding_box/3,
	  ppl_Polyhedron_is_empty/1,
	  ppl_Polyhedron_is_universe/1,
	  ppl_Polyhedron_is_bounded/1,
	  ppl_Polyhedron_bounds_from_above/2,
	  ppl_Polyhedron_bounds_from_below/2,
	  ppl_Polyhedron_maximize/5,
	  ppl_Polyhedron_maximize_with_point/6,
	  ppl_Polyhedron_minimize/5,
	  ppl_Polyhedron_minimize_with_point/6,
	  ppl_Polyhedron_is_topologically_closed/1,
	  ppl_Polyhedron_topological_closure_assign/1,
	  ppl_Polyhedron_contains_Polyhedron/2,
	  ppl_Polyhedron_strictly_contains_Polyhedron/2,
	  ppl_Polyhedron_is_disjoint_from_Polyhedron/2,
	  ppl_Polyhedron_equals_Polyhedron/2,
	  ppl_Polyhedron_OK/1,
	  ppl_Polyhedron_add_constraint/2,
	  ppl_Polyhedron_add_generator/2,
	  ppl_Polyhedron_add_constraint_and_minimize/2,
	  ppl_Polyhedron_add_generator_and_minimize/2,
	  ppl_Polyhedron_add_constraints/2,
	  ppl_Polyhedron_add_generators/2,
	  ppl_Polyhedron_add_constraints_and_minimize/2,
	  ppl_Polyhedron_add_generators_and_minimize/2,
	  ppl_Polyhedron_intersection_assign/2,
	  ppl_Polyhedron_intersection_assign_and_minimize/2,
	  ppl_Polyhedron_poly_hull_assign/2,
	  ppl_Polyhedron_poly_hull_assign_and_minimize/2,
	  ppl_Polyhedron_poly_difference_assign/2,
	  ppl_Polyhedron_affine_image/4,
	  ppl_Polyhedron_affine_preimage/4,
	  ppl_Polyhedron_bounded_affine_image/5,
	  ppl_Polyhedron_bounded_affine_preimage/5,
	  ppl_Polyhedron_generalized_affine_image/5,
	  ppl_Polyhedron_generalized_affine_preimage/5,
	  ppl_Polyhedron_generalized_affine_image_lhs_rhs/4,
	  ppl_Polyhedron_generalized_affine_preimage_lhs_rhs/4,
	  ppl_Polyhedron_time_elapse_assign/2,
	  ppl_Polyhedron_BHRZ03_widening_assign_with_tokens/4,
	  ppl_Polyhedron_H79_widening_assign_with_tokens/4,
	  ppl_Polyhedron_BHRZ03_widening_assign/2,
	  ppl_Polyhedron_H79_widening_assign/2,
	  ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens/5,
	  ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens/5,
	  ppl_Polyhedron_limited_BHRZ03_extrapolation_assign/3,
	  ppl_Polyhedron_limited_H79_extrapolation_assign/3,
	  ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens/5,
	  ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens/5,
	  ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign/3,
	  ppl_Polyhedron_bounded_H79_extrapolation_assign/3,
	  ppl_Polyhedron_add_space_dimensions_and_project/2,
	  ppl_Polyhedron_add_space_dimensions_and_embed/2,
	  ppl_Polyhedron_concatenate_assign/2,
	  ppl_Polyhedron_remove_space_dimensions/2,
	  ppl_Polyhedron_remove_higher_space_dimensions/2,
	  ppl_Polyhedron_expand_space_dimension/3,
	  ppl_Polyhedron_fold_space_dimensions/3,
	  ppl_Polyhedron_map_space_dimensions/2
],
[
        assertions,
        basicmodes,
        regtypes,
        foreign_interface
]).

:- true pred ppl_version_major_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_version_major)).

ppl_version_major(Term1) :-
   ppl_version_major_2(Term1, 1).

:- true pred ppl_version_minor_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_version_minor)).

ppl_version_minor(Term1) :-
   ppl_version_minor_2(Term1, 1).

:- true pred ppl_version_revision_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_version_revision)).

ppl_version_revision(Term1) :-
   ppl_version_revision_2(Term1, 1).

:- true pred ppl_version_beta_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_version_beta)).

ppl_version_beta(Term1) :-
   ppl_version_beta_2(Term1, 1).

:- true pred ppl_version_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_version)).

ppl_version(Term1) :-
   ppl_version_2(Term1, 1).

:- true pred ppl_banner_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_banner)).

ppl_banner(Term1) :-
   ppl_banner_2(Term1, 1).

:- true pred ppl_max_space_dimension_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_max_space_dimension)).

ppl_max_space_dimension(Term1) :-
   ppl_max_space_dimension_2(Term1, 1).

:- true pred ppl_Coefficient_is_bounded_2(go(Success))
          ::  int
  + (returns(Success), foreign(ppl_Coefficient_is_bounded)).

ppl_Coefficient_is_bounded :-
   ppl_Coefficient_is_bounded_2(1).

:- true pred ppl_Coefficient_max_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Coefficient_max)).

ppl_Coefficient_max(Term1) :-
   ppl_Coefficient_max_2(Term1, 1).

:- true pred ppl_Coefficient_min_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Coefficient_min)).

ppl_Coefficient_min(Term1) :-
   ppl_Coefficient_min_2(Term1, 1).

:- true pred ppl_initialize +  foreign.
:- true pred ppl_finalize +  foreign.
:- true pred ppl_set_timeout_exception_atom(in(Term1))
          :: any_term +  foreign.
:- true pred ppl_timeout_exception_atom_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_timeout_exception_atom)).

ppl_timeout_exception_atom(Term1) :-
   ppl_timeout_exception_atom_2(Term1, 1).

:- true pred ppl_set_timeout(in(Term1))
          :: any_term +  foreign.
:- true pred ppl_reset_timeout +  foreign.
:- true pred ppl_new_LP_Problem_trivial_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_new_LP_Problem_trivial)).

ppl_new_LP_Problem_trivial(Term1) :-
   ppl_new_LP_Problem_trivial_2(Term1, 1).

:- true pred ppl_new_LP_Problem_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_LP_Problem)).

ppl_new_LP_Problem(Term1, Term2, Term3, Term4) :-
   ppl_new_LP_Problem_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_new_LP_Problem_from_LP_Problem_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_LP_Problem_from_LP_Problem)).

ppl_new_LP_Problem_from_LP_Problem(Term1, Term2) :-
   ppl_new_LP_Problem_from_LP_Problem_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_swap_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_swap)).

ppl_LP_Problem_swap(Term1, Term2) :-
   ppl_LP_Problem_swap_2(Term1, Term2, 1).

:- true pred ppl_delete_LP_Problem_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_delete_LP_Problem)).

ppl_delete_LP_Problem(Term1) :-
   ppl_delete_LP_Problem_2(Term1, 1).

:- true pred ppl_LP_Problem_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_space_dimension)).

ppl_LP_Problem_space_dimension(Term1, Term2) :-
   ppl_LP_Problem_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_constraints)).

ppl_LP_Problem_constraints(Term1, Term2) :-
   ppl_LP_Problem_constraints_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_objective_function_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_objective_function)).

ppl_LP_Problem_objective_function(Term1, Term2) :-
   ppl_LP_Problem_objective_function_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_optimization_mode_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_optimization_mode)).

ppl_LP_Problem_optimization_mode(Term1, Term2) :-
   ppl_LP_Problem_optimization_mode_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_clear_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_clear)).

ppl_LP_Problem_clear(Term1) :-
   ppl_LP_Problem_clear_2(Term1, 1).

:- true pred ppl_LP_Problem_add_constraint_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_add_constraint)).

ppl_LP_Problem_add_constraint(Term1, Term2) :-
   ppl_LP_Problem_add_constraint_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_add_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_add_constraints)).

ppl_LP_Problem_add_constraints(Term1, Term2) :-
   ppl_LP_Problem_add_constraints_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_set_objective_function_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_set_objective_function)).

ppl_LP_Problem_set_objective_function(Term1, Term2) :-
   ppl_LP_Problem_set_objective_function_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_set_optimization_mode_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_set_optimization_mode)).

ppl_LP_Problem_set_optimization_mode(Term1, Term2) :-
   ppl_LP_Problem_set_optimization_mode_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_is_satisfiable_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_is_satisfiable)).

ppl_LP_Problem_is_satisfiable(Term1) :-
   ppl_LP_Problem_is_satisfiable_2(Term1, 1).

:- true pred ppl_LP_Problem_solve_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_solve)).

ppl_LP_Problem_solve(Term1, Term2) :-
   ppl_LP_Problem_solve_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_feasible_point_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_feasible_point)).

ppl_LP_Problem_feasible_point(Term1, Term2) :-
   ppl_LP_Problem_feasible_point_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_optimizing_point_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_optimizing_point)).

ppl_LP_Problem_optimizing_point(Term1, Term2) :-
   ppl_LP_Problem_optimizing_point_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_optimal_value_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_optimal_value)).

ppl_LP_Problem_optimal_value(Term1, Term2, Term3) :-
   ppl_LP_Problem_optimal_value_2(Term1, Term2, Term3, 1).

:- true pred ppl_LP_Problem_evaluate_objective_function_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_evaluate_objective_function)).

ppl_LP_Problem_evaluate_objective_function(Term1, Term2, Term3, Term4) :-
   ppl_LP_Problem_evaluate_objective_function_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_LP_Problem_OK_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_OK)).

ppl_LP_Problem_OK(Term1) :-
   ppl_LP_Problem_OK_2(Term1, 1).

:- true pred ppl_new_C_Polyhedron_from_space_dimension_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_space_dimension)).

ppl_new_C_Polyhedron_from_space_dimension(Term1, Term2, Term3) :-
   ppl_new_C_Polyhedron_from_space_dimension_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_NNC_Polyhedron_from_space_dimension_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_space_dimension)).

ppl_new_NNC_Polyhedron_from_space_dimension(Term1, Term2, Term3) :-
   ppl_new_NNC_Polyhedron_from_space_dimension_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_C_Polyhedron_from_C_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_C_Polyhedron)).

ppl_new_C_Polyhedron_from_C_Polyhedron(Term1, Term2) :-
   ppl_new_C_Polyhedron_from_C_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_C_Polyhedron_from_NNC_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_NNC_Polyhedron)).

ppl_new_C_Polyhedron_from_NNC_Polyhedron(Term1, Term2) :-
   ppl_new_C_Polyhedron_from_NNC_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_NNC_Polyhedron_from_C_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_C_Polyhedron)).

ppl_new_NNC_Polyhedron_from_C_Polyhedron(Term1, Term2) :-
   ppl_new_NNC_Polyhedron_from_C_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_NNC_Polyhedron_from_NNC_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_NNC_Polyhedron)).

ppl_new_NNC_Polyhedron_from_NNC_Polyhedron(Term1, Term2) :-
   ppl_new_NNC_Polyhedron_from_NNC_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_C_Polyhedron_from_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_constraints)).

ppl_new_C_Polyhedron_from_constraints(Term1, Term2) :-
   ppl_new_C_Polyhedron_from_constraints_2(Term1, Term2, 1).

:- true pred ppl_new_NNC_Polyhedron_from_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_constraints)).

ppl_new_NNC_Polyhedron_from_constraints(Term1, Term2) :-
   ppl_new_NNC_Polyhedron_from_constraints_2(Term1, Term2, 1).

:- true pred ppl_new_C_Polyhedron_from_generators_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_generators)).

ppl_new_C_Polyhedron_from_generators(Term1, Term2) :-
   ppl_new_C_Polyhedron_from_generators_2(Term1, Term2, 1).

:- true pred ppl_new_NNC_Polyhedron_from_generators_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_generators)).

ppl_new_NNC_Polyhedron_from_generators(Term1, Term2) :-
   ppl_new_NNC_Polyhedron_from_generators_2(Term1, Term2, 1).

:- true pred ppl_new_C_Polyhedron_from_bounding_box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_bounding_box)).

ppl_new_C_Polyhedron_from_bounding_box(Term1, Term2) :-
   ppl_new_C_Polyhedron_from_bounding_box_2(Term1, Term2, 1).

:- true pred ppl_new_NNC_Polyhedron_from_bounding_box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_bounding_box)).

ppl_new_NNC_Polyhedron_from_bounding_box(Term1, Term2) :-
   ppl_new_NNC_Polyhedron_from_bounding_box_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_swap(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.
:- true pred ppl_delete_Polyhedron(in(Term1))
          :: any_term +  foreign.
:- true pred ppl_Polyhedron_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_space_dimension)).

ppl_Polyhedron_space_dimension(Term1, Term2) :-
   ppl_Polyhedron_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_affine_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_affine_dimension)).

ppl_Polyhedron_affine_dimension(Term1, Term2) :-
   ppl_Polyhedron_affine_dimension_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_get_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_get_constraints)).

ppl_Polyhedron_get_constraints(Term1, Term2) :-
   ppl_Polyhedron_get_constraints_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_get_generators_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_get_generators)).

ppl_Polyhedron_get_generators(Term1, Term2) :-
   ppl_Polyhedron_get_generators_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_get_minimized_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_get_minimized_constraints)).

ppl_Polyhedron_get_minimized_constraints(Term1, Term2) :-
   ppl_Polyhedron_get_minimized_constraints_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_get_minimized_generators_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_get_minimized_generators)).

ppl_Polyhedron_get_minimized_generators(Term1, Term2) :-
   ppl_Polyhedron_get_minimized_generators_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_relation_with_constraint_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_relation_with_constraint)).

ppl_Polyhedron_relation_with_constraint(Term1, Term2, Term3) :-
   ppl_Polyhedron_relation_with_constraint_2(Term1, Term2, Term3, 1).

:- true pred ppl_Polyhedron_relation_with_generator_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_relation_with_generator)).

ppl_Polyhedron_relation_with_generator(Term1, Term2, Term3) :-
   ppl_Polyhedron_relation_with_generator_2(Term1, Term2, Term3, 1).

:- true pred ppl_Polyhedron_get_bounding_box_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_get_bounding_box)).

ppl_Polyhedron_get_bounding_box(Term1, Term2, Term3) :-
   ppl_Polyhedron_get_bounding_box_2(Term1, Term2, Term3, 1).

:- true pred ppl_Polyhedron_is_empty_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_is_empty)).

ppl_Polyhedron_is_empty(Term1) :-
   ppl_Polyhedron_is_empty_2(Term1, 1).

:- true pred ppl_Polyhedron_is_universe_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_is_universe)).

ppl_Polyhedron_is_universe(Term1) :-
   ppl_Polyhedron_is_universe_2(Term1, 1).

:- true pred ppl_Polyhedron_is_bounded_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_is_bounded)).

ppl_Polyhedron_is_bounded(Term1) :-
   ppl_Polyhedron_is_bounded_2(Term1, 1).

:- true pred ppl_Polyhedron_bounds_from_above_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_bounds_from_above)).

ppl_Polyhedron_bounds_from_above(Term1, Term2) :-
   ppl_Polyhedron_bounds_from_above_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_bounds_from_below_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_bounds_from_below)).

ppl_Polyhedron_bounds_from_below(Term1, Term2) :-
   ppl_Polyhedron_bounds_from_below_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_maximize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_maximize)).

ppl_Polyhedron_maximize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Polyhedron_maximize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Polyhedron_maximize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_maximize_with_point)).

ppl_Polyhedron_maximize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Polyhedron_maximize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Polyhedron_minimize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_minimize)).

ppl_Polyhedron_minimize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Polyhedron_minimize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Polyhedron_minimize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_minimize_with_point)).

ppl_Polyhedron_minimize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Polyhedron_minimize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Polyhedron_is_topologically_closed_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_is_topologically_closed)).

ppl_Polyhedron_is_topologically_closed(Term1) :-
   ppl_Polyhedron_is_topologically_closed_2(Term1, 1).

:- true pred ppl_Polyhedron_topological_closure_assign(in(Term1))
          :: any_term +  foreign.
:- true pred ppl_Polyhedron_contains_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_contains_Polyhedron)).

ppl_Polyhedron_contains_Polyhedron(Term1, Term2) :-
   ppl_Polyhedron_contains_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_strictly_contains_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_strictly_contains_Polyhedron)).

ppl_Polyhedron_strictly_contains_Polyhedron(Term1, Term2) :-
   ppl_Polyhedron_strictly_contains_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_is_disjoint_from_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_is_disjoint_from_Polyhedron)).

ppl_Polyhedron_is_disjoint_from_Polyhedron(Term1, Term2) :-
   ppl_Polyhedron_is_disjoint_from_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_equals_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_equals_Polyhedron)).

ppl_Polyhedron_equals_Polyhedron(Term1, Term2) :-
   ppl_Polyhedron_equals_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_OK_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_OK)).

ppl_Polyhedron_OK(Term1) :-
   ppl_Polyhedron_OK_2(Term1, 1).

:- true pred ppl_Polyhedron_add_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_add_generator(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_add_constraint_and_minimize_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_add_constraint_and_minimize)).

ppl_Polyhedron_add_constraint_and_minimize(Term1, Term2) :-
   ppl_Polyhedron_add_constraint_and_minimize_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_add_generator_and_minimize_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_add_generator_and_minimize)).

ppl_Polyhedron_add_generator_and_minimize(Term1, Term2) :-
   ppl_Polyhedron_add_generator_and_minimize_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_add_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_add_generators(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_add_constraints_and_minimize_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_add_constraints_and_minimize)).

ppl_Polyhedron_add_constraints_and_minimize(Term1, Term2) :-
   ppl_Polyhedron_add_constraints_and_minimize_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_add_generators_and_minimize_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_add_generators_and_minimize)).

ppl_Polyhedron_add_generators_and_minimize(Term1, Term2) :-
   ppl_Polyhedron_add_generators_and_minimize_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_intersection_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_intersection_assign_and_minimize_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_intersection_assign_and_minimize)).

ppl_Polyhedron_intersection_assign_and_minimize(Term1, Term2) :-
   ppl_Polyhedron_intersection_assign_and_minimize_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_poly_hull_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_poly_hull_assign_and_minimize_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_poly_hull_assign_and_minimize)).

ppl_Polyhedron_poly_hull_assign_and_minimize(Term1, Term2) :-
   ppl_Polyhedron_poly_hull_assign_and_minimize_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_poly_difference_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_affine_image(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_bounded_affine_image(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_bounded_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_generalized_affine_image_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_generalized_affine_image)).

ppl_Polyhedron_generalized_affine_image(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Polyhedron_generalized_affine_image_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Polyhedron_generalized_affine_preimage_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_generalized_affine_preimage)).

ppl_Polyhedron_generalized_affine_preimage(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Polyhedron_generalized_affine_preimage_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Polyhedron_generalized_affine_image_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_generalized_affine_image_lhs_rhs)).

ppl_Polyhedron_generalized_affine_image_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_Polyhedron_generalized_affine_image_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Polyhedron_generalized_affine_preimage_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_generalized_affine_preimage_lhs_rhs)).

ppl_Polyhedron_generalized_affine_preimage_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_Polyhedron_generalized_affine_preimage_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Polyhedron_time_elapse_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_BHRZ03_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_BHRZ03_widening_assign_with_tokens)).

ppl_Polyhedron_BHRZ03_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_Polyhedron_BHRZ03_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Polyhedron_H79_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_H79_widening_assign_with_tokens)).

ppl_Polyhedron_H79_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_Polyhedron_H79_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Polyhedron_BHRZ03_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_H79_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens)).

ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens)).

ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Polyhedron_limited_BHRZ03_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_limited_H79_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens)).

ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens)).

ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_bounded_H79_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_add_space_dimensions_and_project(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_add_space_dimensions_and_embed(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_concatenate_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_remove_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_remove_space_dimensions)).

ppl_Polyhedron_remove_space_dimensions(Term1, Term2) :-
   ppl_Polyhedron_remove_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_remove_higher_space_dimensions(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_expand_space_dimension(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.
:- true pred ppl_Polyhedron_fold_space_dimensions_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_fold_space_dimensions)).

ppl_Polyhedron_fold_space_dimensions(Term1, Term2, Term3) :-
   ppl_Polyhedron_fold_space_dimensions_2(Term1, Term2, Term3, 1).

:- true pred ppl_Polyhedron_map_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_map_space_dimensions)).

ppl_Polyhedron_map_space_dimensions(Term1, Term2) :-
   ppl_Polyhedron_map_space_dimensions_2(Term1, Term2, 1).

:- extra_linker_opts('-L.libs').
:- use_foreign_library(ppl_ciao).

:- impl_defined(
[
 
	ppl_version_major_2/2,
	ppl_version_minor_2/2,
	ppl_version_revision_2/2,
	ppl_version_beta_2/2,
	ppl_version_2/2,
	ppl_banner_2/2,
	ppl_max_space_dimension_2/2,
	ppl_Coefficient_is_bounded_2/1,
	ppl_Coefficient_max_2/2,
	ppl_Coefficient_min_2/2,
	ppl_timeout_exception_atom_2/2,
	ppl_new_LP_Problem_trivial_2/2,
	ppl_new_LP_Problem_2/5,
	ppl_new_LP_Problem_from_LP_Problem_2/3,
	ppl_LP_Problem_swap_2/3,
	ppl_delete_LP_Problem_2/2,
	ppl_LP_Problem_space_dimension_2/3,
	ppl_LP_Problem_constraints_2/3,
	ppl_LP_Problem_objective_function_2/3,
	ppl_LP_Problem_optimization_mode_2/3,
	ppl_LP_Problem_clear_2/2,
	ppl_LP_Problem_add_constraint_2/3,
	ppl_LP_Problem_add_constraints_2/3,
	ppl_LP_Problem_set_objective_function_2/3,
	ppl_LP_Problem_set_optimization_mode_2/3,
	ppl_LP_Problem_is_satisfiable_2/2,
	ppl_LP_Problem_solve_2/3,
	ppl_LP_Problem_feasible_point_2/3,
	ppl_LP_Problem_optimizing_point_2/3,
	ppl_LP_Problem_optimal_value_2/4,
	ppl_LP_Problem_evaluate_objective_function_2/5,
	ppl_LP_Problem_OK_2/2,
	ppl_new_C_Polyhedron_from_space_dimension_2/4,
	ppl_new_NNC_Polyhedron_from_space_dimension_2/4,
	ppl_new_C_Polyhedron_from_C_Polyhedron_2/3,
	ppl_new_C_Polyhedron_from_NNC_Polyhedron_2/3,
	ppl_new_NNC_Polyhedron_from_C_Polyhedron_2/3,
	ppl_new_NNC_Polyhedron_from_NNC_Polyhedron_2/3,
	ppl_new_C_Polyhedron_from_constraints_2/3,
	ppl_new_NNC_Polyhedron_from_constraints_2/3,
	ppl_new_C_Polyhedron_from_generators_2/3,
	ppl_new_NNC_Polyhedron_from_generators_2/3,
	ppl_new_C_Polyhedron_from_bounding_box_2/3,
	ppl_new_NNC_Polyhedron_from_bounding_box_2/3,
	ppl_Polyhedron_space_dimension_2/3,
	ppl_Polyhedron_affine_dimension_2/3,
	ppl_Polyhedron_get_constraints_2/3,
	ppl_Polyhedron_get_generators_2/3,
	ppl_Polyhedron_get_minimized_constraints_2/3,
	ppl_Polyhedron_get_minimized_generators_2/3,
	ppl_Polyhedron_relation_with_constraint_2/4,
	ppl_Polyhedron_relation_with_generator_2/4,
	ppl_Polyhedron_get_bounding_box_2/4,
	ppl_Polyhedron_is_empty_2/2,
	ppl_Polyhedron_is_universe_2/2,
	ppl_Polyhedron_is_bounded_2/2,
	ppl_Polyhedron_bounds_from_above_2/3,
	ppl_Polyhedron_bounds_from_below_2/3,
	ppl_Polyhedron_maximize_2/6,
	ppl_Polyhedron_maximize_with_point_2/7,
	ppl_Polyhedron_minimize_2/6,
	ppl_Polyhedron_minimize_with_point_2/7,
	ppl_Polyhedron_is_topologically_closed_2/2,
	ppl_Polyhedron_contains_Polyhedron_2/3,
	ppl_Polyhedron_strictly_contains_Polyhedron_2/3,
	ppl_Polyhedron_is_disjoint_from_Polyhedron_2/3,
	ppl_Polyhedron_equals_Polyhedron_2/3,
	ppl_Polyhedron_OK_2/2,
	ppl_Polyhedron_add_constraint_and_minimize_2/3,
	ppl_Polyhedron_add_generator_and_minimize_2/3,
	ppl_Polyhedron_add_constraints_and_minimize_2/3,
	ppl_Polyhedron_add_generators_and_minimize_2/3,
	ppl_Polyhedron_intersection_assign_and_minimize_2/3,
	ppl_Polyhedron_poly_hull_assign_and_minimize_2/3,
	ppl_Polyhedron_generalized_affine_image_2/6,
	ppl_Polyhedron_generalized_affine_preimage_2/6,
	ppl_Polyhedron_generalized_affine_image_lhs_rhs_2/5,
	ppl_Polyhedron_generalized_affine_preimage_lhs_rhs_2/5,
	ppl_Polyhedron_BHRZ03_widening_assign_with_tokens_2/5,
	ppl_Polyhedron_H79_widening_assign_with_tokens_2/5,
	ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens_2/6,
	ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens_2/6,
	ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens_2/6,
	ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens_2/6,
	ppl_Polyhedron_remove_space_dimensions_2/3,
	ppl_Polyhedron_fold_space_dimensions_2/4,
	ppl_Polyhedron_map_space_dimensions_2/3
]).

:- comment(version_maintenance,off).

