/* Prolog code for checking all predicates.  -*- C++ -*-
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


check_all :-
  ppl_initialize
,
  (ppl_new_C_Polyhedron_from_space_dimension_test) -> true ;
    true,
  (ppl_new_NNC_Polyhedron_from_space_dimension_test) -> true ;
    true,
  (ppl_new_C_Polyhedron_from_C_Polyhedron_test) -> true ;
    true,
  (ppl_new_C_Polyhedron_from_NNC_Polyhedron_test) -> true ;
    true,
  (ppl_new_NNC_Polyhedron_from_C_Polyhedron_test) -> true ;
    true,
  (ppl_new_NNC_Polyhedron_from_NNC_Polyhedron_test) -> true ;
    true,
  (ppl_new_C_Polyhedron_from_constraints_test) -> true ;
    true,
  (ppl_new_NNC_Polyhedron_from_constraints_test) -> true ;
    true,
  (ppl_new_C_Polyhedron_from_generators_test) -> true ;
    true,
  (ppl_new_NNC_Polyhedron_from_generators_test) -> true ;
    true,
  (ppl_new_C_Polyhedron_from_bounding_box_test) -> true ;
    true,
  (ppl_new_NNC_Polyhedron_from_bounding_box_test) -> true ;
    true,
  (ppl_Polyhedron_swap_test) -> true ;
    true,
  (ppl_delete_Polyhedron_test) -> true ;
    true,
  (ppl_Polyhedron_space_dimension_test) -> true ;
    true,
  (ppl_Polyhedron_affine_dimension_test) -> true ;
    true,
  (ppl_Polyhedron_get_constraints_test) -> true ;
    true,
  (ppl_Polyhedron_get_generators_test) -> true ;
    true,
  (ppl_Polyhedron_get_minimized_constraints_test) -> true ;
    true,
  (ppl_Polyhedron_get_minimized_generators_test) -> true ;
    true,
  (ppl_Polyhedron_relation_with_constraint_test) -> true ;
    true,
  (ppl_Polyhedron_relation_with_generator_test) -> true ;
    true,
  (ppl_Polyhedron_get_bounding_box_test) -> true ;
    true,
  (ppl_Polyhedron_is_empty_test) -> true ;
    true,
  (ppl_Polyhedron_is_universe_test) -> true ;
    true,
  (ppl_Polyhedron_is_bounded_test) -> true ;
    true,
  (ppl_Polyhedron_contains_integer_point_test) -> true ;
    true,
  (ppl_Polyhedron_is_topologically_closed_test) -> true ;
    true,
  (ppl_Polyhedron_topological_closure_assign_test) -> true ;
    true,
  (ppl_Polyhedron_bounds_from_above_test) -> true ;
    true,
  (ppl_Polyhedron_bounds_from_below_test) -> true ;
    true,
  (ppl_Polyhedron_maximize_test) -> true ;
    true,
  (ppl_Polyhedron_minimize_test) -> true ;
    true,
  (ppl_Polyhedron_maximize_with_point_test) -> true ;
    true,
  (ppl_Polyhedron_minimize_with_point_test) -> true ;
    true,
  (ppl_Polyhedron_contains_Polyhedron_test) -> true ;
    true,
  (ppl_Polyhedron_strictly_contains_Polyhedron_test) -> true ;
    true,
  (ppl_Polyhedron_is_disjoint_from_Polyhedron_test) -> true ;
    true,
  (ppl_Polyhedron_equals_Polyhedron_test) -> true ;
    true,
  (ppl_Polyhedron_OK_test) -> true ;
    true,
  (ppl_Polyhedron_add_constraint_test) -> true ;
    true,
  (ppl_Polyhedron_add_generator_test) -> true ;
    true,
  (ppl_Polyhedron_add_constraint_and_minimize_test) -> true ;
    true,
  (ppl_Polyhedron_add_generator_and_minimize_test) -> true ;
    true,
  (ppl_Polyhedron_add_constraints_test) -> true ;
    true,
  (ppl_Polyhedron_add_generators_test) -> true ;
    true,
  (ppl_Polyhedron_add_constraints_and_minimize_test) -> true ;
    true,
  (ppl_Polyhedron_add_generators_and_minimize_test) -> true ;
    true,
  (ppl_Polyhedron_intersection_assign_test) -> true ;
    true,
  (ppl_Polyhedron_upper_bound_assign_test) -> true ;
    true,
  (ppl_Polyhedron_difference_assign_test) -> true ;
    true,
  (ppl_Polyhedron_concatenate_assign_test) -> true ;
    true,
  (ppl_Polyhedron_time_elapse_assign_test) -> true ;
    true,
  (ppl_Polyhedron_poly_hull_assign_test) -> true ;
    true,
  (ppl_Polyhedron_poly_difference_assign_test) -> true ;
    true,
  (ppl_Polyhedron_intersection_assign_and_minimize_test) -> true ;
    true,
  (ppl_Polyhedron_poly_hull_assign_and_minimize_test) -> true ;
    true,
  (ppl_Polyhedron_affine_image_test) -> true ;
    true,
  (ppl_Polyhedron_affine_preimage_test) -> true ;
    true,
  (ppl_Polyhedron_bounded_affine_image_test) -> true ;
    true,
  (ppl_Polyhedron_bounded_affine_preimage_test) -> true ;
    true,
  (ppl_Polyhedron_generalized_affine_image_test) -> true ;
    true,
  (ppl_Polyhedron_generalized_affine_preimage_test) -> true ;
    true,
  (ppl_Polyhedron_generalized_affine_image_lhs_rhs_test) -> true ;
    true,
  (ppl_Polyhedron_generalized_affine_preimage_lhs_rhs_test) -> true ;
    true,
  (ppl_Polyhedron_BHRZ03_widening_assign_with_tokens_test) -> true ;
    true,
  (ppl_Polyhedron_H79_widening_assign_with_tokens_test) -> true ;
    true,
  (ppl_Polyhedron_BHRZ03_widening_assign_test) -> true ;
    true,
  (ppl_Polyhedron_H79_widening_assign_test) -> true ;
    true,
  (ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens_test) -> true ;
    true,
  (ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens_test) -> true ;
    true,
  (ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens_test) -> true ;
    true,
  (ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens_test) -> true ;
    true,
  (ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_test) -> true ;
    true,
  (ppl_Polyhedron_limited_H79_extrapolation_assign_test) -> true ;
    true,
  (ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_test) -> true ;
    true,
  (ppl_Polyhedron_bounded_H79_extrapolation_assign_test) -> true ;
    true,
  (ppl_Polyhedron_add_space_dimensions_and_embed_test) -> true ;
    true,
  (ppl_Polyhedron_add_space_dimensions_and_project_test) -> true ;
    true,
  (ppl_Polyhedron_remove_space_dimensions_test) -> true ;
    true,
  (ppl_Polyhedron_remove_higher_space_dimensions_test) -> true ;
    true,
  (ppl_Polyhedron_expand_space_dimension_test) -> true ;
    true,
  (ppl_Polyhedron_fold_space_dimensions_test) -> true ;
    true,
  (ppl_Polyhedron_map_space_dimensions_test) -> true ;
    true,
  (ppl_new_Grid_from_space_dimension_test) -> true ;
    true,
  (ppl_new_Grid_from_Grid_test) -> true ;
    true,
  (ppl_new_Grid_from_constraints_test) -> true ;
    true,
  (ppl_new_Grid_from_grid_generators_test) -> true ;
    true,
  (ppl_new_Grid_from_congruences_test) -> true ;
    true,
  (ppl_new_Grid_from_bounding_box_test) -> true ;
    true,
  (ppl_new_Grid_from_covering_box_test) -> true ;
    true,
  (ppl_Grid_swap_test) -> true ;
    true,
  (ppl_delete_Grid_test) -> true ;
    true,
  (ppl_Grid_space_dimension_test) -> true ;
    true,
  (ppl_Grid_affine_dimension_test) -> true ;
    true,
  (ppl_Grid_get_congruences_test) -> true ;
    true,
  (ppl_Grid_get_grid_generators_test) -> true ;
    true,
  (ppl_Grid_get_minimized_congruences_test) -> true ;
    true,
  (ppl_Grid_get_minimized_grid_generators_test) -> true ;
    true,
  (ppl_Grid_relation_with_congruence_test) -> true ;
    true,
  (ppl_Grid_relation_with_grid_generator_test) -> true ;
    true,
  (ppl_Grid_get_bounding_box_test) -> true ;
    true,
  (ppl_Grid_get_covering_box_test) -> true ;
    true,
  (ppl_Grid_is_empty_test) -> true ;
    true,
  (ppl_Grid_is_universe_test) -> true ;
    true,
  (ppl_Grid_is_bounded_test) -> true ;
    true,
  (ppl_Grid_contains_integer_point_test) -> true ;
    true,
  (ppl_Grid_is_topologically_closed_test) -> true ;
    true,
  (ppl_Grid_is_discrete_test) -> true ;
    true,
  (ppl_Grid_topological_closure_assign_test) -> true ;
    true,
  (ppl_Grid_bounds_from_above_test) -> true ;
    true,
  (ppl_Grid_bounds_from_below_test) -> true ;
    true,
  (ppl_Grid_maximize_test) -> true ;
    true,
  (ppl_Grid_minimize_test) -> true ;
    true,
  (ppl_Grid_maximize_with_point_test) -> true ;
    true,
  (ppl_Grid_minimize_with_point_test) -> true ;
    true,
  (ppl_Grid_contains_Grid_test) -> true ;
    true,
  (ppl_Grid_strictly_contains_Grid_test) -> true ;
    true,
  (ppl_Grid_is_disjoint_from_Grid_test) -> true ;
    true,
  (ppl_Grid_equals_Grid_test) -> true ;
    true,
  (ppl_Grid_OK_test) -> true ;
    true,
  (ppl_Grid_add_constraint_test) -> true ;
    true,
  (ppl_Grid_add_grid_generator_test) -> true ;
    true,
  (ppl_Grid_add_congruence_test) -> true ;
    true,
  (ppl_Grid_add_constraint_and_minimize_test) -> true ;
    true,
  (ppl_Grid_add_grid_generator_and_minimize_test) -> true ;
    true,
  (ppl_Grid_add_congruence_and_minimize_test) -> true ;
    true,
  (ppl_Grid_add_constraints_test) -> true ;
    true,
  (ppl_Grid_add_grid_generators_test) -> true ;
    true,
  (ppl_Grid_add_congruences_test) -> true ;
    true,
  (ppl_Grid_add_constraints_and_minimize_test) -> true ;
    true,
  (ppl_Grid_add_grid_generators_and_minimize_test) -> true ;
    true,
  (ppl_Grid_add_congruences_and_minimize_test) -> true ;
    true,
  (ppl_Grid_intersection_assign_test) -> true ;
    true,
  (ppl_Grid_upper_bound_assign_test) -> true ;
    true,
  (ppl_Grid_difference_assign_test) -> true ;
    true,
  (ppl_Grid_concatenate_assign_test) -> true ;
    true,
  (ppl_Grid_time_elapse_assign_test) -> true ;
    true,
  (ppl_Grid_join_assign_test) -> true ;
    true,
  (ppl_Grid_intersection_assign_and_minimize_test) -> true ;
    true,
  (ppl_Grid_join_assign_and_minimize_test) -> true ;
    true,
  (ppl_Grid_affine_image_test) -> true ;
    true,
  (ppl_Grid_affine_preimage_test) -> true ;
    true,
  (ppl_Grid_generalized_affine_image_test) -> true ;
    true,
  (ppl_Grid_generalized_affine_preimage_test) -> true ;
    true,
  (ppl_Grid_generalized_affine_image_lhs_rhs_test) -> true ;
    true,
  (ppl_Grid_generalized_affine_preimage_lhs_rhs_test) -> true ;
    true,
  (ppl_Grid_congruence_widening_assign_with_tokens_test) -> true ;
    true,
  (ppl_Grid_generator_widening_assign_with_tokens_test) -> true ;
    true,
  (ppl_Grid_congruence_widening_assign_test) -> true ;
    true,
  (ppl_Grid_generator_widening_assign_test) -> true ;
    true,
  (ppl_Grid_limited_congruence_extrapolation_assign_with_tokens_test) -> true ;
    true,
  (ppl_Grid_limited_generator_extrapolation_assign_with_tokens_test) -> true ;
    true,
  (ppl_Grid_limited_congruence_extrapolation_assign_test) -> true ;
    true,
  (ppl_Grid_limited_generator_extrapolation_assign_test) -> true ;
    true,
  (ppl_Grid_add_space_dimensions_and_embed_test) -> true ;
    true,
  (ppl_Grid_add_space_dimensions_and_project_test) -> true ;
    true,
  (ppl_Grid_remove_space_dimensions_test) -> true ;
    true,
  (ppl_Grid_remove_higher_space_dimensions_test) -> true ;
    true,
  (ppl_Grid_expand_space_dimension_test) -> true ;
    true,
  (ppl_Grid_fold_space_dimensions_test) -> true ;
    true,
  (ppl_Grid_map_space_dimensions_test) -> true ;
    true,
  (ppl_new_BD_Shape_int8_t_from_space_dimension_test) -> true ;
    true,
  (ppl_new_BD_Shape_int8_t_from_BD_Shape_int8_t_test) -> true ;
    true,
  (ppl_new_BD_Shape_int8_t_from_Polyhedron_test) -> true ;
    true,
  (ppl_new_BD_Shape_int8_t_from_constraints_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_swap_test) -> true ;
    true,
  (ppl_delete_BD_Shape_int8_t_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_space_dimension_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_affine_dimension_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_get_constraints_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_get_minimized_constraints_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_relation_with_constraint_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_is_empty_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_is_universe_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_is_bounded_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_contains_integer_point_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_equals_BD_Shape_int8_t_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_OK_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_add_constraint_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_add_constraint_and_minimize_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_add_constraints_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_add_constraints_and_minimize_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_intersection_assign_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_upper_bound_assign_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_difference_assign_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_concatenate_assign_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_time_elapse_assign_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_bds_hull_assign_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_intersection_assign_and_minimize_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_affine_image_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_affine_preimage_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_generalized_affine_image_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_generalized_affine_preimage_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_generalized_affine_image_lhs_rhs_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_generalized_affine_preimage_lhs_rhs_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_BHMZ05_widening_assign_with_tokens_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_H79_widening_assign_with_tokens_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_BHMZ05_widening_assign_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_H79_widening_assign_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_limited_BHMZ05_extrapolation_assign_with_tokens_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_limited_H79_extrapolation_assign_with_tokens_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_limited_CC76_extrapolation_assign_with_tokens_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_limited_BHMZ05_extrapolation_assign_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_limited_H79_extrapolation_assign_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_limited_CC76_extrapolation_assign_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_add_space_dimensions_and_embed_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_add_space_dimensions_and_project_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_remove_space_dimensions_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_remove_higher_space_dimensions_test) -> true ;
    true,
  (ppl_BD_Shape_int8_t_map_space_dimensions_test) -> true ;
    true,
  ppl_finalise.











ppl_new_C_Polyhedron_from_space_dimension_test :-
  (
  ppl_new_C_Polyhedron_from_space_dimension(0, empty, PS),
  ppl_delete_Polyhedron(PS),
  !).

ppl_new_NNC_Polyhedron_from_space_dimension_test :-
  (
  ppl_new_NNC_Polyhedron_from_space_dimension(0, empty, PS),
  ppl_delete_Polyhedron(PS),
  !).

ppl_new_C_Polyhedron_from_C_Polyhedron_test :-
  (
  ppl_new_C_Polyhedron_from_space_dimension(0, universe, PS),
  ppl_new_C_Polyhedron_from_C_Polyhedron(PS, PS1),
  ppl_delete_Polyhedron(PS),
  ppl_delete_Polyhedron(PS1),
  !).

ppl_new_C_Polyhedron_from_NNC_Polyhedron_test :-
  (
  ppl_new_NNC_Polyhedron_from_space_dimension(0, universe, PS),
  ppl_new_C_Polyhedron_from_NNC_Polyhedron(PS, PS1),
  ppl_delete_Polyhedron(PS),
  ppl_delete_Polyhedron(PS1),
  !).

ppl_new_NNC_Polyhedron_from_C_Polyhedron_test :-
  (
  ppl_new_C_Polyhedron_from_space_dimension(0, universe, PS),
  ppl_new_NNC_Polyhedron_from_C_Polyhedron(PS, PS1),
  ppl_delete_Polyhedron(PS),
  ppl_delete_Polyhedron(PS1),
  !).

ppl_new_NNC_Polyhedron_from_NNC_Polyhedron_test :-
  (
  ppl_new_NNC_Polyhedron_from_space_dimension(0, universe, PS),
  ppl_new_NNC_Polyhedron_from_NNC_Polyhedron(PS, PS1),
  ppl_delete_Polyhedron(PS),
  ppl_delete_Polyhedron(PS1),
  !).

ppl_new_C_Polyhedron_from_constraints_test :-
  (make_vars(1, Vs),
  ppl_constraints_test_data(1, CS, Vs),
  ppl_new_C_Polyhedron_from_constraints(CS, PS1),
  ppl_delete_Polyhedron(PS1),
  ppl_constraints_test_data(2, CS, Vs),
  ppl_new_C_Polyhedron_from_constraints(CS, PS2),
  ppl_delete_Polyhedron(PS2),
  !).

ppl_new_NNC_Polyhedron_from_constraints_test :-
  (make_vars(1, Vs),
  ppl_constraints_test_data(1, CS, Vs),
  ppl_new_NNC_Polyhedron_from_constraints(CS, PS1),
  ppl_delete_Polyhedron(PS1),
  ppl_constraints_test_data(2, CS, Vs),
  ppl_new_NNC_Polyhedron_from_constraints(CS, PS2),
  ppl_delete_Polyhedron(PS2),
  !).

ppl_new_C_Polyhedron_from_generators_test :-
  (make_vars(1, Vs),
  ppl_generators_test_data(1, CS, Vs),
  ppl_new_C_Polyhedron_from_generators(CS, PS1),
  ppl_delete_Polyhedron(PS1),
  ppl_generators_test_data(2, CS, Vs),
  ppl_new_C_Polyhedron_from_generators(CS, PS2),
  ppl_delete_Polyhedron(PS2),
  !).

ppl_new_NNC_Polyhedron_from_generators_test :-
  (make_vars(1, Vs),
  ppl_generators_test_data(1, CS, Vs),
  ppl_new_NNC_Polyhedron_from_generators(CS, PS1),
  ppl_delete_Polyhedron(PS1),
  ppl_generators_test_data(2, CS, Vs),
  ppl_new_NNC_Polyhedron_from_generators(CS, PS2),
  ppl_delete_Polyhedron(PS2),
  !).

ppl_new_C_Polyhedron_from_bounding_box_test.

ppl_new_NNC_Polyhedron_from_bounding_box_test.

ppl_Polyhedron_swap_test.

ppl_delete_Polyhedron_test.

ppl_Polyhedron_space_dimension_test.

ppl_Polyhedron_affine_dimension_test.

ppl_Polyhedron_get_constraints_test.

ppl_Polyhedron_get_generators_test.

ppl_Polyhedron_get_minimized_constraints_test.

ppl_Polyhedron_get_minimized_generators_test.

ppl_Polyhedron_relation_with_constraint_test.

ppl_Polyhedron_relation_with_generator_test.

ppl_Polyhedron_get_bounding_box_test.

ppl_Polyhedron_is_empty_test.

ppl_Polyhedron_is_universe_test.

ppl_Polyhedron_is_bounded_test.

ppl_Polyhedron_contains_integer_point_test.

ppl_Polyhedron_is_topologically_closed_test.

ppl_Polyhedron_topological_closure_assign_test.

ppl_Polyhedron_bounds_from_above_test.

ppl_Polyhedron_bounds_from_below_test.

ppl_Polyhedron_maximize_test.

ppl_Polyhedron_minimize_test.

ppl_Polyhedron_maximize_with_point_test.

ppl_Polyhedron_minimize_with_point_test.

ppl_Polyhedron_contains_Polyhedron_test.

ppl_Polyhedron_strictly_contains_Polyhedron_test.

ppl_Polyhedron_is_disjoint_from_Polyhedron_test.

ppl_Polyhedron_equals_Polyhedron_test.

ppl_Polyhedron_OK_test.

ppl_Polyhedron_add_constraint_test.

ppl_Polyhedron_add_generator_test.

ppl_Polyhedron_add_constraint_and_minimize_test.

ppl_Polyhedron_add_generator_and_minimize_test.

ppl_Polyhedron_add_constraints_test.

ppl_Polyhedron_add_generators_test.

ppl_Polyhedron_add_constraints_and_minimize_test.

ppl_Polyhedron_add_generators_and_minimize_test.

ppl_Polyhedron_intersection_assign_test.

ppl_Polyhedron_upper_bound_assign_test.

ppl_Polyhedron_difference_assign_test.

ppl_Polyhedron_concatenate_assign_test.

ppl_Polyhedron_time_elapse_assign_test.

ppl_Polyhedron_poly_hull_assign_test.

ppl_Polyhedron_poly_difference_assign_test.

ppl_Polyhedron_intersection_assign_and_minimize_test.

ppl_Polyhedron_poly_hull_assign_and_minimize_test.

ppl_Polyhedron_affine_image_test.

ppl_Polyhedron_affine_preimage_test.

ppl_Polyhedron_bounded_affine_image_test.

ppl_Polyhedron_bounded_affine_preimage_test.

ppl_Polyhedron_generalized_affine_image_test.

ppl_Polyhedron_generalized_affine_preimage_test.

ppl_Polyhedron_generalized_affine_image_lhs_rhs_test.

ppl_Polyhedron_generalized_affine_preimage_lhs_rhs_test.

ppl_Polyhedron_BHRZ03_widening_assign_with_tokens_test.

ppl_Polyhedron_H79_widening_assign_with_tokens_test.

ppl_Polyhedron_BHRZ03_widening_assign_test.

ppl_Polyhedron_H79_widening_assign_test.

ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens_test.

ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens_test.

ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens_test.

ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens_test.

ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_test.

ppl_Polyhedron_limited_H79_extrapolation_assign_test.

ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_test.

ppl_Polyhedron_bounded_H79_extrapolation_assign_test.

ppl_Polyhedron_add_space_dimensions_and_embed_test.

ppl_Polyhedron_add_space_dimensions_and_project_test.

ppl_Polyhedron_remove_space_dimensions_test.

ppl_Polyhedron_remove_higher_space_dimensions_test.

ppl_Polyhedron_expand_space_dimension_test.

ppl_Polyhedron_fold_space_dimensions_test.

ppl_Polyhedron_map_space_dimensions_test.

ppl_new_Grid_from_space_dimension_test :-
  (
  ppl_new_Grid_from_space_dimension(0, empty, PS),
  ppl_delete_Grid(PS),
  !).

ppl_new_Grid_from_Grid_test :-
  (
  ppl_new_Grid_from_space_dimension(0, universe, PS),
  ppl_new_Grid_from_Grid(PS, PS1),
  ppl_delete_Grid(PS),
  ppl_delete_Grid(PS1),
  !).

ppl_new_Grid_from_constraints_test :-
  (make_vars(1, Vs),
  ppl_constraints_test_data(1, CS, Vs),
  ppl_new_Grid_from_constraints(CS, PS1),
  ppl_delete_Grid(PS1),
  ppl_constraints_test_data(2, CS, Vs),
  ppl_new_Grid_from_constraints(CS, PS2),
  ppl_delete_Grid(PS2),
  !).

ppl_new_Grid_from_grid_generators_test :-
  (make_vars(1, Vs),
  ppl_grid_generators_test_data(1, CS, Vs),
  ppl_new_Grid_from_grid_generators(CS, PS1),
  ppl_delete_Grid(PS1),
  ppl_grid_generators_test_data(2, CS, Vs),
  ppl_new_Grid_from_grid_generators(CS, PS2),
  ppl_delete_Grid(PS2),
  !).

ppl_new_Grid_from_congruences_test :-
  (make_vars(1, Vs),
  ppl_congruences_test_data(1, CS, Vs),
  ppl_new_Grid_from_congruences(CS, PS1),
  ppl_delete_Grid(PS1),
  ppl_congruences_test_data(2, CS, Vs),
  ppl_new_Grid_from_congruences(CS, PS2),
  ppl_delete_Grid(PS2),
  !).

ppl_new_Grid_from_bounding_box_test.

ppl_new_Grid_from_covering_box_test.

ppl_Grid_swap_test.

ppl_delete_Grid_test.

ppl_Grid_space_dimension_test.

ppl_Grid_affine_dimension_test.

ppl_Grid_get_congruences_test.

ppl_Grid_get_grid_generators_test.

ppl_Grid_get_minimized_congruences_test.

ppl_Grid_get_minimized_grid_generators_test.

ppl_Grid_relation_with_congruence_test.

ppl_Grid_relation_with_grid_generator_test.

ppl_Grid_get_bounding_box_test.

ppl_Grid_get_covering_box_test.

ppl_Grid_is_empty_test.

ppl_Grid_is_universe_test.

ppl_Grid_is_bounded_test.

ppl_Grid_contains_integer_point_test.

ppl_Grid_is_topologically_closed_test.

ppl_Grid_is_discrete_test.

ppl_Grid_topological_closure_assign_test.

ppl_Grid_bounds_from_above_test.

ppl_Grid_bounds_from_below_test.

ppl_Grid_maximize_test.

ppl_Grid_minimize_test.

ppl_Grid_maximize_with_point_test.

ppl_Grid_minimize_with_point_test.

ppl_Grid_contains_Grid_test.

ppl_Grid_strictly_contains_Grid_test.

ppl_Grid_is_disjoint_from_Grid_test.

ppl_Grid_equals_Grid_test.

ppl_Grid_OK_test.

ppl_Grid_add_constraint_test.

ppl_Grid_add_grid_generator_test.

ppl_Grid_add_congruence_test.

ppl_Grid_add_constraint_and_minimize_test.

ppl_Grid_add_grid_generator_and_minimize_test.

ppl_Grid_add_congruence_and_minimize_test.

ppl_Grid_add_constraints_test.

ppl_Grid_add_grid_generators_test.

ppl_Grid_add_congruences_test.

ppl_Grid_add_constraints_and_minimize_test.

ppl_Grid_add_grid_generators_and_minimize_test.

ppl_Grid_add_congruences_and_minimize_test.

ppl_Grid_intersection_assign_test.

ppl_Grid_upper_bound_assign_test.

ppl_Grid_difference_assign_test.

ppl_Grid_concatenate_assign_test.

ppl_Grid_time_elapse_assign_test.

ppl_Grid_join_assign_test.

ppl_Grid_intersection_assign_and_minimize_test.

ppl_Grid_join_assign_and_minimize_test.

ppl_Grid_affine_image_test.

ppl_Grid_affine_preimage_test.

ppl_Grid_generalized_affine_image_test.

ppl_Grid_generalized_affine_preimage_test.

ppl_Grid_generalized_affine_image_lhs_rhs_test.

ppl_Grid_generalized_affine_preimage_lhs_rhs_test.

ppl_Grid_congruence_widening_assign_with_tokens_test.

ppl_Grid_generator_widening_assign_with_tokens_test.

ppl_Grid_congruence_widening_assign_test.

ppl_Grid_generator_widening_assign_test.

ppl_Grid_limited_congruence_extrapolation_assign_with_tokens_test.

ppl_Grid_limited_generator_extrapolation_assign_with_tokens_test.

ppl_Grid_limited_congruence_extrapolation_assign_test.

ppl_Grid_limited_generator_extrapolation_assign_test.

ppl_Grid_add_space_dimensions_and_embed_test.

ppl_Grid_add_space_dimensions_and_project_test.

ppl_Grid_remove_space_dimensions_test.

ppl_Grid_remove_higher_space_dimensions_test.

ppl_Grid_expand_space_dimension_test.

ppl_Grid_fold_space_dimensions_test.

ppl_Grid_map_space_dimensions_test.

ppl_new_BD_Shape_int8_t_from_space_dimension_test :-
  (
  ppl_new_BD_Shape_int8_t_from_space_dimension(0, empty, PS),
  ppl_delete_BD_Shape_int8_t(PS),
  !).

ppl_new_BD_Shape_int8_t_from_BD_Shape_int8_t_test :-
  (
  ppl_new_BD_Shape_int8_t_from_space_dimension(0, universe, PS),
  ppl_new_BD_Shape_int8_t_from_BD_Shape_int8_t(PS, PS1),
  ppl_delete_BD_Shape_int8_t(PS),
  ppl_delete_BD_Shape_int8_t(PS1),
  !).

ppl_new_BD_Shape_int8_t_from_Polyhedron_test :-
  (
  ppl_new_Polyhedron_from_space_dimension(0, universe, PS),
  ppl_new_BD_Shape_int8_t_from_Polyhedron(PS, PS1),
  ppl_delete_Polyhedron(PS),
  ppl_delete_BD_Shape_int8_t(PS1),
  !).

ppl_new_BD_Shape_int8_t_from_constraints_test :-
  (make_vars(1, Vs),
  ppl_constraints_test_data(1, CS, Vs),
  ppl_new_BD_Shape_int8_t_from_constraints(CS, PS1),
  ppl_delete_BD_Shape_int8_t(PS1),
  ppl_constraints_test_data(2, CS, Vs),
  ppl_new_BD_Shape_int8_t_from_constraints(CS, PS2),
  ppl_delete_BD_Shape_int8_t(PS2),
  !).

ppl_BD_Shape_int8_t_swap_test.

ppl_delete_BD_Shape_int8_t_test.

ppl_BD_Shape_int8_t_space_dimension_test.

ppl_BD_Shape_int8_t_affine_dimension_test.

ppl_BD_Shape_int8_t_get_constraints_test.

ppl_BD_Shape_int8_t_get_minimized_constraints_test.

ppl_BD_Shape_int8_t_relation_with_constraint_test.

ppl_BD_Shape_int8_t_is_empty_test.

ppl_BD_Shape_int8_t_is_universe_test.

ppl_BD_Shape_int8_t_is_bounded_test.

ppl_BD_Shape_int8_t_contains_integer_point_test.

ppl_BD_Shape_int8_t_equals_BD_Shape_int8_t_test.

ppl_BD_Shape_int8_t_OK_test.

ppl_BD_Shape_int8_t_add_constraint_test.

ppl_BD_Shape_int8_t_add_constraint_and_minimize_test.

ppl_BD_Shape_int8_t_add_constraints_test.

ppl_BD_Shape_int8_t_add_constraints_and_minimize_test.

ppl_BD_Shape_int8_t_intersection_assign_test.

ppl_BD_Shape_int8_t_upper_bound_assign_test.

ppl_BD_Shape_int8_t_difference_assign_test.

ppl_BD_Shape_int8_t_concatenate_assign_test.

ppl_BD_Shape_int8_t_time_elapse_assign_test.

ppl_BD_Shape_int8_t_bds_hull_assign_test.

ppl_BD_Shape_int8_t_intersection_assign_and_minimize_test.

ppl_BD_Shape_int8_t_affine_image_test.

ppl_BD_Shape_int8_t_affine_preimage_test.

ppl_BD_Shape_int8_t_generalized_affine_image_test.

ppl_BD_Shape_int8_t_generalized_affine_preimage_test.

ppl_BD_Shape_int8_t_generalized_affine_image_lhs_rhs_test.

ppl_BD_Shape_int8_t_generalized_affine_preimage_lhs_rhs_test.

ppl_BD_Shape_int8_t_BHMZ05_widening_assign_with_tokens_test.

ppl_BD_Shape_int8_t_H79_widening_assign_with_tokens_test.

ppl_BD_Shape_int8_t_BHMZ05_widening_assign_test.

ppl_BD_Shape_int8_t_H79_widening_assign_test.

ppl_BD_Shape_int8_t_limited_BHMZ05_extrapolation_assign_with_tokens_test.

ppl_BD_Shape_int8_t_limited_H79_extrapolation_assign_with_tokens_test.

ppl_BD_Shape_int8_t_limited_CC76_extrapolation_assign_with_tokens_test.

ppl_BD_Shape_int8_t_limited_BHMZ05_extrapolation_assign_test.

ppl_BD_Shape_int8_t_limited_H79_extrapolation_assign_test.

ppl_BD_Shape_int8_t_limited_CC76_extrapolation_assign_test.

ppl_BD_Shape_int8_t_add_space_dimensions_and_embed_test.

ppl_BD_Shape_int8_t_add_space_dimensions_and_project_test.

ppl_BD_Shape_int8_t_remove_space_dimensions_test.

ppl_BD_Shape_int8_t_remove_higher_space_dimensions_test.

ppl_BD_Shape_int8_t_map_space_dimensions_test.


% make_var_list(+I,+Dimension,?Variable_List)
% constructs a list of variables with indices from I to Dimension - 1.
% It is assumed that I =< Dimension.

make_vars(Dim, Var_List):-
  make_var_list(0, Dim, Var_List).
make_var_list(Dim,Dim,[]):- !.
make_var_list(I,Dim,[`$VAR'(I)|Var_List]):-
  (I1 is I + 1,
  make_var_list(I1,Dim,Var_List)).


ppl_constraints_test_data(1, CS, _) :-
  CS = [].

ppl_constraints_test_data(2, CS, [A|_]) :-
  CS = [A = 0].

ppl_congruences_test_data(1, CS, [_]) :-
  CS = [].

ppl_congruence_system_test_data(2, CS, [A]) :-
  CS = [A = 0].

ppl_generator_system_test_data(1, GS, [A]) :-
  GS = [point(), line(A)].

ppl_generator_system_test_data(2, GS, [A]) :-
   [point()].

ppl_grid_generator_system_test_data(1, GS, [A]) :-
  GS = [grid_point(), grid_line(A)].

ppl_grid_generator_system_test_data(2, GS, [A]) :-
   [grid_point()].

