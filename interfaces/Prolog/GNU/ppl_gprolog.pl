/* GNU Prolog interface: GNU Prolog part.
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

:- foreign(ppl_initialize).
:- foreign(ppl_finalize).
:- foreign(ppl_set_timeout_exception_atom(+term)).
:- foreign(ppl_timeout_exception_atom(+term)).
:- foreign(ppl_set_timeout(+term)).
:- foreign(ppl_reset_timeout).
:- foreign(ppl_new_Polyhedron_from_dimension(+term, +term, +term)).
:- foreign(ppl_new_Polyhedron_empty_from_dimension(+term, +term, +term)).
:- foreign(ppl_new_Polyhedron_from_Polyhedron(+term, +term, +term, +term)).
:- foreign(ppl_new_Polyhedron_from_constraints(+term, +term, +term)).
:- foreign(ppl_new_Polyhedron_from_generators(+term, +term, +term)).
:- foreign(ppl_new_Polyhedron_from_bounding_box(+term, +term, +term)).
:- foreign(ppl_Polyhedron_swap(+term, +term)).
:- foreign(ppl_delete_Polyhedron(+term)).
:- foreign(ppl_Polyhedron_space_dimension(+term, +term)).
:- foreign(ppl_Polyhedron_get_constraints(+term, +term)).
:- foreign(ppl_Polyhedron_get_minimized_constraints(+term, +term)).
:- foreign(ppl_Polyhedron_get_generators(+term, +term)).
:- foreign(ppl_Polyhedron_get_minimized_generators(+term, +term)).
:- foreign(ppl_Polyhedron_relation_with_constraint(+term, +term, +term)).
:- foreign(ppl_Polyhedron_relation_with_generator(+term, +term, +term)).
:- foreign(ppl_Polyhedron_get_bounding_box(+term, +term, +term)).
:- foreign(ppl_Polyhedron_check_empty(+term)).
:- foreign(ppl_Polyhedron_check_universe(+term)).
:- foreign(ppl_Polyhedron_check_bounded(+term)).
:- foreign(ppl_Polyhedron_bounds_from_above(+term, +term)).
:- foreign(ppl_Polyhedron_bounds_from_below(+term, +term)).
:- foreign(ppl_Polyhedron_check_topologically_closed(+term)).
:- foreign(ppl_Polyhedron_contains_Polyhedron(+term, +term)).
:- foreign(ppl_Polyhedron_strictly_contains_Polyhedron(+term, +term)).
:- foreign(ppl_Polyhedron_check_disjoint_from_Polyhedron(+term, +term)).
:- foreign(ppl_Polyhedron_equals_Polyhedron(+term, +term)).
:- foreign(ppl_Polyhedron_add_constraint(+term, +term)).
:- foreign(ppl_Polyhedron_add_constraint_and_minimize(+term, +term)).
:- foreign(ppl_Polyhedron_add_generator(+term, +term)).
:- foreign(ppl_Polyhedron_add_generator_and_minimize(+term, +term)).
:- foreign(ppl_Polyhedron_add_constraints(+term, +term)).
:- foreign(ppl_Polyhedron_add_constraints_and_minimize(+term, +term)).
:- foreign(ppl_Polyhedron_add_generators(+term, +term)).
:- foreign(ppl_Polyhedron_add_generators_and_minimize(+term, +term)).
:- foreign(ppl_Polyhedron_intersection_assign(+term, +term)).
:- foreign(ppl_Polyhedron_intersection_assign_and_minimize(+term, +term)).
:- foreign(ppl_Polyhedron_poly_hull_assign(+term, +term)).
:- foreign(ppl_Polyhedron_poly_hull_assign_and_minimize(+term, +term)).
:- foreign(ppl_Polyhedron_poly_difference_assign(+term, +term)).
:- foreign(ppl_Polyhedron_affine_image(+term, +term, +term, +term)).
:- foreign(ppl_Polyhedron_affine_preimage(+term, +term, +term, +term)).
:- foreign(ppl_Polyhedron_generalized_affine_image(+term, +term,
						   +term, +term, +term)).
:- foreign(ppl_Polyhedron_generalized_affine_image_lhs_rhs(+term, +term,
							   +term, +term)).
:- foreign(ppl_Polyhedron_time_elapse_assign(+term, +term)).
:- foreign(ppl_Polyhedron_topological_closure_assign(+term)).
:- foreign(ppl_Polyhedron_BHRZ03_widening_assign(+term, +term)).
:- foreign(ppl_Polyhedron_BHRZ03_widening_assign_with_token(+term,
							    +term,
							    +term)).
:- foreign(ppl_Polyhedron_limited_BHRZ03_extrapolation_assign(+term,
							      +term,
							      +term)).
:- foreign(ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign(+term,
							      +term,
							      +term)).
:- foreign(ppl_Polyhedron_H79_widening_assign(+term, +term)).
:- foreign(ppl_Polyhedron_H79_widening_assign_with_token(+term,
							 +term,
							 +term)).
:- foreign(ppl_Polyhedron_limited_H79_extrapolation_assign(+term,
							   +term,
							   +term)).
:- foreign(ppl_Polyhedron_bounded_H79_extrapolation_assign(+term,
							   +term,
							   +term)).
:- foreign(ppl_Polyhedron_add_dimensions_and_project(+term, +term)).
:- foreign(ppl_Polyhedron_add_dimensions_and_embed(+term, +term)).
:- foreign(ppl_Polyhedron_concatenate_assign(+term, +term)).
:- foreign(ppl_Polyhedron_remove_dimensions(+term, +term)).
:- foreign(ppl_Polyhedron_remove_higher_dimensions(+term, +term)).
:- foreign(ppl_Polyhedron_rename_dimensions(+term, +term)).
