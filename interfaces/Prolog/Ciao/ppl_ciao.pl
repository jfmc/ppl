/* Ciao Prolog interface: Ciao Prolog part.
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

:- module(ppl_ciao,
[
	ppl_initialize/0,
	ppl_finalize/0,
	ppl_set_timeout_exception_atom/1,
	ppl_timeout_exception_atom/1,
	ppl_set_timeout/1,
	ppl_reset_timeout/0,
	ppl_new_Polyhedron_from_dimension/3,
	ppl_new_Polyhedron_empty_from_dimension/3,
	ppl_new_Polyhedron_from_Polyhedron/4,
	ppl_new_Polyhedron_from_constraints/3,
	ppl_new_Polyhedron_from_generators/3,
	ppl_new_Polyhedron_from_bounding_box/3,
	ppl_Polyhedron_swap/2,
	ppl_delete_Polyhedron/1,
	ppl_Polyhedron_space_dimension/2,
	ppl_Polyhedron_get_constraints/2,
	ppl_Polyhedron_get_minimized_constraints/2,
	ppl_Polyhedron_get_generators/2,
	ppl_Polyhedron_get_minimized_generators/2,
	ppl_Polyhedron_relation_with_constraint/3,
	ppl_Polyhedron_relation_with_generator/3,
	ppl_Polyhedron_get_bounding_box/3,
	ppl_Polyhedron_check_empty/1,
	ppl_Polyhedron_check_universe/1,
	ppl_Polyhedron_check_bounded/1,
	ppl_Polyhedron_bounds_from_above/2,
	ppl_Polyhedron_bounds_from_below/2,
        ppl_Polyhedron_check_topologically_closed/1,
	ppl_Polyhedron_contains_Polyhedron/2,
	ppl_Polyhedron_strictly_contains_Polyhedron/2,
	ppl_Polyhedron_check_disjoint_from_Polyhedron/2,
	ppl_Polyhedron_equals_Polyhedron/2,
	ppl_Polyhedron_OK/1,
	ppl_Polyhedron_add_constraint/2,
	ppl_Polyhedron_add_constraint_and_minimize/2,
	ppl_Polyhedron_add_generator/2,
	ppl_Polyhedron_add_generator_and_minimize/2,
	ppl_Polyhedron_add_constraints/2,
	ppl_Polyhedron_add_constraints_and_minimize/2,
	ppl_Polyhedron_add_generators/2,
	ppl_Polyhedron_add_generators_and_minimize/2,
	ppl_Polyhedron_intersection_assign/2,
	ppl_Polyhedron_intersection_assign_and_minimize/2,
	ppl_Polyhedron_poly_hull_assign/2,
	ppl_Polyhedron_poly_hull_assign_and_minimize/2,
	ppl_Polyhedron_poly_difference_assign/2,
	ppl_Polyhedron_affine_image/4,
	ppl_Polyhedron_affine_preimage/4,
	ppl_Polyhedron_generalized_affine_image/5,
	ppl_Polyhedron_generalized_affine_image_lhs_rhs/4,
	ppl_Polyhedron_time_elapse_assign/2,
        ppl_Polyhedron_topological_closure_assign/1,
	ppl_Polyhedron_BHRZ03_widening_assign_with_token/3,
	ppl_Polyhedron_BHRZ03_widening_assign/2,
	ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token/4,
	ppl_Polyhedron_limited_BHRZ03_extrapolation_assign/3,
	ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token/4,
	ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign/3,
	ppl_Polyhedron_H79_widening_assign_with_token/3,
	ppl_Polyhedron_H79_widening_assign/2,
	ppl_Polyhedron_limited_H79_extrapolation_assign_with_token/4,
	ppl_Polyhedron_limited_H79_extrapolation_assign/3,
	ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token/4,
	ppl_Polyhedron_bounded_H79_extrapolation_assign/3,
	ppl_Polyhedron_add_dimensions_and_project/2,
	ppl_Polyhedron_add_dimensions_and_embed/2,
	ppl_Polyhedron_concatenate_assign/2,
	ppl_Polyhedron_remove_dimensions/2,
	ppl_Polyhedron_remove_higher_dimensions/2,
	ppl_Polyhedron_rename_dimensions/2
],
[
	assertions,
	basicmodes,
	regtypes,
	foreign_interface
]).

:- true pred ppl_initialize + foreign.

:- true pred ppl_finalize + foreign.

:- true pred ppl_set_timeout_exception_atom(in(Atom))
             :: any_term + foreign.

:- true pred ppl_timeout_exception_atom(in(Term))
             :: any_term + foreign.

:- true pred ppl_set_timeout(in(Time))
             :: any_term + foreign.

:- true pred ppl_reset_timeout + foreign.

:- true pred ppl_new_Polyhedron_from_dimension(in(Kind),
                                               in(Dimension),
                                               in(Handle))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_new_Polyhedron_empty_from_dimension(in(Kind),
                                                     in(Dimension),
                                                     in(Handle))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_new_Polyhedron_from_Polyhedron(in(SrcKind),
                                                in(SrcHandle),
                                                in(DstKind),
                                                in(DstHandle))
             :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_new_Polyhedron_from_constraints(in(Kind),
                                                 in(CList),
                                                 in(Handle))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_new_Polyhedron_from_generators(in(Kind),
                                                in(GList),
                                                in(Handle))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_new_Polyhedron_from_bounding_box(in(Kind),
                                                  in(BBox),
                                                  in(Handle))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_delete_Polyhedron(in(Handle))
             :: any_term + foreign.

:- true pred ppl_Polyhedron_space_dimension(in(Handle), in(Dimension))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_get_constraints(in(Handle), in(CList))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_get_minimized_constraints(in(Handle), in(CList))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_get_generators(in(Handle), in(CList))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_get_minimized_generators(in(Handle), in(CList))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_relation_with_constraint(in(Handle),
                                                     in(Constraint),
                                                     in(RList))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_relation_with_generator(in(Handle),
                                                    in(Generator),
                                                    in(RList))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_check_empty(in(Handle))
             :: any_term + foreign.

:- true pred ppl_Polyhedron_check_universe(in(Handle))
             :: any_term + foreign.

:- true pred ppl_Polyhedron_check_bounded(in(Handle))
             :: any_term + foreign.

:- true pred ppl_Polyhedron_bounds_from_above(in(Handle), in(LinearExpression))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_bounds_from_below(in(Handle), in(LinearExpression))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_check_topologically_closed(in(Handle))
             :: any_term + foreign.

:- true pred ppl_Polyhedron_contains_Polyhedron(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_strictly_contains_Polyhedron(in(Handle1),
                                                         in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_check_disjoint_from_Polyhedron(in(Handle1),
							in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_equals_Polyhedron(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_OK(in(Handle))
             :: any_term + foreign.

:- true pred ppl_Polyhedron_add_constraint(in(Handle), in(Constraint))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_generator(in(Handle), in(Generator))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_constraints(in(Handle), in(CList))
             :: any_term * any_term + foreign.

%:- true pred ppl_Polyhedron_add_constraints_and_minimize(in(Handle), in(CList))
%             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_constraints_and_minimize_2(in(Handle), in(CList), go(Success))
             :: any_term * any_term * int + (returns(Success), foreign(ppl_Polyhedron_add_constraints_and_minimize)).

ppl_Polyhedron_add_constraints_and_minimize(Handle, CList) :-
	ppl_Polyhedron_add_constraints_and_minimize_2(Handle, CList, 1).

:- true pred ppl_Polyhedron_add_generators(in(Handle), in(GList))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_generators_and_minimize(in(Handle), in(GList))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_intersection_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_intersection_assign_and_minimize(in(Handle1),
                                                             in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_poly_hull_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_poly_hull_assign_and_minimize(in(Handle1),
                                                             in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_poly_difference_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_affine_image(in(Handle), in(Var),
                                         in(LinearExpression), in(Divisor))
             :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_affine_preimage(in(Handle), in(Var),
                                            in(LinearExpression), in(Divisor))
             :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_generalized_affine_image(in(Handle),
						     in(Var), in(Rel),
						     in(LinExpression),
						     in(Divisor))
             :: any_term * any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_generalized_affine_image_lhs_rhs(in(Handle),
							     in(LHS),
							     in(Rel), in(RHS))
             :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_time_elapse_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_topological_closure_assign(in(Handle))
             :: any_term + foreign.

:- true pred ppl_Polyhedron_BHRZ03_widening_assign_with_token(in(Handle1),
							       in(Handle2),
							       in(Tokens))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_BHRZ03_widening_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token(in(Handle1),
								in(Handle2),
                                                               in(CList),
							       in(Tokens))
             :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_limited_BHRZ03_extrapolation_assign(in(Handle1),
                                                               in(CList),
								in(Handle2))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token(in(Handle1),
								in(Handle2),
                                                               in(CList),
							       in(Tokens))
             :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign(in(Handle1),
								in(Handle2),
                                                               in(CList))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_H79_widening_assign_with_token(in(Handle1),
							    in(Handle2),
							    in(Tokens))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_H79_widening_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_limited_H79_extrapolation_assign_with_token(in(Handle1),
							     in(Handle2),
                                                               in(CList),
							    in(Tokens))
             :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_limited_H79_extrapolation_assign(in(Handle1),
							     in(Handle2),
                                                               in(CList))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token(in(Handle1),
								in(Handle2),
                                                               in(CList),
							    in(Tokens))
             :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_bounded_H79_extrapolation_assign(in(Handle1),
								in(Handle2),
                                                               in(CList))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_dimensions_and_project(in(Handle),
                                                       in(NDimensionsToAdd))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_dimensions_and_embed(in(Handle),
                                                     in(NDimensionsToAdd))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_concatenate_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_remove_dimensions(in(Handle), in(VList))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_remove_higher_dimensions(in(Handle),
                                                     in(Dimensions))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_rename_dimensions(in(Handle),
					      in(PIFunc))
             :: any_term * any_term + foreign.

:- extra_linker_opts('-L.libs').
:- use_foreign_library(ppl_ciao).

:- impl_defined(
[
	ppl_initialize/0,
	ppl_finalize/0,
	ppl_set_timeout_exception_atom/1,
	ppl_timeout_exception_atom/1,
	ppl_set_timeout/1,
	ppl_reset_timeout/0,
	ppl_new_Polyhedron_from_dimension/3,
	ppl_new_Polyhedron_empty_from_dimension/3,
	ppl_new_Polyhedron_from_Polyhedron/4,
	ppl_new_Polyhedron_from_constraints/3,
	ppl_new_Polyhedron_from_generators/3,
	ppl_new_Polyhedron_from_bounding_box/3,
	ppl_Polyhedron_swap/2,
	ppl_delete_Polyhedron/1,
	ppl_Polyhedron_space_dimension/2,
	ppl_Polyhedron_get_constraints/2,
	ppl_Polyhedron_get_minimized_constraints/2,
	ppl_Polyhedron_get_generators/2,
	ppl_Polyhedron_get_minimized_generators/2,
	ppl_Polyhedron_relation_with_constraint/3,
	ppl_Polyhedron_relation_with_generator/3,
	ppl_Polyhedron_get_bounding_box/3,
	ppl_Polyhedron_check_empty/1,
	ppl_Polyhedron_check_universe/1,
	ppl_Polyhedron_check_bounded/1,
	ppl_Polyhedron_bounds_from_above/2,
	ppl_Polyhedron_bounds_from_below/2,
        ppl_Polyhedron_check_topologically_closed/1,
	ppl_Polyhedron_contains_Polyhedron/2,
	ppl_Polyhedron_strictly_contains_Polyhedron/2,
	ppl_Polyhedron_check_disjoint_from_Polyhedron/2,
	ppl_Polyhedron_equals_Polyhedron/2,
	ppl_Polyhedron_OK/1,
	ppl_Polyhedron_add_constraint/2,
	ppl_Polyhedron_add_constraint_and_minimize/2,
	ppl_Polyhedron_add_generator/2,
	ppl_Polyhedron_add_generator_and_minimize/2,
	ppl_Polyhedron_add_constraints/2,
%	ppl_Polyhedron_add_constraints_and_minimize/2,
	ppl_Polyhedron_add_constraints_and_minimize_2/3,
	ppl_Polyhedron_add_generators/2,
	ppl_Polyhedron_add_generators_and_minimize/2,
	ppl_Polyhedron_intersection_assign/2,
	ppl_Polyhedron_intersection_assign_and_minimize/2,
	ppl_Polyhedron_poly_hull_assign/2,
	ppl_Polyhedron_poly_hull_assign_and_minimize/2,
	ppl_Polyhedron_poly_difference_assign/2,
	ppl_Polyhedron_affine_image/4,
	ppl_Polyhedron_affine_preimage/4,
	ppl_Polyhedron_generalized_affine_image/5,
	ppl_Polyhedron_generalized_affine_image_lhs_rhs/4,
	ppl_Polyhedron_time_elapse_assign/2,
        ppl_Polyhedron_topological_closure_assign/1,
	ppl_Polyhedron_BHRZ03_widening_assign_with_token/3,
	ppl_Polyhedron_BHRZ03_widening_assign/2,
	ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token/4,
	ppl_Polyhedron_limited_BHRZ03_extrapolation_assign/3,
	ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token/4,
	ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign/3,
	ppl_Polyhedron_H79_widening_assign_with_token/3,
	ppl_Polyhedron_H79_widening_assign/2,
	ppl_Polyhedron_limited_H79_extrapolation_assign_with_token/4,
	ppl_Polyhedron_limited_H79_extrapolation_assign/3,
	ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token/4,
	ppl_Polyhedron_bounded_H79_extrapolation_assign/3,
	ppl_Polyhedron_add_dimensions_and_project/2,
	ppl_Polyhedron_add_dimensions_and_embed/2,
	ppl_Polyhedron_concatenate_assign/2,
	ppl_Polyhedron_remove_dimensions/2,
	ppl_Polyhedron_remove_higher_dimensions/2,
	ppl_Polyhedron_rename_dimensions/2
]).

:- comment(version_maintenance,off).

