/* Ciao Prolog interface: Ciao Prolog part.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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
	ppl_new_Polyhedron_from_dimension/3,
	ppl_new_Polyhedron_empty_from_dimension/3,
	ppl_new_Polyhedron_from_Polyhedron/4,
	ppl_new_Polyhedron_from_constraints/3,
	ppl_new_Polyhedron_from_generators/3,
	ppl_new_Polyhedron_from_bounding_box/3,
	ppl_delete_Polyhedron/1,
	ppl_Polyhedron_space_dimension/2,
	ppl_Polyhedron_intersection_assign/2,
	ppl_Polyhedron_intersection_assign_and_minimize/2,
	ppl_Polyhedron_poly_hull_assign/2,
	ppl_Polyhedron_poly_hull_assign_and_minimize/2,
	ppl_Polyhedron_poly_difference_assign/2,
	ppl_Polyhedron_poly_difference_assign_and_minimize/2,
	ppl_Polyhedron_widening_CC92_assign/2,
	ppl_Polyhedron_limited_widening_CC92_assign/3,
        ppl_Polyhedron_toplogical_closure_assign/1,
	ppl_Polyhedron_get_constraints/2,
	ppl_Polyhedron_get_minimized_constraints/2,
	ppl_Polyhedron_get_generators/2,
	ppl_Polyhedron_get_minimized_generators/2,
	ppl_Polyhedron_add_constraint/2,
	ppl_Polyhedron_add_generator/2,
	ppl_Polyhedron_add_constraints/2,
	ppl_Polyhedron_add_constraints_and_minimize/2,
	ppl_Polyhedron_add_generators/2,
	ppl_Polyhedron_add_generators_and_minimize/2,
	ppl_Polyhedron_add_dimensions_and_constraints/2,
	ppl_Polyhedron_add_dimensions_and_project/2,
	ppl_Polyhedron_add_dimensions_and_embed/2,
	ppl_Polyhedron_remove_dimensions/2,
	ppl_Polyhedron_remove_higher_dimensions/2,
	ppl_Polyhedron_affine_image/4,
	ppl_Polyhedron_affine_preimage/4,
	ppl_Polyhedron_relation_with_constraint/3,
	ppl_Polyhedron_relation_with_generator/3,
	ppl_Polyhedron_check_empty/1,
	ppl_Polyhedron_check_universe/1,
	ppl_Polyhedron_is_bounded/1,
        ppl_Polyhedron_is_toplogically_closed/1,
	ppl_Polyhedron_contains_Polyhedron/2,
	ppl_Polyhedron_strictly_contains_Polyhedron/2,
	ppl_Polyhedron_equals_Polyhedron/2,
	ppl_Polyhedron_get_bounding_box/2
],
[
	assertions,
	basicmodes,
	regtypes,
	foreign_interface
]).

:- true pred ppl_initialize :: foreign.

:- true pred ppl_finalize:: foreign.

:- true pred ppl_new_Polyhedron_from_dimension(in(Kind),
                                               in(Dimension),
                                               in(Handle))
             :: any_term(Kind) * any_term(Dimension) * any_term(Handle)
              + foreign.

:- true pred ppl_new_Polyhedron_empty_from_dimension(in(Kind),
                                                     in(Dimension),
                                                     in(Handle))
             :: any_term(Kind) * any_term(Dimension) * any_term(Handle)
              + foreign.

:- true pred ppl_new_Polyhedron_from_Polyhedron(in(SrcKind),
                                                in(SrcHandle),
                                                in(DstKind),
                                                in(DstHandle))
             :: any_term(SrcKind) * any_term(SrcHandle)
              * any_term(DstKind) * any_term(DstHandle)
              + foreign.

:- true pred ppl_new_Polyhedron_from_constraints(in(Kind),
                                                 in(CList),
                                                 in(Handle))
             :: any_term(Kind) * any_term(CList) * any_term(Handle)
              + foreign.

:- true pred ppl_new_Polyhedron_from_generators(in(Kind),
                                                in(GList),
                                                in(Handle))
             :: any_term(Kind) * any_term(GList) * any_term(Handle)
              + foreign.

:- true pred ppl_new_Polyhedron_from_bounding_box(in(Kind),
                                                  in(BBox),
                                                  in(Handle))
             :: any_term(Kind) * any_term(BBox) * any_term(Handle)
              + foreign.

:- true pred ppl_delete_Polyhedron(in(Handle))
             :: any_term(Handle)
              + foreign.

:- true pred ppl_Polyhedron_space_dimension(in(Handle), in(Dimension))
             :: any_term(Handle) * any_term(Dimension)
              + foreign.

:- true pred ppl_Polyhedron_intersection_assign(in(Handle1), in(Handle2))
             :: any_term(Handle1) * any_term(Handle2)
              + foreign.

:- true pred ppl_Polyhedron_intersection_assign_and_minimize(in(Handle1),
                                                             in(Handle2))
             :: any_term(Handle1) * any_term(Handle2)
              + foreign.

:- true pred ppl_Polyhedron_poly_hull_assign(in(Handle1), in(Handle2))
             :: any_term(Handle1) * any_term(Handle2)
              + foreign.

:- true pred ppl_Polyhedron_poly_hull_assign_and_minimize(in(Handle1),
                                                             in(Handle2))
             :: any_term(Handle1) * any_term(Handle2)
              + foreign.

:- true pred ppl_Polyhedron_poly_difference_assign(in(Handle1), in(Handle2))
             :: any_term(Handle1) * any_term(Handle2)
              + foreign.

:- true pred ppl_Polyhedron_poly_difference_assign_and_minimize(in(Handle1),
                                                                in(Handle2))
             :: any_term(Handle1) * any_term(Handle2)
              + foreign.

:- true pred ppl_Polyhedron_widening_CC92_assign(in(Handle1), in(Handle2))
             :: any_term(Handle1) * any_term(Handle2)
              + foreign.

:- true pred ppl_Polyhedron_limited_widening_CC92_assign(in(Handle1),
                                                         in(Handle2))
             :: any_term(Handle1) * any_term(Handle2)
              + foreign.

:- true pred ppl_Polyhedron_get_constraints(in(Handle), in(CList))
             :: any_term(Handle) * any_term(CList)
              + foreign.

:- true pred ppl_Polyhedron_get_minimized_constraints(in(Handle), in(CList))
             :: any_term(Handle) * any_term(CList)
              + foreign.

:- true pred ppl_Polyhedron_get_generators(in(Handle), in(CList))
             :: any_term(Handle) * any_term(CList)
              + foreign.

:- true pred ppl_Polyhedron_get_minimized_generators(in(Handle), in(CList))
             :: any_term(Handle) * any_term(CList)
              + foreign.

:- true pred ppl_Polyhedron_add_constraint(in(Handle), in(Constraint))
             :: any_term(Handle) * any_term(Constraint)
              + foreign.

:- true pred ppl_Polyhedron_add_generator(in(Handle), in(Generator))
             :: any_term(Handle) * any_term(Generator)
              + foreign.

:- true pred ppl_Polyhedron_add_constraints(in(Handle), in(CList))
             :: any_term(Handle) * any_term(CList)
              + foreign.

:- true pred ppl_Polyhedron_add_constraints_and_minimize(in(Handle), in(CList))
             :: any_term(Handle) * any_term(CList)
              + foreign.

:- true pred ppl_Polyhedron_add_generators(in(Handle), in(GList))
             :: any_term(Handle) * any_term(GList)
              + foreign.

:- true pred ppl_Polyhedron_add_generators_and_minimize(in(Handle), in(GList))
             :: any_term(Handle) * any_term(GList)
              + foreign.

:- true pred ppl_Polyhedron_add_dimensions_and_constraints(in(Handle),
                                                           in(CList))
             :: any_term(Handle) * any_term(CList)
              + foreign.

:- true pred ppl_Polyhedron_add_dimensions_and_project(in(Handle),
                                                       in(NDimensionsToAdd))
             :: any_term(Handle) * any_term(NDimensionsToAdd)
              + foreign.

:- true pred ppl_Polyhedron_add_dimensions_and_embed(in(Handle),
                                                     in(NDimensionsToAdd))
             :: any_term(Handle) * any_term(NDimensionsToAdd)
              + foreign.

:- true pred ppl_Polyhedron_remove_dimensions(in(Handle), in(VList))
             :: any_term(Handle) * any_term(VList)
              + foreign.

:- true pred ppl_Polyhedron_remove_higher_dimensions(in(Handle),
                                                     in(Dimensions))
             :: any_term(Handle) * any_term(Dimensions)
              + foreign.

:- true pred ppl_Polyhedron_affine_image(in(Handle), in(Var),
                                         in(LinearExpression), in(Divisor))
             :: any_term(Handle) * any_term(Dimensions)
              + foreign.

:- true pred ppl_Polyhedron_affine_preimage(in(Handle), in(Var),
                                            in(LinearExpression), in(Divisor))
             :: any_term(Handle) * any_term(Dimensions)
              + foreign.

:- true pred ppl_Polyhedron_relation_with_constraint(in(Handle),
                                                     in(Constraint),
                                                     in(RList))
             :: any_term(Handle) * any_term(Constraint) * any_term(RList)
              + foreign.

:- true pred ppl_Polyhedron_relation_with_generator(in(Handle),
                                                    in(Generator),
                                                    in(RList))
             :: any_term(Handle) * any_term(Generator) * any_term(RList)
              + foreign.

:- true pred ppl_Polyhedron_check_empty(in(Handle))
             :: any_term(Handle)
              + foreign.

:- true pred ppl_Polyhedron_check_foreign(in(Handle))
             :: any_term(Handle)
              + foreign.

:- true pred ppl_Polyhedron_is_bounded(in(Handle))
             :: any_term(Handle)
              + foreign.

:- true pred ppl_Polyhedron_contains_Polyhedron(in(Handle1), in(Handle2))
             :: any_term(Handle1) * any_term(Handle2)
              + foreign.

:- true pred ppl_Polyhedron_strictly_contains_Polyhedron(in(Handle1),
                                                         in(Handle2))
             :: any_term(Handle1) * any_term(Handle2)
              + foreign.

:- true pred ppl_Polyhedron_equals_Polyhedron(in(Handle1), in(Handle2))
             :: any_term(Handle1) * any_term(Handle2)
              + foreign.

:- use_foreign_library(ppl_ciao).

:- impl_defined(
[
	ppl_initialize/0,
	ppl_finalize/0,
	ppl_new_Polyhedron_from_dimension/3,
	ppl_new_Polyhedron_empty_from_dimension/3,
	ppl_new_Polyhedron_from_Polyhedron/4,
	ppl_new_Polyhedron_from_constraints/3,
	ppl_new_Polyhedron_from_generators/3,
	ppl_new_Polyhedron_from_bounding_box/3,
	ppl_delete_Polyhedron/1,
	ppl_Polyhedron_space_dimension/2,
	ppl_Polyhedron_intersection_assign/2,
	ppl_Polyhedron_intersection_assign_and_minimize/2,
	ppl_Polyhedron_poly_hull_assign/2,
	ppl_Polyhedron_poly_hull_assign_and_minimize/2,
	ppl_Polyhedron_poly_difference_assign/2,
	ppl_Polyhedron_poly_difference_assign_and_minimize/2,
	ppl_Polyhedron_widening_CC92_assign/2,
	ppl_Polyhedron_limited_widening_CC92_assign/3,
	ppl_Polyhedron_get_constraints/2,
	ppl_Polyhedron_get_minimized_constraints/2,
	ppl_Polyhedron_get_generators/2,
	ppl_Polyhedron_get_minimized_generators/2,
	ppl_Polyhedron_add_constraint/2,
	ppl_Polyhedron_add_generator/2,
	ppl_Polyhedron_add_constraints/2,
	ppl_Polyhedron_add_constraints_and_minimize/2,
	ppl_Polyhedron_add_generators/2,
	ppl_Polyhedron_add_generators_and_minimize/2,
	ppl_Polyhedron_add_dimensions_and_constraints/2,
	ppl_Polyhedron_add_dimensions_and_project/2,
	ppl_Polyhedron_add_dimensions_and_embed/2,
	ppl_Polyhedron_remove_dimensions/2,
	ppl_Polyhedron_remove_higher_dimensions/2,
	ppl_Polyhedron_affine_image/4,
	ppl_Polyhedron_affine_preimage/4,
	ppl_Polyhedron_relation_with_constraint/3,
	ppl_Polyhedron_relation_with_generator/3,
	ppl_Polyhedron_check_empty/1,
	ppl_Polyhedron_check_universe/1,
	ppl_Polyhedron_is_bounded/1,
	ppl_Polyhedron_contains_Polyhedron/2,
	ppl_Polyhedron_strictly_contains_Polyhedron/2,
	ppl_Polyhedron_equals_Polyhedron/2,
	ppl_Polyhedron_get_bounding_box/2
]).
