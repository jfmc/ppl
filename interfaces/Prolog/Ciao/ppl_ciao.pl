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
        ppl_Polyhedron_is_empty/1,
        ppl_Polyhedron_is_universe/1,
        ppl_Polyhedron_is_bounded/1,
        ppl_Polyhedron_bounds_from_above/2,
        ppl_Polyhedron_bounds_from_below/2,
        ppl_Polyhedron_is_topologically_closed/1,
        ppl_Polyhedron_contains_Polyhedron/2,
        ppl_Polyhedron_strictly_contains_Polyhedron/2,
        ppl_Polyhedron_is_disjoint_from_Polyhedron/2,
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
        ppl_Polyhedron_remap_dimensions/2
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

:- true pred ppl_timeout_exception_atom_2(in(Atom),
                                     go(Success))
  :: any_term * int
  + (returns(Success), foreign(ppl_timeout_exception_atom)).

ppl_timeout_exception_atom(Atom) :-
   ppl_timeout_exception_atom_2(Atom, 1).

:- true pred ppl_set_timeout(in(Time))
             :: any_term + foreign.

:- true pred ppl_reset_timeout + foreign.

:- true pred ppl_new_Polyhedron_from_dimension_2(in(Kind),
                                               in(Dimension),
                                               in(Handle),
                                               go(Success))
  :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Polyhedron_from_dimension)).

ppl_new_Polyhedron_from_dimension(Kind, Dimension, Handle) :-
   ppl_new_Polyhedron_from_dimension_2(Kind, Dimension, Handle, 1).


:- true pred ppl_new_Polyhedron_empty_from_dimension_2(in(Kind),
                                               in(Dimension),
                                               in(Handle),
                                               go(Success))
  :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Polyhedron_empty_from_dimension)).

ppl_new_Polyhedron_empty_from_dimension(Kind, Dimension, Handle) :-
   ppl_new_Polyhedron_empty_from_dimension_2(Kind, Dimension, Handle, 1).


:- true pred ppl_new_Polyhedron_from_Polyhedron_2(in(SrcKind),
                                                in(SrcHandle),
                                                in(DstKind),
                                                in(DstHandle),
                                               go(Success))
  :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Polyhedron_from_Polyhedron)).

ppl_new_Polyhedron_from_Polyhedron(SrcKind, SrcHandle, DstKind, DstHandle) :-
   ppl_new_Polyhedron_from_Polyhedron_2(
               SrcKind, SrcHandle, DstKind, DstHandle, 1).

:- true pred ppl_new_Polyhedron_from_constraints_2(in(Kind),
                                               in(CList),
                                               in(Handle),
                                               go(Success))
  :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Polyhedron_from_constraints)).

ppl_new_Polyhedron_from_constraints(Kind, CList, Handle) :-
   ppl_new_Polyhedron_from_constraints_2(Kind, CList, Handle, 1).

:- true pred ppl_new_Polyhedron_from_generators_2(in(Kind),
                                               in(GList),
                                               in(Handle),
                                               go(Success))
  :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Polyhedron_from_generators)).

ppl_new_Polyhedron_from_generators(Kind, GList, Handle) :-
   ppl_new_Polyhedron_from_generators_2(Kind, GList, Handle, 1).


:- true pred ppl_new_Polyhedron_from_bounding_box_2(in(Kind),
                                               in(BBox),
                                               in(Handle),
                                               go(Success))
  :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Polyhedron_from_bounding_box)).

ppl_new_Polyhedron_from_bounding_box(Kind, BBox, Handle) :-
   ppl_new_Polyhedron_from_bounding_box_2(Kind, BBox, Handle, 1).

:- true pred ppl_Polyhedron_swap(in(Handle1),
                                 in(Handle2))
  :: any_term * any_term + foreign.

:- true pred ppl_delete_Polyhedron(in(Handle))
  :: any_term + foreign.

:- true pred ppl_Polyhedron_space_dimension_2(in(Handle),
                                              in(Dimension),
                                              go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_space_dimension)).

ppl_Polyhedron_space_dimension(Handle, Dimension) :-
        ppl_Polyhedron_space_dimension_2(Handle, Dimension, 1).

:- true pred ppl_Polyhedron_get_constraints_2(in(Handle),
                                              in(CList),
                                              go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_get_constraints)).

ppl_Polyhedron_get_constraints(Handle, CList) :-
        ppl_Polyhedron_get_constraints_2(Handle, CList, 1).


:- true pred ppl_Polyhedron_get_minimized_constraints_2(in(Handle),
                                                        in(CList),
                                                        go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_get_minimized_constraints)).

ppl_Polyhedron_get_minimized_constraints(Handle, CList) :-
        ppl_Polyhedron_get_minimized_constraints_2(Handle, CList, 1).

:- true pred ppl_Polyhedron_get_generators_2(in(Handle),
                                             in(GList),
                                             go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_get_generators)).

ppl_Polyhedron_get_generators(Handle, GList) :-
        ppl_Polyhedron_get_generators_2(Handle, GList, 1).


:- true pred ppl_Polyhedron_get_minimized_generators_2(in(Handle),
                                                       in(GList),
                                                       go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_get_minimized_generators)).

ppl_Polyhedron_get_minimized_generators(Handle, GList) :-
        ppl_Polyhedron_get_minimized_generators_2(Handle, GList, 1).

:- true pred ppl_Polyhedron_relation_with_constraint_2(in(Handle),
                                                       in(Constraint),
                                                       in(RList),
                                                       go(Success))
  :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_relation_with_constraint)).

ppl_Polyhedron_relation_with_constraint(Handle, Constraint, RList) :-
        ppl_Polyhedron_relation_with_constraint_2(Handle, Constraint,
                                                  RList, 1).

:- true pred ppl_Polyhedron_relation_with_generator_2(in(Handle),
                                                     in(Generator),
                                                     in(RList),
                                                     go(Success))
  :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_relation_with_generator)).

ppl_Polyhedron_relation_with_generator(Handle, Generator, RList) :-
        ppl_Polyhedron_relation_with_generator_2(Handle, Generator, RList, 1).

:- true pred ppl_Polyhedron_get_bounding_box_2(in(Handle),
                                               in(Relation),
                                               in(BBox),
                                               go(Success))
  :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_get_bounding_box)).

ppl_Polyhedron_get_bounding_box(Handle, Relation, BBox) :-
        ppl_Polyhedron_get_bounding_box_2(Handle, Relation, BBox, 1).

:- true pred ppl_Polyhedron_is_empty_2(in(Handle),
                                       go(Success))
  :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_is_empty)).

ppl_Polyhedron_is_empty(Handle) :-
	ppl_Polyhedron_is_empty_2(Handle, 1).

:- true pred ppl_Polyhedron_is_universe_2(in(Handle),
                                             go(Success))
  :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_is_universe)).

ppl_Polyhedron_is_universe(Handle) :-
	ppl_Polyhedron_is_universe_2(Handle, 1).

:- true pred ppl_Polyhedron_is_bounded_2(in(Handle),
                                            go(Success))
  :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_is_bounded)).

ppl_Polyhedron_is_bounded(Handle) :-
	ppl_Polyhedron_is_bounded_2(Handle, 1).

:- true pred ppl_Polyhedron_bounds_from_above_2(in(Handle),
                                                in(LinearExpression),
                                                go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_bounds_from_above)).

ppl_Polyhedron_bounds_from_above(Handle, LinearExpression) :-
	ppl_Polyhedron_bounds_from_above_2(Handle, LinearExpression, 1).

:- true pred ppl_Polyhedron_bounds_from_below_2(in(Handle),
                                                in(LinearExpression),
                                                go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_bounds_from_below)).

ppl_Polyhedron_bounds_from_below(Handle, LinearExpression) :-
	ppl_Polyhedron_bounds_from_below_2(Handle, LinearExpression, 1).

:- true pred ppl_Polyhedron_is_topologically_closed_2(in(Handle),
                                                         go(Success))
  :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_is_topologically_closed)).

ppl_Polyhedron_is_topologically_closed(Handle) :-
	ppl_Polyhedron_is_topologically_closed_2(Handle, 1).


:- true pred ppl_Polyhedron_contains_Polyhedron_2(in(Handle1),
                                                  in(Handle2),
                                                  go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_contains_Polyhedron)).

ppl_Polyhedron_contains_Polyhedron(Handle1, Handle2) :-
	ppl_Polyhedron_contains_Polyhedron_2(Handle1, Handle2, 1).


:- true pred ppl_Polyhedron_strictly_contains_Polyhedron_2(in(Handle1),
                                                           in(Handle2),
                                                           go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_strictly_contains_Polyhedron)).

ppl_Polyhedron_strictly_contains_Polyhedron(Handle1, Handle2) :-
	ppl_Polyhedron_strictly_contains_Polyhedron_2(Handle1, Handle2, 1).

ppl_Polyhedron_is_disjoint_from_Polyhedron(Handle1, Handle2) :-
	ppl_Polyhedron_is_disjoint_from_Polyhedron_2(Handle1, Handle2, 1).


:- true pred ppl_Polyhedron_is_disjoint_from_Polyhedron_2(in(Handle1),
                                                             in(Handle2),
                                                             go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_is_disjoint_from_Polyhedron)).

ppl_Polyhedron_equals_Polyhedron(Handle1, Handle2) :-
	ppl_Polyhedron_equals_Polyhedron_2(Handle1, Handle2, 1).

:- true pred ppl_Polyhedron_equals_Polyhedron_2(in(Handle1),
                                                in(Handle2),
                                                go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_equals_Polyhedron)).

:- true pred ppl_Polyhedron_OK_2(in(Handle),
                                 go(Success))
  :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_OK)).

ppl_Polyhedron_OK(Handle) :-
	ppl_Polyhedron_OK_2(Handle, 1).

:- true pred ppl_Polyhedron_add_constraint(in(Handle), in(Constraint))
  :: any_term * any_term + foreign.


:- true pred ppl_Polyhedron_add_constraint_and_minimize_2(in(Handle),
                                                          in(Constraint),
                                                          go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_add_constraint_and_minimize)).

ppl_Polyhedron_add_constraint_and_minimize(Handle, Constraint) :-
        ppl_Polyhedron_add_constraint_and_minimize_2(Handle, Constraint, 1).

:- true pred ppl_Polyhedron_add_generator(in(Handle), in(Generator))
             :: any_term * any_term + foreign.


:- true pred ppl_Polyhedron_add_generator_and_minimize_2(in(Handle),
                                                          in(Generator),
                                                          go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_add_generator_and_minimize)).

ppl_Polyhedron_add_generator_and_minimize(Handle, Generator) :-
        ppl_Polyhedron_add_generator_and_minimize_2(Handle, Generator, 1).

:- true pred ppl_Polyhedron_add_constraints(in(Handle), in(CList))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_constraints_and_minimize_2(in(Handle),
                                                           in(CList),
                                                           go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_add_constraints_and_minimize)).

ppl_Polyhedron_add_constraints_and_minimize(Handle, CList) :-
        ppl_Polyhedron_add_constraints_and_minimize_2(Handle, CList, 1).

:- true pred ppl_Polyhedron_add_generators(in(Handle), in(GList))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_generators_and_minimize_2(in(Handle),
                                                          in(GList),
                                                          go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_add_generators_and_minimize)).

ppl_Polyhedron_add_generators_and_minimize(Handle, GList) :-
        ppl_Polyhedron_add_generators_and_minimize_2(Handle, GList, 1).


:- true pred ppl_Polyhedron_intersection_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_intersection_assign_and_minimize_2(in(Handle1),
                                                          in(Handle2),
                                                          go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_intersection_assign_and_minimize)).

ppl_Polyhedron_intersection_assign_and_minimize(Handle1, Handle2) :-
        ppl_Polyhedron_intersection_assign_and_minimize_2(Handle1, Handle2, 1).

:- true pred ppl_Polyhedron_poly_hull_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_poly_hull_assign_and_minimize_2(in(Handle1),
                                                          in(Handle2),
                                                          go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_poly_hull_assign_and_minimize)).

ppl_Polyhedron_poly_hull_assign_and_minimize(Handle1, Handle2) :-
        ppl_Polyhedron_poly_hull_assign_and_minimize_2(Handle1, Handle2, 1).

:- true pred ppl_Polyhedron_poly_difference_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_affine_image(in(Handle), in(Var),
                                         in(LinearExpression), in(Divisor))
             :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_affine_preimage(in(Handle), in(Var),
                                            in(LinearExpression), in(Divisor))
             :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_generalized_affine_image_2(
                                         in(Handle),
                                         in(Var), in(Rel),
                                         in(LinExpression),
                                         in(Divisor), go(Success))
  :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success),
      foreign(ppl_Polyhedron_generalized_affine_image)).

ppl_Polyhedron_generalized_affine_image(
                  Handle, Var, Rel, LinExpression, Divisor) :-
      ppl_Polyhedron_generalized_affine_image_2(
                  Handle, Var, Rel, LinExpression, Divisor, 1).

:- true pred ppl_Polyhedron_generalized_affine_image_lhs_rhs_2(
                 in(Handle), in(LHS), in(Rel), in(RHS), go(Success))
  :: any_term * any_term * any_term * any_term * int
  + (returns(Success),
      foreign(ppl_Polyhedron_generalized_affine_image_lhs_rhs)).

ppl_Polyhedron_generalized_affine_image_lhs_rhs(Handle, LHS, Rel, RHS) :-
      ppl_Polyhedron_generalized_affine_image_lhs_rhs_2(
                  Handle, LHS, Rel, RHS, 1).

:- true pred ppl_Polyhedron_time_elapse_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_topological_closure_assign(in(Handle))
             :: any_term + foreign.

:- true pred ppl_Polyhedron_BHRZ03_widening_assign_with_token_2(
                 in(Handle1), in(Handle2), in(Tokens), go(Success))
  :: any_term * any_term * any_term * int
  + (returns(Success),
      foreign(ppl_Polyhedron_BHRZ03_widening_assign_with_token)).

ppl_Polyhedron_BHRZ03_widening_assign_with_token(Handle1, Handle2, Tokens) :-
      ppl_Polyhedron_BHRZ03_widening_assign_with_token_2(
                  Handle1, Handle2, Tokens, 1).

:- true pred ppl_Polyhedron_BHRZ03_widening_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token_2(
                 in(Handle1), in(Handle2), in(CList), in(Tokens), go(Success))
  :: any_term * any_term * any_term * any_term * int
  + (returns(Success),
      foreign(ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token)).

ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token(
                  Handle1, Handle2, CList, Tokens) :-
      ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token_2(
                  Handle1, Handle2, CList, Tokens, 1).

:- true pred ppl_Polyhedron_limited_BHRZ03_extrapolation_assign(in(Handle1),
                                                               in(CList),
                                                                in(Handle2))
  :: any_term * any_term * any_term + foreign.


:- true pred ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token_2(
                 in(Handle1), in(Handle2), in(CList), in(Tokens), go(Success))
  :: any_term * any_term * any_term * any_term * int
  + (returns(Success),
      foreign(ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token)).

ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token(
                  Handle1, Handle2, CList, Tokens) :-
      ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token_2(
                  Handle1, Handle2, CList, Tokens, 1).

:- true pred ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign(in(Handle1),
                                                               in(CList),
                                                                in(Handle2))
             :: any_term * any_term * any_term + foreign.


:- true pred ppl_Polyhedron_H79_widening_assign_with_token_2(
                 in(Handle1), in(Handle2), in(Tokens), go(Success))
  :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_H79_widening_assign_with_token)).

ppl_Polyhedron_H79_widening_assign_with_token(Handle1, Handle2, Tokens) :-
      ppl_Polyhedron_H79_widening_assign_with_token_2(
                  Handle1, Handle2, Tokens, 1).

:- true pred ppl_Polyhedron_H79_widening_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_limited_H79_extrapolation_assign_with_token_2(
                 in(Handle1), in(Handle2), in(CList), in(Tokens), go(Success))
  :: any_term * any_term * any_term * any_term * int
  + (returns(Success),
      foreign(ppl_Polyhedron_limited_H79_extrapolation_assign_with_token)).

ppl_Polyhedron_limited_H79_extrapolation_assign_with_token(Handle1,
                                                           Handle2,
                                                           CList,
                                                           Tokens) :-
  ppl_Polyhedron_limited_H79_extrapolation_assign_with_token_2(Handle1,
                                                               Handle2,
                                                               CList,
                                                               Tokens,
                                                               1).

:- true pred ppl_Polyhedron_limited_H79_extrapolation_assign(in(Handle1),
                                                             in(Handle2),
                                                             in(CList))
  :: any_term * any_term * any_term + foreign.

:- true pred
  ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token_2(in(Handle1),
                                                               in(Handle2),
                                                               in(CList),
                                                               in(Tokens),
                                                               go(Success))
  :: any_term * any_term * any_term * any_term * int
  + (returns(Success),
    foreign(ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token)).

ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token(Handle1,
                                                           Handle2,
                                                           CList,
                                                           Tokens) :-
  ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token_2(Handle1,
                                                               Handle2,
                                                               CList,
                                                               Tokens,
                                                               1).

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

:- true pred ppl_Polyhedron_remap_dimensions(in(Handle),
                                              in(PIFunc))
  :: any_term * any_term + foreign.

:- extra_linker_opts('-L.libs').
:- use_foreign_library(ppl_ciao).

:- impl_defined(
[
        ppl_initialize/0,
        ppl_finalize/0,
        ppl_set_timeout_exception_atom/1,
 %       ppl_timeout_exception_atom/1,
        ppl_timeout_exception_atom_2/2,
        ppl_set_timeout/1,
        ppl_reset_timeout/0,
%        ppl_new_Polyhedron_from_dimension/3,
        ppl_new_Polyhedron_from_dimension_2/4,
%        ppl_new_Polyhedron_empty_from_dimension/3,
        ppl_new_Polyhedron_empty_from_dimension_2/4,
%        ppl_new_Polyhedron_from_Polyhedron/4,
        ppl_new_Polyhedron_from_Polyhedron_2/5,
%        ppl_new_Polyhedron_from_constraints/3,
        ppl_new_Polyhedron_from_constraints_2/4,
%        ppl_new_Polyhedron_from_generators/3,
        ppl_new_Polyhedron_from_generators_2/4,
%        ppl_new_Polyhedron_from_bounding_box/3,
        ppl_new_Polyhedron_from_bounding_box_2/4,
        ppl_Polyhedron_swap/2,
        ppl_delete_Polyhedron/1,
%       ppl_Polyhedron_space_dimension/2,
        ppl_Polyhedron_space_dimension_2/3,
%       ppl_Polyhedron_get_constraints/2,
        ppl_Polyhedron_get_constraints_2/3,
%       ppl_Polyhedron_get_minimized_constraints/2,
        ppl_Polyhedron_get_minimized_constraints_2/3,
%       ppl_Polyhedron_get_generators/2,
        ppl_Polyhedron_get_generators_2/3,
%       ppl_Polyhedron_get_minimized_generators/2,
        ppl_Polyhedron_get_minimized_generators_2/3,
%       ppl_Polyhedron_relation_with_constraint/3,
        ppl_Polyhedron_relation_with_constraint_2/4,
%       ppl_Polyhedron_relation_with_generator/3,
        ppl_Polyhedron_relation_with_generator_2/4,
%       ppl_Polyhedron_get_bounding_box/3,
        ppl_Polyhedron_get_bounding_box_2/4,
%       ppl_Polyhedron_is_empty/1,
        ppl_Polyhedron_is_empty_2/2,
%       ppl_Polyhedron_is_universe/1,
        ppl_Polyhedron_is_universe_2/2,
%       ppl_Polyhedron_is_bounded/1,
        ppl_Polyhedron_is_bounded_2/2,
%       ppl_Polyhedron_bounds_from_above/2,
        ppl_Polyhedron_bounds_from_above_2/3,
%       ppl_Polyhedron_bounds_from_below/2,
        ppl_Polyhedron_bounds_from_below_2/3,
%        ppl_Polyhedron_is_topologically_closed/1,
        ppl_Polyhedron_is_topologically_closed_2/2,
%       ppl_Polyhedron_contains_Polyhedron/2,
        ppl_Polyhedron_contains_Polyhedron_2/3,
%       ppl_Polyhedron_strictly_contains_Polyhedron/2,
        ppl_Polyhedron_strictly_contains_Polyhedron_2/3,
%       ppl_Polyhedron_is_disjoint_from_Polyhedron/2,
        ppl_Polyhedron_is_disjoint_from_Polyhedron_2/3,
%       ppl_Polyhedron_equals_Polyhedron/2,
        ppl_Polyhedron_equals_Polyhedron_2/3,
%       ppl_Polyhedron_OK/1,
        ppl_Polyhedron_OK_2/2,
        ppl_Polyhedron_add_constraint/2,
%       ppl_Polyhedron_add_constraint_and_minimize/2,
        ppl_Polyhedron_add_constraint_and_minimize_2/3,
        ppl_Polyhedron_add_generator/2,
%       ppl_Polyhedron_add_generator_and_minimize/2,
        ppl_Polyhedron_add_generator_and_minimize_2/3,
        ppl_Polyhedron_add_constraints/2,
%       ppl_Polyhedron_add_constraints_and_minimize/2,
        ppl_Polyhedron_add_constraints_and_minimize_2/3,
        ppl_Polyhedron_add_generators/2,
%       ppl_Polyhedron_add_generators_and_minimize/2,
        ppl_Polyhedron_add_generators_and_minimize_2/3,
        ppl_Polyhedron_intersection_assign/2,
%       ppl_Polyhedron_intersection_assign_and_minimize/2,
        ppl_Polyhedron_intersection_assign_and_minimize_2/3,
        ppl_Polyhedron_poly_hull_assign/2,
%       ppl_Polyhedron_poly_hull_assign_and_minimize/2,
        ppl_Polyhedron_poly_hull_assign_and_minimize_2/3,
        ppl_Polyhedron_poly_difference_assign/2,
        ppl_Polyhedron_affine_image/4,
        ppl_Polyhedron_affine_preimage/4,
%        ppl_Polyhedron_generalized_affine_image/5,
        ppl_Polyhedron_generalized_affine_image_2/6,
%       ppl_Polyhedron_generalized_affine_image_lhs_rhs/4,
        ppl_Polyhedron_generalized_affine_image_lhs_rhs_2/5,
        ppl_Polyhedron_time_elapse_assign/2,
        ppl_Polyhedron_topological_closure_assign/1,
%       ppl_Polyhedron_BHRZ03_widening_assign_with_token/3,
        ppl_Polyhedron_BHRZ03_widening_assign_with_token_2/4,
        ppl_Polyhedron_BHRZ03_widening_assign/2,
%       ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token/4,
        ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token_2/5,
        ppl_Polyhedron_limited_BHRZ03_extrapolation_assign/3,
%       ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token/4,
        ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token_2/5,
        ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign/3,
%       ppl_Polyhedron_H79_widening_assign_with_token/3,
        ppl_Polyhedron_H79_widening_assign_with_token_2/4,
        ppl_Polyhedron_H79_widening_assign/2,
%       ppl_Polyhedron_limited_H79_extrapolation_assign_with_token/4,
        ppl_Polyhedron_limited_H79_extrapolation_assign_with_token_2/5,
        ppl_Polyhedron_limited_H79_extrapolation_assign/3,
%       ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token/4,
        ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token_2/5,
        ppl_Polyhedron_bounded_H79_extrapolation_assign/3,
        ppl_Polyhedron_add_dimensions_and_project/2,
        ppl_Polyhedron_add_dimensions_and_embed/2,
        ppl_Polyhedron_concatenate_assign/2,
        ppl_Polyhedron_remove_dimensions/2,
        ppl_Polyhedron_remove_higher_dimensions/2,
        ppl_Polyhedron_remap_dimensions/2
]).

:- comment(version_maintenance,off).

/*
***********************************************
This commnted code has been kept for future use
since the above version of this is temporary.
***********************************************

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

:- true pred ppl_Polyhedron_swap(in(Handle1),
                                 in(Handle2))
             :: any_term * any_term + foreign.

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

:- true pred ppl_Polyhedron_get_bounding_box(in(Handle),
                                             in(Relation),
                                             in(BBox))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_is_empty(in(Handle))
             :: any_term + foreign.

:- true pred ppl_Polyhedron_is_universe(in(Handle))
             :: any_term + foreign.

:- true pred ppl_Polyhedron_is_bounded(in(Handle))
             :: any_term + foreign.

:- true pred ppl_Polyhedron_bounds_from_above(in(Handle), in(LinearExpression))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_bounds_from_below(in(Handle), in(LinearExpression))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_is_topologically_closed(in(Handle))
             :: any_term + foreign.

:- true pred ppl_Polyhedron_contains_Polyhedron(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_strictly_contains_Polyhedron(in(Handle1),
                                                         in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_is_disjoint_from_Polyhedron(in(Handle1),
                                                        in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_equals_Polyhedron(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_OK(in(Handle))
             :: any_term + foreign.

:- true pred ppl_Polyhedron_add_constraint(in(Handle), in(Constraint))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_constraint_and_minimize(in(Handle), in(Constraint))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_generator(in(Handle), in(Generator))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_generator_and_minimize(in(Handle), in(Generator))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_constraints(in(Handle), in(CList))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_constraints_and_minimize(in(Handle), in(CList))
             :: any_term * any_term + foreign.

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

:- true pred ppl_Polyhedron_remap_dimensions(in(Handle),
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
        ppl_Polyhedron_is_empty/1,
        ppl_Polyhedron_is_universe/1,
        ppl_Polyhedron_is_bounded/1,
        ppl_Polyhedron_bounds_from_above/2,
        ppl_Polyhedron_bounds_from_below/2,
        ppl_Polyhedron_is_topologically_closed/1,
        ppl_Polyhedron_contains_Polyhedron/2,
        ppl_Polyhedron_strictly_contains_Polyhedron/2,
        ppl_Polyhedron_is_disjoint_from_Polyhedron/2,
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
        ppl_Polyhedron_remap_dimensions/2
]).

:- comment(version_maintenance,off).


*/
