/* Various tests on the Prolog interface.
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

check_all :-
  incl_C,
  incl_NNC,
  strict_incl_C,
  strict_incl_NNC,
  equals_C,
  equals_NNC,
  copy_C_C,
  copy_NNC_NNC,
  copy_C_NNC,
  get_cons_C,
  get_gens_C,
  get_cons_NNC,
  get_gens_NNC,
  space_C,
  space_NNC,
  inters_assign,
  inters_assign_min,
  polyhull_assign,
  polyhull_assign_min,
  polydiff_assign,
  polydiff_assign_min,
  widen_C,
  lim_widen_C,
  widen_NNC,
  lim_widen_NNC,
  top_close_assign,
  add_con,
  add_gen,
  add_cons,
  add_gens,
  add_cons_min,
  add_gens_min,
  add_dim_cons,
  remove_dim,
  remove_high_dim,
  affine,
  affine_pre,
  bounds_from_above,
  bounds_from_below,
  rel_cons,
  rel_gens,
  checks,
  project,
  embed,
  boundingbox,
  poly_from_boundingbox_C,
  poly_from_boundingbox_NNC.

% Tests new_Polyhedron_from_dimension
% and ppl_Polyhedron_contains_Polyhedron for C Polyhedron.
incl_C :-
  ppl_new_Polyhedron_from_dimension(c, 3, P1),
  ppl_new_Polyhedron_from_dimension(c, 3, P2),
  ppl_Polyhedron_contains_Polyhedron(P1, P2),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

% Tests new_Polyhedron_from_dimension
% and ppl_Polyhedron_contains_Polyhedron for NNC Polyhedron.
incl_NNC :-
  ppl_new_Polyhedron_from_dimension(nnc, 3, P1),
  ppl_new_Polyhedron_from_dimension(nnc, 3, P2),
  ppl_Polyhedron_contains_Polyhedron(P1, P2),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

% Tests new_Polyhedron_empty_from_dimension
% and ppl_Polyhedron_strictly_contains_Polyhedron for C Polyhedron.
strict_incl_C :-
  ppl_new_Polyhedron_from_dimension(c, 3, P1),
  ppl_Polyhedron_check_universe(P1),
  ppl_new_Polyhedron_empty_from_dimension(c, 3, P2),
  ppl_Polyhedron_check_empty(P2),
  ppl_Polyhedron_strictly_contains_Polyhedron(P1, P2),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

% Tests new_Polyhedron_empty_from_dimension and
% ppl_Polyhedron_strictly_contains_Polyhedron for NNC Polyhedron.
strict_incl_NNC :-
  ppl_new_Polyhedron_from_dimension(nnc, 3, P1),
  ppl_new_Polyhedron_empty_from_dimension(nnc, 3, P2),
  ppl_Polyhedron_strictly_contains_Polyhedron(P1, P2),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

% Tests ppl_Polyhedron_equals_Polyhedron for C Polyhedron.
equals_C :-
  ppl_new_Polyhedron_from_dimension(c, 3, P1),
  ppl_new_Polyhedron_from_dimension(c, 3, P2),
  ppl_new_Polyhedron_empty_from_dimension(c, 3, P3),
  ppl_Polyhedron_equals_Polyhedron(P1, P2),
  \+ ppl_Polyhedron_equals_Polyhedron(P1, P3),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P3).

% Tests ppl_Polyhedron_equals_Polyhedron for NNC Polyhedron.
equals_NNC :-
  ppl_new_Polyhedron_from_dimension(nnc, 3, P1),
  ppl_new_Polyhedron_from_dimension(nnc, 3, P2),
  ppl_new_Polyhedron_empty_from_dimension(nnc, 3, P3),
  ppl_Polyhedron_equals_Polyhedron(P1, P2),
  \+ ppl_Polyhedron_equals_Polyhedron(P1, P3),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P3).

% Tests ppl_Polyhedron_from_Polyhedron when both Polyhedra are C.
copy_C_C :-
  ppl_new_Polyhedron_from_dimension(c, 3, P1),
  ppl_new_Polyhedron_from_Polyhedron(c, P1, c, P2),
  ppl_Polyhedron_equals_Polyhedron(P1, P2),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

% Tests ppl_Polyhedron_from_Polyhedron when both Polyhedra are NNC.
copy_NNC_NNC :-
  ppl_new_Polyhedron_from_dimension(nnc, 3, P1),
  ppl_new_Polyhedron_from_Polyhedron(nnc, P1, nnc, P2),
  ppl_Polyhedron_equals_Polyhedron(P1, P2),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

% Tests ppl_Polyhedron_from_Polyhedron when one  Polyhedron
% is C and the other is NNC.
% This also uses ppl_new_Polyhedron_from_constraints.
copy_C_NNC :-
  ppl_new_Polyhedron_from_dimension(nnc, 3, P1),
  ppl_new_Polyhedron_from_Polyhedron(nnc, P1, c, P2),
  ppl_new_Polyhedron_from_Polyhedron(c, P2, nnc, P1a),
  ppl_Polyhedron_equals_Polyhedron(P1, P1a),
  ppl_new_Polyhedron_from_Polyhedron(nnc, P1a, c, P2a),
  ppl_Polyhedron_equals_Polyhedron(P2, P2a),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P1a),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P2a),
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [3 >= A, 4 > A,
                                       4*A + B - 2*C >= 5],
                                      P3),
  ppl_new_Polyhedron_from_Polyhedron(nnc, P3, c, P4),
  ppl_Polyhedron_get_constraints(P4, CS),
  CS = [4*A + 1*B + -2*C >= 5, -1*A >= -3],
  ppl_delete_Polyhedron(P3),
  ppl_delete_Polyhedron(P4).

% Tests ppl_new_Polyhedron_from_constraints for a C Polyhedron.
get_cons_C :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
  ppl_new_Polyhedron_from_constraints(c,
                                      [3 >= A, 4*A + B - 2*C >= 5],
                                      P),
  ppl_Polyhedron_get_constraints(P, CS),
  CS = [4*A + 1*B + -2*C >= 5, -1*A >= -3],
  ppl_delete_Polyhedron(P).

% Tests ppl_new_Polyhedron_from_generators for a C Polyhedron.
get_gens_C :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
  ppl_new_Polyhedron_from_generators(c,
                                     [point(1*A + 1*B + 1*C, 1),
                                      point(1*A + 1*B + 1*C, 1)],
                                     P),
  ppl_Polyhedron_get_generators(P, GS),
  GS = [point(1*A + 1*B + 1*C), point(1*A + 1*B + 1*C)],
  ppl_delete_Polyhedron(P).

% Tests ppl_new_Polyhedron_from_constraints for an NNC Polyhedron.
get_cons_NNC :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [3 > A, 4*A + B - 2*C >= 5],
                                      P),
  ppl_Polyhedron_get_constraints(P, CS),
  CS = [4*A + 1*B + -2*C >= 5, -1*A > -3],
  ppl_delete_Polyhedron(P).

% Tests ppl_new_Polyhedron_from_generators for an NNC Polyhedron.
get_gens_NNC :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
  ppl_new_Polyhedron_from_generators(nnc,
                                     [point(1*A + 1*B + 1*C),
                                      closure_point(1*A + 1*B + 1*C)],
                                     P),
  ppl_Polyhedron_get_generators(P, GS),
  GS = [point(1*A + 1*B + 1*C)],
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_space_dimension for a C Polyhedron.
space_C :-
  ppl_new_Polyhedron_from_dimension(c, 3, P),
  ppl_Polyhedron_space_dimension(P, N),
  N = 3,
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_space_dimension for an NNC Polyhedron.
space_NNC :-
  ppl_new_Polyhedron_from_dimension(nnc, 3, P),
  ppl_Polyhedron_space_dimension(P, N),
  N = 3,
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_intersection_assign (using NNC Polyhedra).
inters_assign :-
  A = '$VAR'(0), B = '$VAR'(1), 
  ppl_new_Polyhedron_from_generators(nnc, 
                                     [point(0), point(B), 
                                      point(A), point(A, 2)],
                                     P1),
  ppl_new_Polyhedron_from_generators(nnc,
                                     [point(0), point(A), 
                                      point(A + B), point(A, 2)],
                                     P2),
  ppl_Polyhedron_intersection_assign(P1, P2),
  ppl_Polyhedron_get_generators(P1, GS),
  ppl_Polyhedron_get_constraints(P1, CS),
  CS = [1*A+ -1*B >= 0, 1*B >= 0, -1*A + -1*B >= -1],
  GS = [point(1*A + 1*B, 2), point(1*A), point(0)],
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

% Tests ppl_Polyhedron_intersection_assign_and_minimize
% (using NNC Polyhedra).
inters_assign_min :-
  A = '$VAR'(0), B = '$VAR'(1), 
  ppl_new_Polyhedron_from_generators(nnc,
                                     [point(0), point(B),
                                      point(A), point(A, 2)],
                                     P1),
  ppl_new_Polyhedron_from_generators(nnc,
                                     [point(0), point(A), point(A + B)],
                                     P2),
  ppl_Polyhedron_intersection_assign_and_minimize(P1, P2),
  ppl_Polyhedron_get_generators(P1, GS),
  ppl_Polyhedron_get_constraints(P1, CS),
  CS = [1*A + -1*B >=0, 1*B >= 0, -1*A + -1*B >= -1],
  GS = [point(1*A + 1*B, 2), point(1*A), point(0)],
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

% Tests ppl_Polyhedron_poly_hull_assign (using C Polyhedra).
polyhull_assign :-
  A = '$VAR'(0), B = '$VAR'(1), 
  ppl_new_Polyhedron_from_generators(c,
                                     [point(0), point(B),
                                      point(A), point(A,2)],
                                     P1),
  ppl_new_Polyhedron_from_generators(c,
                                     [point(0), point(A),
                                      point(A + B), point(A, 2)],
                                     P2),
  ppl_Polyhedron_poly_hull_assign(P1, P2),
  ppl_Polyhedron_get_generators(P1, GS),
  ppl_Polyhedron_get_constraints(P1, CS),
  CS = [1*A >= 0, 1*B >= 0, -1*B >= -1, -1*A >= -1],
  GS = [point(1*A + 1*B), point(1*A, 2), point(1*A), point(1*B), point(0)],
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

% Tests ppl_Polyhedron_poly_hull_assign_and_minimize (using C Polyhedra).
polyhull_assign_min :-
  A = '$VAR'(0), B = '$VAR'(1), 
  ppl_new_Polyhedron_from_generators(c,
                                     [point(0), point(B),
                                      point(A), point(A, 2)],
                                     P1),
  ppl_new_Polyhedron_from_generators(c, 
                                     [point(0), point(A),
                                      point(A + B)],
                                     P2),
  ppl_Polyhedron_poly_hull_assign_and_minimize(P1,P2),
  ppl_Polyhedron_get_generators(P1, GS),
  ppl_Polyhedron_get_constraints(P1, CS),
  CS = [1*A >= 0, 1*B >= 0, -1*B >= -1, -1*A >= -1],
  GS = [point(1*A + 1*B), point(1*A), point(1*B), point(0)],
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

% Tests ppl_Polyhedron_poly_difference_assign (using C Polyhedra).
polydiff_assign :-
  A = '$VAR'(0), B = '$VAR'(1), 
  ppl_new_Polyhedron_from_generators(c,
                                     [point(0), point(B),
                                      point(A), point(A,2)],
                                     P1),
  ppl_new_Polyhedron_from_generators(c,
                                     [point(0), point(A),
                                      point(A + B), point(A,2)],
                                     P2),
  ppl_Polyhedron_poly_difference_assign(P1, P2),
  ppl_Polyhedron_get_generators(P1, GS),
  ppl_Polyhedron_get_constraints(P1, CS),
  CS = [1*A >= 0, 1*B >= 0, -1*A + -1*B >= -1],
  GS = [point(1*A + 1*B, 2), point(1*A), point(1*B), point(0)],
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

% Tests ppl_Polyhedron_poly_difference_assign_and_minimize
% (using C Polyhedra).
polydiff_assign_min :-
  A = '$VAR'(0), B = '$VAR'(1), 
  ppl_new_Polyhedron_from_generators(c,
                                     [point(0), point(B),
                                      point(A), point(A,2)],
                                     P1),
  ppl_new_Polyhedron_from_generators(c,
                                     [point(0), point(A),
                                      point(A + B)],
                                     P2),
  ppl_Polyhedron_poly_difference_assign_and_minimize(P1, P2),
  ppl_Polyhedron_get_generators(P1, GS),
  ppl_Polyhedron_get_constraints(P1, CS),
  CS = [1*A >= 0, 1*B >= 0, -1*A + -1*B >= -1],
  GS = [point(1*A), point(1*B), point(0)],
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

% Tests ppl_Polyhedron_H79_widening_assign
% (using C Polyhedra).
widen_C :-
  A = '$VAR'(0), B = '$VAR'(1), 
  ppl_new_Polyhedron_from_dimension(c, 2, P),
  ppl_new_Polyhedron_from_constraints(c, [A >= 1, B >= 0], Q),
  ppl_Polyhedron_H79_widening_assign(P, Q),
  ppl_Polyhedron_get_constraints(P, CP),
  ppl_Polyhedron_get_constraints(Q, CQ), 
  CP = [],
  CQ = [1*A >= 1, 1*B >= 0],
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q).

% Tests ppl_Polyhedron_limited_H79_widening_assign
% (using C Polyhedra).
lim_widen_C :-
  A = '$VAR'(0), B = '$VAR'(1), 
  ppl_new_Polyhedron_from_dimension(c, 2, P),
  ppl_new_Polyhedron_from_constraints(c, [A >= 1, B >= 0], Q),
  ppl_Polyhedron_add_constraints_and_minimize(Q, [A >= 1, B >= 0]),
  ppl_Polyhedron_limited_H79_widening_assign(P, Q, [A >= 2, B >= 1]),
  ppl_Polyhedron_get_constraints(P, CS),
  CS = [1*A >= 2, 1*B >= 1],
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q).

% Tests ppl_Polyhedron_H79_widening_assign
% (using NNC Polyhedra).
widen_NNC :-
  A = '$VAR'(0), B = '$VAR'(1), 
  ppl_new_Polyhedron_from_dimension(nnc, 2, P),
  ppl_new_Polyhedron_from_constraints(nnc, [A >= 1, B >= 0], Q),
  ppl_Polyhedron_H79_widening_assign(P, Q),
  ppl_Polyhedron_get_constraints(P, CP),
  ppl_Polyhedron_get_constraints(Q, CQ),
  CP = [],
  CQ = [1*A >= 1, 1*B >= 0],
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q).

% Tests ppl_Polyhedron_limited_H79_widening_assign
% (using NNC Polyhedra).
lim_widen_NNC :-
  A = '$VAR'(0), B = '$VAR'(1), 
  ppl_new_Polyhedron_from_dimension(nnc, 2, P),
  ppl_new_Polyhedron_from_constraints(nnc, [A >= 1, B >= 0], Q),
  ppl_Polyhedron_add_constraints_and_minimize(Q, [A >= 1, B >= 0]),
  ppl_Polyhedron_limited_H79_widening_assign(P, Q,
                                             [A >= 2, B >= 1]),
  ppl_Polyhedron_get_constraints(P, CS),
  CS = [1*A >= 2, 1*B >= 1],
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q).

% Tests ppl_Polyhedron_topological_closure_assign
% (using NNC Polyhedra).
top_close_assign :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [3 > A, 4*A + B - 2*C >= 5],
                                      P),
  ppl_Polyhedron_topological_closure_assign(P),
  ppl_Polyhedron_get_constraints(P, CS),
  CS = [4*A + 1*B + -2*C >= 5, -1*A >= -3],
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_add_constraint.
add_con :-
  A = '$VAR'(0), B = '$VAR'(1), 
  ppl_new_Polyhedron_from_dimension(c, 2, P),
  ppl_Polyhedron_add_constraint(P, A - B >= 1), 
  ppl_Polyhedron_get_constraints(P, CS),
  CS = [1*A + -1*B >= 1],
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_add_generator.
add_gen :-
  A = '$VAR'(0), B = '$VAR'(1), 
  ppl_new_Polyhedron_from_dimension(c, 2, P),
  ppl_Polyhedron_add_generator(P, point(1*A + 1*B, 1)),
  ppl_Polyhedron_get_generators(P, GS),
  GS = [point(1*A + 1*B), point(0), line(1*A), line(1*B)],
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_add_constraints.
add_cons :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
  ppl_new_Polyhedron_from_dimension(c, 3, P),
  ppl_Polyhedron_add_constraints(P, [A >= 1, B >= 0,
                                     4*A + B - 2*C >= 5]),
  ppl_Polyhedron_get_constraints(P, CS),
  CS = [4*A + 1*B + -2*C >= 5, 1*A >= 1, 1*B >= 0],
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_add_generators.
add_gens :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
  ppl_new_Polyhedron_empty_from_dimension(c, 3, P),
  ppl_Polyhedron_add_generators(P, 
                                [point(1*A + 1*B + 1*C, 1),
                                 ray(1*A), ray(2*A),
                                 point(1*A + 1*B + 1*C, 1),
                                 point(-100*A - 5*B, 8)]),
  ppl_Polyhedron_get_generators(P, GS), 
  GS = [point(1*A + 1*B + 1*C), ray(1*A), point(-100*A + -5*B, 8)],
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_add_constraints_and_minimize.
add_cons_min :-
  A = '$VAR'(0), B = '$VAR'(1), 
  ppl_new_Polyhedron_from_dimension(c, 2, P),
  ppl_Polyhedron_add_constraints_and_minimize(P, [A >= 1, B >= 0]),
  ppl_Polyhedron_get_constraints(P, CS), 
  CS = [1*A >= 1, 1*B >= 0],
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_add_generators_and_minimize.
add_gens_min :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
  ppl_new_Polyhedron_empty_from_dimension(c, 3, P),
  ppl_Polyhedron_add_generators_and_minimize(P, 
                                             [point(1*A + 1*B + 1*C),
                                              ray(1*A), ray(2*A),
                                              point(1*A + 1*B + 1*C)]),
  ppl_Polyhedron_get_generators(P, GS), 
  GS = [point(1*A + 1*B + 1*C), ray(1*A)],
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_add_dimensions_and_constraints.
add_dim_cons :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
  D = '$VAR'(3), E = '$VAR'(4), 
  ppl_new_Polyhedron_from_dimension(nnc, 2, P),
  ppl_Polyhedron_add_dimensions_and_constraints(P, 
                                                [A > 1, B >= 0,
                                                 C >= 0]),
  ppl_Polyhedron_get_constraints(P, CS), 
  CS = [1*C > 1, 1*D >= 0, 1*E >= 0],
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_remove_dimensions.
remove_dim :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
  ppl_new_Polyhedron_from_dimension(nnc, 3, P),
  ppl_Polyhedron_add_constraints(P, [A > 1, B >= 0, C >= 0]),
  ppl_Polyhedron_remove_dimensions(P,[B]),
  ppl_Polyhedron_get_constraints(P, CS),
  CS = [1*A > 1, 1*B >= 0],
  % Note: now 'B' refers to the old 'C' variable.
  ppl_Polyhedron_remove_dimensions(P,[A, B]),
  ppl_Polyhedron_space_dimension(P, 0),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_remove_higher_dimensions.
remove_high_dim :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), 
  ppl_new_Polyhedron_from_dimension(nnc, 3, P),
  ppl_Polyhedron_add_constraints(P, [A > 1, B >= 0, C >= 0]),
  ppl_Polyhedron_get_constraints(P, CS1),
  ppl_Polyhedron_remove_higher_dimensions(P, 1),
  ppl_Polyhedron_get_constraints(P, CS2), 
  CS1 = [1*A > 1, 1*B >= 0, 1*C >= 0],
  CS2 = [1*A > 1],
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_affine_image.
affine :-
  A = '$VAR'(0), B = '$VAR'(1), 
  ppl_new_Polyhedron_from_dimension(nnc, 2, P),
  ppl_Polyhedron_add_constraint(P, A - B = 1),
  ppl_Polyhedron_get_constraints(P, CS),
  ppl_Polyhedron_affine_image(P, A, A + 1, 1),
  ppl_Polyhedron_get_constraints(P, CS1),
  CS = [1*A + -1*B = 1],
  CS1 = [1*A + -1*B = 2],
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_affine_preimage.
affine_pre :-
  A = '$VAR'(0), B = '$VAR'(1), 
  ppl_new_Polyhedron_from_dimension(nnc, 2, P),
  ppl_Polyhedron_add_constraint(P, A + B >= 10),
  ppl_Polyhedron_get_constraints(P, CS),
  ppl_Polyhedron_affine_preimage(P, A, A + 1, 1),
  ppl_Polyhedron_get_constraints(P, CS1),
  CS = [1*A + 1*B >= 10],
  CS1 = [1*A + 1*B >= 9],
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_relation_with_constraint.
rel_cons :-
  A = '$VAR'(0), B = '$VAR'(1), 
  ppl_new_Polyhedron_from_dimension(c, 3, P),
  ppl_Polyhedron_add_constraints_and_minimize(P, [A >= 1, B >= 0]),
  R = [is_disjoint],
  ppl_Polyhedron_relation_with_constraint(P, A = 0, R),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_relation_with_generator.
rel_gens :-
  A = '$VAR'(0), B = '$VAR'(1),  C = '$VAR'(2),
  ppl_new_Polyhedron_empty_from_dimension(nnc, 3, P),
  ppl_Polyhedron_add_generators_and_minimize(P, 
                                             [point(1*A + 1*B + 1*C)]),
  ppl_Polyhedron_relation_with_generator(P, point(1*A), R),
  R = [],
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_check_universe,
%       ppl_Polyhedron_check_empty,
%       ppl_Polyhedron_is_bounded,
%       ppl_Polyhedron_is_topologically_closed
checks :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2),
  ppl_new_Polyhedron_from_dimension(nnc, 3, P),
  ppl_Polyhedron_check_universe(P),
  ppl_new_Polyhedron_empty_from_dimension(nnc, 3, P1),
  ppl_Polyhedron_check_empty(P1),
  ppl_Polyhedron_add_generators_and_minimize(P1, 
                                             [point(1*A + 1*B + 1*C)]),
  ppl_Polyhedron_is_bounded(P1),
  ppl_Polyhedron_add_constraints(P, [A > 1, B =< 3, A =< 2]),
  \+ ppl_Polyhedron_is_topologically_closed(P),
  ppl_Polyhedron_add_constraints(P, [A > 2]),
  ppl_Polyhedron_is_topologically_closed(P),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_add_dimensions_and_project.
project :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), D = '$VAR'(3), 
  ppl_new_Polyhedron_from_dimension(nnc, 2, P),
  ppl_Polyhedron_add_constraints_and_minimize(P, [A >= 1, B >= 0]),
  ppl_Polyhedron_add_dimensions_and_project(P, 2),
  ppl_Polyhedron_get_constraints(P, CS),
  CS = [1*A >= 1, 1*B >= 0, 1*C = 0, 1*D = 0],
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_add_dimensions_and_embed.
embed :-
  A = '$VAR'(0), B = '$VAR'(1), 
  ppl_new_Polyhedron_from_dimension(nnc, 2, P),
  ppl_Polyhedron_add_constraints_and_minimize(P, [A >= 1, B >= 0]),
  ppl_Polyhedron_add_dimensions_and_embed(P, 2),
  ppl_Polyhedron_get_constraints(P,CS), 
  CS = [1*A >= 1, 1*B >= 0],
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_get_bounding_box.
boundingbox :-
  A = '$VAR'(0), B = '$VAR'(1), 
  ppl_new_Polyhedron_from_dimension(c, 2, P),
  ppl_Polyhedron_add_constraints_and_minimize(P, [B >= 0, 4*A =< 2]),
  ppl_Polyhedron_get_bounding_box(P, Box),
  Box = [i(o(minf), c(+1/2)), i(c(0), o(pinf))],
  ppl_delete_Polyhedron(P).

% Tests ppl_new_Polyhedron_from_bounding_box for a C polyhedron.
poly_from_boundingbox_C :-
  ppl_new_Polyhedron_from_bounding_box(c,
                                       [i(c(1/2), o(pinf)),
                                        i(o(minf), c(-1/2))],
                                       P),
  ppl_Polyhedron_get_bounding_box(P, Box),
  Box = [i(c(1/2), o(pinf)), i(o(minf), c(-1/2))],
  ppl_delete_Polyhedron(P).

% Tests ppl_new_Polyhedron_from_bounding_box for an NNC polyhedron.
poly_from_boundingbox_NNC :-
  ppl_new_Polyhedron_from_bounding_box(nnc,
                                       [i(o(0/2), o(pinf)),
                                        i(o(minf), o(1))],
                                       P),
  ppl_Polyhedron_get_bounding_box(P, Box),
  Box = [i(o(0), o(pinf)), i(o(minf), o(1))],
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_bounds_from_above for an NNC polyhedron.
bounds_from_above :-
  A = '$VAR'(0), B = '$VAR'(1), 
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [1*A > 1, 1*B > 0,
                                       -1*B > -1],
                                     P),
  \+ ppl_Polyhedron_bounds_from_above(P, A),
  ppl_Polyhedron_add_constraint(P, A < 2),
  ppl_Polyhedron_bounds_from_above(P, A),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_bounds_from_below for an NNC polyhedron.
bounds_from_below :-
  A = '$VAR'(0), B = '$VAR'(1), 
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [1*B > 0,
                                       -1*B > -1],
                                     P),
  \+ ppl_Polyhedron_bounds_from_below(P, A),
  ppl_Polyhedron_add_constraint(P, A > 1),
  ppl_Polyhedron_bounds_from_below(P, A),
  ppl_delete_Polyhedron(P).

% These next 2 tests demonstrate a bug in the bounding box software.
boundingbox1(Box,CS) :-
  A = '$VAR'(0), B = '$VAR'(1), 
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [1*A > 1, 1*B > 1,
                                       -1*B > -1, -1*A > -1],
                                      P),
  ppl_Polyhedron_get_bounding_box(P, Box),
  ppl_Polyhedron_get_constraints(P,CS), 
  ppl_delete_Polyhedron(P).

boundingbox2(Box,CS) :-
  ppl_new_Polyhedron_from_dimension(nnc, 2, P),
  ppl_Polyhedron_add_constraints(P, [0=1]),
  ppl_Polyhedron_get_bounding_box(P, Box),
  ppl_Polyhedron_get_constraints(P,CS), 
  ppl_delete_Polyhedron(P).

/*
bounds_from_above :-
  A = '$VAR'(0), B = '$VAR'(1), 
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [],
                                     P),
  ppl_Polyhedron_bounds_from_above(P,A),
  ppl_delete_Polyhedron(P).

?- bounds_from_above.
Matrix has no rows but num_columns() is positive!
ppl_pl: ../../ppl/src/ConSys.cc:98: bool Parma_Polyhedra_Library::ConSys::adjust_topology_and_dimension(Parma_Polyhedra_Library::Topology, unsigned int): Assertion `OK()' failed.
Abort (core dumped)        

*/
