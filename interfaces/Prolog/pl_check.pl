/* Various tests on the Prolog interface.
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

check_all :-
  ppl_initialize,
  new_universe_C,
  new_universe_NNC,
  new_empty_C,
  new_empty_NNC,
  copy_C_C,
  copy_NNC_NNC,
  copy_C_NNC,
  new_cons_C,
  new_gens_C,
  new_cons_NNC,
  new_gens_NNC,
  new_poly_from_bounding_box_C,
  new_poly_from_bounding_box_NNC,
  new_poly_from_bounding_box_NNC1,
  space_C,
  space_NNC,
  inters_assign,
  inters_assign_min,
  conc_assign,
  polyhull_assign,
  polyhull_assign_min,
  polydiff_assign,
  polydiff_assign_min,
  bhrz03_widen_C,
  lim_bhrz03_widen_C,
  bhrz03_widen_NNC,
  lim_bhrz03_widen_NNC,
  h79_widen_C,
  lim_h79_widen_C,
  h79_widen_NNC,
  lim_h79_widen_NNC,
  top_close_assign,
  get_cons,
  get_min_cons,
  get_gens,
  get_min_gens,
  add_con,
  add_gen,
  add_con_min,
  add_gen_min,
  add_cons,
  add_gens,
  add_cons_min,
  add_gens_min,
  project,
  embed,
  remove_dim,
  remove_high_dim,
  affine,
  affine_pre,
  rel_cons,
  rel_gens,
  checks,
  bounds_from_above,
  bounds_from_below,
  contains_C,
  contains_NNC,
  strict_contains_C,
  strict_contains_NNC,
  is_disjoint_from_C,
  is_disjoint_from_NNC,
  equals_C,
  equals_NNC,
  get_boundingbox_C,
  get_boundingbox_NNC,
  get_bounding_box_complexity_C,
  get_bounding_box_complexity_NNC,
  time_out,
  !,
  ppl_finalize.
check_all :-
  ppl_finalize,
  fail.


% Tests new_Polyhedron_from_dimension
% and ppl_delete_Polyhedron for C Polyhedron.
new_universe_C :-
  ppl_new_Polyhedron_from_dimension(c, 3, P),
  ppl_Polyhedron_check_universe(P),
  ppl_delete_Polyhedron(P).

% Tests new_Polyhedron_from_dimension
% and ppl_delete_Polyhedron for NNC Polyhedron.
new_universe_NNC :-
  ppl_new_Polyhedron_from_dimension(nnc, 3, P),
  ppl_Polyhedron_check_universe(P),
  ppl_delete_Polyhedron(P).

% Tests new_Polyhedron_empty_from_dimension for C Polyhedron.
new_empty_C :-
  ppl_new_Polyhedron_empty_from_dimension(c, 3, P),
  ppl_Polyhedron_check_empty(P),
  ppl_delete_Polyhedron(P).

% Tests new_Polyhedron_empty_from_dimension for C Polyhedron.
new_empty_NNC :-
  ppl_new_Polyhedron_empty_from_dimension(nnc, 3, P),
  ppl_Polyhedron_check_empty(P),
  ppl_delete_Polyhedron(P).

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
  ppl_new_Polyhedron_from_Polyhedron(c, P4, nnc, P3a),
  ppl_new_Polyhedron_from_Polyhedron(nnc, P3a, c, P4a),
  ppl_Polyhedron_equals_Polyhedron(P3, P3a),
  ppl_Polyhedron_equals_Polyhedron(P4, P4a),
  ppl_delete_Polyhedron(P3),
  ppl_delete_Polyhedron(P4),
  ppl_delete_Polyhedron(P3a),
  ppl_delete_Polyhedron(P4a).

% Tests ppl_new_Polyhedron_from_constraints for a C Polyhedron.
new_cons_C :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2),
  ppl_new_Polyhedron_from_constraints(c,
                                      [3 >= A, 4*A + B - 2*C >= 5],
                                      P),
  ppl_new_Polyhedron_from_constraints(c,
                                      [4*A + 1*B + -2*C >= 5, -1*A >= -3],
                                      Pa),
  ppl_Polyhedron_equals_Polyhedron(P, Pa),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Pa).

% Tests ppl_new_Polyhedron_from_generators for a C Polyhedron.
new_gens_C :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2),
  ppl_new_Polyhedron_from_generators(c,
                                     [point(A + B + C, 1),
                                      point(A + B + C, 1)],
                                     P),
  ppl_new_Polyhedron_from_generators(c,
                                     [point(A + B + C),
                                      point(A + B + C)],
                                     Pa),
  ppl_Polyhedron_equals_Polyhedron(P, Pa),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Pa).

% Tests ppl_new_Polyhedron_from_constraints for an NNC Polyhedron.
new_cons_NNC :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [3 > A, 4*A + B - 2*C >= 5],
                                      P),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [4*A + 1*B + -2*C >= 5, -1*A > -3],
                                      Pa),
  ppl_Polyhedron_equals_Polyhedron(P, Pa),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Pa).

% Tests ppl_new_Polyhedron_from_generators for an NNC Polyhedron.
new_gens_NNC :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2),
  ppl_new_Polyhedron_from_generators(nnc,
                                     [point(A + B + C),
                                      closure_point(A + B + C)],
                                     P),
  ppl_new_Polyhedron_from_generators(nnc,
                                     [point(A + B + C)],
                                     Pa),
  ppl_Polyhedron_equals_Polyhedron(P, Pa),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Pa).

% Tests ppl_new_Polyhedron_from_bounding_box for a C polyhedron.
new_poly_from_bounding_box_C :-
  ppl_new_Polyhedron_from_bounding_box(c,
                                       [i(c(1/2), o(pinf)),
                                        i(o(minf), c(-1/2))],
                                       P),
  ppl_Polyhedron_get_bounding_box(P, any, Box),
  Box = [i(c(1/2), o(pinf)), i(o(minf), c(-1/2))],
  ppl_delete_Polyhedron(P).

% Tests ppl_new_Polyhedron_from_bounding_box for an NNC polyhedron.
new_poly_from_bounding_box_NNC :-
  ppl_new_Polyhedron_from_bounding_box(nnc,
                                       [i(o(0/2), o(pinf)),
                                        i(o(minf), o(1))],
                                       P),
  ppl_Polyhedron_get_bounding_box(P, any, Box),
  Box = [i(o(0), o(pinf)), i(o(minf), o(1))],
  ppl_delete_Polyhedron(P).

new_poly_from_bounding_box_NNC1 :-
  Max = -4,
  ppl_new_Polyhedron_from_bounding_box(nnc,
                                       [i(c(Max), c(1)),
                                        i(c(-1), c(1))],
                                       P),
  ppl_Polyhedron_get_bounding_box(P, any, Box),
  Box = [i(c(Max), c(1)), i(c(-1), c(1))],
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
  ppl_new_Polyhedron_from_generators(nnc,
                                     [point(A + B, 2),
                                      point(A), point(0)],
                                     P1a),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [A+ -1*B >= 0, B >= 0,
                                       -1*A + -1*B >= -1],
                                      P1b),
  ppl_Polyhedron_equals_Polyhedron(P1, P1a),
  ppl_Polyhedron_equals_Polyhedron(P1, P1b),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P1a),
  ppl_delete_Polyhedron(P1b).

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
  ppl_new_Polyhedron_from_generators(nnc,
                                     [point(A + B, 2),
                                      point(A), point(0)],
                                     P1a),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [A+ -1*B >= 0, B >= 0,
                                       A + B =< 1],
                                      P1b),
  ppl_Polyhedron_equals_Polyhedron(P1, P1a),
  ppl_Polyhedron_equals_Polyhedron(P1, P1b),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P1a),
  ppl_delete_Polyhedron(P1b).

% Tests ppl_Polyhedron_concatenate_assign.
conc_assign :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2),
  D = '$VAR'(3), E = '$VAR'(4),
  ppl_new_Polyhedron_from_dimension(nnc, 2, P),
  ppl_new_Polyhedron_from_constraints(nnc, [A > 1, B >= 0, C >= 0], Q),
  ppl_Polyhedron_concatenate_assign(P, Q),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [C > 1, D >= 0, E >= 0],
                                      P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q).

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
  ppl_new_Polyhedron_from_generators(c,
                                     [point(A + B), point(A, 2),
                                      point(A), point(B), point(0)],
                                     P1a),
  ppl_new_Polyhedron_from_constraints(c,
                                      [A >= 0, B >= 0,
                                       B =< 1, A =< 1],
                                      P1b),
  ppl_Polyhedron_equals_Polyhedron(P1, P1a),
  ppl_Polyhedron_equals_Polyhedron(P1, P1b),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P1a),
  ppl_delete_Polyhedron(P1b).

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
  ppl_new_Polyhedron_from_generators(c,
                                     [point(A + B),
                                      point(A), point(B), point(0)],
                                     P1a),
  ppl_new_Polyhedron_from_constraints(c,
                                      [A >= 0, B >= 0,
                                       B =< 1, A =< 1],
                                      P1b),
  ppl_Polyhedron_equals_Polyhedron(P1, P1a),
  ppl_Polyhedron_equals_Polyhedron(P1, P1b),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P1a),
  ppl_delete_Polyhedron(P1b).

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
  ppl_new_Polyhedron_from_generators(c,
                                     [point(A + B, 2), point(A),
                                      point(B), point(0)],
                                     P1a),
  ppl_new_Polyhedron_from_constraints(c,
                                      [A >= 0, B >= 0, A + B =< 1],
                                      P1b),
  ppl_Polyhedron_equals_Polyhedron(P1, P1a),
  ppl_Polyhedron_equals_Polyhedron(P1, P1b),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P1a),
  ppl_delete_Polyhedron(P1b).

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
  ppl_new_Polyhedron_from_generators(c,
                                     [point(1*A), point(1*B), point(0)],
                                     P1a),
  ppl_new_Polyhedron_from_constraints(c,
                                      [A >= 0, B >= 0, A + B =< 1],
                                      P1b),
  ppl_Polyhedron_equals_Polyhedron(P1, P1a),
  ppl_Polyhedron_equals_Polyhedron(P1, P1b),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P1a),
  ppl_delete_Polyhedron(P1b).

% Tests ppl_Polyhedron_BHRZ03_widening_assign
% (using C Polyhedra).
bhrz03_widen_C :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_dimension(c, 2, P),
  ppl_new_Polyhedron_from_constraints(c, [A >= 1, B >= 0], Q),
  ppl_Polyhedron_BHRZ03_widening_assign(P, Q),
  ppl_new_Polyhedron_from_dimension(c, 2, Pa),
  ppl_new_Polyhedron_from_constraints(c,
                                      [A >= 1, B >= 0],
                                      Qa),
  ppl_Polyhedron_equals_Polyhedron(P, Pa),
  ppl_Polyhedron_equals_Polyhedron(Q, Qa),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q),
  ppl_delete_Polyhedron(Pa),
  ppl_delete_Polyhedron(Qa).

% Tests ppl_Polyhedron_limited_BHRZ03_widening_assign
% (using C Polyhedra).
lim_bhrz03_widen_C :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_dimension(c, 2, P),
  ppl_new_Polyhedron_from_constraints(c, [A >= 1, B >= 0], Q),
  ppl_Polyhedron_add_constraints_and_minimize(Q, [A >= 1, B >= 0]),
  ppl_Polyhedron_limited_BHRZ03_widening_assign(P, Q, [A >= 2, B >= 1]),
  ppl_new_Polyhedron_from_constraints(c,
                                      [A >= 2, B >= 1],
                                      Pa),
  ppl_Polyhedron_equals_Polyhedron(P, Pa),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q),
  ppl_delete_Polyhedron(Pa).

% Tests ppl_Polyhedron_BHRZ03_widening_assign
% (using NNC Polyhedra).
bhrz03_widen_NNC :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_dimension(nnc, 2, P),
  ppl_new_Polyhedron_from_constraints(nnc, [A >= 1, B >= 0], Q),
  ppl_Polyhedron_BHRZ03_widening_assign(P, Q),
  ppl_new_Polyhedron_from_dimension(nnc, 2, Pa),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [A >= 1, B >= 0],
                                      Qa),
  ppl_Polyhedron_equals_Polyhedron(P, Pa),
  ppl_Polyhedron_equals_Polyhedron(Q, Qa),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q),
  ppl_delete_Polyhedron(Pa),
  ppl_delete_Polyhedron(Qa).

% Tests ppl_Polyhedron_limited_BHRZ03_widening_assign
% (using NNC Polyhedra).
lim_bhrz03_widen_NNC :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_dimension(nnc, 2, P),
  ppl_new_Polyhedron_from_constraints(nnc, [A >= 1, B >= 0], Q),
  ppl_Polyhedron_add_constraints_and_minimize(Q, [A >= 1, B >= 0]),
  ppl_Polyhedron_limited_BHRZ03_widening_assign(P, Q,
                                             [A >= 2, B >= 1]),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [A >= 2, B >= 1],
                                      Pa),
  ppl_Polyhedron_equals_Polyhedron(P, Pa),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q),
  ppl_delete_Polyhedron(Pa).

% Tests ppl_Polyhedron_H79_widening_assign
% (using C Polyhedra).
h79_widen_C :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_dimension(c, 2, P),
  ppl_new_Polyhedron_from_constraints(c, [A >= 1, B >= 0], Q),
  ppl_Polyhedron_H79_widening_assign(P, Q),
  ppl_new_Polyhedron_from_dimension(c, 2, Pa),
  ppl_new_Polyhedron_from_constraints(c,
                                      [A >= 1, B >= 0],
                                      Qa),
  ppl_Polyhedron_equals_Polyhedron(P, Pa),
  ppl_Polyhedron_equals_Polyhedron(Q, Qa),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q),
  ppl_delete_Polyhedron(Pa),
  ppl_delete_Polyhedron(Qa).

% Tests ppl_Polyhedron_limited_H79_widening_assign
% (using C Polyhedra).
lim_h79_widen_C :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_dimension(c, 2, P),
  ppl_new_Polyhedron_from_constraints(c, [A >= 1, B >= 0], Q),
  ppl_Polyhedron_add_constraints_and_minimize(Q, [A >= 1, B >= 0]),
  ppl_Polyhedron_limited_H79_widening_assign(P, Q, [A >= 2, B >= 1]),
  ppl_new_Polyhedron_from_constraints(c,
                                      [A >= 2, B >= 1],
                                      Pa),
  ppl_Polyhedron_equals_Polyhedron(P, Pa),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q),
  ppl_delete_Polyhedron(Pa).

% Tests ppl_Polyhedron_H79_widening_assign
% (using NNC Polyhedra).
h79_widen_NNC :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_dimension(nnc, 2, P),
  ppl_new_Polyhedron_from_constraints(nnc, [A >= 1, B >= 0], Q),
  ppl_Polyhedron_H79_widening_assign(P, Q),
  ppl_new_Polyhedron_from_dimension(nnc, 2, Pa),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [A >= 1, B >= 0],
                                      Qa),
  ppl_Polyhedron_equals_Polyhedron(P, Pa),
  ppl_Polyhedron_equals_Polyhedron(Q, Qa),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q),
  ppl_delete_Polyhedron(Pa),
  ppl_delete_Polyhedron(Qa).

% Tests ppl_Polyhedron_limited_H79_widening_assign
% (using NNC Polyhedra).
lim_h79_widen_NNC :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_dimension(nnc, 2, P),
  ppl_new_Polyhedron_from_constraints(nnc, [A >= 1, B >= 0], Q),
  ppl_Polyhedron_add_constraints_and_minimize(Q, [A >= 1, B >= 0]),
  ppl_Polyhedron_limited_H79_widening_assign(P, Q,
                                             [A >= 2, B >= 1]),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [A >= 2, B >= 1],
                                      Pa),
  ppl_Polyhedron_equals_Polyhedron(P, Pa),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q),
  ppl_delete_Polyhedron(Pa).

% Tests ppl_Polyhedron_topological_closure_assign
% (using NNC Polyhedra).
top_close_assign :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [3 > A, 4*A + B - 2*C >= 5],
                                      P),
  ppl_Polyhedron_topological_closure_assign(P),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [4*A + B + -2*C >= 5, A =< 3],
                                      Pa),
  ppl_Polyhedron_equals_Polyhedron(P, Pa),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Pa).


% Tests ppl_Polyhedron_get_constraints.
get_cons :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_dimension(c, 2, P),
  ppl_Polyhedron_get_constraints(P, []),
  ppl_Polyhedron_add_constraint(P, A - B >= 1),
  ppl_Polyhedron_get_constraints(P, [C]),
  ppl_new_Polyhedron_from_constraints(c, [C], Q),
  ppl_Polyhedron_equals_Polyhedron(P, Q),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q).

% Tests ppl_Polyhedron_get_minimized_constraints.
get_min_cons :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_dimension(c, 2, P),
  ppl_Polyhedron_get_minimized_constraints(P, []),
  ppl_Polyhedron_add_constraints(P, [A - B >= 1, A - B >= 0]),
  ppl_Polyhedron_get_minimized_constraints(P, [C]),
  ppl_new_Polyhedron_from_constraints(c, [C], Q),
  ppl_Polyhedron_equals_Polyhedron(P, Q),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q).

% Tests ppl_Polyhedron_get_generators.
get_gens :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_empty_from_dimension(nnc, 2, P),
  ppl_Polyhedron_get_generators(P, []),
  ppl_Polyhedron_add_generator(P, point(A+B)),
  ppl_Polyhedron_get_generators(P, [G]),
  ppl_new_Polyhedron_from_generators(nnc, [G], Q),
  ppl_Polyhedron_equals_Polyhedron(P, Q),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q).

% Tests ppl_Polyhedron_get_minimized_generators.
get_min_gens :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_empty_from_dimension(nnc, 2, P),
  ppl_Polyhedron_add_generators(P, [point(2*A), point(A+B), point(2*B)]),
  ppl_Polyhedron_get_minimized_generators(P, [G1, G2]),
  ppl_new_Polyhedron_from_generators(nnc, [G1, G2], Q),
  ppl_Polyhedron_equals_Polyhedron(P, Q),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q).

% Tests ppl_Polyhedron_add_constraint.
add_con :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_dimension(c, 2, P),
  ppl_Polyhedron_add_constraint(P, A - B >= 1),
  ppl_new_Polyhedron_from_constraints(c,
                                      [A + -1*B >= 1],
                                      Pa),
  ppl_Polyhedron_equals_Polyhedron(P, Pa),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Pa).

% Tests ppl_Polyhedron_add_generator.
add_gen :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_dimension(c, 2, P),
  ppl_Polyhedron_add_generator(P, point(A + B)),
  ppl_new_Polyhedron_from_generators(c,
                                     [point(A + B), point(0),
                                      line(A), line(B)], P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_add_constraint_and_minimize.
add_con_min :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_dimension(c, 2, P),
  ppl_Polyhedron_add_constraint_and_minimize(P, A - B >= 1),
  ppl_new_Polyhedron_from_constraints(c,
                                      [A + -1*B >= 1],
                                      P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_add_generator_and_minimize.
add_gen_min :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_dimension(c, 2, P),
  ppl_Polyhedron_add_generator_and_minimize(P, point(A + B, 1)),
  ppl_new_Polyhedron_from_generators(c,
                                     [point(A + B), point(0),
                                      line(A), line(B)], P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_add_constraints.
add_cons :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2),
  ppl_new_Polyhedron_from_dimension(c, 3, P),
  ppl_Polyhedron_add_constraints(P, [A >= 1, B >= 0,
                                     4*A + B - 2*C >= 5]),
  ppl_new_Polyhedron_from_constraints(c,
                                      [4*A + B + -2*C >= 5,
                                       A >= 1, B >= 0],
                                      P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_add_generators.
add_gens :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2),
  ppl_new_Polyhedron_empty_from_dimension(c, 3, P),
  ppl_Polyhedron_add_generators(P,
                                [point(A + B + C),
                                 ray(A), ray(2*A),
                                 point(A + B + C, 1),
                                 point(-100*A - 5*B, 8)]),
  ppl_new_Polyhedron_from_generators(c,
                                     [point(A + B + C), ray(A),
                                      point(-100*A + -5*B, 8)],
                                     P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_add_constraints_and_minimize.
add_cons_min :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_dimension(c, 2, P),
  ppl_Polyhedron_add_constraints_and_minimize(P, [A >= 1, B >= 0]),
  ppl_new_Polyhedron_from_constraints(c,
                                      [A >= 1, B >= 0],
                                      P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_add_generators_and_minimize.
add_gens_min :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2),
  ppl_new_Polyhedron_empty_from_dimension(c, 3, P),
  ppl_Polyhedron_add_generators_and_minimize(P,
                                             [point(A + B + C),
                                              ray(A), ray(2*A),
                                              point(A + B + C)]),
  ppl_new_Polyhedron_from_generators(c,
                                     [point(A + B + C), ray(A)],
                                     P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_add_dimensions_and_project.
project :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), D = '$VAR'(3),
  ppl_new_Polyhedron_from_dimension(nnc, 2, P),
  ppl_Polyhedron_add_constraints_and_minimize(P, [A >= 1, B >= 0]),
  ppl_Polyhedron_add_dimensions_and_project(P, 2),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [A >= 1, B >= 0, C = 0, D = 0],
                                      P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_add_dimensions_and_embed.
embed :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_dimension(nnc, 2, P),
  ppl_Polyhedron_add_constraints_and_minimize(P, [A >= 1, B >= 0]),
  ppl_Polyhedron_add_dimensions_and_embed(P, 2),
  ppl_new_Polyhedron_from_dimension(nnc, 4, P1),
  ppl_Polyhedron_add_constraints(P1, [A >= 1, B >= 0]),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_remove_dimensions.
remove_dim :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2),
  ppl_new_Polyhedron_from_dimension(nnc, 3, P),
  ppl_Polyhedron_add_constraints(P, [A > 1, B >= 0, C >= 0]),
  ppl_Polyhedron_remove_dimensions(P,[B]),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [A > 1, B >= 0],
                                      P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P1),
  % Note: now 'B' refers to the old 'C' variable.
  ppl_Polyhedron_remove_dimensions(P,[A, B]),
  ppl_Polyhedron_space_dimension(P, 0),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_remove_higher_dimensions.
remove_high_dim :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2),
  ppl_new_Polyhedron_from_dimension(nnc, 3, P),
  ppl_Polyhedron_add_constraints(P, [A > 1, B >= 0, C >= 0]),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [A > 1, B >= 0, C >= 0],
                                      P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_Polyhedron_remove_higher_dimensions(P, 1),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [A > 1],
                                      P2),
  ppl_Polyhedron_equals_Polyhedron(P, P2),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_affine_image.
affine :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_dimension(nnc, 2, P),
  ppl_Polyhedron_add_constraint(P, A - B = 1),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [A + -1*B = 1],
                                      P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_Polyhedron_affine_image(P, A, A + 1, 1),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [A + -1*B = 2],
                                      P2),
  ppl_Polyhedron_equals_Polyhedron(P, P2),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_affine_preimage.
affine_pre :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_dimension(nnc, 2, P),
  ppl_Polyhedron_add_constraint(P, A + B >= 10),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [A + B >= 10],
                                      P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_Polyhedron_affine_preimage(P, A, A + 1, 1),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [A + B >= 9],
                                      P2),
  ppl_Polyhedron_equals_Polyhedron(P, P2),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
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
                                             [point(A + B + C),ray(A)]),
  ppl_Polyhedron_relation_with_generator(P, point(A), R),
  R = [],
  ppl_Polyhedron_relation_with_generator(P, ray(A), R1),
  R1 = [subsumes],
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
                                             [point(A + B + C)]),
  ppl_Polyhedron_is_bounded(P1),
  ppl_Polyhedron_add_constraints(P, [A > 1, B =< 3, A =< 2]),
  \+ ppl_Polyhedron_is_topologically_closed(P),
  ppl_Polyhedron_add_constraints(P, [A > 2]),
  ppl_Polyhedron_is_topologically_closed(P),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_contains_Polyhedron for C Polyhedron.
contains_C :-
  ppl_new_Polyhedron_from_dimension(c, 3, P1),
  ppl_new_Polyhedron_from_dimension(c, 3, P2),
  ppl_Polyhedron_contains_Polyhedron(P1, P2),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

% Tests ppl_Polyhedron_contains_Polyhedron for NNC Polyhedron.
contains_NNC :-
  ppl_new_Polyhedron_from_dimension(nnc, 3, P1),
  ppl_new_Polyhedron_from_dimension(nnc, 3, P2),
  ppl_Polyhedron_contains_Polyhedron(P1, P2),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

% Tests ppl_Polyhedron_strictly_contains_Polyhedron for C Polyhedron.
strict_contains_C :-
  ppl_new_Polyhedron_from_dimension(c, 3, P1),
  ppl_Polyhedron_check_universe(P1),
  ppl_new_Polyhedron_empty_from_dimension(c, 3, P2),
  ppl_Polyhedron_check_empty(P2),
  ppl_Polyhedron_strictly_contains_Polyhedron(P1, P2),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

% Tests ppl_Polyhedron_strictly_contains_Polyhedron for NNC Polyhedron.
strict_contains_NNC :-
  ppl_new_Polyhedron_from_dimension(nnc, 3, P1),
  ppl_new_Polyhedron_empty_from_dimension(nnc, 3, P2),
  ppl_Polyhedron_strictly_contains_Polyhedron(P1, P2),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

% Tests ppl_Polyhedron_is_disjoint_from_Polyhedron for C Polyhedron.
is_disjoint_from_C :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2),
  ppl_new_Polyhedron_from_constraints(c,
                                      [3 >= A, 4*A + B - 2*C >= 5],
                                      P1),
  ppl_new_Polyhedron_from_constraints(c,
                                      [4 =< A, 4*A + B - 2*C >= 5],
                                      P2),
  ppl_Polyhedron_is_disjoint_from_Polyhedron(P1, P2),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

% Tests ppl_Polyhedron_is_disjoint_from_Polyhedron for NNC Polyhedron.
is_disjoint_from_NNC :-
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [3 >= A, 4*A + B - 2*C >= 5],
                                      P1),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [3 < A, 4*A + B - 2*C >= 5],
                                      P2),
  ppl_Polyhedron_is_disjoint_from_Polyhedron(P1, P2),
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

% Tests ppl_Polyhedron_get_bounding_box for C Polyhedron.
get_boundingbox_C :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_dimension(c, 2, P),
  ppl_Polyhedron_add_constraints_and_minimize(P, [B >= 0, 4*A =< 2]),
  ppl_Polyhedron_get_bounding_box(P, any, Box),
  Box = [i(o(minf), c(1/2)), i(c(0), o(pinf))],
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_get_bounding_box for NNC Polyhedron.
get_boundingbox_NNC :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_dimension(nnc, 2, P),
  ppl_Polyhedron_add_constraints_and_minimize(P, [B >= 0, 4*A < 2]),
  ppl_Polyhedron_get_bounding_box(P, any, Box),
  Box = [i(o(minf), o(1/2)), i(c(0), o(pinf))],
  ppl_delete_Polyhedron(P).

get_bounding_box_complexity_C:-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_constraints(nnc, [4*A =< 2, B >= 0], P),
  ppl_Polyhedron_get_bounding_box(P, any, Box),
  ppl_Polyhedron_get_bounding_box(P, polynomial, Box),
  ppl_Polyhedron_get_bounding_box(P, simplex, Box),
  Box = [i(o(minf), c(1/2)), i(c(0), o(pinf))],
  ppl_delete_Polyhedron(P).

get_bounding_box_complexity_NNC:-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_constraints(nnc, [4*A =< 2, B > 0], P),
  ppl_Polyhedron_get_bounding_box(P, any, Box),
  ppl_Polyhedron_get_bounding_box(P, polynomial, Box),
  ppl_Polyhedron_get_bounding_box(P, simplex, Box),
  Box = [i(o(minf), c(1/2)), i(o(0), o(pinf))],
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_bounds_from_above for an NNC polyhedron.
bounds_from_above :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [A > 1, B > 0,
                                       B < 1],
                                     P),
  \+ ppl_Polyhedron_bounds_from_above(P, A),
  ppl_Polyhedron_add_constraint(P, A < 2),
  ppl_Polyhedron_bounds_from_above(P, A),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_bounds_from_below for an NNC polyhedron.
bounds_from_below :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [B > 0,
                                       B < 1],
                                     P),
  \+ ppl_Polyhedron_bounds_from_below(P, A),
  ppl_Polyhedron_add_constraint(P, A > 1),
  ppl_Polyhedron_bounds_from_below(P, A),
  ppl_delete_Polyhedron(P).

% Tests Watchdog predicates
% ppl_set_timeout
% ppl_set_timeout_exception_atom
% ppl_timeout_exception_atom
% ppl_reset_timeout
%

time_out :- 
  time_out(c), time_out(nnc).

time_out(T) :-
  ppl_initialize,
  A = '$VAR'(0), B = '$VAR'(1), C = '$VAR'(2), D = '$VAR'(3), E = '$VAR'(4), F = '$VAR'(5),
  CS = [8*A - 7*B + 4*D - E - 8*F >= -3,
        6*A + 8*B + 4*C - 6*D + 6*E + 6*F >= 5,
        6*A + 7*B - 6*C + 3*D + 3*E + 5*F >= 4,
        6*A + C + 8*D - 2*E - 3*F >= -6,
        4*A - 3*B + 3*D - 3*E + 4*F >= 0,
        3*A - 3*B - 7*C - 4*D - 7*E + 8*F >= 8,
        -2*A + 5*B + C + 2*D - 2*E + 6*F >= -7,
        -4*A + 7*B - 7*C + 2*D - 2*E - 7*F >= 1,
        -5*A + 7*B + 5*C + 6*D - 5*E - 2*F >= -7,
        -5*A + 6*B - 6*C - 2*D + 4*E - 2*F >= -5,
        -5*A + 5*B + 8*C + D + E - 6*F >= -6],
  ppl_new_Polyhedron_from_dimension(T, 6, Q),
  ppl_set_timeout_exception_atom(pl_time_out),
  ppl_timeout_exception_atom(pl_time_out),
  ppl_set_timeout(1),
  ppl_new_Polyhedron_from_dimension(T, 6, P),
  time_watch(T, ppl_Polyhedron_add_constraints_and_minimize(P, CS),
             (ppl_reset_timeout,
              ppl_Polyhedron_add_constraints_and_minimize(Q, CS)),
              true),
  ppl_Polyhedron_equals_Polyhedron(P, Q),
  ppl_reset_timeout,
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q),
  ppl_new_Polyhedron_from_dimension(T, 6, Q1),
  ppl_set_timeout(100),
  ppl_new_Polyhedron_from_dimension(T, 6, P1),
  time_watch(T, ppl_Polyhedron_add_constraints_and_minimize(P1, CS),
             (ppl_reset_timeout,
              ppl_Polyhedron_add_constraints_and_minimize(Q1, CS)),
              true),
  ppl_Polyhedron_equals_Polyhedron(P1, Q1),
  ppl_set_timeout_exception_atom(time_out),
  ppl_reset_timeout,
  ppl_finalize.

time_watch(T, Goal, NoTimeOut, TimeOut) :-
   !,
   Goal =.. [PPLFunct, Poly|Args],
   ppl_new_Polyhedron_from_Polyhedron(T, Poly, T, PolyCopy),
   ppl_timeout_exception_atom(TimeOutAtom),
   GoalCopy =.. [PPLFunct, PolyCopy|Args],
     (catch(GoalCopy, TimeOutAtom, fail) ->
         (ppl_reset_timeout,
          call(NoTimeOut), Goal)
    ; 
      call(TimeOut)
   ),
   ppl_delete_Polyhedron(PolyCopy).

% These next 2 tests demonstrate a bug in the bounding box software.
boundingbox1(Box,CS) :-
  A = '$VAR'(0), B = '$VAR'(1),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [A > 1, B > 1,
                                       B < 1, A < 1],
                                      P),
  ppl_Polyhedron_get_bounding_box(P, any, Box),
  ppl_Polyhedron_get_constraints(P,CS),
  ppl_delete_Polyhedron(P).

boundingbox2(Box,CS) :-
  ppl_new_Polyhedron_from_dimension(nnc, 2, P),
  ppl_Polyhedron_add_constraints(P, [0=1]),
  ppl_Polyhedron_get_bounding_box(P, any, Box),
  ppl_Polyhedron_get_constraints(P,CS),
  ppl_delete_Polyhedron(P).
