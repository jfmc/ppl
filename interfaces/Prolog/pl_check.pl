/* Various tests on the Prolog interface.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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

:- dynamic(noisy/0).

% noisy
% When noisy is defined, a message is displayed if a time out 
% occurs when running the `timeout' predicate.
% Also, the values of the PPL versions and banner are displayed.
% When noisy is not defined, no 'time out' message or versions are displayed.

%noisy.

% check_all
% This executes all the test predicates which, together, check all
% the ppl interface predicates.

check_all :-
   run_all.

run_all:-
   ppl_initialize,
   (all_versions_and_banner -> true ;
        error_message(['error in a versions or a banner predicate'])),
   (max_dim -> true ;
        error_message(['error in the maximum dimension predicate'])),
   (new_polys -> true ;
        error_message(['error in a new poyhedron predicate'])),
   (swap_polys -> true ;
        error_message(['error in swap predicate'])),
   (space_dim -> true ;
        error_message(['error in space dimension predicate'])),
   (basic_operators -> true ;
        error_message(['error in a basic operator predicate'])),
   (transform_polys -> true ;
        error_message(['error in a transformation predicate'])),
   (extrapolation_operators -> true ;
        error_message(['error in a widening/extrapolation predicate'])),
   (get_system -> true ;
        error_message(['error in a get system predicate'])),
   (add_to_system -> true ;
        error_message(['error in an add to system predicate'])),
   (revamp_dim -> true ;
        error_message(['error in a remove/rename dimension predicate'])),
   (check_polys -> true ;
        error_message(['error in a check predicate'])),
   (minmax_polys -> true ;
        error_message(['error in a minimize or maximize predicate'])),
   (compare_polys -> true ;
        error_message(['error in a polyhedra comparison predicate'])),
   (poly_boxes -> true ;
        error_message(['error in a get bounding box predicate'])),
   (catch_time -> true ;
        error_message(['error in a time out predicate'])),
   !,
   ppl_finalize.

run_all:-
   ppl_finalize,
   fail.

new_polys :-
  ppl_initialize,
  new_universe,
  new_empty,
  copy,
  new_poly_from_cons,
  new_poly_from_gens,
  new_poly_from_bounding_box,
  !,
  ppl_finalize.

swap_polys :-
  ppl_initialize,
  swap,
  !,
  ppl_finalize.

space_dim :-
   ppl_initialize,
   space,
   !,
   ppl_finalize.

basic_operators :-
   ppl_initialize,
   inters_assign,
   inters_assign_min,
   polyhull_assign,
   polyhull_assign_min,
   polydiff_assign,
   time_elapse,
   top_close_assign,
   !,
   ppl_finalize.

transform_polys :-
   ppl_initialize,
   affine,
   affine_pre,
   affine_gen,
   affine_genlr,
   !,
   ppl_finalize.

extrapolation_operators :-
   ppl_initialize,
   widen_BHRZ03_C,
   widen_BHRZ03_NNC,
   widen_BHRZ03_with_token_C,
   lim_extrapolate_BHRZ03_C,
   lim_extrapolate_BHRZ03_NNC,
   lim_extrapolate_BHRZ03_with_token_C,
   bound_extrapolate_BHRZ03_C,
   bound_extrapolate_BHRZ03_with_token_C,
   widen_H79_C,
   widen_H79_NNC,
   widen_H79_with_token_C,
   lim_extrapolate_H79_C,
   lim_extrapolate_H79_NNC,
   lim_extrapolate_H79_with_token_C,
   bound_extrapolate_H79_C,
   bound_extrapolate_H79_NNC,
   bound_extrapolate_H79_with_token_C,
   bound_extrapolate_H79_with_token_NNC,
   !,
   ppl_finalize.

get_system :-
   ppl_initialize,
   get_cons,
   get_min_cons,
   get_gens,
   get_min_gens,
   !,
   ppl_finalize.

add_to_system :-
   ppl_initialize,
   add_con,
   add_con_min,
   add_gen,
   add_gen_min,
   add_cons,
   add_cons_min,
   add_gens,
   add_gens_min,
   !,
   ppl_finalize.

revamp_dim :-
   ppl_initialize,
   project,
   embed,
   conc_assign,
   remove_dim,
   remove_high_dim,
   map_dim,
   ppl_finalize.

check_polys :-
   ppl_initialize,
   rel_cons,
   rel_gens,
   checks,
   bounds_from_above,
   bounds_from_below,
   !,
   ppl_finalize.

minmax_polys :-
   ppl_initialize,
   maximize,
   minimize,
   maximize_with_point,
   minimize_with_point,
   !,
   ppl_finalize.

compare_polys :-
   ppl_initialize,
   contains,
   strict_contains,
   disjoint_from,
   equals,
   ok,
   !,
   ppl_finalize.

poly_boxes :-
   ppl_initialize,
   get_bounding_box,
   !,
   ppl_finalize.

catch_time :-
  ppl_initialize,
  time_out,
  !,
  ppl_finalize.
  

% Tests predicates that return the versions nad the PPL banner.
% If noisy is not defined, there is no output but if is is,
% all the versions are printed and the banner is pretty printed.
all_versions_and_banner :-
  ppl_initialize,
  ppl_version_major(Vmajor),
  ppl_version_minor(Vminor),
  ppl_version_revision(Vrevision),
  ppl_version_beta(Vbeta),
  ppl_version(V),
  ppl_banner(B),
  (noisy ->
     (
      write('Version major is '), write(Vmajor), nl,
      write('Version minor is '), write(Vminor), nl,
      write('Version revision is '), write(Vrevision), nl,
      write('Version beta is '), write(Vbeta), nl,
      write('Version is '), write(V), nl,
      banner_pp(B), nl
     )
  ; true
  ),
  !,
  ppl_finalize.

banner_pp(B) :-
  name(B,Bcodes),
  nl,
  !,
  format_banner(Bcodes).

format_banner([]).
format_banner([_]).
format_banner(["/n"|Chars]):-
  nl,
  format_banner(Chars), !.
format_banner([C|Chars]):- 
  put_code(C),
  format_banner(Chars).

% Tests predicates that return the maximum allowed dimension.
% If noisy does not hold, there is no output but if not,
% the maximum is printed.
max_dim :-
  ppl_initialize,
  ppl_max_space_dimension(M),
  (noisy -> 
     (
      write('Maximum possible dimension is '), write(M), nl
     )
  ; true
  ),
  !,
  ppl_finalize.

% Tests new_Polyhedron_from_dimension
% and ppl_delete_Polyhedron for C and NNC Polyhedron.
new_universe :-
  make_vars(1,[A]),
  new_universe(c, A >= 0), new_universe(nnc, A > 0).

new_universe(T, Con) :-
  \+ ppl_new_Polyhedron_from_dimension(T, 3, 0),
  ppl_new_Polyhedron_from_dimension(T, 3, P),
  ppl_Polyhedron_is_universe(P),
  ppl_Polyhedron_add_constraint(P, Con),
  \+ ppl_Polyhedron_is_universe(P),
  ppl_delete_Polyhedron(P).

% Tests new_Polyhedron_empty_from_dimension for C and NNC Polyhedron.
new_empty :-
  new_empty(c), new_empty(nnc).

new_empty(T) :-
  \+  ppl_new_Polyhedron_empty_from_dimension(T, 3, 0),
  ppl_new_Polyhedron_empty_from_dimension(T, 3, P),
  ppl_Polyhedron_is_empty(P),
  ppl_Polyhedron_add_generator(P,point(0)),
  \+ ppl_Polyhedron_is_empty(P),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_from_Polyhedron for C and NNC polyhedra.
copy :-
  copy(c, c), copy(nnc, nnc), copy(c, nnc), copy(nnc, c).

% Tests ppl_Polyhedron_from_Polyhedron when one  Polyhedron
% is C and the other is NNC.
% This also uses ppl_new_Polyhedron_from_constraints.
copy(T1, T2) :-
  ppl_new_Polyhedron_from_dimension(T1, 3, P1),
  \+ ppl_new_Polyhedron_from_Polyhedron(T1, P1, T2, 0),
  ppl_new_Polyhedron_from_Polyhedron(T1, P1, T2, P2),
  ppl_new_Polyhedron_from_Polyhedron(T2, P2, T1, P1a),
  ppl_Polyhedron_equals_Polyhedron(P1, P1a),
  ppl_new_Polyhedron_from_Polyhedron(T1, P1a, T2, P2a),
  ppl_Polyhedron_equals_Polyhedron(P2, P2a),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P1a),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P2a),
  make_vars(3, [A, B, C]),
  (T1 = c
          -> CS = [3 >= A, 4 >= A, 4*A + B - 2*C >= 5]
          ;  CS = [3 >= A, 4 >  A, 4*A + B - 2*C >= 5]
  ),
  ppl_new_Polyhedron_from_constraints(T1, CS, P3),
  ppl_new_Polyhedron_from_Polyhedron(T1, P3, T2, P4),
  ppl_new_Polyhedron_from_Polyhedron(T2, P4, T1, P3a),
  ppl_new_Polyhedron_from_Polyhedron(T1, P3a, T2, P4a),
  ppl_Polyhedron_equals_Polyhedron(P3, P3a),
  ppl_Polyhedron_equals_Polyhedron(P4, P4a),
  ppl_delete_Polyhedron(P3),
  ppl_delete_Polyhedron(P4),
  ppl_delete_Polyhedron(P3a),
  ppl_delete_Polyhedron(P4a).

% Tests ppl_new_Polyhedron_from_constraints for C and NNC Polyhedra.
new_poly_from_cons :-
  make_vars(4, [A, B, C, D]),
  new_poly_from_cons(c, [3 >= A, 4*A + B - 2*C >= 5, D = 1]),
  new_poly_from_cons(nnc, [3 > A, 4*A + B - 2*C >= 5, D = 1]).

new_poly_from_cons(T, CS) :-
  ppl_new_Polyhedron_from_constraints(T, [], P),
  \+ ppl_new_Polyhedron_from_constraints(T, [], 0),
  ppl_Polyhedron_is_universe(P),
  ppl_new_Polyhedron_from_constraints(T, CS, Pa),
  \+ ppl_Polyhedron_is_universe(Pa),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Pa).

% Tests ppl_new_Polyhedron_from_generators for a C Polyhedron.
new_poly_from_gens :-
  make_vars(3, [A, B, C]),
  new_poly_from_gens(c,[point(A + B + C, 1), point(A + B + C)] ),
  new_poly_from_gens(nnc,  [point(A + B + C), closure_point(A + B + C)]).

new_poly_from_gens(T, GS) :-
  \+ ppl_new_Polyhedron_from_generators(T, [], 0),
  ppl_new_Polyhedron_from_generators(T, [], P),
  ppl_Polyhedron_is_empty(P),
  ppl_new_Polyhedron_from_generators(T, GS, Pa),
  \+ ppl_Polyhedron_is_empty(Pa),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Pa).

% Tests ppl_new_Polyhedron_from_bounding_box for a C polyhedron.
new_poly_from_bounding_box :-
  new_poly_from_bounding_box(c, [i(c(1/2), o(pinf)), i(o(minf), c(-1/2))]),
  new_poly_from_bounding_box(nnc,[i(o(0/2), o(pinf)), i(o(minf), o(1))]),
  Max = -4,
  new_poly_from_bounding_box(c, [i(c(Max), c(1)), i(c(-1), c(1))]),
  new_poly_from_bounding_box(nnc, [i(c(Max), c(1)), i(c(-1), c(1))]).

new_poly_from_bounding_box(T, Box) :-
  \+ ppl_new_Polyhedron_from_bounding_box(T, Box, 0),
  ppl_new_Polyhedron_from_bounding_box(T, Box, P),
  ppl_Polyhedron_get_bounding_box(P, any, Box1),
  ppl_new_Polyhedron_from_bounding_box(T, Box1, P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_swap for a C Polyhedron.
swap :-
  swap(c), swap(nnc).

swap(T) :-
  ppl_new_Polyhedron_from_dimension(T, 3, P),
  ppl_new_Polyhedron_empty_from_dimension(T, 2, Q),
  ppl_Polyhedron_swap(P, Q),
  ppl_Polyhedron_is_empty(P),
  ppl_Polyhedron_is_universe(Q),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q).

% Tests ppl_Polyhedron_space_dimension for C and NNC Polyhedra.
space :-
 space(c), space(nnc).

space(T) :-
  ppl_new_Polyhedron_from_dimension(T, 3, P),
  ppl_Polyhedron_space_dimension(P, N),
  N = 3,
\+  ppl_Polyhedron_space_dimension(P, 4),
  ppl_new_Polyhedron_from_generators(T, [], Q),
  ppl_Polyhedron_space_dimension(Q, M),
  M == 0,
  ppl_new_Polyhedron_from_constraints(T, [], Q1),
  ppl_Polyhedron_space_dimension(Q1, M1),
  M1 == 0,
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q),
  ppl_delete_Polyhedron(Q1).

% Tests ppl_Polyhedron_intersection_assign for C and NNC Polyhedra.
inters_assign :-
  inters_assign(c), inters_assign(nnc).

inters_assign(T) :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_from_generators(T,
                                     [point(0), point(B),
                                      point(A), point(A, 2)],
                                     P1),
  ppl_new_Polyhedron_from_generators(T,
                                     [point(0), point(A),
                                      point(A + B), point(A, 2)],
                                     P2),
  ppl_Polyhedron_intersection_assign(P1, P2),
  ppl_new_Polyhedron_from_generators(T,
                                     [point(A + B, 2),
                                      point(A), point(0)],
                                     P1a),
  ppl_new_Polyhedron_from_constraints(T,
                                      [A - B >= 0, B >= 0,
                                       A + B =< 1],
                                      P1b),
  ppl_Polyhedron_equals_Polyhedron(P1, P1a),
  ppl_Polyhedron_equals_Polyhedron(P1, P1b),
  ppl_new_Polyhedron_from_constraints(T, [A =< -1, B =< -1], P3),
  ppl_Polyhedron_intersection_assign(P1, P3),
  ppl_Polyhedron_is_empty(P1),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P3),
  ppl_delete_Polyhedron(P1a),
  ppl_delete_Polyhedron(P1b).

% Tests ppl_Polyhedron_intersection_assign_and_minimize for C and NNC Polyhedra
inters_assign_min :-
  inters_assign_min(c), inters_assign_min(nnc).

inters_assign_min(T) :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_from_generators(T,
                                     [point(0), point(B),
                                      point(A), point(A, 2)],
                                     P1),
  ppl_new_Polyhedron_from_generators(T,
                                     [point(0), point(A), point(A + B)],
                                     P2),
  ppl_Polyhedron_intersection_assign_and_minimize(P1, P2),
  ppl_new_Polyhedron_from_generators(T,
                                     [point(A + B, 2),
                                      point(A), point(0)],
                                     P1a),
  ppl_new_Polyhedron_from_constraints(T,
                                      [A - B >= 0, B >= 0,
                                       A + B =< 1],
                                      P1b),
  ppl_Polyhedron_equals_Polyhedron(P1, P1a),
  ppl_Polyhedron_equals_Polyhedron(P1, P1b),
  ppl_new_Polyhedron_from_constraints(T, [A =< -1, B =< -1], P3),
  \+ppl_Polyhedron_intersection_assign_and_minimize(P1, P3),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P3),
  ppl_delete_Polyhedron(P1a),
  ppl_delete_Polyhedron(P1b).

% Tests ppl_Polyhedron_concatenate_assign for C and NNC Polyhedra.
conc_assign :-
  conc_assign(c), conc_assign(nnc).

conc_assign(T) :-
  make_vars(5, [A, B, C, D, E]),
  ppl_new_Polyhedron_from_dimension(T, 2, P),
  ppl_new_Polyhedron_from_constraints(T, [A >= 1, B >= 0, C >= 0], Q),
  ppl_Polyhedron_concatenate_assign(P, Q),
  ppl_new_Polyhedron_from_constraints(T,
                                      [C >= 1, D >= 0, E >= 0],
                                      P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q).

% Tests ppl_Polyhedron_poly_hull_assign for C and NNC Polyhedra.
polyhull_assign :-
  polyhull_assign(c), polyhull_assign(nnc).

polyhull_assign(T) :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_from_generators(T,
                                     [point(0), point(B),
                                      point(A), point(A,2)],
                                     P1),
  ppl_new_Polyhedron_from_generators(T,
                                     [point(0), point(A),
                                      point(A + B), point(A, 2)],
                                     P2),
  ppl_Polyhedron_poly_hull_assign(P1, P2),
  ppl_new_Polyhedron_from_generators(T,
      [point(1*A+1*B), point(1*A, 2), point(1*A), point(1*B), point(0)], P1a),
  ppl_new_Polyhedron_from_constraints(T,
      [1*A>=0, 1*B>=0, -1*B>= -1, -1*A>= -1], P1b),
  ppl_Polyhedron_equals_Polyhedron(P1, P1a),
  ppl_Polyhedron_equals_Polyhedron(P1, P1b),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P1a),
  ppl_delete_Polyhedron(P1b).

% Tests ppl_Polyhedron_poly_hull_assign_and_minimize for C and NNC Polyhedra.
polyhull_assign_min :-
  polyhull_assign_min(c), polyhull_assign_min(nnc).

polyhull_assign_min(T) :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_empty_from_dimension(T, 2, P1),
  ppl_new_Polyhedron_empty_from_dimension(T, 2, P2),
  \+ ppl_Polyhedron_poly_hull_assign_and_minimize(P1, P2),
  ppl_Polyhedron_add_generators(P1, [point(0), point(B),
                                     point(A), point(A, 2)]),
  ppl_Polyhedron_add_generators(P2, [point(0), point(A),
                                     point(A + B), point(A, 2)]),
  ppl_Polyhedron_poly_hull_assign_and_minimize(P1, P2),
  ppl_new_Polyhedron_from_generators(T,
      [point(1*A+1*B), point(1*A, 2), point(1*A), point(1*B), point(0)], P1a),
  ppl_new_Polyhedron_from_constraints(T,
      [1*A>=0, 1*B>=0, -1*B>= -1, -1*A>= -1], P1b),
  ppl_Polyhedron_equals_Polyhedron(P1, P1a),
  ppl_Polyhedron_equals_Polyhedron(P1, P1b),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P1a),
  ppl_delete_Polyhedron(P1b).

% Tests ppl_Polyhedron_poly_difference_assign for C and NNC Polyhedra.
polydiff_assign :-
  make_vars(2, [A, B]),
  polydiff_assign(c, [A >= 0, B >= 0, A+ B =< 1],
            [point(A + B, 2), point(B), point(A), point(0)]),
  polydiff_assign(nnc,[A >= 0, A - B < 0, A + B =< 1],
            [closure_point(A+B, 2), point(B), closure_point(0)]).

polydiff_assign(T, CS,GS) :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_from_generators(T,
                                     [point(0), point(B),
                                      point(A), point(A,2)],
                                     P1),
  ppl_new_Polyhedron_from_generators(T,
                                     [point(0), point(A),
                                      point(A + B), point(A,2)],
                                     P2),
  ppl_Polyhedron_poly_difference_assign(P1, P2),
  ppl_new_Polyhedron_from_generators(T, GS,
                                     P1a),
  ppl_new_Polyhedron_from_constraints(T,
                                      CS,
                                      P1b),
  ppl_Polyhedron_equals_Polyhedron(P1, P1a),
  ppl_Polyhedron_equals_Polyhedron(P1, P1b),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P1a),
  ppl_delete_Polyhedron(P1b).

% Tests ppl_Polyhedron_time_elapse for C and NNC Polyhedra.
time_elapse :-
  time_elapse(c), time_elapse(nnc).

% Tests ppl_Polyhedron_time_elapse for C Polyhedra.
time_elapse(T) :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_from_dimension(T, 2, P),
  ppl_Polyhedron_add_constraints(P,
                          [A >= 1, A =< 3, B >= 1, B =< 3]),
  ppl_new_Polyhedron_from_dimension(T, 2, Q),
  ppl_Polyhedron_add_constraints(Q, [B = 5]),
  ppl_Polyhedron_time_elapse_assign(P, Q),
  ppl_new_Polyhedron_from_dimension(T, 2, Pa),
  ppl_Polyhedron_add_constraints(Pa, [B >= 1]),
  ppl_new_Polyhedron_from_constraints(T, [B = 5], Qa),
  ppl_Polyhedron_equals_Polyhedron(Q, Qa),
  ppl_Polyhedron_equals_Polyhedron(P, Pa),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q),
  ppl_delete_Polyhedron(Pa),
  ppl_delete_Polyhedron(Qa).

% Tests ppl_Polyhedron_topological_closure_assign
% (using NNC Polyhedra).
top_close_assign :-
  make_vars(3, [A, B, C]),
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

% Tests ppl_Polyhedron_affine_image
affine :-
  affine(c), affine(nnc).

affine(T) :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_from_dimension(T, 2, P),
  ppl_Polyhedron_add_constraint(P, A - B = 1),
  ppl_new_Polyhedron_from_constraints(T,
                                      [A - B = 1],
                                      P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_Polyhedron_affine_image(P, A, A + 1, 1),
  ppl_new_Polyhedron_from_constraints(T,
                                      [A - B = 2],
                                      P2),
  ppl_Polyhedron_equals_Polyhedron(P, P2),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_affine_preimage.
affine_pre :-
  affine_pre(c), affine_pre(nnc).

affine_pre(T) :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_from_dimension(T, 2, P),
  ppl_Polyhedron_add_constraint(P, A + B >= 10),
  ppl_new_Polyhedron_from_constraints(T,
                                      [A + B >= 10],
                                      P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_Polyhedron_affine_preimage(P, A, A + 1, 1),
  ppl_new_Polyhedron_from_constraints(T,
                                      [A + B >= 9],
                                      P2),
  ppl_Polyhedron_equals_Polyhedron(P, P2),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_generalized_affine_image
% (using NNC Polyhedra).
affine_gen :-
  affine_gen(c), affine_gen(nnc).

affine_gen(T) :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_from_dimension(T, 2, P),
  ppl_Polyhedron_add_constraint(P, A - B = 1),
  \+ ppl_Polyhedron_generalized_affine_image(P, A, x, A + 1, 1),
  ppl_Polyhedron_generalized_affine_image(P, A, =<, A + 1, 1),
  ppl_new_Polyhedron_from_constraints(T,
                                      [A - B =< 2],
                                      P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_generalized_affine_image_lhs_rhs
% (using NNC Polyhedra).
affine_genlr :-
  affine_genlr(c), affine_genlr(nnc).

affine_genlr(T) :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_from_dimension(T, 2, P),
  ppl_Polyhedron_add_constraint(P, A - B = 1),
  \+  ppl_Polyhedron_generalized_affine_image_lhs_rhs(P, B - 1, x, A + 1),
  ppl_Polyhedron_generalized_affine_image_lhs_rhs(P, B - 1, =<, A + 1),
  ppl_new_Polyhedron_from_constraints(T,
                                      [B - A =< 2],
                                      P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P).

widen_extrapolation_init(P, CS, Topology):-
  ppl_new_Polyhedron_from_dimension(Topology, 2, P),
  ppl_Polyhedron_add_constraints(P, CS).

widen_extrapolation_final(P,CS, Topology):-
  ppl_new_Polyhedron_from_dimension(Topology, 2, P1),
  ppl_Polyhedron_add_constraints(P1, CS),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_BHRZ03_widening_assign for C Polyhedra.
widen_BHRZ03_C :-
  Topology = c,
  make_vars(2, [A, B]),
  CS_P = [A >= 1, B >= 0],
  CS_Q = [A >= 1, B >= 1],
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  ppl_Polyhedron_BHRZ03_widening_assign(P, Q),
  widen_extrapolation_final(P, [A >= 1], Topology),
  widen_extrapolation_final(Q, CS_Q, Topology).

% Tests ppl_Polyhedron_BHRZ03_widening_assign for NNC Polyhedra.
widen_BHRZ03_NNC :-
  Topology = nnc,
  make_vars(2, [A, B]),
  CS_P = [A > 1, B > 0],
  CS_Q = [A > 1, B > 1],
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  ppl_Polyhedron_BHRZ03_widening_assign(P, Q),
  CS_Pa = [A > 1],
  widen_extrapolation_final(P, CS_Pa, Topology),
  widen_extrapolation_final(Q, CS_Q, Topology).

% Tests ppl_Polyhedron_BHRZ03_widening_assign_with_token for C Polyhedra.
widen_BHRZ03_with_token_C :-
  Topology = c,
  make_vars(2, [A, B]),
  CS_P = [A >= 1],
  CS_Q = [A >= 1, B >= 1],
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  \+  ppl_Polyhedron_BHRZ03_widening_assign_with_token(P, Q, 1),
  ppl_Polyhedron_BHRZ03_widening_assign_with_token(P, Q, Token),
  Token = 0,
  widen_extrapolation_final(P, [A >= 1], Topology),
  widen_extrapolation_final(Q, CS_Q, Topology),
  CS_P1 = [A >= 1, B>= 0],
  widen_extrapolation_init(P1, CS_P1, Topology),
  widen_extrapolation_init(Q1, CS_Q, Topology),
  \+  ppl_Polyhedron_BHRZ03_widening_assign_with_token(P1, Q1, 0),
  ppl_Polyhedron_BHRZ03_widening_assign_with_token(P1, Q1, Token1),
  Token1 = 1,
  widen_extrapolation_final(P1, [A >= 1, B>= 0], Topology),
  widen_extrapolation_final(Q1, CS_Q, Topology).

% Tests ppl_Polyhedron_limited_BHRZ03_extrapolation_assign for C Polyhedra.
lim_extrapolate_BHRZ03_C :-
  Topology = c,
  make_vars(2, [A, B]),
  CS_P = [A >= 1, B >= 0],
  CS_Q = [A >= 2, B >= 1],
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  ppl_Polyhedron_limited_BHRZ03_extrapolation_assign(P, Q, [A >= 1, B >= 0]),
  widen_extrapolation_final(P, [A >= 1, B >= 0], Topology),
  ppl_delete_Polyhedron(Q),
  widen_extrapolation_init(P1, CS_P, Topology),
  widen_extrapolation_init(Q1, CS_Q, Topology),
  ppl_Polyhedron_limited_BHRZ03_extrapolation_assign(P1, Q1, [A >= 2]),
  widen_extrapolation_final(P1, [], Topology),
  ppl_delete_Polyhedron(Q1).

% Tests ppl_Polyhedron_limited_BHRZ03_extrapolation_assign for NNC Polyhedra.
lim_extrapolate_BHRZ03_NNC :-
  Topology = nnc,
  make_vars(2, [A, B]),
  CS_P = [A >= 1, B >= 0],
  CS_Q = [A >= 2, B >= 1],
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  ppl_Polyhedron_limited_BHRZ03_extrapolation_assign(P, Q, [A >= 1]),
  widen_extrapolation_final(P, [A >= 1], Topology),
  ppl_delete_Polyhedron(Q),
  widen_extrapolation_init(P1, CS_P, Topology),
  widen_extrapolation_init(Q1, CS_Q, Topology),
  ppl_Polyhedron_limited_BHRZ03_extrapolation_assign(P1, Q1, [A >= 2]),
  widen_extrapolation_final(P1, [], Topology),
  ppl_delete_Polyhedron(Q1).

% Tests ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token
% for C Polyhedra.
lim_extrapolate_BHRZ03_with_token_C :-
  Topology = c,
  make_vars(2, [A, B]),
  CS_P = [A >= 1, B >= 0],
  CS_Q = [A >= 1, B >= 1],
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
\+  ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token(P, Q,
                                                   [A >= 1, B >= 0], 0),
  ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token(P, Q,
                                                   [A >= 1, B >= 0], T),
  T = 1,
  CS_Pa = [A >= 1, B >= 0],
  widen_extrapolation_final(P, CS_Pa, Topology),
  widen_extrapolation_final(Q, CS_Q, Topology).

% Tests ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign for C Polyhedra.
bound_extrapolate_BHRZ03_C :-
  Topology = c,
  make_vars(2, [A, B]),
  CS_P = [A >= 1, B >= 0],
  CS_Q = [A >= 2, B >= 1],
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign(P, Q, [A >= 1, B >= 0]),
  widen_extrapolation_final(P, [A >= 1, B >= 0], Topology),
  ppl_delete_Polyhedron(Q),
  widen_extrapolation_init(P1, CS_P, Topology),
  widen_extrapolation_init(Q1, CS_Q, Topology),
  ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign(P1, Q1, [A >= 2]),
  widen_extrapolation_final(P1, [A >= 1, B >= 0], Topology),
  ppl_delete_Polyhedron(Q1).

% Tests ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token
% for C Polyhedra.
bound_extrapolate_BHRZ03_with_token_C :-
  Topology = c,
  make_vars(2, [A, B]),
  CS_P = [A >= 1, B >= 0],
  CS_Q = [A >= 1, B >= 1],
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
\+  ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token(P, Q,
                                                       [A >= 1, B >= 0], 0),
  ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token(P, Q,
                                                       [A >= 1, B >= 0], T),
  T = 1,
  CS_Pa = [A >= 1, B >= 0],
  widen_extrapolation_final(P, CS_Pa, Topology),
  widen_extrapolation_final(Q, CS_Q, Topology).

% Tests ppl_Polyhedron_H79_widening_assign for C Polyhedra.
widen_H79_C :-
  Topology = c,
  make_vars(2, [A, B]),
  CS_P = [A >= 1, B >= 0],
  CS_Q = [A >= 1, B >= 1],
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  ppl_Polyhedron_H79_widening_assign(P, Q),
  CS_Pa = [A >= 1],
  widen_extrapolation_final(P, CS_Pa, Topology),
  widen_extrapolation_final(Q, CS_Q, Topology).

% Tests ppl_Polyhedron_H79_widening_assign for NNC Polyhedra.
widen_H79_NNC :-
  Topology = nnc,
  make_vars(2, [A, B]),
  CS_P = [A > 1, B > 0],
  CS_Q = [A > 1, B > 1],
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  ppl_Polyhedron_H79_widening_assign(P, Q),
  CS_Pa = [A > 1],
  widen_extrapolation_final(P, CS_Pa, Topology),
  widen_extrapolation_final(Q, CS_Q, Topology).

% Tests ppl_Polyhedron_H79_widening_assign for C Polyhedra.
widen_H79_with_token_C :-
  Topology = c,
  make_vars(2, [A, B]),
  CS_P = [A >= 1, B >= 0],
  CS_Q = [A >= 1, B >= 1],
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
\+  ppl_Polyhedron_H79_widening_assign_with_token(P, Q, 0),
  ppl_Polyhedron_H79_widening_assign_with_token(P, Q, T),
  T = 1,
  CS_Pa = [A >= 1, B >= 0],
  widen_extrapolation_final(P, CS_Pa, Topology),
  widen_extrapolation_final(Q, CS_Q, Topology).

% Tests ppl_Polyhedron_limited_H79_extrapolation_assign for C Polyhedra.
lim_extrapolate_H79_C :-
  Topology = c,
  make_vars(2, [A, B]),
  CS_P = [A >= 1, B >= 0],
  CS_Q = [A >= 2, B >= 1],
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  ppl_Polyhedron_limited_H79_extrapolation_assign(P, Q, [A >= 1, B >= 0]),
  CS_Pa = [A >= 1, B >= 0],
  widen_extrapolation_final(P, CS_Pa, Topology),
  ppl_delete_Polyhedron(Q).


% Tests ppl_Polyhedron_limited_H79_extrapolation_assign for NNC Polyhedra.
lim_extrapolate_H79_NNC :-
  Topology = nnc,
  make_vars(2, [A, B]),
  CS_P = [A >= 1, B >= 0],
  CS_Q = [A >= 2, B >= 1],
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  ppl_Polyhedron_limited_H79_extrapolation_assign(P, Q, [A >= 1, B >= 0]),
  CS_Pa = [A >= 1, B >= 0],
  widen_extrapolation_final(P, CS_Pa, Topology),
  ppl_delete_Polyhedron(Q).

% Tests ppl_Polyhedron_H79_widening_assign for C Polyhedra.
lim_extrapolate_H79_with_token_C :-
  Topology = c,
  make_vars(2, [A, B]),
  CS_P = [A >= 1, B >= 0],
  CS_Q = [A >= 1, B >= 1],
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
\+  ppl_Polyhedron_limited_H79_extrapolation_assign_with_token(P, Q,
                                                 [A >= 1, B >= 0], 0),
  ppl_Polyhedron_limited_H79_extrapolation_assign_with_token(P, Q,
                                                 [A >= 1, B >= 0], T),
  T = 1,
  CS_Pa = [A >= 1, B >= 0],
  widen_extrapolation_final(P, CS_Pa, Topology),
  widen_extrapolation_final(Q, CS_Q, Topology).

% Tests ppl_Polyhedron_bounded_H79_extrapolation_assign for C Polyhedra.
bound_extrapolate_H79_C :-
  Topology = c,
  make_vars(2, [A, B]),
  CS_P = [A >= 1, B >= 0],
  CS_Q = [A >= 2, B >= 1],
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  ppl_Polyhedron_bounded_H79_extrapolation_assign(P, Q, [A >= 1, B >= 0]),
  widen_extrapolation_final(P, [A >= 1, B >= 0], Topology),
  ppl_delete_Polyhedron(Q),
  widen_extrapolation_init(P1, CS_P, Topology),
  widen_extrapolation_init(Q1, CS_Q, Topology),
  ppl_Polyhedron_bounded_H79_extrapolation_assign(P1, Q1, [A >= 2]),
  widen_extrapolation_final(P1, [A >= 1, B >= 0], Topology),
  ppl_delete_Polyhedron(Q1).

% Tests ppl_Polyhedron_bounded_H79_extrapolation_assign for NNC Polyhedra.
bound_extrapolate_H79_NNC :-
  Topology = nnc,
  make_vars(2, [A, B]),
  CS_P = [A > 1, B > 0],
  CS_Q = [A > 2, B > 1],
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  ppl_Polyhedron_bounded_H79_extrapolation_assign(P, Q, [A >= 1]),
  CS_Pa = [A > 1, B > 0],
  widen_extrapolation_final(P, CS_Pa, Topology),
  ppl_delete_Polyhedron(Q).

% Tests ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token
% for C Polyhedra.
bound_extrapolate_H79_with_token_C :-
  Topology = c,
  make_vars(2, [A, B]),
  CS_P = [A >= 1, B >= 0],
  CS_Q = [A >= 1, B >= 1],
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
\+  ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token(P, Q,
                                                       [A >= 1, B >= 0], 0),
  ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token(P, Q,
                                                       [A >= 1, B >= 0], T),
  T = 1,
  CS_Pa = [A >= 1, B >= 0],
  widen_extrapolation_final(P, CS_Pa, Topology),
  widen_extrapolation_final(Q, CS_Q, Topology).

% Tests ppl_Polyhedron_H79_widening_assign for NNC Polyhedra.
bound_extrapolate_H79_with_token_NNC :-
  Topology = nnc,
  make_vars(2, [A, B]),
  CS_P = [A >= 1, B >= 0],
  CS_Q = [A >= 1, B >= 1],
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
\+  ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token(P, Q,
                                               [A >= 1, B >= 0], 0),
  ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token(P, Q,
                                                [A >= 1, B >= 0], T),
  T = 1,
  CS_Pa = [A >= 1, B >= 0],
  widen_extrapolation_final(P, CS_Pa, Topology),
  widen_extrapolation_final(Q, CS_Q, Topology).

% Tests ppl_Polyhedron_get_constraints
get_cons :-
  get_cons(c), get_cons(nnc).

get_cons(T) :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_from_dimension(T, 2, P),
  ppl_Polyhedron_get_constraints(P, []),
  ppl_Polyhedron_add_constraint(P, A - B >= 1),
  \+  ppl_Polyhedron_get_constraints(P, []),
  ppl_Polyhedron_get_constraints(P, [C]),
  ppl_new_Polyhedron_from_constraints(T, [C], Q),
  ppl_Polyhedron_equals_Polyhedron(P, Q),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q).

% Tests ppl_Polyhedron_get_minimized_constraints
get_min_cons :-
  get_min_cons(c), get_min_cons(nnc).

get_min_cons(T) :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_from_dimension(T, 2, P),
  ppl_Polyhedron_get_minimized_constraints(P, []),
  ppl_Polyhedron_add_constraints(P, [A - B >= 1, A - B >= 0]),
  ppl_Polyhedron_get_minimized_constraints(P, [C]),
  ppl_new_Polyhedron_from_constraints(T, [C], Q),
  ppl_Polyhedron_equals_Polyhedron(P, Q),
  ppl_Polyhedron_add_constraints(P, [A - B =< 0]),
  \+ppl_Polyhedron_get_minimized_constraints(P, [C]),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q).

% Tests ppl_Polyhedron_get_generators
% (using NNC Polyhedra).
get_gens :-
  get_gens(c), get_gens(nnc).

get_gens(T) :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_empty_from_dimension(T, 2, P),
  ppl_Polyhedron_get_generators(P, []),
  \+ ppl_Polyhedron_get_generators(P, [_]),
  ppl_Polyhedron_add_generator(P, point(A+B)),
  ppl_Polyhedron_get_generators(P, [G]),
  ppl_new_Polyhedron_from_generators(T, [G], Q),
  ppl_Polyhedron_equals_Polyhedron(P, Q),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q).

% Tests ppl_Polyhedron_get_minimized_generators
get_min_gens :-
  get_min_gens(c), get_min_gens(nnc).

get_min_gens(T) :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_empty_from_dimension(T, 2, P),
  ppl_Polyhedron_add_generators(P, [point(2*A), point(A+B), point(2*B)]),
  \+ ppl_Polyhedron_get_minimized_generators(P, [_]),
  ppl_Polyhedron_get_minimized_generators(P, [G1, G2]),
  ppl_new_Polyhedron_from_generators(T, [G1, G2], Q),
  ppl_Polyhedron_equals_Polyhedron(P, Q),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q).

% Tests ppl_Polyhedron_add_constraint
add_con :-
  add_con(c), add_con(nnc).

add_con(T) :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_from_dimension(T, 2, P),
  ppl_Polyhedron_add_constraint(P, A - B >= 1),
  ppl_new_Polyhedron_from_constraints(T,
                                      [A - B >= 1],
                                      Pa),
  ppl_Polyhedron_equals_Polyhedron(P, Pa),
  ppl_Polyhedron_add_constraint(P, A = 0),
  ppl_new_Polyhedron_from_constraints(T,
                                      [A = 0, B =< -1],
                                      Pb),
  ppl_Polyhedron_equals_Polyhedron(P, Pb),
  ppl_Polyhedron_add_constraint(P, A = 1),
  ppl_new_Polyhedron_empty_from_dimension(T, 2, Pc),
  ppl_Polyhedron_equals_Polyhedron(P, Pc),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Pa),
  ppl_delete_Polyhedron(Pb),
  ppl_delete_Polyhedron(Pc).

% Tests ppl_Polyhedron_add_constraint_and_minimize
add_con_min :-
  add_con_min(c), add_con_min(nnc).

add_con_min(T) :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_from_dimension(T, 2, P),
  ppl_Polyhedron_add_constraint_and_minimize(P, A - B >= 1),
  ppl_new_Polyhedron_from_constraints(T,
                                      [A - B >= 1],
                                      P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  \+ ppl_Polyhedron_add_constraint_and_minimize(P, A - B =< 0),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_add_generator
add_gen :-
  add_gen(c), add_gen(nnc).

add_gen(T) :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_from_dimension(T, 2, P),
  ppl_Polyhedron_add_generator(P, point(A + B)),
  ppl_new_Polyhedron_from_generators(T,
                                     [point(A + B), point(0),
                                      line(A), line(B)], P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_add_generator_and_minimize
add_gen_min :-
  add_gen_min(c), add_gen_min(nnc).

add_gen_min(T) :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_empty_from_dimension(T, 2, P),
  ppl_Polyhedron_add_generator(P, point(A + B)),
  ppl_Polyhedron_add_generator(P, point(0)),
  ppl_Polyhedron_add_generator_and_minimize(P, point(2*A + 2*B, 1)),
  ppl_Polyhedron_get_generators(P,[_G1,_G2]),
  ppl_new_Polyhedron_from_generators(T,
                                     [point(2*A + 2*B), point(0)],
                                     P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_add_constraints
% (using C Polyhedra).
add_cons :-
  add_cons(c), add_cons(nnc).

add_cons(T) :-
  make_vars(3, [A, B, C]),
  ppl_new_Polyhedron_from_dimension(T, 3, P),
  ppl_Polyhedron_add_constraints(P, [A >= 1, B >= 0,
                                     4*A + B - 2*C >= 5]),
  ppl_new_Polyhedron_from_constraints(T,
                                      [4*A + B + -2*C >= 5,
                                       A >= 1, B >= 0],
                                      P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_Polyhedron_add_constraints(P, [A =< 0]),
  ppl_Polyhedron_is_empty(P),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_add_constraints_and_minimize
% (using C Polyhedra).
add_cons_min :-
  add_cons_min(c), add_cons_min(nnc).

add_cons_min(T) :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_from_dimension(T, 2, P),
  ppl_Polyhedron_add_constraints_and_minimize(P, [A >= 1, B >= 0]),
  ppl_new_Polyhedron_from_constraints(T,
                                      [A >= 1, B >= 0],
                                      P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  \+ppl_Polyhedron_add_constraints_and_minimize(P, [A + B =< 0]),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_add_generators
add_gens :-
  add_gens(c), add_gens(nnc).

add_gens(T) :-
  make_vars(3, [A, B, C]),
  ppl_new_Polyhedron_empty_from_dimension(T, 3, P),
  ppl_Polyhedron_add_generators(P,
                                [point(A + B + C),
                                 ray(A), ray(2*A),
                                 point(A + B + C, 1),
                                 point(100*A + 5*B, -8)]),
  ppl_new_Polyhedron_from_generators(T,
                                     [point(A + B + C), ray(A),
                                      point(100*A + 5*B, -8)],
                                     P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_add_generators_and_minimize
add_gens_min :-
  add_gens_min(c), add_gens_min(nnc).

add_gens_min(T) :-
  make_vars(3, [A, B, C]),
  ppl_new_Polyhedron_empty_from_dimension(T, 3, P),
  \+  ppl_Polyhedron_add_generators_and_minimize(P, []),
  ppl_Polyhedron_add_generators_and_minimize(P,
                                             [point(A + B + C),
                                              ray(A), ray(2*A),
                                              point(A + B + C)]),
  ppl_new_Polyhedron_from_generators(T,
                                     [point(A + B + C), ray(A)],
                                     P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_add_dimensions_and_project
project :-
  project(c), project(nnc).

project(T) :-
  make_vars(4, [A, B, C, D]),
  ppl_new_Polyhedron_from_dimension(T, 2, P),
  ppl_Polyhedron_add_constraints(P, [A >= 1, B >= 0]),
  ppl_Polyhedron_add_dimensions_and_project(P, 2),
  ppl_new_Polyhedron_from_constraints(T,
                                      [A >= 1, B >= 0, C = 0, D = 0],
                                      P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_add_dimensions_and_embed
% (using NNC Polyhedra).
embed :-
  embed(c), embed(nnc).

embed(T) :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_from_dimension(T, 2, P),
  ppl_Polyhedron_add_constraints(P, [A >= 1, B >= 0]),
  ppl_Polyhedron_add_dimensions_and_embed(P, 2),
  ppl_new_Polyhedron_from_dimension(T, 4, P1),
  ppl_Polyhedron_add_constraints(P1, [A >= 1, B >= 0]),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_remove_dimensions
remove_dim :-
  remove_dim(c), remove_dim(nnc).

remove_dim(T) :-
  make_vars(3, [A, B, C]),
  ppl_new_Polyhedron_from_dimension(T, 3, P),
  ppl_Polyhedron_add_constraints(P, [A >= 1, B >= 0, C >= 2]),
  ppl_Polyhedron_remove_dimensions(P,[B]),
  ppl_new_Polyhedron_from_constraints(T,
                                      [A >= 1, B >= 2],
                                      P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P1),
  % Note: now 'B' refers to the old 'C' variable.
  ppl_Polyhedron_remove_dimensions(P,[A, B]),
  ppl_Polyhedron_space_dimension(P, 0),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_remove_higher_dimensions
remove_high_dim :-
  remove_high_dim(c), remove_high_dim(nnc).

remove_high_dim(T) :-
  make_vars(3, [A, B, C]),
  ppl_new_Polyhedron_from_dimension(T, 3, P),
  ppl_Polyhedron_add_constraints(P, [A >= 1, B >= 0, C >= 0]),
  ppl_new_Polyhedron_from_constraints(T,
                                      [A >= 1, B >= 0, C >= 0],
                                      P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_Polyhedron_remove_higher_dimensions(P, 1),
  ppl_new_Polyhedron_from_constraints(T,
                                      [A >= 1],
                                      P2),
  ppl_Polyhedron_equals_Polyhedron(P, P2),
  ppl_Polyhedron_remove_higher_dimensions(P, 1),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_map_dimensions using constraints and generators
map_dim:-
  map_dim(c), map_dim(nnc).

map_dim(T) :-
  make_vars(3, [A, B, C]),
  ppl_new_Polyhedron_from_dimension(T, 3, P),
  ppl_Polyhedron_add_constraints(P, [A >= 2, B >= 1, C >= 0]),
  ppl_Polyhedron_map_dimensions(P, [A-B, B-C, C-A]),
  ppl_new_Polyhedron_from_dimension(T, 3, Q),
  ppl_Polyhedron_add_constraints(Q, [A >= 0, B >= 2, C >= 1]),
  ppl_Polyhedron_equals_Polyhedron(P, Q),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q),
  ppl_new_Polyhedron_empty_from_dimension(T, 4, P1),
  ppl_Polyhedron_add_generators(P1, [point(2*C), line(A+B), ray(A+C)]),
  ppl_Polyhedron_map_dimensions(P1, [A-C, C-A, B-B]),
  ppl_new_Polyhedron_empty_from_dimension(T, 3, Q1),
  ppl_Polyhedron_add_generators(Q1, [point(2*A), ray(A+C), line(B+C)]),
  ppl_Polyhedron_equals_Polyhedron(P1, Q1),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(Q1).

% Tests ppl_Polyhedron_relation_with_constraint.
rel_cons :-
  make_vars(3, [A, B, C]),
  ppl_new_Polyhedron_from_dimension(c, 3, P),
  ppl_Polyhedron_add_constraints(P, [A >= 1, B >= 0, C = 0]),
  \+ ppl_Polyhedron_relation_with_constraint(P, A = 0, x),
  ppl_Polyhedron_relation_with_constraint(P, A = 0, R),
  R = [is_disjoint],
  ppl_Polyhedron_relation_with_constraint(P, A = 1, R1),
  R1 = [strictly_intersects],
  ppl_Polyhedron_relation_with_constraint(P, A >= 0, R2),
  R2 = [is_included],
  ppl_new_Polyhedron_from_dimension(c, 3, P1),
  ppl_Polyhedron_add_constraints(P1, [A >= 1, B >= 0, C = 0]),
  ppl_Polyhedron_relation_with_constraint(P1, C >= 0, R3),
%  (R3 = [is_included, saturates] ; R3 = [saturates, is_included]),
 (R3 = [is_included, saturates] ; R3 = [saturates, is_included] ; R3 = [is_included, is_included]),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_relation_with_generator.
rel_gens :-
  make_vars(3, [A, B, C]),
  ppl_new_Polyhedron_empty_from_dimension(nnc, 3, P),
  ppl_Polyhedron_add_generators(P,
                                             [point(A + B + C),ray(A)]),
  \+ppl_Polyhedron_relation_with_generator(P, point(A), x),
  ppl_Polyhedron_relation_with_generator(P, point(A), R),
  R = [],
  ppl_Polyhedron_relation_with_generator(P, ray(A), R1),
  R1 = [subsumes],
  ppl_delete_Polyhedron(P).

%    checks_C/0 and checks_NNC/0
%  test ppl_Polyhedron_is_universe,
%       ppl_Polyhedron_is_empty,
%       ppl_Polyhedron_is_bounded,
%       ppl_Polyhedron_is_topologically_closed
checks :-
  checks(c), checks(nnc).

checks(T) :-
  make_vars(3, [A, B, C]),
  ppl_new_Polyhedron_from_dimension(T, 3, P),
  ppl_new_Polyhedron_empty_from_dimension(T, 3, P1),
  ppl_Polyhedron_is_universe(P),
  ppl_Polyhedron_is_empty(P1),
  \+ppl_Polyhedron_is_universe(P1),
  \+ppl_Polyhedron_is_empty(P),
  ppl_Polyhedron_add_generators(P1, [point(A + B + C)]),
  ppl_Polyhedron_is_bounded(P1),
  ppl_Polyhedron_add_generators(P1, [ray(A + B + C)]),
  \+ ppl_Polyhedron_is_bounded(P1),
  ppl_Polyhedron_add_constraints(P, [A >= 1, B =< 3, A =< 2]),
  ppl_Polyhedron_is_topologically_closed(P),
   (T = nnc ->
     (ppl_Polyhedron_add_constraints(P, [A > 1, B =< 3, A =< 2]),
      \+ ppl_Polyhedron_is_topologically_closed(P),
      ppl_Polyhedron_add_constraints(P, [A > 2]),
      ppl_Polyhedron_is_topologically_closed(P))
   ; true
   ),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_contains_Polyhedron for C and NNC Polyhedron.
contains :-
  contains(c), contains(nnc).

contains(T) :-
  ppl_new_Polyhedron_from_dimension(T, 3, P1),
  ppl_new_Polyhedron_empty_from_dimension(T, 3, P2),
  ppl_Polyhedron_contains_Polyhedron(P1, P2),
  \+ppl_Polyhedron_contains_Polyhedron(P2, P1),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

% Tests ppl_Polyhedron_strictly_contains_Polyhedron for C and NNC Polyhedron.
strict_contains :-
  strict_contains(c),
  strict_contains(nnc).

strict_contains(T) :-
  ppl_new_Polyhedron_from_dimension(T, 3, P1),
  ppl_new_Polyhedron_empty_from_dimension(T, 3, P2),
  ppl_Polyhedron_strictly_contains_Polyhedron(P1, P2),
  \+ppl_Polyhedron_strictly_contains_Polyhedron(P1, P1),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

% Tests ppl_Polyhedron_is_disjoint_from_Polyhedron for C and NNC Polyhedron.
  disjoint_from :-
  disjoint_from(c), disjoint_from(nnc).

disjoint_from(T) :-
  make_vars(3, [A, B, C]),
  ppl_new_Polyhedron_from_constraints(T,
                                      [3 >= A, 4*A + B - 2*C >= 5],
                                      P1),
  ppl_new_Polyhedron_from_constraints(T,
                                      [4 =< A, 4*A + B - 2*C >= 5],
                                      P2),
  ppl_Polyhedron_is_disjoint_from_Polyhedron(P1, P2),
  \+ppl_Polyhedron_is_disjoint_from_Polyhedron(P1, P1),
  (T = nnc ->
     (ppl_new_Polyhedron_from_constraints(nnc,
                                      [3 < A, 4*A + B - 2*C >= 5],
                                      P2a),
    ppl_Polyhedron_is_disjoint_from_Polyhedron(P1, P2a),
    ppl_delete_Polyhedron(P2a))
  ; true
  ),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

% Tests ppl_Polyhedron_equals_Polyhedron for C and NNC Polyhedron.
equals :-
  equals(c), equals(nnc).

equals(T) :-
  ppl_new_Polyhedron_from_dimension(T, 3, P1),
  ppl_new_Polyhedron_from_dimension(T, 3, P2),
  ppl_new_Polyhedron_empty_from_dimension(T, 3, P3),
  ppl_Polyhedron_equals_Polyhedron(P1, P2),
  \+ ppl_Polyhedron_equals_Polyhedron(P1, P3),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P3).

ok :-
  ok(c), ok(nnc).

ok(T) :-
  ppl_new_Polyhedron_from_dimension(T, 0, P1),
  ppl_new_Polyhedron_from_dimension(T, 3, P2),
  ppl_new_Polyhedron_empty_from_dimension(T, 0, P3),
  ppl_new_Polyhedron_empty_from_dimension(T, 3, P4),
  ppl_Polyhedron_OK(P1),
  ppl_Polyhedron_OK(P2),
  ppl_Polyhedron_OK(P3),
  ppl_Polyhedron_OK(P4),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P3),
  ppl_delete_Polyhedron(P4).

% Tests ppl_Polyhedron_get_bounding_box.

get_bounding_box:-
  make_vars(2, [A, B]),
  get_bounding_box(c, [B >= 0, 4*A =< 2],
                     [i(o(minf), c(1/2)), i(c(0), o(pinf))]),
  get_bounding_box(nnc,[B > 0, 4*A =< 2],
                     [i(o(minf), c(1/2)), i(o(0), o(pinf))]).

get_bounding_box(T, CS, Box) :-
  ppl_new_Polyhedron_from_dimension(T, 2, P),
  ppl_Polyhedron_add_constraints(P, CS),
  \+ppl_Polyhedron_get_bounding_box(P, a, Box),
  \+ppl_Polyhedron_get_bounding_box(P, any, box),
  ppl_Polyhedron_get_bounding_box(P, any, Box),
  ppl_Polyhedron_get_bounding_box(P, polynomial, Box),
  ppl_Polyhedron_get_bounding_box(P, simplex, Box),
  ppl_new_Polyhedron_from_bounding_box(T, Box, P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_bounds_from_above for an NNC polyhedron.
bounds_from_above :-
  make_vars(2, [A, B]),
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
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [B > 0,
                                       B < 1],
                                     P),
  \+ ppl_Polyhedron_bounds_from_below(P, A),
  ppl_Polyhedron_add_constraint(P, A > 1),
  ppl_Polyhedron_bounds_from_below(P, A),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_maximize.
maximize :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_from_constraints(c,
                                      [A >= -1, A =< 1, B >= -1,
                                       B =< 1],
                                     P),
  ppl_Polyhedron_maximize(P, A + B, N, D, Max),
  N = 2,
  D = 1,
  Max = true,
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [A > -1, A < 1, B > -1,
                                       B < 1],
                                     P1),
  ppl_Polyhedron_maximize(P1, A + B - 1, N1, D1, Max1),
  N1 = 1,
  D1 = 1,
  Max1 = false,
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_maximize_with_point.
maximize_with_point :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_from_constraints(c,
                                      [A >= -1, A =< 1, B >= -1,
                                       B =< 1],
                                     P),
  ppl_Polyhedron_maximize_with_point(P, A + B, N, D, Max, Point),
  N = 2,
  D = 1,
  Max = true,
  ppl_new_Polyhedron_from_generators(c, [Point], Pm),
  ppl_new_Polyhedron_from_generators(c, [point(A+B)], Qm),
  ppl_Polyhedron_equals_Polyhedron(Pm, Qm),
  ppl_delete_Polyhedron(Pm),
  ppl_delete_Polyhedron(Qm),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [A > -1, A < 1, B > -1,
                                       B < 1],
                                     P1),
  ppl_Polyhedron_maximize_with_point(P1, A + B - 1, N1, D1, Max1, Point1),
  N1 = 1,
  D1 = 1,
  Max1 = false,
  Point1 = closure_point(Cp),
  ppl_new_Polyhedron_from_generators(c, [point(Cp)], Pm1),
  ppl_new_Polyhedron_from_generators(c, [point(A+B)], Qm1),
  ppl_Polyhedron_equals_Polyhedron(Pm1, Qm1),
  ppl_delete_Polyhedron(Pm1),
  ppl_delete_Polyhedron(Qm1),
  ppl_new_Polyhedron_from_constraints(c,
                                      [A =< 0],
                                     P2),
  ppl_Polyhedron_maximize_with_point(P2, A, N2, D2, Max2, Point2),
  N2 = 0, D2 = 1, Max2 = true,
  ppl_new_Polyhedron_from_generators(c, [Point2], Pm2),
  ppl_new_Polyhedron_from_generators(c, [point(0)], Qm2),
  ppl_Polyhedron_equals_Polyhedron(Pm2, Qm2),
  ppl_delete_Polyhedron(Pm2),
  ppl_delete_Polyhedron(Qm2),
  ppl_new_Polyhedron_from_constraints(c,
                                      [A >= 0],
                                     P3),
  \+ppl_Polyhedron_maximize_with_point(P3, A, _, _, _, _),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P3).

% Tests ppl_Polyhedron_minimize.
minimize :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_from_constraints(c,
                                      [A >= -1, A =< 1, B >= -1,
                                       B =< 1],
                                     P),
  ppl_Polyhedron_minimize(P, A + B, N, D, Min),
  N = -2,
  D = 1,
  Min = true,
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [A > -2, A =< 2, B > -2,
                                       B =< 2],
                                     P1),
  ppl_Polyhedron_minimize(P1, A + B + 1, N1, D1, Min1),
  N1 = -3,
  D1 = 1,
  Min1 = false,
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_minimize_with_point.
minimize_with_point :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_from_constraints(c,
                                      [A >= -1, A =< 1, B >= -1,
                                       B =< 1],
                                     P),
  ppl_Polyhedron_minimize_with_point(P, A + B, N, D, Min, Point),
  N = -2, D = 1, Min = true,
  ppl_new_Polyhedron_from_generators(c, [Point], Pm),
  ppl_new_Polyhedron_from_generators(c, [point(-A-B)], Qm),
  ppl_Polyhedron_equals_Polyhedron(Pm, Qm),
  ppl_delete_Polyhedron(Pm),
  ppl_delete_Polyhedron(Qm),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [A > -2, A =< 2, B > -2,
                                       B =< 2],
                                     P1),
  ppl_Polyhedron_minimize_with_point(P1, A + B, N1, D1, Min1, Point1),
  N1 = -4, D1 = 1, Min1 = false,
  Point1 = closure_point(Cp),
  ppl_new_Polyhedron_from_generators(c, [point(Cp)], Pm1),
  ppl_new_Polyhedron_from_generators(c, [point(-2*A-2*B)], Qm1),
  ppl_Polyhedron_equals_Polyhedron(Pm1, Qm1),
  ppl_delete_Polyhedron(Pm1),
  ppl_delete_Polyhedron(Qm1),
  ppl_new_Polyhedron_from_constraints(c,
                                      [A >= 0],
                                     P2),
  ppl_Polyhedron_minimize_with_point(P2, A, N2, D2, Min2, Point2),
  N2 = 0, D2 = 1, Min2 = true,
  ppl_new_Polyhedron_from_generators(c, [Point2], Pm2),
  ppl_new_Polyhedron_from_generators(c, [point(0)], Qm2),
  ppl_Polyhedron_equals_Polyhedron(Pm2, Qm2),
  ppl_delete_Polyhedron(Pm2),
  ppl_delete_Polyhedron(Qm2),
  ppl_new_Polyhedron_from_constraints(c,
                                      [A =< 0],
                                     P3),
  \+ppl_Polyhedron_minimize_with_point(P3, A, _, _, _, _),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P3).

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
  make_vars(6, [A, B, C, D, E, F]),
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
  \+  ppl_timeout_exception_atom(pl_x),
  ppl_timeout_exception_atom(pl_time_out),
  N1 = 1,
  ppl_set_timeout(N1),
  ppl_new_Polyhedron_from_dimension(T, 6, P),
  time_watch(T, ppl_Polyhedron_add_constraints_and_minimize(P, CS),
             (ppl_Polyhedron_add_constraints_and_minimize(Q, CS)),
              (true, display_message(
                 ['polyhedron with topology',T,'timeout after', N1,ms]))),
  ppl_Polyhedron_equals_Polyhedron(P, Q),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q),
  ppl_new_Polyhedron_from_dimension(T, 6, Q1),
  N2 = 10,
  ppl_set_timeout(N2),
  ppl_new_Polyhedron_from_dimension(T, 6, P1),
  time_watch(T, ppl_Polyhedron_add_constraints_and_minimize(P1, CS),
             (ppl_Polyhedron_add_constraints_and_minimize(Q1, CS)),
              (true, display_message(
                 ['polyhedron with topology',T,'timeout after',N2,ms]))),
  ppl_Polyhedron_equals_Polyhedron(P1, Q1),
  ppl_set_timeout_exception_atom(time_out),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(Q1),
  ppl_finalize.

% time_watch(+Topology, +Goal, +NoTimeOut, +TimeOut).
% time_watch makes a copy of Goal with a copy of the polyhedron
% and executes it with the currrent timeout exception settings.
% If the call exceeds the time allowed, it catches the exception
% and performs the TimeOut goal.
% If the call does not exceed the time allowed,
% then the timeout exception time is reset and
% then Goal is executed and then the NoTmeOut is executed.

time_watch(Topology, Goal, NoTimeOut, TimeOut) :-
   !,
   Goal =.. [PPLFunct, Poly|Args],
   ppl_new_Polyhedron_from_Polyhedron(Topology, Poly, Topology, PolyCopy),
   GoalCopy =.. [PPLFunct, PolyCopy|Args],
   ppl_timeout_exception_atom(TimeOutAtom),
     (catch(GoalCopy, TimeOutAtom, fail) ->
       (ppl_reset_timeout,ppl_Polyhedron_swap(Poly, PolyCopy), call(NoTimeOut))
     ;
       call(TimeOut)
   ),
   ppl_delete_Polyhedron(PolyCopy).

% These next 2 tests demonstrate a bug in the bounding box software
% and are not executed by run_all.

boundingbox1(Box,CS) :-
  make_vars(2, [A, B]),
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

%%%%%%%%%%%% predicates for output messages %%%%%%%%%%%%%%%

error_message(Message):-
   write_all(Message),
   fail.

display_message(Message):-
   noisy, !,
   nl, write_all(Message), nl.
display_message(_).

write_all([]).
write_all([Phrase|Phrases]):-
   write(Phrase),
   write(' '),
   write_all(Phrases).

% make_var_list(+I,+Dimension,?VariableList)
% constructs a list of variables with indices from I to Dimension - 1.
% It is assumed that I =< Dimension.

make_vars(Dim, VarList):-
  make_var_list(0, Dim, VarList).
make_var_list(Dim,Dim,[]):- !.
make_var_list(I,Dim,['$VAR'(I)|VarList]):-
  I1 is I + 1,
  make_var_list(I1,Dim,VarList).

:- ppl_initialize.
