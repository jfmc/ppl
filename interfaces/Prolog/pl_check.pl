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


% noisy(F)
% When F = 1, a message is displayed if a time out occurs
% when running the `timeout' predicate.
% Also, the values of the PPL versions and banner are displayed.
% When F = 0, no 'time out' message or versions are displayed.
% noisy/1 can be reset by calling make_noisy/0 or make_quiet/0.

:- dynamic(noisy/1).

% check_all
% This executes all the test predicates which, together, check all
% the ppl interface predicates.

check_all :-
   make_quiet,
   run_all.

check_noisy :-
   make_noisy,
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
   (handle_exceptions -> true ;
        error_message(['error in an exception predicate'])),
   !,
   ppl_finalize.

run_all:-
   ppl_finalize,
   fail.

% Tests predicates that return PPL version information and the PPL banner.
% If noisy(0) holds, there is no output but if not,
% all the versions are printed and the banner is pretty printed.
all_versions_and_banner :-
  ppl_initialize,
  ppl_version_major(Vmajor),
  ppl_version_minor(Vminor),
  ppl_version_revision(Vrevision),
  ppl_version_beta(Vbeta),
  ppl_version(V),
  ppl_banner(B),
  (noisy(0) -> true ;
     (
      write('Version major is '), write(Vmajor), nl,
      write('Version minor is '), write(Vminor), nl,
      write('Version revision is '), write(Vrevision), nl,
      write('Version beta is '), write(Vbeta), nl,
      write('Version is '), write(V), nl,
      banner_pp(B), nl
     )
  ),
  !,
  ppl_finalize.

% Tests predicates that return the maximum allowed dimension.
% If noisy(0) holds, there is no output but if not, the maximum is printed.
max_dim :-
  ppl_initialize,
  ppl_max_space_dimension(M),
  (noisy(0) -> true ;
     display_message(['Maximum possible dimension is', M, nl])
  ),
  !,
  ppl_finalize.

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
   widen_BHRZ03,
   widen_BHRZ03_with_token,
   lim_extrapolate_BHRZ03,
   lim_extrapolate_BHRZ03_with_token,
   bound_extrapolate_BHRZ03,
   bound_extrapolate_BHRZ03_with_token,
   widen_H79,
   widen_H79_with_token,
   lim_extrapolate_H79,
   lim_extrapolate_H79_with_token,
   bound_extrapolate_H79,
   bound_extrapolate_H79_with_token,
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
   expand_dim,
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
  
handle_exceptions :-
  ppl_initialize,
  exceptions,
  !,
  ppl_finalize.

%%%%%%%%%%%%%%%%% New Polyhedron %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tests new_Polyhedron_from_dimension/3 and ppl_delete_Polyhedron/1.
new_universe :-
  make_vars(1,[A]),
  new_universe(c, A >= 0), new_universe(nnc, A > 0).

% This also uses ppl_Polyhedron_is_universe/1
% and ppl_Polyhedron_add_constraint/2.
new_universe(T, Con) :-
  \+ ppl_new_Polyhedron_from_dimension(T, 3, 0),
  ppl_new_Polyhedron_from_dimension(T, 3, P),
  ppl_Polyhedron_is_universe(P),
  ppl_Polyhedron_add_constraint(P, Con),
  \+ ppl_Polyhedron_is_universe(P),
  ppl_delete_Polyhedron(P).

% Tests new_Polyhedron_empty_from_dimension/3 for C and NNC Polyhedron.
new_empty :-
  new_empty(c), new_empty(nnc).

% This also uses ppl_Polyhedron_is_empty/1
%  and ppl_Polyhedron_add_generator/2.
new_empty(T) :-
  \+  ppl_new_Polyhedron_empty_from_dimension(T, 3, 0),
  ppl_new_Polyhedron_empty_from_dimension(T, 3, P),
  ppl_Polyhedron_is_empty(P),
  ppl_Polyhedron_add_generator(P,point(0)),
  \+ ppl_Polyhedron_is_empty(P),
  ppl_delete_Polyhedron(P).

% Tests ppl_new_Polyhedron_from_Polyhedron/4.
copy :-
  copy(c, c), copy(nnc, nnc), copy(c, nnc), copy(nnc, c).

% This also uses ppl_new_Polyhedron_from_constraints/3 and
% ppl_Polyhedron_equals_Polyhedron/2.
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

% Tests ppl_new_Polyhedron_from_constraints/3.
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

% Tests ppl_new_Polyhedron_from_generators/3.
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

% Tests ppl_new_Polyhedron_from_bounding_box/2.
new_poly_from_bounding_box :-
  new_poly_from_bounding_box(c, [i(c(1/2), o(pinf)), i(o(minf), c(-1/2))]),
  new_poly_from_bounding_box(c, [empty]),
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

%%%%%%%%%%%%%%%%% Swap Polyhedra %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tests ppl_Polyhedron_swap/2.
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

%%%%%%%%%%%%%%%%%% Space Dimension %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tests ppl_Polyhedron_space_dimension/2.
space :-
 space(c), space(nnc).

space(T) :-
  ppl_new_Polyhedron_from_dimension(T, 3, P),
  ppl_Polyhedron_space_dimension(P, N),
  N = 3,
  \+ ppl_Polyhedron_space_dimension(P, 4),
  ppl_new_Polyhedron_from_generators(T, [], Q),
  ppl_Polyhedron_space_dimension(Q, M),
  M == 0,
  ppl_new_Polyhedron_from_constraints(T, [], Q1),
  ppl_Polyhedron_space_dimension(Q1, M1),
  M1 == 0,
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Q),
  ppl_delete_Polyhedron(Q1).

% Tests ppl_Polyhedron_intersection_assign/2.
inters_assign :-
  inters_assign(c), inters_assign(nnc).

%%%%%%%%%%%%%%%%%% Basic Operators %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

% Tests ppl_Polyhedron_intersection_assign_and_minimize/2.
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

% Tests ppl_Polyhedron_concatenate_assign/2.
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

% Tests ppl_Polyhedron_poly_hull_assign/2.
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

% Tests ppl_Polyhedron_poly_hull_assign_and_minimize/2.
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

% Tests ppl_Polyhedron_poly_difference_assign/2.
polydiff_assign :-
  make_vars(2, [A, B]),
  polydiff_assign(c, [point(0), point(2*A)],
                     [point(0), point(A)],
                     [point(A), point(2*A)]),
  polydiff_assign(nnc, [point(0), point(2*A)],
                       [point(0), point(A)],
                       [closure_point(A), point(2*A)]),
  polydiff_assign(c, [point(0), point(B), point(A)],
                     [point(0), point(A), point(A + B)],
                     [point(A + B, 2), point(B), point(0)]),
  polydiff_assign(nnc,[point(0), point(B), point(A)],
                      [point(0), point(A), point(A + B)],
                      [closure_point(A + B, 2), point(B), closure_point(0)]).

polydiff_assign(T, GS1, GS2, GS3) :-
  ppl_new_Polyhedron_from_generators(T, GS1, P1),
  ppl_new_Polyhedron_from_generators(T, GS2, P2),
  ppl_Polyhedron_poly_difference_assign(P1, P2),
  ppl_new_Polyhedron_from_generators(T, GS3, P1a),
  ppl_Polyhedron_equals_Polyhedron(P1, P1a),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P1a).

% Tests ppl_Polyhedron_time_elapse_assign/2.
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

% Tests ppl_Polyhedron_topological_closure_assign/1.
top_close_assign :-
  make_vars(3, [A, B, C]),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [4*A + B - 2*C >= 5, A < 3],
                                      P),
  ppl_Polyhedron_topological_closure_assign(P),
  ppl_new_Polyhedron_from_constraints(nnc,
                                      [4*A + B + -2*C >= 5, A =< 3],
                                      Pa),
  ppl_Polyhedron_equals_Polyhedron(P, Pa),
  ppl_new_Polyhedron_from_Polyhedron(nnc, P, c, Q),
  ppl_Polyhedron_topological_closure_assign(Q),
  ppl_new_Polyhedron_from_constraints(c,
                                      [4*A + B + -2*C >= 5, A =< 3],
                                      Qa),
  ppl_Polyhedron_equals_Polyhedron(Q, Qa),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(Pa),
  ppl_delete_Polyhedron(Q),
  ppl_delete_Polyhedron(Qa).

%%%%%%%%%%%%%%%%%% Affine Transformations %%%%%%%%%%%%%%%%%%%

% Tests ppl_Polyhedron_affine_image/4.
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

% Tests ppl_Polyhedron_affine_preimage/4.
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

% Tests ppl_Polyhedron_generalized_affine_image/5.
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

% Tests ppl_Polyhedron_generalized_affine_image_lhs_rhs/4.
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

%%%%%%%%%%%%%%%%%% Widen and Extrapolation Operators %%%%%%%%%%%%%%%%%%%

% Tests ppl_Polyhedron_BHRZ03_widening_assign/2.
widen_BHRZ03 :-
  make_vars(2, [A, B]),
  widen_BHRZ03(c, [A >= 1, B >= 0], [A >= 1, B >= 1],
                  [A >= 1], [A >= 1, B >= 1]
              ),
  widen_BHRZ03(nnc, [A > 1, B > 0], [A > 1, B > 1],
                    [A > 1], [A > 1, B > 1]
              ).

widen_BHRZ03(Topology, CS_P, CS_Q, CS_Pa, CS_Qa) :-
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  ppl_Polyhedron_BHRZ03_widening_assign(P, Q),
  widen_extrapolation_final(P, CS_Pa, Topology),
  widen_extrapolation_final(Q, CS_Qa, Topology).

% Tests ppl_Polyhedron_BHRZ03_widening_assign_with_token/3.
widen_BHRZ03_with_token :-
  make_vars(2, [A, B]),
  widen_BHRZ03_with_token(c, [A >= 1], [A >= 1, B >= 1],
                  [A >= 1], [A >= 1, B >= 1], 0
              ),
  widen_BHRZ03_with_token(c, [A >= 1, B >= 0], [A >= 1, B >= 1],
                  [A >= 1, B >= 0], [A >= 1, B >= 1], 1
              ),
  widen_BHRZ03_with_token(nnc, [A > 1], [A > 1, B > 1],
                    [A > 1], [A > 1, B > 1], 0
              ),
  widen_BHRZ03_with_token(nnc, [A > 1, B >= 0], [A > 1, B >= 1],
                  [A > 1, B >= 0], [A > 1, B >= 1], 1
              ).

widen_BHRZ03_with_token(Topology, CS_P, CS_Q, CS_Pa, CS_Qa, Token) :-
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  WrongToken is (Token + 1) mod 2,
  \+  ppl_Polyhedron_BHRZ03_widening_assign_with_token(P, Q, WrongToken),
  ppl_Polyhedron_BHRZ03_widening_assign_with_token(P, Q, Token),
  widen_extrapolation_final(P, CS_Pa, Topology),
  widen_extrapolation_final(Q, CS_Qa, Topology).

% Tests ppl_Polyhedron_limited_BHRZ03_extrapolation_assign/3.
lim_extrapolate_BHRZ03 :-
  make_vars(2, [A, B]),
  lim_extrapolate_BHRZ03(c, [A >= 1, B >= 0], [A >= 2, B >= 1],
                            [A >= 1, B >= 0], [A >= 1, B >= 0]
              ),
  lim_extrapolate_BHRZ03(c, [A >= 1, B >= 0], [A >= 2, B >= 1],
                            [A >= 2], []
              ),
  lim_extrapolate_BHRZ03(nnc, [A > 1, B > 0], [A > 2, B > 1],
                              [A > 1, B > 0], [A > 1, B > 0]
              ),
  lim_extrapolate_BHRZ03(nnc, [A > 1, B >= 0], [A > 2, B >= 1],
                              [A >= 2], []
              ).

lim_extrapolate_BHRZ03(Topology, CS_P, CS_Q, CS_lim, CS_Pa)  :-
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  ppl_Polyhedron_limited_BHRZ03_extrapolation_assign(P, Q, CS_lim),
  widen_extrapolation_final(P, CS_Pa, Topology),
  ppl_delete_Polyhedron(Q).

% Tests ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token/4.
lim_extrapolate_BHRZ03_with_token :-
  make_vars(2, [A, B]),
  lim_extrapolate_BHRZ03_with_token(c, [A >= 1, B >= 0], [A >= 1, B >= 1],
                  [A >= 1, B >= 0], [A >= 1, B >= 0], 1
              ),
  lim_extrapolate_BHRZ03_with_token(nnc, [A > 1, B > 0], [A > 1, B > 1],
                    [A > 1, B > 0], [A > 1, B > 0], 1
              ).

lim_extrapolate_BHRZ03_with_token(Topology,
                 CS_P, CS_Q, CS_lim, CS_Pa, Token) :-
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  WrongToken is (Token + 1) mod 2,
\+  ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token(P, Q,
                                                   CS_lim, WrongToken),
  ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token(P, Q,
                                                   CS_lim, Token),
  widen_extrapolation_final(P, CS_Pa, Topology),
  ppl_delete_Polyhedron(Q).


% Tests ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign/3.
bound_extrapolate_BHRZ03 :-
  make_vars(2, [A, B]),
  bound_extrapolate_BHRZ03(c, [A >= 1, B >= 0], [A >= 2, B >= 1],
                            [A >= 1, B >= 0], [A >= 1, B >= 0]
              ),
  bound_extrapolate_BHRZ03(c, [A >= 1, B >= 0], [A >= 2, B >= 1],
                            [A >= 2], [A >= 1, B >= 0]
              ),
  bound_extrapolate_BHRZ03(nnc, [A > 1, B > 0], [A > 2, B > 1],
                              [A > 1, B > 0], [A > 1, B > 0]
              ),
  bound_extrapolate_BHRZ03(nnc, [A > 1, B >= 0], [A > 2, B >= 1],
                              [A >= 2], [A > 1, B >= 0]
              ).

bound_extrapolate_BHRZ03(Topology, CS_P, CS_Q, CS_lim, CS_Pa)  :-
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign(P, Q, CS_lim),
  widen_extrapolation_final(P, CS_Pa, Topology),
  ppl_delete_Polyhedron(Q).

% Tests ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token/4.
bound_extrapolate_BHRZ03_with_token :-
  make_vars(2, [A, B]),
  bound_extrapolate_BHRZ03_with_token(c, [A >= 1, B >= 0], [A >= 1, B >= 1],
                  [A >= 1, B >= 0], [A >= 1, B >= 0], 1
              ),
  bound_extrapolate_BHRZ03_with_token(nnc, [A > 1, B > 0], [A > 1, B > 1],
                    [A > 1, B > 0], [A > 1, B > 0], 1
              ).

bound_extrapolate_BHRZ03_with_token(Topology,
                 CS_P, CS_Q, CS_lim, CS_Pa, Token) :-
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  WrongToken is (Token + 1) mod 2,
\+  ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token(P, Q,
                                                   CS_lim, WrongToken),
  ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token(P, Q,
                                                   CS_lim, Token),
  widen_extrapolation_final(P, CS_Pa, Topology),
  ppl_delete_Polyhedron(Q).

% Tests ppl_Polyhedron_H79_widening_assign/2.
widen_H79 :-
  make_vars(2, [A, B]),
  widen_H79(c, [A >= 1, B >= 0], [A >= 1, B >= 1],
               [A >= 1], [A >= 1, B >= 1]
              ),
  widen_H79(nnc, [A > 1, B > 0], [A > 1, B > 1],
                 [A > 1], [A > 1, B > 1]
              ).

widen_H79(Topology, CS_P, CS_Q, CS_Pa, CS_Qa) :-
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  ppl_Polyhedron_H79_widening_assign(P, Q),
  widen_extrapolation_final(P, CS_Pa, Topology),
  widen_extrapolation_final(Q, CS_Qa, Topology).

% Tests ppl_Polyhedron_H79_widening_assign_with_token/3.
widen_H79_with_token :-
  make_vars(2, [A, B]),
  widen_H79_with_token(c, [A >= 1], [A >= 1, B >= 1],
                  [A >= 1], [A >= 1, B >= 1], 0
              ),
  widen_H79_with_token(c, [A >= 1, B >= 0], [A >= 1, B >= 1],
                  [A >= 1, B >= 0], [A >= 1, B >= 1], 1
              ),
  widen_H79_with_token(nnc, [A > 1], [A > 1, B > 1],
                    [A > 1], [A > 1, B > 1], 0
              ),
  widen_H79_with_token(nnc, [A > 1, B >= 0], [A > 1, B >= 1],
                  [A > 1, B >= 0], [A > 1, B >= 1], 1
              ).

widen_H79_with_token(Topology, CS_P, CS_Q, CS_Pa, CS_Qa, Token) :-
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  WrongToken is (Token + 1) mod 2,
  \+  ppl_Polyhedron_H79_widening_assign_with_token(P, Q, WrongToken),
  ppl_Polyhedron_H79_widening_assign_with_token(P, Q, Token),
  widen_extrapolation_final(P, CS_Pa, Topology),
  widen_extrapolation_final(Q, CS_Qa, Topology).

% Tests ppl_Polyhedron_limited_H79_extrapolation_assign/3.
lim_extrapolate_H79 :-
  make_vars(2, [A, B]),
  lim_extrapolate_H79(c, [A >= 1, B >= 0], [A >= 2, B >= 1],
                            [A >= 1, B >= 0], [A >= 1, B >= 0]
              ),
  lim_extrapolate_H79(c, [A >= 1, B >= 0], [A >= 2, B >= 1],
                            [A >= 2], []
              ),
  lim_extrapolate_H79(nnc, [A > 1, B > 0], [A > 2, B > 1],
                              [A > 1, B > 0], [A > 1, B > 0]
              ),
  lim_extrapolate_H79(nnc, [A > 1, B >= 0], [A > 2, B >= 1],
                              [A >= 2], []
              ).

lim_extrapolate_H79(Topology, CS_P, CS_Q, CS_lim, CS_Pa)  :-
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  ppl_Polyhedron_limited_H79_extrapolation_assign(P, Q, CS_lim),
  widen_extrapolation_final(P, CS_Pa, Topology),
  ppl_delete_Polyhedron(Q).

% Tests ppl_Polyhedron_limited_H79_extrapolation_assign_with_token/4.
lim_extrapolate_H79_with_token :-
  make_vars(2, [A, B]),
  lim_extrapolate_H79_with_token(c, [A >= 1, B >= 0], [A >= 1, B >= 1],
                  [A >= 1, B >= 0], [A >= 1, B >= 0], 1
              ),
  lim_extrapolate_H79_with_token(nnc, [A > 1, B > 0], [A > 1, B > 1],
                    [A > 1, B > 0], [A > 1, B > 0], 1
              ).

lim_extrapolate_H79_with_token(Topology,
                 CS_P, CS_Q, CS_lim, CS_Pa, Token) :-
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  WrongToken is (Token + 1) mod 2,
\+  ppl_Polyhedron_limited_H79_extrapolation_assign_with_token(P, Q,
                                                   CS_lim, WrongToken),
  ppl_Polyhedron_limited_H79_extrapolation_assign_with_token(P, Q,
                                                   CS_lim, Token),
  widen_extrapolation_final(P, CS_Pa, Topology),
  ppl_delete_Polyhedron(Q).


% Tests ppl_Polyhedron_bounded_H79_extrapolation_assign/3.
bound_extrapolate_H79 :-
  make_vars(2, [A, B]),
  bound_extrapolate_H79(c, [A >= 1, B >= 0], [A >= 2, B >= 1],
                            [A >= 1, B >= 0], [A >= 1, B >= 0]
              ),
  bound_extrapolate_H79(c, [A >= 1, B >= 0], [A >= 2, B >= 1],
                            [A >= 2], [A >= 1, B >= 0]
              ),
  bound_extrapolate_H79(nnc, [A > 1, B > 0], [A > 2, B > 1],
                              [A > 1, B > 0], [A > 1, B > 0]
              ),
  bound_extrapolate_H79(nnc, [A > 1, B >= 0], [A > 2, B >= 1],
                              [A >= 2], [A > 1, B >= 0]
              ).

bound_extrapolate_H79(Topology, CS_P, CS_Q, CS_lim, CS_Pa)  :-
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  ppl_Polyhedron_bounded_H79_extrapolation_assign(P, Q, CS_lim),
  widen_extrapolation_final(P, CS_Pa, Topology),
  ppl_delete_Polyhedron(Q).

% Tests ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token/4.
bound_extrapolate_H79_with_token :-
  make_vars(2, [A, B]),
  bound_extrapolate_H79_with_token(c, [A >= 1, B >= 0], [A >= 1, B >= 1],
                  [A >= 1, B >= 0], [A >= 1, B >= 0], 1
              ),
  bound_extrapolate_H79_with_token(nnc, [A > 1, B > 0], [A > 1, B > 1],
                    [A > 1, B > 0], [A > 1, B > 0], 1
              ).

bound_extrapolate_H79_with_token(Topology,
                 CS_P, CS_Q, CS_lim, CS_Pa, Token) :-
  widen_extrapolation_init(P, CS_P, Topology),
  widen_extrapolation_init(Q, CS_Q, Topology),
  WrongToken is (Token + 1) mod 2,
\+  ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token(P, Q,
                                                   CS_lim, WrongToken),
  ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token(P, Q,
                                                   CS_lim, Token),
  widen_extrapolation_final(P, CS_Pa, Topology),
  ppl_delete_Polyhedron(Q).

% widen_extrapolation_init/3 and widen_extrapolation_final/3 
% are used in the tests for widening and extrapolation predicates.
widen_extrapolation_init(P, CS, Topology):-
  ppl_new_Polyhedron_from_dimension(Topology, 2, P),
  ppl_Polyhedron_add_constraints(P, CS).

widen_extrapolation_final(P,CS, Topology):-
  ppl_new_Polyhedron_from_dimension(Topology, 2, P1),
  ppl_Polyhedron_add_constraints(P1, CS),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

%%%%%%%%%%%%%%%%%% Get Constraint or Generator System %%%%%%%%%%%%%%%%%%%

% Tests ppl_Polyhedron_get_constraints/2.
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

% Tests ppl_Polyhedron_get_minimized_constraints/2.
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

% Tests ppl_Polyhedron_get_generators/2.
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

% Tests ppl_Polyhedron_get_minimized_generators/2.
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

%%%%%%%%%%%%%%%%%% Add Constraints or Generators %%%%%%%%%%%%%%%%%%%

% Tests ppl_Polyhedron_add_constraint/2.
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

% Tests ppl_Polyhedron_add_constraint_and_minimize/2.
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

% Tests ppl_Polyhedron_add_generator/2.
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

% Tests ppl_Polyhedron_add_generator_and_minimize/2.
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

% Tests ppl_Polyhedron_add_constraints/2.
add_cons :-
  make_vars(3, [A, B, C]),
  add_cons(c, [A >= 1, B >= 0, 4*A + B - 2*C >= 5], [A =< 0]),
  add_cons(nnc, [A > 1, B >= 0, 4*A + B - 2*C > 5], [A < 0]).

add_cons(T, CS, CS1) :-
  ppl_new_Polyhedron_from_dimension(T, 3, P),
  ppl_Polyhedron_add_constraints(P, CS),
  ppl_new_Polyhedron_from_constraints(T, CS, P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_Polyhedron_add_constraints(P, CS1),
  ppl_Polyhedron_is_empty(P),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_add_constraints_and_minimize/2.
add_cons_min :-
  make_vars(2, [A, B]),
  add_cons_min(c, [A >= 1, B >= 0], [A + B =< 0]),
  add_cons_min(nnc, [A > 1, B >= 0], [A < 0]).

add_cons_min(T, CS, CS1) :-
  ppl_new_Polyhedron_from_dimension(T, 2, P),
  ppl_Polyhedron_add_constraints_and_minimize(P, CS),
  ppl_new_Polyhedron_from_constraints(T, CS, P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  \+ppl_Polyhedron_add_constraints_and_minimize(P, CS1),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_add_generators/2.
add_gens :-
  make_vars(3, [A, B, C]),
  add_gens(c, [point(A + B + C), ray(A), ray(2*A), point(A + B + C, 1),
               point(100*A + 5*B, -8)]),
  add_gens(nnc, [point(A + B + C), ray(A), ray(2*A), point(A + B + C, 1),
               point(100*A + 5*B, -8)]).

add_gens(T, GS) :-
  ppl_new_Polyhedron_empty_from_dimension(T, 3, P),
  ppl_Polyhedron_add_generators(P, GS),
  ppl_new_Polyhedron_from_generators(T, GS, P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

% Tests ppl_Polyhedron_add_generators_and_minimize/2.
add_gens_min :-
  make_vars(3, [A, B, C]),
  add_gens_min(c, [point(A + B + C), ray(A), ray(2*A), point(A + B + C)]),
  add_gens_min(nnc, [point(A + B + C), ray(A), ray(2*A), point(A + B + C)]).

add_gens_min(T, GS) :-
  ppl_new_Polyhedron_empty_from_dimension(T, 3, P),
  \+  ppl_Polyhedron_add_generators_and_minimize(P, []),
  ppl_Polyhedron_add_generators_and_minimize(P, GS),
  ppl_new_Polyhedron_from_generators(T, GS, P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1).

%%%%%%%%%%%%%%%%%% Change Dimensions %%%%%%%%%%%%%%%%%%%%%%%%%%

% Tests ppl_Polyhedron_add_dimensions_and_project/2.
project :-
  project(c), project(nnc).

project(T) :-
  make_vars(4, [A, B, C, D]),
  ppl_new_Polyhedron_from_dimension(T, 2, P),
  ppl_Polyhedron_add_constraints(P, [A >= 1, B >= 0]),
  ppl_Polyhedron_add_dimensions_and_project(P, 0),
  ppl_new_Polyhedron_from_constraints(T,
                                      [A >= 1, B >= 0],
                                      P0),
  ppl_Polyhedron_equals_Polyhedron(P, P0),
  ppl_delete_Polyhedron(P0),
  ppl_Polyhedron_add_dimensions_and_project(P, 2),
  ppl_new_Polyhedron_from_constraints(T,
                                      [A >= 1, B >= 0, C = 0, D = 0],
                                      P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_add_dimensions_and_embed/2.
embed :-
  embed(c), embed(nnc).

embed(T) :-
  make_vars(2, [A, B]),
  ppl_new_Polyhedron_from_dimension(T, 2, P),
  ppl_Polyhedron_add_constraints(P, [A >= 1, B >= 0]),
  ppl_Polyhedron_add_dimensions_and_embed(P, 0),
  ppl_new_Polyhedron_from_constraints(T,
                                      [A >= 1, B >= 0],
                                      P0),
  ppl_Polyhedron_equals_Polyhedron(P, P0),
  ppl_delete_Polyhedron(P0),
  ppl_Polyhedron_add_dimensions_and_embed(P, 2),
  ppl_new_Polyhedron_from_dimension(T, 4, P1),
  ppl_Polyhedron_add_constraints(P1, [A >= 1, B >= 0]),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_remove_dimensions/2.
remove_dim :-
  remove_dim(c), remove_dim(nnc).

remove_dim(T) :-
  make_vars(3, [A, B, C]),
  ppl_new_Polyhedron_from_dimension(T, 3, P),
  ppl_Polyhedron_add_constraints(P, [A >= 1, B >= 0, C >= 2]),
  ppl_Polyhedron_remove_dimensions(P,[]),
  ppl_new_Polyhedron_from_constraints(T,
                                      [A >= 1, B >= 0, C >= 2],
                                      P0),
  ppl_Polyhedron_equals_Polyhedron(P, P0),
  ppl_delete_Polyhedron(P0),
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

% Tests ppl_Polyhedron_remove_higher_dimensions/2.
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
  ppl_Polyhedron_equals_Polyhedron(P, P2),
  ppl_Polyhedron_remove_higher_dimensions(P, 0),
  ppl_Polyhedron_space_dimension(P, 0),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_expand_dimension/3.
expand_dim :-
  expand_dim(c), expand_dim(nnc).

expand_dim(T) :-
  make_vars(4, [A, B, C, D]),
  ppl_new_Polyhedron_from_dimension(T, 3, P),
  ppl_Polyhedron_add_constraints(P, [A >= 1, B >= 0, C >= 2]),
  ppl_Polyhedron_expand_dimension(P, B, 1),
  ppl_Polyhedron_space_dimension(P, 4),
  ppl_new_Polyhedron_from_constraints(T,
                                      [A >= 1, B >= 0, C >= 2, D >= 0],
                                      P1),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P1),
  ppl_Polyhedron_remove_higher_dimensions(P, 2),
  ppl_Polyhedron_expand_dimension(P, A, 2),
  ppl_new_Polyhedron_from_constraints(T,
                                      [D >= 1, C >= 1, A >= 1, B >= 0],
                                      P2),
  ppl_Polyhedron_equals_Polyhedron(P, P2),
  ppl_delete_Polyhedron(P2),
  ppl_Polyhedron_space_dimension(P, 4),
  ppl_delete_Polyhedron(P),
% Example taken from GopanDMDRS04, page 519.
  ppl_new_Polyhedron_empty_from_dimension(T, 2, Ptacas),
  ppl_Polyhedron_add_generators(Ptacas,
       [point(A + 2*B), point(A + 3*B), point(A + 4*B)]),
  ppl_Polyhedron_expand_dimension(Ptacas, B, 1),
  ppl_Polyhedron_space_dimension(Ptacas, 3),
  ppl_new_Polyhedron_from_generators(T,
       [point(A + 2*B + 2*C), point(A + 2*B + 3*C), point(A + 2*B + 4*C),
        point(A + 3*B + 2*C), point(A + 3*B + 3*C), point(A + 3*B + 4*C),
        point(A + 4*B + 2*C), point(A + 4*B + 3*C), point(A + 4*B + 4*C)],
                                      Ptacas1),
  ppl_Polyhedron_equals_Polyhedron(Ptacas, Ptacas1),
  ppl_delete_Polyhedron(Ptacas1),
  ppl_delete_Polyhedron(Ptacas).

% Tests ppl_Polyhedron_fold_dimension/3.
fold_dims :-
  fold_dims(c), fold_dims(nnc).

fold_dims(T) :-
  make_vars(4, [A, B, C, D]),
  ppl_new_Polyhedron_from_dimension(T, 4, P),
  ppl_Polyhedron_add_constraints(P, [A >= 1, B >= 0, C >= 2, D >= 0]),
  ppl_Polyhedron_fold_dimensions(P, [D], B),
  ppl_Polyhedron_space_dimension(P, 3),
  ppl_new_Polyhedron_from_dimension(T, 3, P1),
  ppl_Polyhedron_add_constraints(P1, [A >= 1, B >= 0, C >= 2]),
  ppl_Polyhedron_equals_Polyhedron(P, P1),
  ppl_delete_Polyhedron(P1),
  ppl_Polyhedron_fold_dimensions(P, [A, C], B),
  ppl_new_Polyhedron_from_dimension(T, 1, P2),
  ppl_Polyhedron_add_constraints(P2, [A >= 0]),
  ppl_Polyhedron_equals_Polyhedron(P, P2),
  ppl_delete_Polyhedron(P2),
  ppl_Polyhedron_space_dimension(P, 1),
  ppl_delete_Polyhedron(P),
  ppl_new_Polyhedron_from_dimension(T, 2, Ptacas),
  ppl_Polyhedron_add_constraints(Ptacas, [A >= 1, A =< 3, B >= 7, B =< 12]),
  ppl_Polyhedron_fold_dimensions(Ptacas, [A], B),
  ppl_Polyhedron_space_dimension(Ptacas, 1),
  ppl_new_Polyhedron_from_dimension(T, 1, Ptacas1),
  ppl_Polyhedron_add_constraints(Ptacas1, [A >= 1, A =< 12]),
  ppl_Polyhedron_equals_Polyhedron(Ptacas, Ptacas1),
  ppl_delete_Polyhedron(Ptacas1),
  ppl_delete_Polyhedron(Ptacas).

% Tests ppl_Polyhedron_map_dimensions/2.
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

%%%%%%%%%%%%%%%%%% Polyhedral Relations %%%%%%%%%%%%%%%%%%%%%%%%%%

% Tests ppl_Polyhedron_relation_with_constraint/3.
rel_cons :-
  make_vars(3, [A, B, C]),
  rel_cons(c, [A >= 1, B >= 0, C = 0], [A, B, C]),
  rel_cons(nnc, [A > 1, B >= 0, C = 0], [A, B, C]).

rel_cons(T, CS, [A, B, C]) :-
  ppl_new_Polyhedron_from_dimension(T, 3, P),
  ppl_Polyhedron_add_constraints(P, CS),
  \+ ppl_Polyhedron_relation_with_constraint(P, A = 0, x),
  ppl_Polyhedron_relation_with_constraint(P, A = 0, R),
  R = [is_disjoint],
  ppl_Polyhedron_relation_with_constraint(P, B = 0, R1),
  R1 = [strictly_intersects],
  ppl_Polyhedron_relation_with_constraint(P, A >= 0, R2),
  R2 = [is_included],
  ppl_Polyhedron_relation_with_constraint(P, C >= 0, R3),
  (R3 = [is_included, saturates] ; R3 = [saturates, is_included]),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_relation_with_generator/3.
rel_gens :-
  make_vars(3, [A, B, C]),
  rel_gens(c, [point(A + B + C), ray(A)], [A, B, C]),
  rel_gens(nnc, [point(A + B + C), ray(A)], [A, B, C]).

rel_gens(T, GS, [A, _, _]) :-
  ppl_new_Polyhedron_empty_from_dimension(T, 3, P),
  ppl_Polyhedron_add_generators(P, GS),
  \+ppl_Polyhedron_relation_with_generator(P, point(A), x),
  ppl_Polyhedron_relation_with_generator(P, point(A), R),
  R = [],
  ppl_Polyhedron_relation_with_generator(P, ray(A), R1),
  R1 = [subsumes],
  ppl_delete_Polyhedron(P).

%%%%%%%%%%%%%%%%%% Check Properties %%%%%%%%%%%%%%%%%%%%%%%%%%

%  tests ppl_Polyhedron_is_universe/1,
%        ppl_Polyhedron_is_empty/1,
%        ppl_Polyhedron_is_bounded/1,
%        ppl_Polyhedron_is_topologically_closed/1.
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

% Tests ppl_Polyhedron_contains_Polyhedron/2.
contains :-
  contains(c), contains(nnc).

contains(T) :-
  ppl_new_Polyhedron_from_dimension(T, 3, P1),
  ppl_new_Polyhedron_empty_from_dimension(T, 3, P2),
  ppl_Polyhedron_contains_Polyhedron(P1, P2),
  \+ppl_Polyhedron_contains_Polyhedron(P2, P1),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

% Tests ppl_Polyhedron_strictly_contains_Polyhedron for C/2.
strict_contains :-
  strict_contains(c), strict_contains(nnc).

strict_contains(T) :-
  ppl_new_Polyhedron_from_dimension(T, 3, P1),
  ppl_new_Polyhedron_empty_from_dimension(T, 3, P2),
  ppl_Polyhedron_strictly_contains_Polyhedron(P1, P2),
  \+ppl_Polyhedron_strictly_contains_Polyhedron(P1, P1),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

% Tests ppl_Polyhedron_is_disjoint_from_Polyhedron/2.
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

% Tests ppl_Polyhedron_equals_Polyhedron/2.
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

% Tests ppl_Polyhedron_OK/1.
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

%%%%%%%%%%%%%%%%%%%%%%%%% Polyhedron Bounding Values %%%%%%%%%%%%%%%%%%%%%%%

% Tests ppl_Polyhedron_get_bounding_box/3.

get_bounding_box:-
  make_vars(2, [A, B]),
  get_bounding_box(c, [B >= 0, 4*A =< 2],
                     [i(o(minf), c(1/2)), i(c(0), o(pinf))]),
  get_bounding_box(c, [], [i(o(minf), o(pinf)), i(o(minf), o(pinf))]),
  get_bounding_box(c, [1=0], [empty, empty]),
  get_bounding_box(c, [A =< 4, B =< 4, 3*A + B >= 2],
                     [i(c(-2/3), c(4)), i(c(-10), c(4))]),
  get_bounding_box(nnc, [B > 0, 4*A =< 2],
                     [i(o(minf), c(1/2)), i(o(0), o(pinf))]),
  get_bounding_box(nnc,[A > 1, B > 1, A < 1, B < 1], [empty, empty]),
  get_bounding_box(nnc, [A =< 4, B =< 4, 3*A + B > 2],
                     [i(o(-2/3), c(4)), i(o(-10), c(4))]).

get_bounding_box(T, CS, Box) :-
  ppl_new_Polyhedron_from_dimension(T, 2, P),
  ppl_Polyhedron_add_constraints(P, CS),
  \+ppl_Polyhedron_get_bounding_box(P, a, Box),
  \+ppl_Polyhedron_get_bounding_box(P, any, box),
  ppl_Polyhedron_get_bounding_box(P, any, Box),
  ppl_Polyhedron_get_bounding_box(P, polynomial, Box1),
  ppl_Polyhedron_get_bounding_box(P, simplex, Box2),
  ppl_new_Polyhedron_from_bounding_box(T, Box, P1),
  ppl_new_Polyhedron_from_bounding_box(T, Box1, P2),
  ppl_new_Polyhedron_from_bounding_box(T, Box2, P3),
  ppl_Polyhedron_contains_Polyhedron(P1, P),
  ppl_Polyhedron_contains_Polyhedron(P2, P1),
  ppl_Polyhedron_contains_Polyhedron(P3, P1),
  ppl_delete_Polyhedron(P),
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2),
  ppl_delete_Polyhedron(P3).

% Tests ppl_Polyhedron_bounds_from_above/2.
bounds_from_above :-
  make_vars(2, [A, B]),
  bounds_from_above(c, [A >= 1, B >= 0], [B =< 2], B),
  bounds_from_above(nnc, [A > 1, B > 0, B < 1], [A < 2], A).

bounds_from_above(T, CS1, CS2, Var) :-
  ppl_new_Polyhedron_from_constraints(T, CS1, P),
  \+ ppl_Polyhedron_bounds_from_above(P, Var),
  ppl_Polyhedron_add_constraints(P, CS2),
  ppl_Polyhedron_bounds_from_above(P, Var),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_bounds_from_below/2.
bounds_from_below :-
  make_vars(2, [A, B]),
  bounds_from_below(c, [B >= 0, B =< 1], [A >= 1], A),
  bounds_from_below(nnc, [B > 0, B < 1], [A > 2], A).

bounds_from_below(T, CS1, CS2, Var) :-
  ppl_new_Polyhedron_from_constraints(T, CS1, P),
  \+ ppl_Polyhedron_bounds_from_below(P, Var),
  ppl_Polyhedron_add_constraints(P, CS2),
  ppl_Polyhedron_bounds_from_below(P, Var),
  ppl_delete_Polyhedron(P).

%%%%%%%%%%%%%%%%%%%%%%%%% Maximize and Minimize %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tests ppl_Polyhedron_maximize/5.
maximize :-
  make_vars(2, [A, B]),
  maximize(c, [A >= -1, A =< 1, B >= -1, B =< 1], A + B, 2, 1, true),
  maximize(nnc, [A > -1, A < 1, B > -1, B < 1], A + B -1, 1, 1, false).

maximize(T, CS, LE, N, D, Max) :-
  ppl_new_Polyhedron_from_constraints(T, CS, P),
  ppl_Polyhedron_maximize(P, LE, N, D, Max),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_maximize_with_point/5.
maximize_with_point :-
  make_vars(2, [A, B]),
  maximize_with_point(c, [A >= -1, A =< 1, B >= -1, B =< 1],
                                        A + B, 2, 1, true, point(A+B)),
  maximize_with_point(c, [A =< 0],
                                        A, 0, 1, true, point(0)),
  maximize_with_point(c, [A >= 0],
                                        A, 0, 0, _, _),
  maximize_with_point(nnc, [A > -1, A < 1, B > -1, B < 1],
                                        A + B -1, 1, 1, false, point(A+B)).

maximize_with_point(T, CS, LE, N, D, Max, Point) :-
  ppl_new_Polyhedron_from_constraints(T, CS, P),
  (D > 0
   ->
    (ppl_Polyhedron_maximize_with_point(P, LE, N, D, Max, PointMax),
    (PointMax = closure_point(E) ; PointMax = point(E)),
    ppl_new_Polyhedron_from_generators(T, [point(E)], Pm),
    ppl_new_Polyhedron_from_generators(T, [Point], Qm),
    ppl_Polyhedron_equals_Polyhedron(Pm, Qm),
    ppl_delete_Polyhedron(Pm),
    ppl_delete_Polyhedron(Qm))
   ;
    \+ ppl_Polyhedron_maximize_with_point(P, LE, _, _, _, _)
  ),
  ppl_delete_Polyhedron(P).


% Tests ppl_Polyhedron_minimize/5.
minimize :-
  make_vars(2, [A, B]),
  minimize(c, [A >= -1, A =< 1, B >= -1, B =< 1], A + B, -2, 1, true),
  minimize(nnc, [A > -2, A =< 2, B > -2, B =< 2], A + B + 1, -3, 1, false).

minimize(T, CS, LE, N, D, Min) :-
  ppl_new_Polyhedron_from_constraints(T, CS, P),
  ppl_Polyhedron_minimize(P, LE, N, D, Min),
  ppl_delete_Polyhedron(P).

% Tests ppl_Polyhedron_minimize_with_point/5.
minimize_with_point :-
  make_vars(2, [A, B]),
  minimize_with_point(c, [A >= -1, A =< 1, B >= -1, B =< 1],
                                        A + B, -2, 1, true, point(-A-B)),
  minimize_with_point(c, [A >= 0],
                                        A, 0, 1, true, point(0)),
  minimize_with_point(c, [A =< 0],
                                        A, 0, 0, _, _),
  minimize_with_point(nnc, [A > -2, A =< 2, B > -2, B =< 2],
                                        A + B, -4, 1, false, point(-2*A-2*B)).

minimize_with_point(T, CS, LE, N, D, Min, Point) :-
  ppl_new_Polyhedron_from_constraints(T, CS, P),
  (D > 0
   ->
    (ppl_Polyhedron_minimize_with_point(P, LE, N, D, Min, PointMin),
    (PointMin = closure_point(E) ; PointMin = point(E)),
    ppl_new_Polyhedron_from_generators(T, [point(E)], Pm),
    ppl_new_Polyhedron_from_generators(T, [Point], Qm),
    ppl_Polyhedron_equals_Polyhedron(Pm, Qm),
    ppl_delete_Polyhedron(Pm),
    ppl_delete_Polyhedron(Qm))
   ;
    \+ ppl_Polyhedron_minimize_with_point(P, LE, _, _, _, _)
  ),
  ppl_delete_Polyhedron(P).

%%%%%%%%%%%%%%%%% Watchdog tests %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tests Watchdog predicates
% ppl_set_timeout/1
% ppl_set_timeout_exception_atom/1
% ppl_timeout_exception_atom/1
% ppl_reset_timeout/0
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

% time_watch(+Topology, +Goal, +NoTimeOut, +TimeOut)
% time_watch/4 makes a copy of Goal with a copy of the polyhedron
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
       (ppl_reset_timeout,
        ppl_Polyhedron_swap(Poly, PolyCopy),
        call(NoTimeOut))
     ;
       call(TimeOut)
   ),
   ppl_delete_Polyhedron(PolyCopy).

%%%%%%%%%%%%%%%%% Exceptions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% exceptions/0 tests both Prolog and C++ exceptions using:
%
% exception_prolog(+N, +V)
% exception_sys_prolog(+N, +V)
% exception_cplusplus(+N, +V)
%
% N is the number of the test while V is a list of 3 PPL variables
%
% In exceptions/0, the calls to these predicates should fail
% so that all the tests are tried on backtracking.
% When all the tests have been tried,
% (and, for the Prolog interface, providing the correct
% exception message),
% the call to exceptions/0 succeeds.
% If one of the tests succeeds or a Prolog interface exception
% has a wrong exception message, then exceptions/0 will fail.

exceptions :-
   current_prolog_flag(bounded, Y),
   make_vars(3, V),
   exception_prolog(V),
   (Y == true -> exception_sys_prolog(V) ; true),
   exception_cplusplus(V),
   !.

% exception_prolog(+N, +V) checks exceptions thrown by the Prolog interface.
% It does not check those that are dependent on a specific Prolog system.

exception_prolog(V) :-
   exception_prolog1(8, V).

exception_prolog1(0, _) :- !.
exception_prolog1(N, V) :-
   exception_prolog(N, V),
   N1 is N - 1,
   exception_prolog1(N1, V).

%% TEST: Prolog_unsigned_out_of_range
exception_prolog(1, _) :-
   (current_prolog_flag(bounded, false)
    ->
     (I = 21474836470,
     catch(ppl_new_Polyhedron_from_generators(_, [point('$VAR'(I))], _),
          M, 
         check_exception(M)
         )
      )
    ;
   true
   ).

%% TEST: not_unsigned_integer
exception_prolog(2, _) :-
  catch(ppl_new_Polyhedron_from_dimension(c, n, _),
          M, 
          check_exception(M)
        ).

%% TEST: not_unsigned_integer
exception_prolog(3, _) :-
  catch(ppl_set_timeout(-1),
          M, 
          check_exception(M)
        ).

%% TEST: non_linear
exception_prolog(4, [A,B,C]) :-
  catch(ppl_new_Polyhedron_from_generators(c, [point(B + A*C)], _),
          M, 
         check_exception(M)
        ).

%% TEST: not_a_variable
exception_prolog(5, [A,_,_]) :-
  ppl_new_Polyhedron_from_dimension(c, 3, P),
  catch(ppl_Polyhedron_remove_dimensions(P, [A,1]),
          M, 
         (cleanup_ppl_Polyhedron(P),
          check_exception(M))
        ),
  !,
  ppl_delete_Polyhedron(P).

%% TEST: not_an_integer
exception_prolog(6, [A,B,_]) :-
  ppl_new_Polyhedron_from_generators(c, 
               [point(A + B), ray(A), ray(B)], P),
  catch(ppl_Polyhedron_affine_image(P, A, A + B + 1, i),
          M, 
         (cleanup_ppl_Polyhedron(P),
          check_exception(M))
        ),
  !,
  ppl_delete_Polyhedron(P).

%% TEST: not_a_polyhedron_kind
exception_prolog(7, [A,B,C]) :-
   catch(ppl_new_Polyhedron_from_generators(_, [point(A + B + C, 1)], _),
          M, 
         check_exception(M)
        ).

%% TEST: not_a_polyhedron_handle
exception_prolog(8, _) :-
  catch(ppl_Polyhedron_space_dimension(_, _N),
          M, 
          check_exception(M)
        ).

% exception_sys_prolog(+N, +V) checks exceptions thrown by Prolog interfaces
% that are dependent on a specific Prolog system.
% These are only checked if current_prolog_flag(bounded, false) holds. 

exception_sys_prolog(V) :-
   exception_sys_prolog1(4, V).

exception_sys_prolog1(0, _) :- !.
exception_sys_prolog1(N, V) :-
   exception_sys_prolog(N, V),
   N1 is N - 1,
   exception_sys_prolog1(N1, V).

exception_sys_prolog(1, [A,B,_]) :-
  current_prolog_flag(max_integer, MaxInt),
  ppl_new_Polyhedron_from_constraints(c, [MaxInt * A - B >= 0, 3 >= A], P),
  catch(ppl_Polyhedron_get_generators(P, _),
          M, 
         (cleanup_ppl_Polyhedron(P),
          check_exception(M))
       ),
  !,
  ppl_delete_Polyhedron(P).

 exception_sys_prolog(2, [A,B,_]) :-
  current_prolog_flag(min_integer, MinInt),
  ppl_new_Polyhedron_from_constraints(c, [MinInt * A - B =< 0, 2 >= A], P),
  catch(ppl_Polyhedron_get_generators(P, _),
          M, 
         (cleanup_ppl_Polyhedron(P),
          check_exception(M))
       ),
  ppl_delete_Polyhedron(P).

exception_sys_prolog(3, [A,B,_]) :-
  current_prolog_flag(max_integer, MaxInt),
  ppl_new_Polyhedron_from_generators(c, 
               [point(MaxInt * A + B)], P),
  ppl_Polyhedron_affine_image(P, A, A + 1, 1),
  catch(ppl_Polyhedron_get_generators(P, _),
          M, 
         (cleanup_ppl_Polyhedron(P),
          check_exception(M))
        ),
  !,
  ppl_delete_Polyhedron(P).

exception_sys_prolog(4, [A,B,_]) :-
  current_prolog_flag(min_integer, MinInt),
  ppl_new_Polyhedron_from_generators(c, 
               [point(MinInt * A + B)], P),
  ppl_Polyhedron_affine_image(P, A, A - 1, 1),
  catch(ppl_Polyhedron_get_generators(P, _GS),
          M, 
         (cleanup_ppl_Polyhedron(P),
          check_exception(M))
        ),
  !,
  ppl_delete_Polyhedron(P).

% exception_cplusplus(+N, +V) checks exceptions thrown by the C++
% interface for the PPL.

exception_cplusplus(V) :-
   exception_cplusplus1(10, V).

exception_cplusplus1(0, _) :- !.
exception_cplusplus1(N, V) :-
   exception_cplusplus(N, V),
   N1 is N - 1,
   exception_cplusplus1(N1, V).

exception_cplusplus(1, [A,B,C]) :-
  catch(ppl_new_Polyhedron_from_generators(C, [point(A + B + C, 0)], _),
          M, 
         (check_exception([M]))
         ).

exception_cplusplus(2, [A,B,_]) :-
  ppl_new_Polyhedron_from_generators(c, 
               [point(A + B), ray(A), ray(B)], P),
  catch(ppl_Polyhedron_affine_image(P, A, A + B + 1, 0),
          M, 
         (cleanup_ppl_Polyhedron(P),
          check_exception([M]))
        ),
  !,
  ppl_delete_Polyhedron(P).

exception_cplusplus(3, [A, B, _]) :-
  ppl_new_Polyhedron_from_dimension(c, 0, P1),
  ppl_new_Polyhedron_from_generators(c, 
               [point(A + B)], P2),
  catch(ppl_Polyhedron_poly_hull_assign_and_minimize(P1, P2),
          M, 
         (cleanup_ppl_Polyhedra([P1,P2]),
          check_exception([M]))
        ),
  !,
  ppl_delete_Polyhedron(P1),
  ppl_delete_Polyhedron(P2).

exception_cplusplus(4, [A,B,C]) :-
   catch(ppl_new_Polyhedron_from_generators(c, [line(A + B + C)], _),
          M, 
         (check_exception([M]))
        ).

exception_cplusplus(5, [A,B,C]) :-
  ppl_new_Polyhedron_from_generators(c, [point(B + 2*C)], P),
  ppl_Polyhedron_remove_dimensions(P,[C]),
  catch(ppl_Polyhedron_remove_dimensions(P,[A,C]),
          M, 
         (cleanup_ppl_Polyhedron(P),
          check_exception([M]))
        ),
  !,
  ppl_delete_Polyhedron(P).

exception_cplusplus(6, [A,B,_]) :-
  ppl_new_Polyhedron_from_constraints(c, 
               [A >= 1], P),
  catch(ppl_Polyhedron_affine_image(P, B, A + 1, 1),
          M, 
         (check_exception([M]))
        ),
  !,
  ppl_delete_Polyhedron(P).

exception_cplusplus(7, [A, B, C]) :-
  ppl_new_Polyhedron_from_constraints(c, 
               [A >= 1, B>= 1], P),
  catch(ppl_Polyhedron_affine_image(P, B, A + C + 1, 1),
          M, 
         (check_exception([M]))
        ),
  !,
  ppl_delete_Polyhedron(P).

exception_cplusplus(8, [A,B,_]) :-
  ppl_new_Polyhedron_from_constraints(c, 
               [A >= B], P),
  catch(ppl_Polyhedron_affine_preimage(P, A, A + B + 1, 0),
          M, 
         (check_exception([M]))
        ),
  !,
  ppl_delete_Polyhedron(P).

exception_cplusplus(9, [A, B, C]) :-
  ppl_new_Polyhedron_from_generators(c, 
               [point(0), ray(A + B), ray(A)], P),
  catch(ppl_Polyhedron_affine_preimage(P, C, A + 1, 1),
          M, 
         (cleanup_ppl_Polyhedron(P),
          check_exception([M]))
        ),
  !,
  ppl_delete_Polyhedron(P).


exception_cplusplus(10, [A, B, C]) :-
  ppl_new_Polyhedron_from_generators(c, 
               [point(0), point(A), line(A + B)], P),
  catch(ppl_Polyhedron_affine_preimage(P, B, A + C, 1),
          M, 
         (cleanup_ppl_Polyhedron(P),
          check_exception([M])
          )
        ),
  !,
  ppl_delete_Polyhedron(P).

% check_exception(+Exception) checks and prints the exception message;
% if the message is ok, then after printing it fails;
% otherwise it just succeeds.

check_exception(Exception):-
         (call(format_exception_message(Exception)) ->
                true ; fail).

%%%%%%%%%%%% predicate for making list of ppl variables %%%%%%

% make_var_list(+I,+Dimension,?VariableList)
% constructs a list of variables with indices from I to Dimension - 1.
% It is assumed that I =< Dimension.

make_vars(Dim, VarList):-
  make_var_list(0, Dim, VarList).
make_var_list(Dim,Dim,[]):- !.
make_var_list(I,Dim,['$VAR'(I)|VarList]):-
  I1 is I + 1,
  make_var_list(I1,Dim,VarList).

%%%%%%%%%%%% predicate for safely deleting polyhedra on failure %

cleanup_ppl_Polyhedron(_).
cleanup_ppl_Polyhedron(P) :-
  ppl_delete_Polyhedron(P), fail.

cleanup_ppl_Polyhedra([]).
cleanup_ppl_Polyhedra([_|_]).
cleanup_ppl_Polyhedra([P|Ps]) :-
  delete_all_ppl_Polyhedra([P|Ps]).

delete_all_ppl_Polyhedra([]).
delete_all_ppl_Polyhedra([P|Ps]) :-
  ppl_delete_Polyhedron(P),
  delete_all_ppl_Polyhedra(Ps).

%%%%%%%%%%%% predicates for switching on/off output messages %

make_noisy :-
  (retract(noisy(_)) ->
      make_noisy
  ;
      assertz(noisy(1))
  ).

make_quiet :-
  (retract(noisy(_)) ->
      make_quiet
   ; assertz(noisy(0))
  ).

%%%%%%%%%%%% predicates for pretty printing the PPL banner %%%%%%%%%%
%
% The banner is read as an atom with"/n" denoting where there should
% new lines. Here we print the banner as intended with new lines instead
% of "/n".
%

banner_pp(B) :-
  name(B,Bcodes),
  nl,
  !,
  format_banner(Bcodes).

format_banner([]) :- nl.
format_banner([C]) :- put_code(C), nl.
format_banner([C,C1|Chars]):-
  ([C,C1] == "/n" ->
     (nl,
     format_banner(Chars))
   ;
     (put_code(C),
     format_banner([C1|Chars]))
  ).

%%%%%%%%%%%% predicate for printing exception messages %%%%%%%%%%

format_exception_message(
             ppl_invalid_argument( found(F), expected(E), where(W))
                        ) :-
  !,
  display_message(['PPL Prolog Interface Exception: ', nl, '   ',
                   F, 'is an invalid argument for', W, nl, '   ',
                  F, 'should be', E, '.']).

format_exception_message(
             ppl_representation_error(I, where(W))
                        ) :-
  !,
  display_message(['PPL Prolog Interface Exception: ', nl, '   ',
                   'This Prolog system has bounded integers', nl, '   ',
                   I, 'is not in the allowed range of integers', nl, '   ',
                   'in call to', W, '.']).

format_exception_message(Error) :-
  display_message([Error]).

%%%%%%%%%%%% predicates for output messages %%%%%%%%%%%%%%%%%%

error_message(Message):-
   write_all(Message),
   fail.

display_message(Message):-
   noisy(1), !,
   nl, write_all(Message).
display_message(_).

write_all([]) :- nl.
write_all([Phrase|Phrases]):-
   (Phrase == nl ->
      nl
   ; 
      (write(Phrase),
      write(' '))
   ),
   write_all(Phrases).

