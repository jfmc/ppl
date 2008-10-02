/* Various tests on the Prolog interface.
   Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */


% noisy(F)
% When F = 1, a message is displayed if a time out occurs
% when running the `timeout'` predicate.
% Also, the values of the PPL versions and banner are displayed.
% When F = 0, no 'time out' message or versions are displayed.
% When F = 2, if a test fails and the backtracking returns to a polyhedron
% constructor, the caught error will cause the constraint and generator systems
% for the polyhedron to be displayed.
% noisy/1 can be reset by calling make_noisy/0 or make_quiet/0.

:- dynamic(noisy/1).

% check_all
% This executes all the test predicates which, together, check all
% the ppl interface predicates.

check_all :-
   (noisy(_) -> true; make_quiet),
   list_groups(Groups),
   catch(run_all(Groups), Exception,
       (print_exception_term(Exception), fail)).

% check_quiet
% This alo executes all the test predicates with no output.

check_quiet :-
   make_quiet,
   check_all.

% check_noisy
% This also executes all the test predicates but also prints some messages
% including the banner, version numbers and expected output from
% the exception tests.

check_noisy :-
   make_noisy,
   check_all.

check_extra_noisy :-
   make_extra_noisy,
   check_all.

run_all([Group|Groups]):-
   ppl_initialize,
   (catch(run_one(Group), Exception,
         run_exception(Group, Exception)) -> true ; run_fail(Group)),
   !,
   ppl_finalize,
   run_all(Groups).

run_all([]).

run_all([_|_]) :-
   error_message(['Prolog interface checks failed.']),
   !,
   ppl_finalize,
   fail.

run_fail(Group) :-
   group_predicates(Group, Predicates),
   error_message(['Error occurred while performing test', Group,
              'which checks predicates:', nl, Predicates]),
   !,
   ppl_finalize,
   fail.

run_exception(Group, ppl_overflow_error(Cause)) :-
   !,
   group_predicates(Group, Predicates),
   display_message(
            ['Overflow exception occurred while performing test ', Group,
              'which checks predicates ', nl, Predicates]),
   print_exception_term(ppl_overflow_error(Cause)).

run_exception(Group, Exception) :-
   group_predicates(Group, Predicates),
   display_message(
            ['Exception occurred while performing test ', Group,
              'which checks predicates ', nl, Predicates]),
   print_exception_term(Exception),
   fail.

% Tests predicates that return PPL version information and the PPL banner.
% If noisy(0) holds, there is no output but if not,
% all the versions are printed and the banner is pretty printed.
run_one(all_versions_and_banner) :-
  \+ ppl_version_major(-1),
  ppl_version_major(Vmajor),
  ppl_version_minor(Vminor),
  ppl_version_revision(Vrevision),
  ppl_version_beta(Vbeta),
  ppl_version(V),
  ppl_banner(B),
  (noisy(0) -> true ;
     (
      nl,
      write('Version major is '), write(Vmajor), nl,
      write('Version minor is '), write(Vminor), nl,
      write('Version revision is '), write(Vrevision), nl,
      write('Version beta is '), write(Vbeta), nl,
      write('Version is '), write(V), nl,
      banner_pp(B), nl
     )
  ).

% Tests predicates that return the maximum allowed dimension and coefficients.
% If noisy(0) holds, there is no output but if not, the maximums/miniumums
% are printed.

run_one(grid_from_dimension) :-
  new_grid_from_dim,
  (noisy(0) -> true ;
     display_message(['grid_from_dimension ok'])
  ).

run_one(grid_from_grid) :-
  new_grid_from_grid,
  (noisy(0) -> true ;
     display_message(['grid_from_grid ok'])
  ).

run_one(grid_from_representation) :-
  new_grid_from_congs,
  new_grid_from_gens,
  new_grid_from_bounding_box,
  new_grid_from_covering_box,
  (noisy(0) -> true ;
     display_message(['grid_from_representation ok'])
  ).

run_one(swap_grids) :-
  grid_swap,
  (noisy(0) -> true ;
     display_message(['swap_grids ok'])
  ).

run_one(grid_dimension) :-
   grid_space_dim,
   grid_affine_dim,
  (noisy(0) -> true ;
     display_message(['grid_dimension ok'])
  ).

run_one(grid_basic_operators) :-
   grid_inters_assign,
   grid_join_assign,
   grid_join_assign,
   grid_time_elapse,
   grid_top_close_assign,
  (noisy(0) -> true ;
     display_message(['grid_basic_operators ok'])
  ).

run_one(grid_add_to_system) :-
   grid_add_con,
   grid_add_gen,
   grid_add_cons,
   grid_add_gens,
  (noisy(0) -> true ;
     display_message(['grid_add_to_system ok'])
  ).

run_one(grid_revise_dimensions) :-
   grid_project,
   grid_embed,
   grid_conc_assign,
   grid_remove_dim,
   grid_remove_high_dim,
   grid_expand_dim,
   grid_map_dim,
   grid_fold_dims,
  (noisy(0) -> true ;
     display_message(['grid_revise_dimensions ok'])
  ).

run_one(grid_transform) :-
   grid_affine_image,
   grid_affine_preimage,
   grid_affine_image_gen,
   grid_affine_preimage_gen,
   grid_affine_image_genlr,
   grid_affine_preimage_genlr,
  (noisy(0) -> true ;
     display_message(['transform_grid ok'])
  ).

run_one(grid_get_system) :-
   grid_get_cons,
   grid_get_min_cons,
   grid_get_gens,
   grid_get_min_gens,
  (noisy(0) -> true ;
     display_message(['grid_get_system ok'])
  ).

run_one(check_grid) :-
   grid_rel_cons,
   grid_rel_gens,
   grid_checks,
   grid_bounds_from_above,
   grid_bounds_from_below,
  (noisy(0) -> true ;
     display_message(['check_grids ok'])
  ).

run_one(minmax_grid) :-
   grid_maximize,
   grid_minimize,
   grid_maximize_with_point,
   grid_minimize_with_point,
  (noisy(0) -> true ;
     display_message(['minmax_grid ok'])
  ).

run_one(compare_grids) :-
   grid_contains,
   grid_strict_contains,
   grid_disjoint_from,
   grid_equals,
   grid_ok,
  (noisy(0) -> true ;
     display_message(['compare_grids ok'])
  ).

run_one(grid_get_boxes) :-
   grid_get_bounding_box,
   grid_get_covering_box,
  (noisy(0) -> true ;
     display_message(['grid_get_boxes ok'])
  ).

run_one(grid_extrapolation_operators) :-
   grid_widen_congruence,
   grid_widen_congruence_with_tokens,
   grid_lim_extrapolate_congruence,
   grid_lim_extrapolate_congruence_with_tokens,
   grid_widen_generator,
   grid_widen_generator_with_tokens,
   grid_lim_extrapolate_generator,
   grid_lim_extrapolate_generator_with_tokens,
  (noisy(0) -> true ;
     display_message(['grid_extrapolation_operators ok'])
  ).

run_one(grid_handle_exceptions) :-
   grid_exceptions,
  (noisy(0) -> true ;
     display_message(['grid_exceptions ok'])
  ).

%%%%%%%%%%%%%%%%% New Grid %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Note that throughout the tests, all "new_Grid_from_...(...,P)" calls
% are made in such a way that, if the test fails, P is deleted.
% This is done by using special "clean_ppl_new_Grid_from_...(...,P)"
% forms of the predicates that are defined later.
%
% As we also delete P on success of the test, to prevent trying to
% delete P again when a later test fails, we always have a cut before these
% in-line calls to ppl_Grid_delete(P).

% Tests new_Grid_from_space_dimension/3 and
%       ppl_delete_Grid/1.
new_grid_from_dim :-
  new_grid_from_dim(universe),
  new_grid_from_dim(empty).

% This also uses ppl_Grid_is_universe/1
% and ppl_Grid_is_empty.
new_grid_from_dim(Universe_Or_Empty) :-
  \+ clean_ppl_new_Grid_from_space_dimension(3, Universe_Or_Empty, 0),
  clean_ppl_new_Grid_from_space_dimension(3, Universe_Or_Empty, P),
  (Universe_Or_Empty = universe ->
      (ppl_Grid_is_universe(P),
      \+ ppl_Grid_is_empty(P))
   ;
      (ppl_Grid_is_empty(P),
      \+ ppl_Grid_is_universe(P))
  ),
  !,
  ppl_delete_Grid(P).

% Tests ppl_new_Grid_from_Grid/2,
% This also uses ppl_new_Grid_from_congruences/2 and
% ppl_Grid_equals_Grid/2.
new_grid_from_grid :-
  clean_ppl_new_Grid_from_space_dimension(3, universe, P1),
  \+ clean_ppl_new_Grid_from_Grid(P1, 0),
  clean_ppl_new_Grid_from_Grid(P1, P2),
  clean_ppl_new_Grid_from_Grid(P2, P1a),
  ppl_Grid_equals_Grid(P1, P1a),
  clean_ppl_new_Grid_from_Grid(P1a, P2a),
  ppl_Grid_equals_Grid(P2, P2a),
  !,
  ppl_delete_Grid(P1),
  ppl_delete_Grid(P1a),
  ppl_delete_Grid(P2),
  ppl_delete_Grid(P2a),
  make_vars(3, [A, B, C]),
  CS = [3 =:= A, 4*A + B - 2*C =:= 5],
  clean_ppl_new_Grid_from_congruences(CS, P3),
  clean_ppl_new_Grid_from_Grid(P3, P4),
  clean_ppl_new_Grid_from_Grid(P4, P3a),
  clean_ppl_new_Grid_from_Grid(P3a, P4a),
  ppl_Grid_equals_Grid(P3, P3a),
  ppl_Grid_equals_Grid(P4, P4a),
  !,
  ppl_delete_Grid(P3),
  ppl_delete_Grid(P4),
  ppl_delete_Grid(P3a),
  ppl_delete_Grid(P4a).

% Tests ppl_new_Grid_from_congruences/2.
new_grid_from_congs :-
  make_vars(4, [A, B, C, D]),
  new_grid_from_congs([3 =:= A, 4*A + B - 2*C =:= 5, (D =:= 1) / 0]),
  new_grid_from_congs([B =:= A, (4*A + B - 2*C =:= 5) / 2, D = 1]).

new_grid_from_congs(CS) :-
  clean_ppl_new_Grid_from_congruences([], P),
  \+ clean_ppl_new_Grid_from_congruences([], 0),
  ppl_Grid_is_universe(P),
  clean_ppl_new_Grid_from_congruences(CS, Pa),
  \+ ppl_Grid_is_universe(Pa),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(Pa).

% Tests ppl_new_Grid_from_constraints/2.
new_grid_from_conss :-
  make_vars(4, [A, B, C, D]),
  new_grid_from_conss([3 = A, 4*A + B - 2*C = 5, D >= 1]),
  new_grid_from_conss([B = A, 4*A + B - 2*C =< 5, D = 1]).

new_grid_from_conss(CS) :-
  make_vars(1, [A]),
  clean_ppl_new_Grid_from_constraints([], P),
  ppl_Grid_is_universe(P),
  \+ clean_ppl_new_Grid_from_constraints([], 0),
  clean_ppl_new_Grid_from_constraints([A = 0, A = 1], P1),
  ppl_Grid_is_empty(P1),
  clean_ppl_new_Grid_from_constraints(CS, P2),
  \+ ppl_Grid_is_universe(P2),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(P1),
  ppl_delete_Grid(P2).

% Tests ppl_new_Grid_from_generators/2 and
%       ppl_new_Grid_from_generators/2.
new_grid_from_gens :-
  make_vars(3, [A, B, C]),
  new_grid_from_gens([grid_point(A + B + C, 1), grid_point(A + B + C)] ).

new_grid_from_gens(GS) :-
  \+ clean_ppl_new_Grid_from_generators([], 0),
  clean_ppl_new_Grid_from_generators([], P),
  ppl_Grid_is_empty(P),
  clean_ppl_new_Grid_from_generators(GS, Pa),
  \+ ppl_Grid_is_empty(Pa),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(Pa).

% Tests ppl_new_Grid_from_bounding_box/3.
new_grid_from_bounding_box :-
  clean_ppl_new_Grid_from_bounding_box([empty], PEmpty),
  ppl_Grid_get_bounding_box(PEmpty, any, BoxEmpty1),
  clean_ppl_new_Grid_from_bounding_box(BoxEmpty1, PEmpty1),
  ppl_Grid_equals_Grid(PEmpty1, PEmpty),
  !,
  ppl_delete_Grid(PEmpty),
  ppl_delete_Grid(PEmpty1),
  clean_ppl_new_Grid_from_bounding_box(
         [i(o(minf), o(pinf)), i(c(-1/2), c(-1/2))], PUniverse
                                      ),
  ppl_Grid_get_bounding_box(PUniverse, any, BoxUniverse1),
  clean_ppl_new_Grid_from_bounding_box(BoxUniverse1, PUniverse1),
  ppl_Grid_equals_Grid(PUniverse1, PUniverse),
  !,
  ppl_delete_Grid(PUniverse),
  ppl_delete_Grid(PUniverse1),
  Box = [i(c(1/2), c(1/2)), i(c(-1/2), c(-1/2))],
  clean_ppl_new_Grid_from_bounding_box(Box, P),
  ppl_Grid_get_bounding_box(P, any, Box1),
  clean_ppl_new_Grid_from_bounding_box(Box1, P1),
  ppl_Grid_equals_Grid(P, P1),
  \+ clean_ppl_new_Grid_from_bounding_box(Box, 0),
  \+ clean_ppl_new_Grid_from_bounding_box(
             [i(x, c(1/2)), i(c(0), o(pinf))], _),
  \+ clean_ppl_new_Grid_from_bounding_box(
             [i(x(minf), c(1/2)), i(c(0), o(pinf))], _),
  \+ clean_ppl_new_Grid_from_bounding_box(
             [i(o(minf), c(1/2)), i(c(0), c(pinf))], _),
  \+ clean_ppl_new_Grid_from_bounding_box(
             [i(c(minf), c(1/2)), i(c(0), o(pinf))], _),
  \+ clean_ppl_new_Grid_from_bounding_box(
             [i(o(minf), c(inf)), i(c(0), o(pinf))], _),
  \+ clean_ppl_new_Grid_from_bounding_box(
             [i(c(minf), c(1+2)), i(c(0), o(pinf))], _),
  \+ clean_ppl_new_Grid_from_bounding_box(
             [i(c(minf), c(n/2)), i(c(0), o(pinf))], _),
  \+ clean_ppl_new_Grid_from_bounding_box(
             [i(c(minf), c(2/d)), i(c(0), o(pinf))], _),
  \+ clean_ppl_new_Grid_from_bounding_box(
             [i(c(minf), c(2/1)), i(c(n), o(pinf))], _),
  \+ clean_ppl_new_Grid_from_bounding_box(
             [i(e), i(c(n), o(pinf))], _),
  \+ clean_ppl_new_Grid_from_bounding_box(
             [i(c(minf), c(2/1), c(1)), i(c(n), o(pinf))], _),
  \+ clean_ppl_new_Grid_from_bounding_box(
             [x(c(minf), c(2/1)), i(c(n), o(pinf))], _),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(P1).


% Tests ppl_new_Grid_from_covering_box/2.
new_grid_from_covering_box :-
  clean_ppl_new_Grid_from_covering_box([i(c(-1/2), c(1/2)), empty], PEmpty),
  ppl_Grid_get_covering_box(PEmpty, BoxEmpty1),
  clean_ppl_new_Grid_from_covering_box(BoxEmpty1, PEmpty1),
  ppl_Grid_equals_Grid(PEmpty1, PEmpty),
  !,
  ppl_delete_Grid(PEmpty),
  ppl_delete_Grid(PEmpty1),
  clean_ppl_new_Grid_from_covering_box(
         [i(c(1), c(1)), i(c(-1/2), c(-1/2))], PUniverse
                                      ),
  ppl_Grid_get_covering_box(PUniverse, BoxUniverse1),
  clean_ppl_new_Grid_from_covering_box(BoxUniverse1, PUniverse1),
  ppl_Grid_equals_Grid(PUniverse1, PUniverse),
  !,
  ppl_delete_Grid(PUniverse),
  ppl_delete_Grid(PUniverse1),
  Box = [i(c(1/2), c(8)), i(c(-1/2), o(pinf))],
  clean_ppl_new_Grid_from_covering_box(Box, P),
  ppl_Grid_get_covering_box(P, Box1),
  clean_ppl_new_Grid_from_covering_box(Box1, P1),
  ppl_Grid_equals_Grid(P, P1),
  \+ clean_ppl_new_Grid_from_covering_box(Box, 0),
  \+ clean_ppl_new_Grid_from_covering_box(
             [i(x, c(1/2)), i(c(0), o(pinf))], _),
  \+ clean_ppl_new_Grid_from_covering_box(
             [i(x(minf), c(1/2)), i(c(0), o(pinf))], _),
  \+ clean_ppl_new_Grid_from_covering_box(
             [i(o(minf), c(1/2)), i(c(0), c(pinf))], _),
  \+ clean_ppl_new_Grid_from_covering_box(
             [i(c(minf), c(1/2)), i(c(0), o(pinf))], _),
  \+ clean_ppl_new_Grid_from_covering_box(
             [i(o(minf), c(inf)), i(c(0), o(pinf))], _),
  \+ clean_ppl_new_Grid_from_covering_box(
             [i(c(minf), c(1+2)), i(c(0), o(pinf))], _),
  \+ clean_ppl_new_Grid_from_covering_box(
             [i(c(minf), c(n/2)), i(c(0), o(pinf))], _),
  \+ clean_ppl_new_Grid_from_covering_box(
             [i(c(minf), c(2/d)), i(c(0), o(pinf))], _),
  \+ clean_ppl_new_Grid_from_covering_box(
             [i(c(minf), c(2/1)), i(c(n), o(pinf))], _),
  \+ clean_ppl_new_Grid_from_covering_box(
             [i(e), i(c(n), o(pinf))], _),
  \+ clean_ppl_new_Grid_from_covering_box(
             [i(c(minf), c(2/1), c(1)), i(c(n), o(pinf))], _),
  \+ clean_ppl_new_Grid_from_covering_box(
             [x(c(minf), c(2/1)), i(c(n), o(pinf))], _),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(P1).

%%%%%%%%%%%%%%%%% Swap Grids %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tests ppl_Grid_swap/2.
grid_swap :-
  clean_ppl_new_Grid_from_space_dimension(3, universe, P),
  clean_ppl_new_Grid_from_space_dimension(2, empty, Q),
  ppl_Grid_swap(P, Q),
  ppl_Grid_is_empty(P),
  ppl_Grid_is_universe(Q),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(Q).

%%%%%%%%%%%%%%%%%% Grid Dimension %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tests ppl_Grid_space_dimension/2.

grid_space_dim :-
  clean_ppl_new_Grid_from_space_dimension(3, universe, P),
  ppl_Grid_space_dimension(P, N),
  N = 3,
  \+ ppl_Grid_space_dimension(P, 4),
  clean_ppl_new_Grid_from_generators([], Q),
  ppl_Grid_space_dimension(Q, M),
  M == 0,
  clean_ppl_new_Grid_from_congruences([], Q1),
  ppl_Grid_space_dimension(Q1, M1),
  M1 == 0,
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(Q),
  ppl_delete_Grid(Q1).


% Tests ppl_Grid_affine_dimension/2.
grid_affine_dim :-
  clean_ppl_new_Grid_from_space_dimension(3, universe, P),
  ppl_Grid_affine_dimension(P, N),
  N == 3,
  \+ ppl_Grid_affine_dimension(P, 2),
  clean_ppl_new_Grid_from_generators([], Q),
  ppl_Grid_affine_dimension(Q, M),
  M == 0,
  clean_ppl_new_Grid_from_congruences([], Q1),
  ppl_Grid_affine_dimension(Q1, M1),
  M1 == 0,
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(Q),
  ppl_delete_Grid(Q1),
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_generators([grid_point(A), parameter(B)],
                                     P1),
  ppl_Grid_space_dimension(P1, 2),
  ppl_Grid_affine_dimension(P1, 1),
  clean_ppl_new_Grid_from_generators([grid_point(A + B, 2)],
                                     P2),
  ppl_Grid_space_dimension(P2, 2),
  ppl_Grid_affine_dimension(P2, 0),
  clean_ppl_new_Grid_from_congruences([A - B =:= 0, B =:= 0,
                                       (A + B =:= 1)/0],
                                      P3),
  ppl_Grid_space_dimension(P3, 2),
  ppl_Grid_affine_dimension(P3, 1),
  clean_ppl_new_Grid_from_congruences([A - B =:= 0, B =:= 1,
                                       A + B =:= 1],
                                      P4),
  ppl_Grid_add_congruence(P4, (2*B =:= 1)/2),
  ppl_Grid_space_dimension(P4, 2),
  ppl_Grid_affine_dimension(P4, 0),
  !,
  ppl_delete_Grid(P1),
  ppl_delete_Grid(P2),
  ppl_delete_Grid(P3),
  ppl_delete_Grid(P4).

%%%%%%%%%%%%%%%% Basic Operators %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tests ppl_Grid_intersection_assign/2.
grid_inters_assign :-
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_generators([grid_point(0), grid_point(B),
                                      grid_point(A), grid_point(A, 2)],
                                     P1),
  clean_ppl_new_Grid_from_generators([grid_point(0), grid_point(A),
                                      grid_point(A + B), grid_point(A, 2)],
                                     P2),
  ppl_Grid_intersection_assign(P1, P2),
  clean_ppl_new_Grid_from_generators([grid_point(A, 2),
                                      grid_point(B), grid_point(0)],
                                     P1a),
  clean_ppl_new_Grid_from_congruences([2*A =:= 0, B =:= 0],
                                      P1b),
  ppl_Grid_equals_Grid(P1, P1a),
  ppl_Grid_equals_Grid(P1, P1b),
  clean_ppl_new_Grid_from_congruences([(4*A =:= 1)/2, B =:= 0], P3),
  ppl_Grid_intersection_assign(P1, P3),
  ppl_Grid_is_empty(P1),
  !,
  ppl_delete_Grid(P1),
  ppl_delete_Grid(P2),
  ppl_delete_Grid(P3),
  ppl_delete_Grid(P1a),
  ppl_delete_Grid(P1b).

% Tests ppl_Grid_concatenate_assign/2.
grid_conc_assign :-
  make_vars(5, [A, B, C, D, E]),
  clean_ppl_new_Grid_from_space_dimension( 2, universe, P),
  clean_ppl_new_Grid_from_congruences( [A =:= 1, B =:= 0, C =:= 0], Q),
  ppl_Grid_concatenate_assign(P, Q),
  clean_ppl_new_Grid_from_congruences(
                                      [C =:= 1, D =:= 0, E =:= 0],
                                      P1),
  ppl_Grid_equals_Grid(P, P1),
  !,
  ppl_delete_Grid(P1),
  ppl_delete_Grid(P),
  ppl_delete_Grid(Q).

% Tests ppl_Grid_join_assign/2.
grid_join_assign :-
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_generators(
                                     [grid_point(0), grid_point(2*B),
                                      parameter(A,2)],
                                     P1),
  clean_ppl_new_Grid_from_generators(
                                     [grid_point(0), grid_point(A + B)],
                                     P2),
  ppl_Grid_join_assign(P1, P2),
  clean_ppl_new_Grid_from_generators(
                                     [grid_point(0), grid_point(B),
                                      parameter(A,2)],
                                     P1a),
  ppl_Grid_equals_Grid(P1, P1a),
  !,
  ppl_delete_Grid(P1),
  ppl_delete_Grid(P2),
  ppl_delete_Grid(P1a).

% Tests ppl_Grid_difference_assign/2.
grid_diff_assign :-
  make_vars(1, [A]),
  GS0 = [grid_point(2*A)],
  GS1 = [grid_point(0), grid_point(2*A)],
  GS2 = [grid_point(0), grid_point(A)],
  GS3 = [grid_point(-2*A), grid_point(2*A)],
  GS4 = [grid_point(0), grid_point(4*A)],
  grid_diff_assign(GS1, GS2, []),
  grid_diff_assign(GS1, GS3, GS4),
  grid_diff_assign(GS3, GS0, GS3),
  grid_diff_assign(GS0, GS0, []).

grid_diff_assign( GS1, GS2, GS3) :-
  clean_ppl_new_Grid_from_generators( GS1, P1),
  ppl_Grid_space_dimension(P1, Dim),
  clean_ppl_new_Grid_from_space_dimension( Dim, empty, P2),
  ppl_Grid_add_grid_generators(P2, GS2),
  ppl_Grid_difference_assign(P1, P2),
  clean_ppl_new_Grid_from_space_dimension( Dim, empty, P3),
  ppl_Grid_add_grid_generators(P3, GS3),
  ppl_Grid_equals_Grid(P1, P3),
  !,
  ppl_delete_Grid(P1),
  ppl_delete_Grid(P2),
  ppl_delete_Grid(P3).

% Tests ppl_Grid_time_elapse_assign/2.
grid_time_elapse :-
  make_vars(1, [A]),
  clean_ppl_new_Grid_from_space_dimension( 2, universe, P),
  ppl_Grid_add_constraints(P,
                          [A = 2]),
  clean_ppl_new_Grid_from_space_dimension( 2, universe, Q),
  ppl_Grid_add_congruences(Q, [(A =:= 0) /3]),
  ppl_Grid_time_elapse_assign(P, Q),
  clean_ppl_new_Grid_from_space_dimension( 2, universe, Pa),
  ppl_Grid_add_congruences(Pa, [(A =:= 2)/3]),
  clean_ppl_new_Grid_from_space_dimension( 2, universe, Qa),
  ppl_Grid_add_congruences(Qa, [(A =:= 0)/3]),
  ppl_Grid_equals_Grid(Q, Qa),
  ppl_Grid_equals_Grid(P, Pa),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(Q),
  ppl_delete_Grid(Pa),
  ppl_delete_Grid(Qa).

% Tests ppl_Grid_topological_closure_assign/1.
grid_top_close_assign :-
  make_vars(3, [A, B, C]),
  GS_close = [grid_point(A + B), grid_point(0),
               parameter(A), parameter(B)],
  CS_close = [4*A + B + -2*C =:= 5, A =:= 3],
  grid_top_close_assign(gensys, GS_close, GS_close),
  grid_top_close_assign(consys, CS_close, CS_close).

grid_top_close_assign( gensys, GS, GS_close) :-
  clean_ppl_new_Grid_from_generators( GS, P),
  ppl_Grid_topological_closure_assign(P),
  clean_ppl_new_Grid_from_generators( GS_close, Pa),
  ppl_Grid_equals_Grid(P, Pa),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(Pa).

grid_top_close_assign( consys, CS, CS_close) :-
  clean_ppl_new_Grid_from_congruences( CS, P),
  ppl_Grid_topological_closure_assign(P),
  clean_ppl_new_Grid_from_congruences( CS_close, Pa),
  ppl_Grid_equals_Grid(P, Pa),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(Pa).

%%%%%%%%%%%%%%%%%% Grid_Add Constraints or Generators %%%%%%%%%%%%%%%%%%%

% Tests ppl_Grid_add_congruence/2 and ppl_Grid_add_constraint/2.
grid_add_con :-
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_space_dimension( 2, universe, P),
  ppl_Grid_add_constraint(P, A - B = 1),
  clean_ppl_new_Grid_from_constraints(
                                      [A - B = 1],
                                      Pa),
  ppl_Grid_equals_Grid(P, Pa),
  ppl_Grid_add_congruence(P, A =:= 0),
  clean_ppl_new_Grid_from_congruences(
                                      [A =:= 0, (B =:= A - 1)/0],
                                      Pb),
  ppl_Grid_equals_Grid(P, Pb),
  ppl_Grid_add_congruence(P, (2*A =:= 1)/2),
  clean_ppl_new_Grid_from_space_dimension( 2, empty, Pc),
  ppl_Grid_equals_Grid(P, Pc),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(Pa),
  ppl_delete_Grid(Pb),
  ppl_delete_Grid(Pc).

% Tests ppl_Grid_add_grid_generator/2.
grid_add_gen :-
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_space_dimension( 2, empty, P),
  ppl_Grid_add_grid_generator(P, grid_point(0)),
  ppl_Grid_add_grid_generator(P, parameter(A + B)),
  ppl_Grid_add_grid_generator(P, grid_line(A)),
  clean_ppl_new_Grid_from_space_dimension(2, universe, P1),
  ppl_Grid_add_congruence(P1, B =:= 1),
  ppl_Grid_equals_Grid(P, P1),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(P1).

% Tests ppl_Grid_add_congruences/2.
% and ppl_Grid_add_constraints/2.
grid_add_cons :-
  make_vars(3, [A, B, C]),
  clean_ppl_new_Grid_from_space_dimension( 3, universe, P),
  ppl_Grid_add_congruences(P,
        [A =:= 1, B =:= 0, 4*A + B - 2*C =:= 5]),
  ppl_Grid_add_constraints(P,
        [2*A + B - 2*C = 5]),
  clean_ppl_new_Grid_from_congruences(
        [A =:= 1, B =:= 0, 4*A + B - 2*C =:= 5], P1),
  ppl_Grid_add_congruences(P1, [2*A + B - 2*C = 5]),
  ppl_Grid_equals_Grid(P, P1),
  ppl_Grid_add_congruences(P, [(3*A =:= 1)/3]),
  ppl_Grid_is_empty(P),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(P1).

% Tests ppl_Grid_add_grid_generators/2.
grid_add_gens :-
  make_vars(3, [A, B, C]),
  clean_ppl_new_Grid_from_space_dimension( 3, empty, P),
  ppl_Grid_add_grid_generators(P, [grid_point(A + B + C),
                parameter(A), parameter(2*A), parameter(A + B + C, 1),
                parameter(100*A + 5*B, -8)]),
  clean_ppl_new_Grid_from_generators([grid_point(A + B + C),
                parameter(A), parameter(2*A), parameter(A + B + C, 1),
                parameter(100*A + 5*B, -8)], P1),
  ppl_Grid_equals_Grid(P, P1),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(P1).

%%%%%%%%%%%%%%%%%% Change Dimensions %%%%%%%%%%%%%%%%%%%%%%%%%%

% Tests ppl_Grid_add_space_dimensions_and_project/2.
grid_project :-
  make_vars(4, [A, B, C, D]),
  clean_ppl_new_Grid_from_space_dimension( 2, universe, P),
  ppl_Grid_add_congruences(P, [A =:= 1, B =:= 0]),
  ppl_Grid_add_space_dimensions_and_project(P, 0),
  clean_ppl_new_Grid_from_congruences(
                                      [A =:= 1, B =:= 0],
                                      P0),
  ppl_Grid_equals_Grid(P, P0),
  ppl_delete_Grid(P0),
  ppl_Grid_add_space_dimensions_and_project(P, 2),
  clean_ppl_new_Grid_from_congruences(
            [A =:= 1, B =:= 0, (C =:= 0)/0, (D =:= 0)/0],
                                      P1),
  ppl_Grid_equals_Grid(P, P1),
  !,
  ppl_delete_Grid(P1),
  ppl_delete_Grid(P).

% Tests ppl_Grid_add_space_dimensions_and_embed/2.
grid_embed :-
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_space_dimension( 2, universe, P),
  ppl_Grid_add_congruences(P, [A =:= 1, B =:= 0]),
  ppl_Grid_add_space_dimensions_and_embed(P, 0),
  clean_ppl_new_Grid_from_congruences(
                                      [A =:= 1, B =:= 0],
                                      P0),
  ppl_Grid_equals_Grid(P, P0),
  ppl_delete_Grid(P0),
  ppl_Grid_add_space_dimensions_and_embed(P, 2),
  clean_ppl_new_Grid_from_space_dimension( 4, universe, P1),
  ppl_Grid_add_congruences(P1, [A =:= 1, B =:= 0]),
  ppl_Grid_equals_Grid(P, P1),
  !,
  ppl_delete_Grid(P1),
  ppl_delete_Grid(P).

% Tests ppl_Grid_remove_space_dimensions/2.
grid_remove_dim :-
  make_vars(3, [A, B, C]),
  clean_ppl_new_Grid_from_space_dimension( 3, universe, P),
  ppl_Grid_add_congruences(P, [A =:= 1, B =:= 0, C =:= 2]),
  ppl_Grid_remove_space_dimensions(P, []),
  clean_ppl_new_Grid_from_congruences(
                                      [A =:= 1, B =:= 0, C =:= 2],
                                      P0),
  ppl_Grid_equals_Grid(P, P0),
  ppl_delete_Grid(P0),
  ppl_Grid_remove_space_dimensions(P,[B]),
  clean_ppl_new_Grid_from_congruences(
                                      [A =:= 1, B =:= 2],
                                      P1),
  ppl_Grid_equals_Grid(P, P1),
  ppl_delete_Grid(P1),
  % Note: now 'B' refers to the old 'C' variable.
  ppl_Grid_remove_space_dimensions(P,[A, B]),
  ppl_Grid_space_dimension(P, 0),
  !,
  ppl_delete_Grid(P).

% Tests ppl_Grid_remove_higher_space_dimensions/2.
grid_remove_high_dim :-
  make_vars(3, [A, B, C]),
  clean_ppl_new_Grid_from_space_dimension( 3, universe, P),
  ppl_Grid_add_congruences(P, [A =:= 1, B =:= 0, C =:= 0]),
  clean_ppl_new_Grid_from_congruences(
                                      [A =:= 1, B =:= 0, C =:= 0],
                                      P1),
  ppl_Grid_equals_Grid(P, P1),
  ppl_Grid_remove_higher_space_dimensions(P, 1),
  clean_ppl_new_Grid_from_congruences(
                                      [A =:= 1],
                                      P2),
  ppl_Grid_equals_Grid(P, P2),
  ppl_Grid_remove_higher_space_dimensions(P, 1),
  ppl_Grid_equals_Grid(P, P2),
  ppl_Grid_remove_higher_space_dimensions(P, 0),
  ppl_Grid_space_dimension(P, 0),
  !,
  ppl_delete_Grid(P1),
  ppl_delete_Grid(P2),
  ppl_delete_Grid(P).

% Tests ppl_Grid_expand_space_dimension/3.
grid_expand_dim :-
  make_vars(4, [A, B, C, D]),
  clean_ppl_new_Grid_from_space_dimension( 3, universe, P),
  ppl_Grid_add_congruences(P, [A =:= 1, B =:= 0, C =:= 2]),
  ppl_Grid_expand_space_dimension(P, B, 1),
  ppl_Grid_space_dimension(P, 4),
  clean_ppl_new_Grid_from_congruences(
                                      [A =:= 1, B =:= 0, C =:= 2, D =:= 0],
                                      P1),
  ppl_Grid_equals_Grid(P, P1),
  ppl_delete_Grid(P1),
  ppl_Grid_remove_higher_space_dimensions(P, 2),
  ppl_Grid_expand_space_dimension(P, A, 2),
  clean_ppl_new_Grid_from_congruences(
                                      [D =:= 1, C =:= 1, A =:= 1, B =:= 0],
                                      P2),
  ppl_Grid_equals_Grid(P, P2),
  ppl_delete_Grid(P2),
  ppl_Grid_space_dimension(P, 4),
  !,
  ppl_delete_Grid(P).

% Tests ppl_Grid_fold_space_dimension/3.
grid_fold_dims :-
  make_vars(4, [A, B, C, D]),
  clean_ppl_new_Grid_from_space_dimension( 4, universe, P),
  ppl_Grid_add_congruences(P, [A =:= 1, B =:= 0, C =:= 2, D =:= 0]),
  ppl_Grid_fold_space_dimensions(P, [D], B),
  ppl_Grid_space_dimension(P, 3),
  clean_ppl_new_Grid_from_space_dimension( 3, universe, P1),
  ppl_Grid_add_congruences(P1, [A =:= 1, B =:= 0, C =:= 2]),
  ppl_Grid_equals_Grid(P, P1),
  !,
  ppl_delete_Grid(P1),
  ppl_Grid_fold_space_dimensions(P, [A, C], B),
  clean_ppl_new_Grid_from_space_dimension( 1, universe, P2),
  ppl_Grid_add_congruences(P2, [A =:= 0]),
  ppl_Grid_equals_Grid(P, P2),
  ppl_delete_Grid(P2),
  ppl_Grid_space_dimension(P, 1),
  !,
  ppl_delete_Grid(P),
  clean_ppl_new_Grid_from_space_dimension( 2, universe, Ptacas),
  ppl_Grid_add_congruences(Ptacas, [A =:= 1, A =:= 3, B =:= 7, B =:= 12]),
  ppl_Grid_fold_space_dimensions(Ptacas, [A], B),
  ppl_Grid_space_dimension(Ptacas, 1),
  clean_ppl_new_Grid_from_space_dimension( 1, universe, Ptacas1),
  ppl_Grid_add_congruences(Ptacas1, [A =:= 1, A =:= 12]),
  ppl_Grid_equals_Grid(Ptacas, Ptacas1),
  !,
  ppl_delete_Grid(Ptacas1),
  ppl_delete_Grid(Ptacas).

% Tests ppl_Grid_map_space_dimensions/2.
grid_map_dim :-
  make_vars(7, [A, B, C, D, E, F, G]),
  clean_ppl_new_Grid_from_space_dimension( 3, universe, P),
  ppl_Grid_add_congruences(P, [A =:= 2, B =:= 1, C =:= 0]),
  ppl_Grid_map_space_dimensions(P, [A-B, B-C, C-A]),
  clean_ppl_new_Grid_from_space_dimension( 3, universe, Q),
  ppl_Grid_add_congruences(Q, [A =:= 0, B =:= 2, C =:= 1]),
  ppl_Grid_equals_Grid(P, Q),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(Q),
  clean_ppl_new_Grid_from_space_dimension( 4, empty, P0),
  ppl_Grid_add_grid_generators(P0, [grid_point(2*C), grid_line(A+B), parameter(A+C)]),
  \+ppl_Grid_map_space_dimensions(P0, [A+C, C-A, B-B]), % A+C not map
  \+ppl_Grid_map_space_dimensions(P0, [A, C-A, B-B]),   % A not map
  \+ppl_Grid_map_space_dimensions(P0, [D-A, C-A, B-B]), % D not dimension
  \+ppl_Grid_map_space_dimensions(P0, [B-A, C-A, B-B]), % not injective
  \+ppl_Grid_map_space_dimensions(P0, [B-A, C-A, B-C]), % not function
  ppl_delete_Grid(P0),
  clean_ppl_new_Grid_from_space_dimension( 4, empty, P1),
  ppl_Grid_add_grid_generators(P1,
     [grid_point(2*C), grid_line(A+B), parameter(A+C)]),
  ppl_Grid_map_space_dimensions(P1, [A-C, C-A, B-B]),
  clean_ppl_new_Grid_from_space_dimension( 3, empty, Q1),
  ppl_Grid_add_grid_generators(Q1,
     [grid_point(2*A), parameter(A+C), grid_line(B+C)]),
  ppl_Grid_equals_Grid(P1, Q1),
  !,
  ppl_delete_Grid(P1),
  ppl_delete_Grid(Q1),
  clean_ppl_new_Grid_from_space_dimension( 5, universe, P2),
  ppl_Grid_add_constraints(P2, [B = 2, E = 8]),
  ppl_Grid_add_space_dimensions_and_embed(P2, 2),
  ppl_Grid_map_space_dimensions(P2, [A-A, B-B, C-E, D-F, E-G, F-C, G-D]),
  clean_ppl_new_Grid_from_space_dimension( 7, universe, Q2),
  ppl_Grid_add_constraints(Q2, [B = 2, G = 8]),
  ppl_Grid_equals_Grid(P2, Q2),
  !,
  ppl_delete_Grid(P2),
  ppl_delete_Grid(Q2).


%%%%%%%%%%%%%%%%%% Affine Transformations %%%%%%%%%%%%%%%%%%%

% Tests ppl_Grid_affine_image/4.
grid_affine_image :-
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_space_dimension( 2, universe, P),
  ppl_Grid_add_constraint(P, A - B = 1),
  clean_ppl_new_Grid_from_constraints(
                                      [A - B = 1],
                                      P1),
  ppl_Grid_equals_Grid(P, P1),
  ppl_Grid_affine_image(P, A, A + 1, 1),
  clean_ppl_new_Grid_from_constraints(
                                      [A - B = 2],
                                      P2),
  ppl_Grid_equals_Grid(P, P2),
  !,
  ppl_delete_Grid(P1),
  ppl_delete_Grid(P2),
  ppl_delete_Grid(P).

% Tests ppl_Grid_affine_preimage/4.
grid_affine_preimage :-
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_space_dimension( 2, universe, P),
  ppl_Grid_add_congruence(P, A + B =:= 10),
  clean_ppl_new_Grid_from_congruences(
                                      [A + B =:= 10],
                                      P1),
  ppl_Grid_equals_Grid(P, P1),
  ppl_Grid_affine_preimage(P, A, A + 1, 1),
  clean_ppl_new_Grid_from_congruences(
                                      [A + B =:= 9],
                                      P2),
  ppl_Grid_equals_Grid(P, P2),
  !,
  ppl_delete_Grid(P1),
  ppl_delete_Grid(P2),
  ppl_delete_Grid(P).

% Tests ppl_Grid_generalized_affine_image/5.
grid_affine_image_gen :-
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_space_dimension( 2, universe, P),
  ppl_Grid_add_congruence(P, A =:= 0),
  ppl_Grid_add_congruence(P, (A + B =:= 0) / 2),
  ppl_Grid_generalized_affine_image(P, A, =, A + 1, 1, 1),
  clean_ppl_new_Grid_from_generators(
        [grid_point(0), grid_point(A - B), grid_point(B)],
                                      P1),
  ppl_Grid_equals_Grid(P, P1),
  !,
  ppl_delete_Grid(P1),
  ppl_delete_Grid(P).

% Tests ppl_Grid_generalized_affine_image/5.
grid_affine_preimage_gen :-
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_space_dimension( 2, universe, P),
  ppl_Grid_add_congruence(P, A =:= 0),
  ppl_Grid_add_congruence(P, (A + B =:= 0) / 2),
  ppl_Grid_generalized_affine_preimage(P, B, =, A + 1, 2, 3),
  clean_ppl_new_Grid_from_generators(
        [grid_point(-3*A), grid_point(A), grid_point(-A), grid_line(B)],
                                      P1),
  ppl_Grid_equals_Grid(P, P1),
  !,
  ppl_delete_Grid(P1),
  ppl_delete_Grid(P).

% Tests ppl_Grid_generalized_affine_image_lhs_rhs/4.
grid_affine_image_genlr :-
  make_vars(3, [A, B, C]),
  clean_ppl_new_Grid_from_space_dimension(3, universe, P),
  ppl_Grid_add_congruences(P, [(C =:= 0)/ 3, (A - 2*B =:= 1)/ 0]),
  ppl_Grid_generalized_affine_image_lhs_rhs(P, A - B + C, =, 2*A - B - C, 5),
  clean_ppl_new_Grid_from_space_dimension(3, universe, P1),
  ppl_Grid_equals_Grid(P, P1),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(P1).

% % Tests ppl_Grid_generalized_affine_preimage_lhs_rhs/4.
grid_affine_preimage_genlr :-
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_space_dimension( 2, universe, P),
  ppl_Grid_add_constraint(P, A - B = 0),
  ppl_Grid_generalized_affine_preimage_lhs_rhs(P, A-B, =, A, 0),
  clean_ppl_new_Grid_from_space_dimension( 2, universe, P1),
  ppl_Grid_add_constraint(P1, A = 0),
  ppl_Grid_equals_Grid(P, P1),
  !,
  ppl_delete_Grid(P1),
  ppl_delete_Grid(P),

  clean_ppl_new_Grid_from_space_dimension( 2, universe, P2),
  ppl_Grid_add_congruences(P2, [A =:= 0, (B =:= 0)/ 2]),
  ppl_Grid_generalized_affine_preimage_lhs_rhs(P2, A + 2*B, =, A - B, 3),
  clean_ppl_new_Grid_from_space_dimension( 2, empty, P3),
  ppl_Grid_add_grid_generators(P3,
           [grid_point(0), grid_point(A), grid_line(A + B)]),
  ppl_Grid_equals_Grid(P2, P3),
  !,
  ppl_delete_Grid(P2),
  ppl_delete_Grid(P3).

%%%%%%%%%%%%%%%%%% Get Congruence or Generator System %%%%%%%%%%%%%%%%%%%

% Tests ppl_Grid_get_congruences/2.
grid_get_cons :-
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_space_dimension(2, universe, P),
  ppl_Grid_get_congruences(P, []),
  ppl_Grid_add_congruence(P, A - B =:= 1),
  \+  ppl_Grid_get_congruences(P, []),
  ppl_Grid_get_congruences(P, [C]),
  clean_ppl_new_Grid_from_congruences([C], Q),
  ppl_Grid_equals_Grid(P, Q),
  ppl_Grid_add_congruence(P, A - B = 1),
  ppl_Grid_get_congruences(P, C1),
  clean_ppl_new_Grid_from_congruences(C1, Q1),
  ppl_Grid_equals_Grid(P, Q1),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(Q),
  ppl_delete_Grid(Q1).

% Tests ppl_Grid_get_minimized_congruences/2.
grid_get_min_cons :-
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_space_dimension(2, universe, P),
  ppl_Grid_get_minimized_congruences(P, []),
  ppl_Grid_add_congruences(P, [A - B =:= 1, A - B =:= 3, (A + B =:= 0)/3]),
  ppl_Grid_get_minimized_congruences(P, [C1, C2]),
  clean_ppl_new_Grid_from_congruences([C1, C2], Q),
  ppl_Grid_equals_Grid(P, Q),
  ppl_Grid_add_congruences(P, [(A + B =:= 1)/3]),
%  \+ppl_Grid_get_minimized_congruences(P, _),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(Q).

% Tests ppl_Grid_get_grid_generators/2.
grid_get_gens :-
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_space_dimension(2, empty, P),
  ppl_Grid_get_grid_generators(P, []),
  \+ ppl_Grid_get_grid_generators(P, [_]),
  ppl_Grid_add_grid_generator(P, grid_point(A+B)),
  ppl_Grid_get_grid_generators(P, [G]),
  clean_ppl_new_Grid_from_generators([G], Q),
  ppl_Grid_equals_Grid(P, Q),
  ppl_Grid_add_grid_generator(P, grid_point(A+B, 2)),
  ppl_Grid_get_grid_generators(P, GS1),
  ppl_Grid_add_grid_generators(Q, GS1),
  ppl_Grid_equals_Grid(P, Q),
  ppl_Grid_add_grid_generator(P, grid_line(A)),
  ppl_Grid_get_grid_generators(P, GS2),
  ppl_Grid_add_grid_generators(Q, GS2),
  ppl_Grid_equals_Grid(P, Q),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(Q).

% Tests ppl_Grid_get_minimized_grid_generators/2.
grid_get_min_gens :-
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_space_dimension(2, empty, P),
  ppl_Grid_add_grid_generators(P,
     [grid_point(0), grid_point(2*A), grid_point(A+B), grid_point(2*B)]),
  \+ ppl_Grid_get_minimized_grid_generators(P, [_]),
  ppl_Grid_get_minimized_grid_generators(P, [G1, G2, G3]),
  clean_ppl_new_Grid_from_generators([G1, G2, G3], Q),
  ppl_Grid_equals_Grid(P, Q),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(Q).


%%%%%%%%%%%%%%%%%% Grid Relations %%%%%%%%%%%%%%%%%%%%%%%%%%

% Tests ppl_Grid_relation_with_congruence/3.
grid_rel_cons :-
  make_vars(3, [A, B, C]),
  clean_ppl_new_Grid_from_space_dimension(3, universe, P),
  ppl_Grid_add_congruences(P, [(A =:= 1) / 3, B =:= 0, (C =:= 0) / 0]),
  \+ ppl_Grid_relation_with_congruence(P, (A =:= 0)/0, x),
  ppl_Grid_relation_with_congruence(P, (A =:= 0)/0, R),
  R = [is_disjoint],
  ppl_Grid_relation_with_congruence(P, (B =:= 0) / 0, R1),
  R1 = [strictly_intersects],
  ppl_Grid_relation_with_congruence(P, A =:= 0, R2),
  R2 = [is_included],
  ppl_Grid_relation_with_congruence(P, C =:= 0, R3),
  R3 = [is_included],
  ppl_Grid_add_constraint(P, A = B + 1),
  ppl_Grid_relation_with_congruence(P, (A =:= B + 1) / 0, R4),
  (R4 = [is_included, saturates] ; R4 = [saturates, is_included]),
  !,
  ppl_delete_Grid(P).

% Tests ppl_Grid_relation_with_grid_generator/3.
grid_rel_gens :-
  make_vars(3, [A, B, C]),
  clean_ppl_new_Grid_from_space_dimension(3, empty, P),
  ppl_Grid_add_grid_generators(P, [grid_point(A + B + C), parameter(A)]),
  \+ppl_Grid_relation_with_grid_generator(P, grid_point(A), x),
  ppl_Grid_relation_with_grid_generator(P, grid_point(A), R),
  R = [],
  ppl_Grid_relation_with_grid_generator(P, parameter(A), R1),
  R1 = [subsumes],
  !,
  ppl_delete_Grid(P).

%%%%%%%%%%%%%%%%%% Check Properties %%%%%%%%%%%%%%%%%%%%%%%%%%

%  tests ppl_Grid_is_universe/1,
%        ppl_Grid_is_empty/1,
%        ppl_Grid_is_bounded/1,
%        ppl_Grid_is_discrete/1,
%        ppl_Grid_is_topologically_closed/1.
grid_checks :-
  make_vars(3, [A, B, C]),
  clean_ppl_new_Grid_from_space_dimension(3, universe, P),
  clean_ppl_new_Grid_from_space_dimension(3, empty, P1),
  ppl_Grid_is_universe(P),
  ppl_Grid_is_empty(P1),
  \+ ppl_Grid_is_universe(P1),
  \+ ppl_Grid_is_empty(P),
  ppl_Grid_add_grid_generators(P1, [grid_point(A + B + C)]),
  ppl_Grid_is_bounded(P1),
  ppl_Grid_is_discrete(P1),
  ppl_Grid_add_grid_generators(P1, [parameter(A + B + C)]),
  \+ ppl_Grid_is_bounded(P1),
  ppl_Grid_is_discrete(P1),
  ppl_Grid_add_congruences(P, [(A =:= 1)/0, (B =:= 1)/0, (C =:= 1)/0]),
  ppl_Grid_is_topologically_closed(P),
  ppl_Grid_add_grid_generators(P1, [grid_line(A + B + C)]),
  \+ ppl_Grid_is_bounded(P1),
  \+ ppl_Grid_is_discrete(P1),
  ppl_Grid_is_topologically_closed(P),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(P1).

% Tests ppl_Grid_contains_Grid/2.
grid_contains :-
  clean_ppl_new_Grid_from_space_dimension(3, universe, P1),
  clean_ppl_new_Grid_from_space_dimension(3, empty, P2),
  ppl_Grid_contains_Grid(P1, P2),
  \+ppl_Grid_contains_Grid(P2, P1),
  ppl_delete_Grid(P1),
  ppl_delete_Grid(P2).

% Tests ppl_Grid_strictly_contains_Grid for C/2.
grid_strict_contains :-
  clean_ppl_new_Grid_from_space_dimension(3, universe, P1),
  clean_ppl_new_Grid_from_space_dimension(3, empty, P2),
  ppl_Grid_strictly_contains_Grid(P1, P2),
  \+ppl_Grid_strictly_contains_Grid(P1, P1),
  !,
  ppl_delete_Grid(P1),
  ppl_delete_Grid(P2).

% Tests ppl_Grid_is_disjoint_from_Grid/2.
grid_disjoint_from :-
  make_vars(3, [A, B, C]),
  clean_ppl_new_Grid_from_congruences([(3 =:= A) / 5, 4*A + B - 2*C =:= 5],
                                      P1),
  clean_ppl_new_Grid_from_congruences([(4 =:= A) / 5, 4*A + B - 2*C =:= 5],
                                      P2),
  ppl_Grid_is_disjoint_from_Grid(P1, P2),
  \+ppl_Grid_is_disjoint_from_Grid(P1, P1),
  !,
  ppl_delete_Grid(P1),
  ppl_delete_Grid(P2).

% Tests ppl_Grid_equals_Grid/2.
grid_equals :-
  clean_ppl_new_Grid_from_space_dimension(3, universe, P1),
  clean_ppl_new_Grid_from_space_dimension(3, universe, P2),
  clean_ppl_new_Grid_from_space_dimension(3, empty, P3),
  ppl_Grid_equals_Grid(P1, P2),
  \+ ppl_Grid_equals_Grid(P1, P3),
  !,
  ppl_delete_Grid(P1),
  ppl_delete_Grid(P2),
  ppl_delete_Grid(P3).

% Tests ppl_Grid_OK/1.
grid_ok :-
  clean_ppl_new_Grid_from_space_dimension(0, universe, P1),
  clean_ppl_new_Grid_from_space_dimension(3, universe, P2),
  clean_ppl_new_Grid_from_space_dimension(0, empty, P3),
  clean_ppl_new_Grid_from_space_dimension(3, empty, P4),
  ppl_Grid_OK(P1),
  ppl_Grid_OK(P2),
  ppl_Grid_OK(P3),
  ppl_Grid_OK(P4),
  !,
  ppl_delete_Grid(P1),
  ppl_delete_Grid(P2),
  ppl_delete_Grid(P3),
  ppl_delete_Grid(P4).

%%%%%%%%%%%%%%%%%%%%%%%%% Grid Bounding Values %%%%%%%%%%%%%%%%%%%%%%%

% Tests ppl_Grid_get_bounding_box/3.

grid_get_bounding_box :-
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_space_dimension(2, universe, P),
  ppl_Grid_get_bounding_box(P, any, Box),
  clean_ppl_new_Grid_from_bounding_box(Box, P1),
  ppl_Grid_equals_Grid(P1, P),
  ppl_Grid_add_constraints(P, [A = 1, B = 2]),
  ppl_Grid_get_bounding_box(P, any, Box1),
  clean_ppl_new_Grid_from_bounding_box(Box1, P2),
  ppl_Grid_equals_Grid(P, P2),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(P1),
  ppl_delete_Grid(P2).

% Tests ppl_Grid_get_covering_box/3.

grid_get_covering_box :-
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_space_dimension(2, universe, P),
  ppl_Grid_get_covering_box(P, Box),
  clean_ppl_new_Grid_from_covering_box(Box, P1),
  ppl_Grid_equals_Grid(P1, P),
  ppl_Grid_add_congruences(P, [A =:= 1, B =:= 2]),
  ppl_Grid_get_covering_box(P, Box1),
  clean_ppl_new_Grid_from_covering_box(Box1, P2),
  ppl_Grid_equals_Grid(P, P2),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(P1),
  ppl_delete_Grid(P2).

% Tests ppl_Grid_bounds_from_above/2 and ppl_Grid_bounds_from_below/2.
grid_bounds_from_above :-
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_congruences([A =:= 1, B =:= 0], P),
  \+ ppl_Grid_bounds_from_above(P, B),
  ppl_Grid_add_constraints(P, [B = 2]),
  ppl_Grid_bounds_from_above(P, B),
  !,
  ppl_delete_Grid(P).

grid_bounds_from_below :-
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_congruences([A =:= 1, B =:= 0], P),
  \+ ppl_Grid_bounds_from_below(P, B),
  ppl_Grid_add_constraints(P, [B = 2]),
  ppl_Grid_bounds_from_below(P, B),
  ppl_Grid_bounds_from_below(P, B),
  !,
  ppl_delete_Grid(P).

%%%%%%%%%%%%%%%%%%%%%%%%% Maximize and Minimize %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tests ppl_Grid_maximize/5.
grid_maximize :-
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_constraints([A = 0, B = 0], P),
  ppl_Grid_maximize(P, A + B, 0, 1, true),
  ppl_Grid_add_grid_generator(P, parameter(A + B)),
  \+ ppl_Grid_maximize(P, A + B, _, _, _),
  !,
  ppl_delete_Grid(P).

% Tests ppl_Grid_maximize/5.
grid_maximize_with_point :-
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_constraints([A = 0, B = 0], P),
  ppl_Grid_maximize_with_point(P, A + B, 0, 1, true, point(0)),
  ppl_Grid_add_grid_generator(P, parameter(A + B)),
  \+ ppl_Grid_maximize_with_point(P, A + B, _, _, _, _),
  !,
  ppl_delete_Grid(P).

% Tests ppl_Grid_minimize/5.
grid_minimize :-
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_constraints([A = 0, B = 0], P),
  ppl_Grid_minimize(P, A + B, 0, 1, true),
  ppl_Grid_add_grid_generator(P, parameter(A + B)),
  \+ ppl_Grid_minimize(P, A + B, _, _, _),
  !,
  ppl_delete_Grid(P).

% Tests ppl_Grid_minimize/5.
grid_minimize_with_point :-
  make_vars(2, [A, B]),
  clean_ppl_new_Grid_from_constraints([A = 0, B = 0], P),
  ppl_Grid_minimize_with_point(P, A + B, 0, 1, true, point(0)),
  ppl_Grid_add_grid_generator(P, parameter(A + B)),
  \+ ppl_Grid_minimize_with_point(P, A + B, _, _, _, _),
  !,
  ppl_delete_Grid(P).

%%%%%%%%%%%%%%%%%% Widen and Extrapolation Operators %%%%%%%%%%%%%%%%%%%

% Tests ppl_Grid_congruence_widening_assign/2.
grid_widen_congruence :-
  make_vars(3, [A, B, C]),
  grid_widen_extrapolation_init(P, [(A =:= 0) / 2, (B =:= 0) / 2, C =:= 0]),
  grid_widen_extrapolation_init(Q, [(A =:= 0) / 2, C =:= 0]),
  ppl_Grid_congruence_widening_assign(Q, P),
  grid_widen_extrapolation_final(P, [(A =:= 0) / 2, (B =:= 0) / 2, C =:= 0]),
  grid_widen_extrapolation_final(Q, [(A =:= 0) / 2, C =:= 0]).

% Tests ppl_Grid_congruence_widening_assign_with_tokens/4.
grid_widen_congruence_with_tokens :-
  make_vars(3, [A, B, C]),
  grid_widen_extrapolation_init(P, [(A =:= 0) / 2, (B =:= 0) / 2, C =:= 0]),
  grid_widen_extrapolation_init(Q, [(A =:= 0) / 2, C =:= 0]),
  grid_widen_extrapolation_init(R, [(A =:= 0) / 1, C =:= 0]),
  \+ ppl_Grid_congruence_widening_assign_with_tokens(Q, P, 4, 3),
  \+ ppl_Grid_congruence_widening_assign_with_tokens(Q, P, 4, not_a_number),
  ppl_Grid_congruence_widening_assign_with_tokens(Q, P, 5, 5),
  ppl_Grid_congruence_widening_assign_with_tokens(R, Q, 1, 0),
  grid_widen_extrapolation_final(P, [(A =:= 0) / 2, (B =:= 0) / 2, C =:= 0]),
  grid_widen_extrapolation_final(Q, [(A =:= 0) / 2, C =:= 0]),
  grid_widen_extrapolation_final(R, [(A =:= 0) / 1, C =:= 0]).

% Tests ppl_Grid_limited_congruence_extrapolation_assign/3.
grid_lim_extrapolate_congruence :-
  make_vars(3, [A, B, C]),
  grid_widen_extrapolation_init(P, [(A =:= 0) / 4, (B =:= 0) / 2, C =:= 0]),
  grid_widen_extrapolation_init(Q, [(A =:= 0) / 2, (B =:= 0) / 2, C =:= 0]),
  ppl_Grid_limited_congruence_extrapolation_assign(Q, P, [(A =:= 0) / 2]),
  grid_widen_extrapolation_final(P, [(A =:= 0) / 4, (B =:= 0) / 2, C =:= 0]),
  grid_widen_extrapolation_final(Q, [(A =:= 0) / 2, (B =:= 0) / 2, C =:= 0]).

% Tests ppl_Grid_limited_congruence_extrapolation_assign_with_tokens/5.
grid_lim_extrapolate_congruence_with_tokens :-
  make_vars(3, [A, B, C]),
  grid_widen_extrapolation_init(P, [(A =:= 0) / 4, (B =:= 0) / 2, C =:= 0]),
  grid_widen_extrapolation_init(Q, [(A =:= 0) / 2, (B =:= 0) / 2]),
  grid_widen_extrapolation_init(R, [(A =:= 0) / 1, (B =:= 0) / 2]),
  \+ ppl_Grid_limited_congruence_extrapolation_assign_with_tokens(Q, P,
                                                  [(A =:= 0) / 2], 4, 4),
  \+ ppl_Grid_limited_congruence_extrapolation_assign_with_tokens(Q, P,
                                    [(A =:= 0) / 2],  3, not_a_number),
  ppl_Grid_limited_congruence_extrapolation_assign_with_tokens(Q, P,
                                                  [(A =:= 0) / 2], 4, 3),
  ppl_Grid_limited_congruence_extrapolation_assign_with_tokens(R, Q,
                                                      [(A =:= 0) / 1], 2, 1),
  grid_widen_extrapolation_final(P, [(A =:= 0) / 4, (B =:= 0) / 2, C =:= 0]),
  grid_widen_extrapolation_final(Q, [(A =:= 0) / 2, (B =:= 0) / 2]),
  grid_widen_extrapolation_final(R, [(A =:= 0) / 1, (B =:= 0) / 2]).

% Tests ppl_Grid_generator_widening_assign/2.
grid_widen_generator :-
  make_vars(3, [A, B, C]),
  grid_gen_widen_extrapolation_init(P, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C)]),
  grid_gen_widen_extrapolation_init(Q, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C),
                                    grid_point(4*C + A, 4)]),
  grid_gen_widen_extrapolation_init(R, [grid_point(A, 2), grid_point(B),
                                    grid_point(C)]),
  grid_gen_widen_extrapolation_init(S, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C, 2)]),
  ppl_Grid_generator_widening_assign(Q, P),
  ppl_Grid_generator_widening_assign(S, R),
  grid_gen_widen_extrapolation_final(P, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C)]),
  grid_gen_widen_extrapolation_final(Q, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C),
                                    grid_point(4*C + A, 4)]),
  grid_gen_widen_extrapolation_final(R, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C)]),
  grid_gen_widen_extrapolation_final(S, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C, 2),
                                    grid_line(B), grid_line(C)]).

% Tests ppl_Grid_generator_widening_assign_with_tokens/4.
grid_widen_generator_with_tokens :-
  make_vars(3, [A, B, C]),
  grid_gen_widen_extrapolation_init(P, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C)]),
  grid_gen_widen_extrapolation_init(Q, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C),
                                    grid_point(4*C + A, 4)]),
  grid_gen_widen_extrapolation_init(R, [grid_point(A, 2), grid_point(B),
                                    grid_point(C)]),
  grid_gen_widen_extrapolation_init(S, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C, 2)]),
  \+ ppl_Grid_generator_widening_assign_with_tokens(Q, P, 1, 0),
  \+ ppl_Grid_generator_widening_assign_with_tokens(Q, P, 0, any),
  ppl_Grid_generator_widening_assign_with_tokens(Q, P, 1, 1),
  ppl_Grid_generator_widening_assign_with_tokens(S, R, 4, 4),
  grid_gen_widen_extrapolation_final(P, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C)]),
  grid_gen_widen_extrapolation_final(Q, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C),
                                    grid_point(4*C + A, 4)]),
  grid_gen_widen_extrapolation_final(R, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C)]),
  grid_gen_widen_extrapolation_final(S, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C, 2),
                                    grid_line(B), grid_line(C)]).

% Tests ppl_Grid_generator_extrapolation_assign/2.
grid_lim_extrapolate_generator :-
  make_vars(3, [A, B, C]),
  grid_gen_widen_extrapolation_init(P, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C)]),
  grid_gen_widen_extrapolation_init(Q, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C),
                                    grid_point(4*C + A, 4)]),
  grid_gen_widen_extrapolation_init(R, [grid_point(A, 2), grid_point(B),
                                    grid_point(C)]),
  grid_gen_widen_extrapolation_init(S, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C, 2)]),
  ppl_Grid_limited_generator_extrapolation_assign(Q, P, []),
  ppl_Grid_limited_generator_extrapolation_assign(S, R, []),
  grid_gen_widen_extrapolation_final(P, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C)]),
  grid_gen_widen_extrapolation_final(Q, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C),
                                    grid_point(4*C + A, 4)]),
  grid_gen_widen_extrapolation_final(R, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C)]),
  grid_gen_widen_extrapolation_final(S, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C, 2),
                                    grid_line(B), grid_line(C)]).

% Tests ppl_Grid_generator_extrapolation_assign_with_tokens/4.
grid_lim_extrapolate_generator_with_tokens :-
  make_vars(3, [A, B, C]),
  grid_gen_widen_extrapolation_init(P, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C)]),
  grid_gen_widen_extrapolation_init(Q, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C),
                                    grid_point(4*C + A, 4)]),
  grid_gen_widen_extrapolation_init(R, [grid_point(A, 2), grid_point(B),
                                    grid_point(C)]),
  grid_gen_widen_extrapolation_init(S, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C, 2)]),
  \+ ppl_Grid_limited_generator_extrapolation_assign_with_tokens(Q, P, [], 1, 0),
%  \+ ppl_Grid_limited_generator_extrapolation_assign_with_tokens(Q, P, [], any, _),
  ppl_Grid_limited_generator_extrapolation_assign_with_tokens(Q, P, [], 1, 1),
  ppl_Grid_limited_generator_extrapolation_assign_with_tokens(S, R,
                                    [2*B =:= 0], 4, 4),
  grid_gen_widen_extrapolation_final(P, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C)]),
  grid_gen_widen_extrapolation_final(Q, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C),
                                    grid_point(4*C + A, 4)]),
  grid_gen_widen_extrapolation_final(R, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C)]),
  grid_gen_widen_extrapolation_final(S, [grid_point(A, 2), grid_point(B, 2),
                                    grid_point(C, 2),
                                    grid_line(C)]).

% grid_widen_extrapolation_init/3 and widen_extrapolation_final/3
% are used in the tests for widening and extrapolation predicates.
grid_widen_extrapolation_init(P, CS):-
  clean_ppl_new_Grid_from_space_dimension(3, universe, P),
  ppl_Grid_add_congruences(P, CS).

grid_widen_extrapolation_final(P, CS):-
  clean_ppl_new_Grid_from_space_dimension(3, universe, P1),
  ppl_Grid_add_congruences(P1, CS),
  ppl_Grid_equals_Grid(P, P1),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(P1).

% grid_gen_widen_extrapolation_init/3 and grid_gen_widen_extrapolation_final/3
% are used in the tests for widening and extrapolation predicates.
grid_gen_widen_extrapolation_init(P, GS):-
  clean_ppl_new_Grid_from_space_dimension(3, universe, P),
  ppl_Grid_add_grid_generators(P, GS).

grid_gen_widen_extrapolation_final(P, GS):-
  clean_ppl_new_Grid_from_space_dimension(3, universe, P1),
  ppl_Grid_add_grid_generators(P1, GS),
  ppl_Grid_equals_Grid(P, P1),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(P1).

%%%%%%%%%%%%%%%%% Exceptions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% grid_exceptions/0 tests both Prolog and C++ exceptions using:
%
% grid_exception_prolog(+N, +V)
% grid_exception_sys_prolog(+N, +V)
% grid_exception_cplusplus(+N, +V)
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

grid_exceptions :-
   current_prolog_flag(bounded, Y),
   make_vars(3, V),
   grid_exception_prolog(V),
    /* XSB does not throw catchable exceptions for integers out of range;
	so call to exception_sys_prolog is not made when testing XSB.
        The same exclusion for XSB and exception_sys_prolog/1 test is also
        made in pl_check.pl (See log message 2007-09-19 10:29:08) */
   ((Y == true,\+prolog_system('XSB'))  -> grid_exception_sys_prolog(V) ; true),
   grid_exception_cplusplus(V),
   !.


%% TEST: Prolog_unsigned_out_of_range
grid_exception_yap :-
     I = 21474836470, J = 3, K = 0,
     ppl_new_Grid_from_grid_generators(
        [grid_point('$VAR'(I)),grid_point('$VAR'(J))], P),
     ppl_Grid_get_grid_generators(P, GS),
     nl, write(GS), nl,
     ppl_new_Grid_from_grid_generators(
        [grid_point('$VAR'(I)),grid_point('$VAR'(K))], P1),
     ppl_Grid_get_grid_generators(P1, GS1),
     nl, write(GS1), nl,
     ppl_delete_Grid(P),
     ppl_delete_Grid(P1).

% exception_prolog(+N, +V) checks exceptions thrown by the Prolog interface.
% It does not check those that are dependent on a specific Prolog system.

grid_exception_prolog(V) :-
   grid_exception_prolog1(10, V).

grid_exception_prolog1(0, _) :- !.
grid_exception_prolog1(N, V) :-
   grid_exception_prolog(N, V),
   N1 is N - 1,
   grid_exception_prolog1(N1, V).

%% TEST: Prolog_unsigned_out_of_range
grid_exception_prolog(1, _) :-
    pl_check_prolog_flag(bounded, Y),
   (Y == true ->
     true
    ;
     (I = 21474836470,
     must_catch(ppl_new_Grid_from_grid_generators([grid_point('$VAR'(I))], _))
      )
   ).

%% TEST: not_unsigned_integer
grid_exception_prolog(2, _) :-
  must_catch(ppl_new_Grid_from_grid_generators([grid_point('$VAR'(n))], _)),
  must_catch(ppl_new_Grid_from_grid_generators([grid_point('$VAR'(-1))], _)).

%% TEST: not_unsigned_integer
grid_exception_prolog(3, _) :-
  clean_ppl_new_Grid_from_space_dimension(3, universe, P),
  clean_ppl_new_Grid_from_space_dimension(3, universe, Q),
  must_catch(ppl_Grid_congruence_widening_assign_with_tokens(
             Q, P, -1, _X)),
  must_catch(ppl_Grid_limited_congruence_extrapolation_assign_with_tokens(
             Q, P, [], -1, _X)),
  must_catch(ppl_Grid_generator_widening_assign_with_tokens(
             Q, P, -1, _X)),
  must_catch(ppl_Grid_limited_generator_extrapolation_assign_with_tokens(
             Q, P, [], -1, _X)),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(Q).

%% TEST: non_linear
grid_exception_prolog(4, [A,B,C]) :-
  must_catch(ppl_new_Grid_from_grid_generators([grid_point(B + A*C)], _)),
  must_catch(ppl_new_Grid_from_grid_generators(
                     [grid_point(C), parameter(B + C, _)], _)),
  must_catch(ppl_new_Grid_from_grid_generators(
                     [grid_point], _)),
  must_catch(ppl_new_Grid_from_grid_generators(
                     [grid_point(_D)], _)),
  must_catch(ppl_new_Grid_from_constraints(
                     [_E >= 3], _)),
  must_catch(ppl_new_Grid_from_constraints(
                     [A*B = 0], _)),
  must_catch(ppl_new_Grid_from_constraints(
                     [A], _)).

%% TEST: not_a_variable
grid_exception_prolog(5, [A,_,_]) :-
  clean_ppl_new_Grid_from_space_dimension(3, universe, P),
  must_catch(ppl_Grid_remove_space_dimensions(P, [A,1])),
  !,
  ppl_delete_Grid(P).

%% TEST: not_an_integer
grid_exception_prolog(6, [A,B,_]) :-
  clean_ppl_new_Grid_from_generators(
               [grid_point(A + B), parameter(A), parameter(B)], P),
  must_catch(ppl_Grid_affine_image(P, A, A + B + 1, i)),
  !,
  ppl_delete_Grid(P).

%% TEST: not_a_grid_handle
grid_exception_prolog(7, _) :-
  must_catch(ppl_Grid_space_dimension(_, _N)).

%% TEST: not_universe_or_empty
grid_exception_prolog(8, _) :-
  must_catch(ppl_new_Grid_from_space_dimension(3, xxx, _)).

%% TEST: not_relation
grid_exception_prolog(9, [A, B, _]) :-
  clean_ppl_new_Grid_from_generators(
               [grid_point(A)], P),
  must_catch(ppl_Grid_generalized_affine_image(P, A, x, A + 1, 1)),
  must_catch(
     ppl_Grid_generalized_affine_image_lhs_rhs(P, B - 1, x, A + 1)),
  must_catch(
     ppl_Grid_generalized_affine_image_lhs_rhs(P, B - 1, x + y, A + 1)).

%% TEST: not_a_nil_terminated_list
grid_exception_prolog(10, [A, B, C]) :-
  must_catch(ppl_new_Grid_from_grid_generators(
     [grid_point(A + B + C, 1) | not_a_list], _)),
  must_catch(ppl_new_Grid_from_constraints(
     [A = 0 | not_a_list], _)),
  must_catch(ppl_new_Grid_from_bounding_box(0, 0)),
  must_catch(ppl_new_Grid_from_bounding_box(
             [i(c(-3), c(2/1)), i(c(2), c(8)) | d], _)),
  must_catch(ppl_new_Grid_from_covering_box(
             [i(c(-4), c(2/1)), i(c(1), c(2)) | _], _)),
  clean_ppl_new_Grid_from_space_dimension(3, universe, P),
  must_catch(ppl_Grid_add_congruences(P, _)),
  must_catch(ppl_Grid_add_congruences(P, not_a_list)),
  must_catch(ppl_Grid_add_grid_generators(P, not_a_list)),
  must_catch(ppl_Grid_add_grid_generators(P, _)),
  clean_ppl_new_Grid_from_space_dimension(3, empty, Q),
  must_catch(ppl_Grid_map_space_dimensions(Q, not_a_list)),
  must_catch(ppl_Grid_fold_space_dimensions(Q, not_a_list, B)),
  must_catch(ppl_Grid_remove_space_dimensions(Q, not_a_list)),
  must_catch(ppl_Grid_limited_generator_extrapolation_assign(
             Q, P, not_a_list)),
  must_catch(ppl_Grid_limited_generator_extrapolation_assign_with_tokens(
             Q, P, not_a_list, 1, _)),
  must_catch(ppl_Grid_limited_congruence_extrapolation_assign(

             Q, P, not_a_list)),
  must_catch(ppl_Grid_limited_congruence_extrapolation_assign_with_tokens(
             Q, P, not_a_list, 1, _)),
  !,
  ppl_delete_Grid(P),
  ppl_delete_Grid(Q).

% grid_exception_sys_prolog(+N, +V)
% checks exceptions thrown by Prolog interfaces
% that are dependent on a specific Prolog system.
% These are only checked if current_prolog_flag(bounded, true) holds.

grid_exception_sys_prolog(V) :-
   grid_exception_sys_prolog1(4, V).

grid_exception_sys_prolog1(0, _) :- !.
grid_exception_sys_prolog1(N, V) :-
   grid_exception_sys_prolog(N, V),
   N1 is N - 1,
   grid_exception_sys_prolog1(N1, V).

grid_exception_sys_prolog(1, [A,B,_]) :-
  pl_check_prolog_flag(max_integer, Max_Int),
  catch((
          clean_ppl_new_Grid_from_congruences(
               [Max_Int * A - B =:= 0], P),
          ppl_Grid_affine_image(P, B, 2*B, 1),
          must_catch(ppl_Grid_get_grid_generators(P, _GS)),
          !,
          ppl_delete_Grid(P)
        ),
        ppl_overflow_error(Cause),
        check_exception_term(ppl_overflow_error(Cause))
       ).

 grid_exception_sys_prolog(2, [A,B,_]) :-
  pl_check_prolog_flag(min_integer, Min_Int),
  catch((
          clean_ppl_new_Grid_from_congruences(
               [Min_Int * A - B =:= 0], P),
          ppl_Grid_affine_image(P, B, 2*B, 1),
          must_catch(ppl_Grid_get_grid_generators(P, _GS)),
          !,
          ppl_delete_Grid(P)
        ),
        ppl_overflow_error(Cause),
        check_exception_term(ppl_overflow_error(Cause))
       ).

grid_exception_sys_prolog(3, [A,B,_]) :-
  pl_check_prolog_flag(max_integer, Max_Int),
  catch((
          clean_ppl_new_Grid_from_generators(
               [grid_point(Max_Int * A + B)], P),
          ppl_Grid_affine_image(P, A, A + 1, 1),
          must_catch(ppl_Grid_get_grid_generators(P, _GS)),
          !,
          ppl_delete_Grid(P)
        ),
        ppl_overflow_error(Cause),
        check_exception_term(ppl_overflow_error(Cause))
       ).

grid_exception_sys_prolog(4, [A,B,_]) :-
  pl_check_prolog_flag(min_integer, Min_Int),
  catch((
          clean_ppl_new_Grid_from_generators(
               [grid_point(Min_Int * A + B)], P),
          ppl_Grid_affine_image(P, A, A - 1, 1),
          must_catch(ppl_Grid_get_grid_generators(P, _GS)),
          !,
          ppl_delete_Grid(P)
        ),
        ppl_overflow_error(Cause),
        check_exception_term(ppl_overflow_error(Cause))
       ).

% grid_exception_cplusplus(+N, +V) checks exceptions thrown by the C++
% interface for the PPL.

grid_exception_cplusplus(V) :-
   grid_exception_cplusplus1(10, V).

grid_exception_cplusplus1(0, _) :- !.
grid_exception_cplusplus1(N, V) :-
   grid_exception_cplusplus(N, V),
   N1 is N - 1,
   grid_exception_cplusplus1(N1, V).

grid_exception_cplusplus(1, [A,B,C]) :-
  must_catch(ppl_new_Grid_from_grid_generators([grid_point(A + B + C, 0)], _)).

grid_exception_cplusplus(2, [A,B,_]) :-
  clean_ppl_new_Grid_from_generators(
               [grid_point(A + B), parameter(A), parameter(B)], P),
  must_catch(ppl_Grid_affine_image(P, A, A + B + 1, 0)),
  !,
  ppl_delete_Grid(P).

grid_exception_cplusplus(4, [A,B,C]) :-
   must_catch(ppl_new_Grid_from_grid_generators([grid_line(A + B + C)], _)).

grid_exception_cplusplus(5, [A,B,C]) :-
  clean_ppl_new_Grid_from_generators([grid_point(B + 2*C)], P),
  ppl_Grid_remove_space_dimensions(P,[C]),
  must_catch(ppl_Grid_remove_space_dimensions(P,[A,C])),
  !,
  ppl_delete_Grid(P).

grid_exception_cplusplus(6, [A,B,_]) :-
  clean_ppl_new_Grid_from_congruences([A =:= 1], P),
  must_catch(ppl_Grid_affine_image(P, B, A + 1, 1)),
  !,
  ppl_delete_Grid(P).

grid_exception_cplusplus(7, [A, B, C]) :-
  clean_ppl_new_Grid_from_congruences([A =:= 1, B =:= 1], P),
  must_catch(ppl_Grid_affine_image(P, B, A + C + 1, 1)),
  !,
  ppl_delete_Grid(P).

grid_exception_cplusplus(8, [A,B,_]) :-
  clean_ppl_new_Grid_from_congruences([A =:= B], P),
  must_catch(ppl_Grid_affine_preimage(P, A, A + B + 1, 0)),
  !,
  ppl_delete_Grid(P).

grid_exception_cplusplus(9, [A, B, C]) :-
  clean_ppl_new_Grid_from_generators(
               [grid_point(0), parameter(A + B), parameter(A)], P),
  must_catch(ppl_Grid_affine_preimage(P, C, A + 1, 1)),
  !,
  ppl_delete_Grid(P).


grid_exception_cplusplus(10, [A, B, C]) :-
  clean_ppl_new_Grid_from_generators(
               [grid_point(0), grid_point(A), grid_line(A + B)], P),
  must_catch(ppl_Grid_affine_preimage(P, B, A + C, 1)),
  !,
  ppl_delete_Grid(P).


% must_catch(+Call) calls Call using catch and checks exception.
% If exception it succeeds and fails if there is no exception caught.

must_catch(Call) :-
   ( catch(Call, M0, check_exception(M0) ) -> fail ; true).

% check_exception(+Exception) checks and prints the exception message;
% and then fails.

check_exception(Exception):-
         format_exception_message(Exception), fail.

%%%%%%%%%%%% predicate for making list of ppl variables %%%%%%

% make_var_list(+I,+Dimension,?Variable_List)
% constructs a list of variables with indices from I to Dimension - 1.
% It is assumed that I = Dimension.

make_vars(Dim, Var_List):-
  make_var_list(0, Dim, Var_List).
make_var_list(Dim,Dim,[]):- !.
make_var_list(I,Dim,['$VAR'(I)|Var_List]):-
  I1 is I + 1,
  make_var_list(I1,Dim,Var_List).

%%%%%%%%%%%% predicate for safely deleting polyhedra on failure %

cleanup_ppl_Polyhedron(_).
cleanup_ppl_Polyhedron(P) :-
  out(cs, P),
  out(gs, P),
  ppl_delete_Polyhedron(P), fail.

cleanup_ppl_Polyhedra([]).
cleanup_ppl_Polyhedra([_|_]).
cleanup_ppl_Polyhedra([P|Ps]) :-
  delete_all_ppl_Polyhedra([P|Ps]).

delete_all_ppl_Polyhedra([]).
delete_all_ppl_Polyhedra([P|Ps]) :-
  ppl_delete_Polyhedron(P),
  delete_all_ppl_Polyhedra(Ps).

cleanup_ppl_MIP_Problem(_).
cleanup_ppl_MIP_Problem(MIP) :-
  out(mip, MIP),
  ppl_delete_MIP_Problem(MIP), fail.

cleanup_ppl_Grid(_).
cleanup_ppl_Grid(GR) :-
%%  out(cgs, GR),
%%  out(ggs, GR),
  ppl_delete_Grid(GR), fail.

out(cs, P):-
  ((noisy(N), N < 2) -> true ;
    ppl_Polyhedron_get_constraints(P, CS),
    nl, write(CS), nl
  ).

out(gs, P):-
  ((noisy(N), N < 2) -> true ;
    ppl_Polyhedron_get_grid_generators(P, GS),
    nl, write(GS), nl
  ).

out(mip, MIP):-
  ((noisy(N), N < 2) -> true ;
    ppl_MIP_Problem_constraints(MIP, CS),
    ppl_MIP_Problem_objective_function(MIP, Obj),
    ppl_MIP_Problem_optimization_mode(MIP, Opt),
    nl,
    write(' constraint system is: '), write(CS), nl,
    write(' objective function is: '), write(Obj), nl,
    write(' optimization mode is: '), write(Opt),
    nl
  ).

out(cgs, P):-
  ((noisy(N), N < 2) -> true ;
    ppl_Grid_get_congruences(P, CS),
    nl, write(CS), nl
  ).

out(ggs, P):-
  ((noisy(N), N < 2) -> true ;
    ppl_Grid_get_grid_generators(P, GS),
    nl, write(GS), nl
  ).

out(sys_large_int, init):-
  !,
  prolog_system(System),
  ((noisy(N), N < 2) -> true ;
    nl, write_all([' At the Prolog/C++ interface, for', System, 'Prolog', nl,
       ' the extra numbers tested are: ']),
    nl
  ).

out(sys_large_int, Num):-
  ((noisy(N), N < 2) -> true ;
      write_all([Num, ',  '])
  ).

out(large_int, init):-
  !,
  ((noisy(N), N < 2) -> true ;
    nl, write(' At the Prolog/C++ interface, the numbers tested are: '),
    nl
  ).

out(large_int, Num, Sign, Add, Exp):-
  ((noisy(N), N < 2) -> true ;
    write_all([Num, ' = ', Sign, ' * ', '((1 << ', Exp, ') + ', Add, '),  '])
  ).

%%% predicates for ensuring new grids are always deleted on failure %

clean_ppl_new_Grid_from_space_dimension(D, Universe_or_Empty, P) :-
  ppl_new_Grid_from_space_dimension(D, Universe_or_Empty, P),
  cleanup_ppl_Grid(P).

clean_ppl_new_Grid_from_congruences(CS, P) :-
  ppl_new_Grid_from_congruences(CS, P),
  cleanup_ppl_Grid(P).

clean_ppl_new_Grid_from_constraints(CS, P) :-
  ppl_new_Grid_from_constraints(CS, P),
  cleanup_ppl_Grid(P).

clean_ppl_new_Grid_from_generators(GS, P) :-
  ppl_new_Grid_from_grid_generators(GS, P),
  cleanup_ppl_Grid(P).

clean_ppl_new_Grid_from_Grid(Q, P) :-
  ppl_new_Grid_from_Grid(Q, P),
  cleanup_ppl_Grid(P).

clean_ppl_new_Grid_from_bounding_box(Box, P) :-
  ppl_new_Grid_from_bounding_box(Box, P),
  cleanup_ppl_Grid(P).

clean_ppl_new_Grid_from_covering_box(Box, P) :-
  ppl_new_Grid_from_covering_box(Box, P),
  cleanup_ppl_Grid(P).

%%%%%%%%%%%% predicates for switching on/off output messages %

make_extra_noisy :-
  (retract(noisy(_)) ->
      make_extra_noisy
  ;
      assertz(noisy(2))
  ).

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

%%%%%%%%%%%% predicate for handling an unintended exception %%%%

check_exception_term(ppl_overflow_error(Cause)) :-
  ((Cause == 'Negative overflow.'; Cause == 'Positive overflow.') ->
    true
  ;
    print_exception_term(ppl_overflow_error(Cause))
  ),
  !.

print_exception_term(ppl_overflow_error(Cause)) :-
  nl,
  write('Error: an overflow has been detected by the PPL: '),
  write(Cause),
  nl,
  !.

print_exception_term(Exception) :-
  write('exception'), nl,
  nl,
  writeq(Exception),
  nl.

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
    noisy(_),
    (noisy(0) -> true ;
     (nl, write_all(Message))
    ).

write_all([]) :- nl.
write_all([Phrase|Phrases]):-
   (Phrase == nl ->
      nl
   ;
      (write(Phrase),
      write(' '))
   ),
   write_all(Phrases).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% list_groups(G)
% The interface predicates are partitioned into related sets called
% groups and here is a list of the groups.

list_groups( [
   grid_from_dimension,
   grid_from_grid,
   grid_from_representation,
   swap_grids,
   grid_dimension,
   grid_basic_operators,
   grid_add_to_system,
   grid_transform,
   grid_revise_dimensions,
   grid_get_system,
   check_grid,
   minmax_grid,
   compare_grids,
   grid_extrapolation_operators,
   grid_handle_exceptions
             ] ).

% group_predicates(G, P)
% P is a list of the interface predicates checked by test for group G.
% This is used to generate more informative error and exception messages.

group_predicates(grid_from_dimension,
  [ppl_new_Grid_from_space_dimension/3,
   ppl_Grid_is_universe/1,
   ppl_Grid_is_empty/1,
   ppl_delete_grid/1
  ]).

group_predicates(grid_from_grid,
  [ppl_new_Grid_from_Grid/2,
   ppl_new_Grid_from_congruences/2,
   ppl_Grid_equals_Grid/2
  ]).

group_predicates(new_grid_from_representations,
  [ppl_new_Grid_from_congruences/2,
   ppl_new_Grid_from_constraints/2,
   ppl_new_Grid_from_generators/2,
   ppl_new_Grid_from_bounding_box/2,
   ppl_new_Grid_from_covering_box/2
  ]).


group_predicates(swap_grids,
  [ppl_Grid_swap/2
  ]).

group_predicates(grid_dimension,
  [ppl_Grid_affine_dimension/2,
   ppl_Grid_space_dimension/2]).

group_predicates(grid_basic_operators,
  [ppl_Grid_intersection_assign/2,
   ppl_Grid_join_assign/2,
   ppl_Grid_difference_assign/2,
   ppl_Grid_time_elapse_assign/2,
   ppl_Grid_topological_closure_assign/1
  ]).

group_predicates(grid_add_to_system,
  [ppl_Grid_add_congruence/2,
   ppl_Grid_add_constraint/2,
   ppl_Grid_add_grid_generator/2,
   ppl_Grid_add_congruences/2,
   ppl_Grid_add_constraints/2,
   ppl_Grid_add_grid_generators/2
  ]).

group_predicates(grid_revise_dimensions,
  [ppl_Grid_remove_space_dimensions/2,
   ppl_Grid_remove_higher_space_dimensions/2,
   ppl_Grid_expand_space_dimension/3,
   ppl_Grid_fold_space_dimensions/3,
   ppl_Grid_map_space_dimensions/2,
   ppl_Grid_concatenate_assign/2
  ]).

group_predicates(grid_transform,
  [ppl_Grid_affine_image/4,
   ppl_Grid_affine_preimage/4,
   ppl_Grid_generalized_affine_image/5,
   ppl_Grid_generalized_affine_preimage/5,
   ppl_Grid_generalized_affine_image_lhs_rhs/4,
   ppl_Grid_generalized_affine_preimage_lhs_rhs/4
  ]).

group_predicates(grid_get_system,
  [ppl_Grid_get_constraints/2,
   ppl_Grid_get_minimized_constraints/2,
   ppl_Grid_get_grid_generators/2,
   ppl_Grid_get_minimized_grid_generators/2
  ]).

group_predicates(grid_check_grid,
  [ppl_Grid_relation_with_constraint/3,
   ppl_Grid_relation_with_grid_generator/3,
   ppl_Grid_is_empty/1,
   ppl_Grid_is_universe/1,
   ppl_Grid_is_bounded/1,
   ppl_Grid_is_discrete/1,
   ppl_Grid_is_topologically_closed/1,
   ppl_Grid_bounds_from_above/1,
   ppl_Grid_bounds_from_below/1,
   ppl_Grid_contains_Grid/2,
   ppl_Grid_strictly_contains_Grid/2,
   ppl_Grid_is_disjoint_from_Grid/2,
   ppl_Grid_equals_Grid/2,
   ppl_Grid_OK/1
  ]).

group_predicates(minmax_grid,
  [ppl_Grid_maximize/5,
   ppl_Grid_maximize_with_point/6,
   ppl_Grid_minimize/5,
   ppl_Grid_minimize_with_point/6
  ]).

group_predicates(compare_grids,
  [ppl_Grid_contains_Grid/2,
   ppl_Grid_strictly_contains_Grid/2,
   ppl_Grid_is_disjoint_from_Grid/2,
   ppl_Grid_equals_Grid/2
  ]).

group_predicates(grid_boxes,
  [ppl_Grid_get_bounding_box/3,
   ppl_Grid_get_covering_box/3]).


group_predicates(grid_extrapolation_operators,
  [ppl_Grid_congruence_widening_assign_with_token/3,
   ppl_Grid_congruence_widening_assign/2,
   ppl_Grid_limited_congruence_extrapolation_assign_with_token/4,
   ppl_Grid_limited_congruence_extrapolation_assign/3,
   ppl_Grid_generator_widening_assign_with_token/3,
   ppl_Grid_generator_widening_assign/2,
   ppl_Grid_limited_generator_extrapolation_assign_with_token/4,
   ppl_Grid_limited_generator_extrapolation_assign/3
  ]).


group_predicates(grid_handle_exceptions,
  'all Grid predicates'' exception handling.'
  ).

%%%%%%%%%%%%%%%%%%%%%%% System flags %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% pl_check_prolog_flag/2
% returns true or false (if the 1st argument is 'bounded')
% or (if the 1st argument is 'max_integer' or  'min_integer')
% the maximum or minimum integer for Prolog
% systems that have bounded integers.
% Note that 268435456 is 2^28.

pl_check_prolog_flag(bounded, TF) :-
  \+ prolog_system('XSB'),
  current_prolog_flag(bounded, TF).

pl_check_prolog_flag(bounded, true) :-
  prolog_system('XSB').

pl_check_prolog_flag(max_integer, Max_Int) :-
  \+ prolog_system('XSB'),
  current_prolog_flag(max_integer, Max_Int).

pl_check_prolog_flag(max_integer, Max_Int) :-
  prolog_system('XSB'), Max_Int is 268435455.

pl_check_prolog_flag(min_integer, Min_Int) :-
  \+ prolog_system('XSB'),
  current_prolog_flag(min_integer, Min_Int).

pl_check_prolog_flag(min_integer, Min_Int) :-
  prolog_system('XSB'), Min_Int is -268435456.
