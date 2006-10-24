m4_divert(-1)
m4_define(`dnl', `m4_dnl')
dnl This file contains the schematic tests for the Prolog interface predicates.
dnl
m4_define(`m4_add_topology_class_code', `dnl
m4_ifelse(m4_cplusplus_class$1, Polyhedron,
  `
clean_ppl_new_Polyhedron_from_space_dimension(Dim, UorE, PS) :-
    clean_ppl_new_C_Polyhedron_from_space_dimension(Dim, UorE, PS).
')

')

m4_define(`m4_add_cleanup_class_code', `dnl
ppl_cleanup_@CLASS@(_).
ppl_cleanup_@CLASS@(P) :-
  (out_@CLASS@(P), fail).

ppl_cleanup_all_@CLASS@([]).
ppl_cleanup_all_@CLASS@([_|_]).
ppl_cleanup_all_@CLASS@([P|Ps]) :-
  delete_all_ppl_all_@CLASS@([P|Ps]).

ppl_delete_all_@CLASS@([]).
ppl_delete_all_@CLASS@([P|Ps]) :-
  (ppl_delete_@CLASS@(P),
  ppl_delete_all_@CLASS@(Ps)).

')

m4_define(`m4_add_out_class_code', `dnl
out_@CLASS@(P):-
  ((noisy(N), N < 2) -> true ;
    ppl_@CLASS@_get_@GET_REPRESENT@s(P, RS),
    display_message([nl, @GET_REPRESENT@s, are, nl, RS, nl]),
    fail
  ).

')

m4_define(`m4_add_out_extra_class_code', `dnl
out_@CLASS@(P) :-
  ppl_delete_@CLASS@(P).

')

dnl Note that to avoid m4 treating commas as m4 argument separators,
dnl all tests must be between `(' and `)'.
m4_divert(-1)
m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_code',
`
ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_3_test :-
  (
   clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(0, empty, PS),
   ppl_delete_@CLASS@(PS)
  ->
   fail ; true).

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@_code',
`
ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@_2_test :-
  (
   clean_ppl_new_@INTOPOLOGY@@FRIEND@_from_space_dimension(0, universe, PS),
   clean_ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@(PS, PS1),
   ppl_delete_@FRIEND@(PS),
   ppl_delete_@CLASS@(PS1)
  ->
   fail ; true).

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_code',
`
ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_2_test :-
  (
   (TEST_DATA = e0 ; TEST_DATA = 0 ;
    TEST_DATA = 1 ; TEST_DATA = 2 ; TEST_DATA = 3),
   (
    ppl_build_test_data(TEST_DATA, t_@TOPOLOGY@,
                        @BUILD_REPRESENT@s, Space_Dim, RS1),
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s(RS1, PS1),
    ppl_build_test_data(TEST_DATA, t_@TOPOLOGY@,
                        @ALT_BUILD_REPRESENT@s, Space_Dim, RS1a),
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_@ALT_BUILD_REPRESENT@s(RS1a, PS1a),
    ppl_@CLASS@_equals_@CLASS@(PS1, PS1a),
    ppl_delete_@CLASS@(PS1),
    ppl_delete_@CLASS@(PS1a)
    ->
    fail ; true)
  ).

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@_code',
`
ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@_2_test :-
  (
   (TEST_DATA = e0; TEST_DATA = e1; TEST_DATA = 0;
    TEST_DATA = 1; TEST_DATA = 2; TEST_DATA = 3;
    TEST_DATA = 4; TEST_DATA = 5; TEST_DATA = 6),
   (
    ppl_build_test_data(TEST_DATA, t_@TOPOLOGY@, box, _Space_Dim, Box),
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@(Box, PS),
    (@BOX@ == bounding_box
    ->
      ppl_@CLASS@_get_@BOX@(PS, any, Box1)
    ;
      ppl_@CLASS@_get_@BOX@(PS, Box1)
    ),
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@(Box1, PS1),
    ppl_@CLASS@_equals_@CLASS@(PS, PS1),
    ppl_delete_@CLASS@(PS),
    ppl_delete_@CLASS@(PS1),
    \+ clean_ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@(Box, 0),
    \+ clean_ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@(
             [i(x, c(1/2)), i(c(0), o(pinf))], _),
    \+ clean_ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@(
             [i(x(minf), c(1/2)), i(c(0), o(pinf))], _),
    \+ clean_ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@(
             [i(o(minf), c(1/2)), i(c(0), c(pinf))], _),
    \+ clean_ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@(
             [i(c(minf), c(1/2)), i(c(0), o(pinf))], _),
    \+ clean_ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@(
             [i(o(minf), c(inf)), i(c(0), o(pinf))], _),
    \+ clean_ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@(
             [i(c(minf), c(1+2)), i(c(0), o(pinf))], _),
    \+ clean_ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@(
             [i(c(minf), c(n/2)), i(c(0), o(pinf))], _),
    \+ clean_ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@(
             [i(c(minf), c(2/d)), i(c(0), o(pinf))], _),
    \+ clean_ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@(
             [i(c(minf), c(2/1)), i(c(n), o(pinf))], _),
    \+ clean_ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@(
             [i(e), i(c(n), o(pinf))], _),
    \+ clean_ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@(
             [i(c(minf), c(2/1), c(1)), i(c(n), o(pinf))], _),
    \+ clean_ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@(
             [x(c(minf), c(2/1)), i(c(n), o(pinf))], _)
   ->
    fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_swap_code',
`
ppl_@CLASS@_swap_2_test :-
  (
   (
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(3, universe, PS),
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(2, empty, PS1),
    ppl_@CLASS@_swap(PS, PS1),
    ppl_@CLASS@_is_empty(PS),
    ppl_@CLASS@_is_universe(PS1),
    ppl_delete_@CLASS@(PS),
    ppl_delete_@CLASS@(PS1)
   ->
     fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_@DIMENSION@_code',
`
ppl_@CLASS@_@DIMENSION@_2_test :-
  (
   (
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(2, universe, PS),
    \+ppl_@CLASS@_@DIMENSION@(PS, 3),
    ppl_@CLASS@_@DIMENSION@(PS, N),
    N == 2,
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(2, empty, PS1),
    ppl_@CLASS@_@DIMENSION@(PS1, N1),
    (@DIMENSION@ == space_dimension -> N1 == 2 ; N1 == 0),
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s([], PS2),
    ppl_@CLASS@_@DIMENSION@(PS2, N2),
    N2 == 0,
    ppl_build_test_data(1, t_@TOPOLOGY@, @BUILD_REPRESENT@s, Space_Dim, RS3),
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s(RS3, PS3),
    ppl_@CLASS@_@DIMENSION@(PS3, N3),
    (@DIMENSION@ == space_dimension -> N3 == Space_Dim ; N3 == Space_Dim),
    ppl_build_test_data(2, t_@TOPOLOGY@, @BUILD_REPRESENT@s, Space_Dim, RS4),
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s(RS4, PS4),
    ppl_@CLASS@_@DIMENSION@(PS4, N4),
    (@DIMENSION@ == space_dimension -> N4 == Space_Dim ; N4 == 0),
    ppl_delete_@CLASS@(PS),
    ppl_delete_@CLASS@(PS1),
    ppl_delete_@CLASS@(PS2),
    ppl_delete_@CLASS@(PS3),
    ppl_delete_@CLASS@(PS4)
   ->
     fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_get_@GET_REPRESENT@s_code',
`
ppl_@CLASS@_get_@GET_REPRESENT@s_2_test :-
  (
   (TEST_DATA = e0; TEST_DATA = 0;
    TEST_DATA = 1; TEST_DATA = 2; TEST_DATA = 3),
   (
    ppl_build_test_data(TEST_DATA, t_@TOPOLOGY@,
                        @CONSTRAINER@s, Space_Dim, RS),
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_@CONSTRAINER@s(RS, PS),
    ppl_@CLASS@_get_@GET_REPRESENT@s(PS, RS1),
    (predicate_exists(ppl_@CLASS@_add_@GET_REPRESENT@s)
    ->
      (ppl_initial_test_system(@GET_REPRESENT@, U_or_E),
      clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Space_Dim,
                                                           U_or_E, PS1),
      ppl_@CLASS@_add_@GET_REPRESENT@s(PS1, RS1),
      ppl_@CLASS@_equals_@CLASS@(PS, PS1),
      ppl_delete_@CLASS@(PS1))
    ;
      true
    ),
    ppl_delete_@CLASS@(PS)
   ->
    fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_get_minimized_@GET_REPRESENT@s_code',
`
ppl_@CLASS@_get_minimized_@GET_REPRESENT@s_2_test :-
  (
   (TEST_DATA = e0; TEST_DATA = 0;
    TEST_DATA = 1; TEST_DATA = 2; TEST_DATA = 3),
   (
    ppl_build_test_data(TEST_DATA, t_@TOPOLOGY@,
                        @CONSTRAINER@s, Space_Dim, RS),
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_@CONSTRAINER@s(RS, PS),
    ppl_@CLASS@_get_minimized_@GET_REPRESENT@s(PS, RS1),
    (predicate_exists(ppl_@CLASS@_add_@GET_REPRESENT@s)
    ->
      (ppl_initial_test_system(@GET_REPRESENT@, U_or_E),
      clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Space_Dim,
                                                           U_or_E, PS1),
      ppl_@CLASS@_add_@GET_REPRESENT@s(PS1, RS1),
      ppl_@CLASS@_equals_@CLASS@(PS, PS1),
      ppl_delete_@CLASS@(PS1))
    ;
      true
    ),
    ppl_delete_@CLASS@(PS)
   ->
    fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_relation_with_@RELATION_REPRESENT@_code',
`
ppl_@CLASS@_relation_with_@RELATION_REPRESENT@_3_test :-
  (
   (TEST_DATA = e0; TEST_DATA = 0;
    TEST_DATA = 1; TEST_DATA = 2; TEST_DATA = 3),
   (
    ppl_build_test_data(TEST_DATA, t_@TOPOLOGY@,
                        @CONSTRAINER@s, _Space_Dim, RS),
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_@CONSTRAINER@s(RS, PS),
    ppl_relation_test_data(TEST_DATA, @RELATION_REPRESENT@, R, Rel_Expected),
    ppl_@CLASS@_relation_with_@RELATION_REPRESENT@(PS, R, Rel),
    Rel = Rel_Expected,
    ppl_delete_@CLASS@(PS)
   ->
    fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_get_bounding_box_code',
`
ppl_@CLASS@_get_bounding_box_3_test :-
  (
   (TEST_DATA = e0; TEST_DATA = 0;
    TEST_DATA = 1; TEST_DATA = 2; TEST_DATA = 3),
   (CC = any ; CC = simplex ; CC = polynomial),
   (
    ppl_build_test_data(TEST_DATA, t_@TOPOLOGY@,
                        @CONSTRAINER@s, _Space_Dim, RS),
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_@CONSTRAINER@s(RS, PS),
    ppl_@CLASS@_get_bounding_box(PS, CC, Box),
    (predicate_exists(ppl_new_@TOPOLOGY@@CLASS@_from_bounding_box)
    ->
      clean_ppl_new_@TOPOLOGY@@CLASS@_from_bounding_box(Box, PS1),
      ppl_@CLASS@_get_bounding_box(PS1, CC, Box1),
      clean_ppl_new_@TOPOLOGY@@CLASS@_from_bounding_box(Box1, PS2),
      ppl_@CLASS@_equals_@CLASS@(PS1, PS2),
      ppl_@CLASS@_contains_@CLASS@(PS2, PS),
      ppl_delete_@CLASS@(PS1),
      ppl_delete_@CLASS@(PS2)
    ;
      true
    ),
    ppl_delete_@CLASS@(PS)
   ->
    fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_get_covering_box_code',
`
ppl_@CLASS@_get_covering_box_2_test :-
  (
   (TEST_DATA = e0; TEST_DATA = 0;
    TEST_DATA = 1; TEST_DATA = 2; TEST_DATA = 3),
   (
    ppl_build_test_data(TEST_DATA, _, @CONSTRAINER@s, _Space_Dim, RS),
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_congruences(RS, PS),
    ppl_@CLASS@_get_covering_box(PS, Box),
    (predicate_exists(ppl_new_@TOPOLOGY@@CLASS@_from_covering_box)
    ->
      clean_ppl_new_@TOPOLOGY@@CLASS@_from_covering_box(Box, PS1),
      ppl_@CLASS@_get_covering_box(PS1, Box1),
      clean_ppl_new_@CLASS@_from_covering_box(Box1, PS2),
      ppl_@CLASS@_equals_@CLASS@(PS1, PS2),
      ppl_@CLASS@_contains_@CLASS@(PS2, PS),
      ppl_delete_@CLASS@(PS1),
      ppl_delete_@CLASS@(PS2)
    ;
      true
    ),
    ppl_delete_@CLASS@(PS)
   ->
    fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_@HAS_PROPERTY@_code',
`
ppl_@CLASS@_@HAS_PROPERTY@_2_test :-
  (
   (TEST_DATA = e0; TEST_DATA = 0;
    TEST_DATA = 1; TEST_DATA = 2; TEST_DATA = 3),
   (
    ppl_build_test_data(TEST_DATA, _, @CONSTRAINER@s, _Space_Dim, RS),
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_@CONSTRAINER@s(RS, PS),
    (ppl_property_test_data(TEST_DATA, t_@TOPOLOGY@,
                            @CONSTRAINER@, @HAS_PROPERTY@)
    ->
      ppl_@CLASS@_@HAS_PROPERTY@(PS)
    ;
      \+ ppl_@CLASS@_@HAS_PROPERTY@(PS)
    ),
    ppl_delete_@CLASS@(PS)
   ->
    fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_@SIMPLIFY@_code',
`
ppl_@CLASS@_@SIMPLIFY@_1_test :-
  (
   (TEST_DATA = e0; TEST_DATA = 0;
    TEST_DATA = 1; TEST_DATA = 2; TEST_DATA = 3),
   (
    ppl_build_test_data(TEST_DATA, _, @CONSTRAINER@s, _Space_Dim, RS),
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_@CONSTRAINER@s(RS, PS),
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_@CONSTRAINER@s(RS, PS1),
    ppl_@CLASS@_@SIMPLIFY@(PS),
    ppl_@CLASS@_OK(PS),
    ppl_@CLASS@_contains_@CLASS@(PS1, PS),
    ppl_delete_@CLASS@(PS)
   ->
    fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_code',
`
ppl_@CLASS@_bounds_from_@ABOVEBELOW@_2_test :-
  (
   (TEST_DATA = e0; TEST_DATA = 0;
    TEST_DATA = 1; TEST_DATA = 2; TEST_DATA = 3),
   (
    ppl_build_test_data(TEST_DATA, _, @CONSTRAINER@s, _Space_Dim, RS),
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_@CONSTRAINER@s(RS, PS),
    (ppl_bounds_test_data(TEST_DATA, @CONSTRAINER@s, LE,
                          @ABOVEBELOW@, true)
    ->
      ppl_@CLASS@_bounds_from_@ABOVEBELOW@(PS, LE)
    ;
      true
    ),
    (ppl_bounds_test_data(TEST_DATA, @CONSTRAINER@s, LE1,
                          @ABOVEBELOW@, false)
    ->
      \+ ppl_@CLASS@_bounds_from_@ABOVEBELOW@(PS, LE1)
    ;
      true
    ),
    ppl_@CLASS@_OK(PS),
    ppl_delete_@CLASS@(PS)
   ->
    fail ; true)
  ).

')

m4_divert`'dnl
