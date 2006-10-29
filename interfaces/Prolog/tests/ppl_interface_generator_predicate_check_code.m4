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

m4_define(`m4_add_build_class_code', `dnl
ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS) :-
  (ppl_dimension_test_data(TEST_DATA, space_dimension, Dim),
  clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Dim, universe, PS),
  ppl_build_test_data(TEST_DATA, t_@TOPOLOGY@, @CONSTRAINER@s, RS),
  ppl_@CLASS@_add_@CONSTRAINER@s(PS, RS)).

')

m4_define(`m4_add_comparison_class_code', `dnl
ppl_@CLASS@_comparison_check(is_disjoint_from, PS1, PS2, Result) :-
  (
   ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS1, PS1_Copy),
   ppl_@CLASS@_intersection_assign(PS1_Copy, PS2),
   (ppl_@CLASS@_is_empty(PS1_Copy)
   ->
    Result = true
   ;
    Result = false
   ),
   ppl_delete_@CLASS@(PS1_Copy)
  ).

ppl_@CLASS@_comparison_check(contains, PS1, PS2, Result) :-
  (
   ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS1, PS1_Copy),
   ppl_@CLASS@_intersection_assign(PS1_Copy, PS2),
   (ppl_@CLASS@_equals_@CLASS@(PS1_Copy, PS2)
   ->
    Result = true
   ;
    Result = false
   ),
  ppl_delete_@CLASS@(PS1_Copy)
  ).

ppl_@CLASS@_comparison_check(strictly_contains, PS1, PS2, Result) :-
  (
   (ppl_@CLASS@_equals_@CLASS@(PS1, PS2)
   ->
     Result = false
   ;
     ppl_@CLASS@_comparison_check(contains, PS1, PS2, Result)
   )
  ).

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
   clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(1, empty, PS1),
   clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(0, universe, PS2),
   clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(1, universe, PS3),
   ppl_delete_@CLASS@(PS),
   ppl_delete_@CLASS@(PS1),
   ppl_delete_@CLASS@(PS2),
   ppl_delete_@CLASS@(PS3)
  ->
   fail ; true).

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@_code',
`
ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@_2_test :-
  (
   clean_ppl_new_@INTOPOLOGY@@FRIEND@_from_space_dimension(0, universe, PS),
   clean_ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@(PS, PS1),
   ppl_@FRIEND@_OK(PS),
   ppl_@CLASS@_OK(PS1),
   ppl_delete_@FRIEND@(PS),
   ppl_delete_@CLASS@(PS1)
  ->
   fail ; true).

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_code',
`
ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_2_test :-
  (
   member(TEST_DATA, [test00, test02, test03, test04, test05, test06]),
   (
    ppl_build_test_data(TEST_DATA, t_@TOPOLOGY@,
                        @BUILD_REPRESENT@s, RS1),
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s(RS1, PS1),
    ppl_build_test_data(TEST_DATA, t_@TOPOLOGY@,
                        @ALT_BUILD_REPRESENT@s, RS1a),
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
   member(TEST_DATA,
          [test00, test02, test04, test05, test06, test10, test11, test12]),
   (
    ppl_build_test_data(TEST_DATA, t_@TOPOLOGY@, box, Box),
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@(Box, PS),
    (@BOX@ == bounding_box
    ->
      ppl_@CLASS@_get_@BOX@(PS, any, Box1)
    ;
      ppl_@CLASS@_get_@BOX@(PS, Box1)
    ),
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@(Box1, PS1),
    ppl_@CLASS@_equals_@CLASS@(PS, PS1),
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
             [x(c(minf), c(2/1)), i(c(n), o(pinf))], _),
    ppl_delete_@CLASS@(PS),
    ppl_delete_@CLASS@(PS1)
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
    ppl_@CLASS@_OK(PS),
    ppl_@CLASS@_OK(PS1),
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
   member(TEST_DATA, [test00, test01, test02, test03, test04, test05, test06]),
    (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS),
     \+ppl_@CLASS@_@DIMENSION@(PS, 3),
     ppl_dimension_test_data(TEST_DATA, @DIMENSION@, Dim),
     ppl_@CLASS@_@DIMENSION@(PS, Dim),
     ppl_delete_@CLASS@(PS)
   ->
     fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_get_@GET_REPRESENT@s_code',
`
ppl_@CLASS@_get_@GET_REPRESENT@s_2_test :-
  (
   member(TEST_DATA, [test00, test02, test03, test04, test05, test06]),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS),
     ppl_@CLASS@_get_@GET_REPRESENT@s(PS, RS1),
     (predicate_exists(ppl_@CLASS@_add_@GET_REPRESENT@s)
     ->
       (ppl_initial_test_system(@GET_REPRESENT@, U_or_E),
        ppl_dimension_test_data(TEST_DATA, space_dimension, Space_Dim),
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
   member(TEST_DATA, [test00, test01, test02, test03, test04, test05, test06]),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS),
     ppl_@CLASS@_get_minimized_@GET_REPRESENT@s(PS, RS1),
     (predicate_exists(ppl_@CLASS@_add_@GET_REPRESENT@s)
    ->
       (ppl_initial_test_system(@GET_REPRESENT@, U_or_E),
        ppl_dimension_test_data(TEST_DATA, space_dimension, Space_Dim),
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
   member(TEST_DATA, [test00, test01, test02, test03, test04, test05, test06]),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS),
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
   member(TEST_DATA, [test00, test01, test02, test03, test04, test05, test06]),
   (CC = any ; CC = simplex ; CC = polynomial),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS),
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
   member(TEST_DATA, [test00, test01, test02, test03, test04, test05, test06]),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS),
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
   member(TEST_DATA, [test00, test01, test02, test03, test04, test05, test06]),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS),
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
   member(TEST_DATA, [test00, test02, test03, test04, test05, test06]),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS1),
     ppl_@CLASS@_@SIMPLIFY@(PS),
     ppl_@CLASS@_OK(PS),
     ppl_@CLASS@_contains_@CLASS@(PS, PS1),
     ppl_delete_@CLASS@(PS)
   ->
    fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_code',
`
ppl_@CLASS@_bounds_from_@ABOVEBELOW@_2_test :-
  (
   member(TEST_DATA, [test00, test01, test02, test03, test04, test05, test06]),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS),
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

m4_define(`ppl_@CLASS@_@MAXMIN@_code',
`
ppl_@CLASS@_@MAXMIN@_5_test :-
  (
   member(TEST_DATA, [test00, test01, test02, test03, test04, test05, test06]),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS),
     ppl_maxmin_test_data(TEST_DATA, t_@TOPOLOGY@, @CONSTRAINER@, @MAXMIN@,
                          LE, Nexptd, Dexptd, Bexptd, _, SuccessFlag),
     (SuccessFlag == true
     ->
       (ppl_@CLASS@_@MAXMIN@(PS, LE, N, D, B),
        B == Bexptd, N == Nexptd, D == Dexptd)
     ;
       \+ ppl_@CLASS@_@MAXMIN@(PS, LE, N, D, B)
     ),
     ppl_@CLASS@_OK(PS),
     ppl_delete_@CLASS@(PS)
   ->
     fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_@MAXMIN@_with_point_code',
`
ppl_@CLASS@_@MAXMIN@_with_point_6_test :-
  (
   member(TEST_DATA, [test00, test01, test02, test03, test04, test05, test06]),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS),
     ppl_dimension_test_data(TEST_DATA, space_dimension, Dim),
     ppl_maxmin_test_data(TEST_DATA, t_@TOPOLOGY@, @CONSTRAINER@, @MAXMIN@,
                          LE, Nexptd, Dexptd, Bexptd, Gexptd, SuccessFlag),
     (SuccessFlag == true
     ->
       (ppl_@CLASS@_@MAXMIN@_with_point(PS, LE, N, D, B, G),
        B == Bexptd, N == Nexptd, D == Dexptd,
        clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Dim, empty, PSG),
        clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Dim, empty,
                                                             PSGexptd),
        (G = closure_point(V)
        ->
          (Gexptd = closure_point(Vexptd),
           ppl_@CLASS@_add_@GENERATOR@(PSG, point(V)),
           ppl_@CLASS@_add_@GENERATOR@(PSGexptd, point(Vexptd)))
        ;
          (ppl_@CLASS@_add_@GENERATOR@(PSG, G),
           ppl_@CLASS@_add_@GENERATOR@(PSGexptd, Gexptd))
        ),
        ppl_@CLASS@_equals_@CLASS@(PSG, PSGexptd))
     ;
       \+ ppl_@CLASS@_@MAXMIN@_with_point(PS, LE, N, _, _, _)
     ),
     ppl_@CLASS@_OK(PS),
     ppl_delete_@CLASS@(PS)
   ->
     fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_@COMPARISON@_@CLASS@_code',
`
ppl_@CLASS@_@COMPARISON@_@CLASS@_2_test :-
  (
   member(TEST_DATA1, [test00, test01, test02, test03,
          test04, test05, test06]),
   ppl_dimension_test_data(TEST_DATA1, space_dimension, Dim),
   member(TEST_DATA2, [test00, test02, test03, test04, test05, test06]),
   ppl_dimension_test_data(TEST_DATA2, space_dimension, Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA1, PS1),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA2, PS2),
     ppl_@CLASS@_comparison_check(@COMPARISON@, PS1, PS2, Result),
     (Result == true
     ->
       ppl_@CLASS@_@COMPARISON@_@CLASS@(PS1, PS2)
     ;
       \+ ppl_@CLASS@_@COMPARISON@_@CLASS@(PS1, PS2)
     ),
     ppl_@CLASS@_OK(PS1),
     ppl_@CLASS@_OK(PS2),
     ppl_delete_@CLASS@(PS1),
     ppl_delete_@CLASS@(PS2)
   ->
     fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_code',
`
ppl_@CLASS@_add_@ADD_REPRESENT@s_2_test :-
  (
   member(TEST_DATA, [test00, test01, test02, test03, test04, test05, test06]),
   ppl_dimension_test_data(TEST_DATA, space_dimension, Dim),
   member(TEST_DATA1,
          [test00, test02, test03, test04, test05, test06]),
   ppl_dimension_test_data(TEST_DATA1, space_dimension, Dim),
   (
     ppl_build_test_data(TEST_DATA, t_@TOPOLOGY@, @ADD_REPRESENT@s, RS),
     ppl_build_test_data(TEST_DATA1, t_@TOPOLOGY@, @ADD_REPRESENT@s, RS1),
     ppl_initial_test_system(@ADD_REPRESENT@, U_or_E),
     ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Dim, U_or_E, PS),
     ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Dim, U_or_E, PS1),
     ppl_@CLASS@_add_@ADD_REPRESENT@s(PS, RS),
     ppl_@CLASS@_add_@ADD_REPRESENT@s(PS, RS1),
     ppl_@CLASS@_add_@ADD_REPRESENT@s(PS1, RS1),
     ppl_@CLASS@_add_@ADD_REPRESENT@s(PS1, RS),
     ppl_@CLASS@_equals_@CLASS@(PS, PS1),
     ppl_delete_@CLASS@(PS),
     ppl_delete_@CLASS@(PS1)
   ->
     fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize_code',
`
ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize_2_test :-
  (
   member(TEST_DATA, [test00, test01, test02, test03, test04, test05, test06]),
   ppl_dimension_test_data(TEST_DATA, space_dimension, Dim),
   member(TEST_DATA1,
          [test00, test02, test03, test04, test05, test06]),
   ppl_dimension_test_data(TEST_DATA1, space_dimension, Dim),
   (
     ppl_build_test_data(TEST_DATA, t_@TOPOLOGY@, @ADD_REPRESENT@s, RS),
     ppl_build_test_data(TEST_DATA1, t_@TOPOLOGY@, @ADD_REPRESENT@s, RS1),
     ppl_initial_test_system(@ADD_REPRESENT@, U_or_E),
     ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Dim, U_or_E, PS),
     ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Dim, U_or_E, PS1),
     ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Dim, U_or_E, PS_min),
     ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Dim, U_or_E, PS1_min),
     ppl_@CLASS@_add_@ADD_REPRESENT@s(PS, RS),
     ppl_@CLASS@_add_@ADD_REPRESENT@s(PS, RS1),
     ppl_@CLASS@_add_@ADD_REPRESENT@s(PS_min, RS),
     (ppl_@CLASS@_is_empty(PS)
     ->
       \+ ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize(PS_min, RS1),
       ppl_@CLASS@_add_@ADD_REPRESENT@s(PS_min, RS1)
     ;
       ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize(PS_min, RS1)
     ),
     ppl_@CLASS@_add_@ADD_REPRESENT@s(PS1, RS1),
     ppl_@CLASS@_add_@ADD_REPRESENT@s(PS1, RS),
     ppl_@CLASS@_add_@ADD_REPRESENT@s(PS1_min, RS1),
     (ppl_@CLASS@_is_empty(PS1)
     ->
       \+ ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize(PS1_min, RS),
       ppl_@CLASS@_add_@ADD_REPRESENT@s(PS1_min, RS)
     ;
       ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize(PS1_min, RS)
     ),
     ppl_@CLASS@_equals_@CLASS@(PS_min, PS1_min),
     ppl_@CLASS@_equals_@CLASS@(PS, PS1_min),
     ppl_@CLASS@_equals_@CLASS@(PS1, PS1_min),
     ppl_delete_@CLASS@(PS),
     ppl_delete_@CLASS@(PS1),
     ppl_delete_@CLASS@(PS1_min)
   ->
     fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_code',
`
:- discontiguous(ppl_@CLASS@_add_@ADD_REPRESENT@_2_test1/3).

ppl_@CLASS@_add_@ADD_REPRESENT@_2_test :-
  (
   member(TEST_DATA, [test00, test01, test02, test03, test04, test05, test06]),
   ppl_dimension_test_data(TEST_DATA, space_dimension, Dim),
   ppl_build_test_data(TEST_DATA, t_@TOPOLOGY@, @ADD_REPRESENT@s, RS),
   ppl_initial_test_system(@ADD_REPRESENT@, U_or_E),
   ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Dim, U_or_E, PS),
   ppl_@CLASS@_add_@ADD_REPRESENT@s(PS, RS),
   ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Dim, U_or_E, PS1),
   (ppl_@CLASS@_add_@ADD_REPRESENT@_2_test1(PS, PS1, RS)
   ->
     fail ; true)
  ).

ppl_@CLASS@_add_@ADD_REPRESENT@_2_test1(PS, PS1, []) :-
  (
   ppl_@CLASS@_equals_@CLASS@(PS, PS1),
   ppl_delete_@CLASS@(PS),
   ppl_delete_@CLASS@(PS1)
  ).
ppl_@CLASS@_add_@ADD_REPRESENT@_2_test1(PS, PS1, [R | RS]) :-
  (
     ppl_@CLASS@_add_@ADD_REPRESENT@(PS1, R),
     ppl_@CLASS@_add_@ADD_REPRESENT@_2_test1(PS, PS1, RS)
  ).

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_code',
`
:- discontiguous(ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_2_test1/3).

ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_2_test :-
  (
   member(TEST_DATA, [test00, test01, test02, test03, test04, test05, test06]),
   ppl_dimension_test_data(TEST_DATA, space_dimension, Dim),
   ppl_build_test_data(TEST_DATA, t_@TOPOLOGY@, @ADD_REPRESENT@s, RS),
   ppl_initial_test_system(@ADD_REPRESENT@, U_or_E),
   ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Dim, U_or_E, PS),
   ppl_@CLASS@_add_@ADD_REPRESENT@s(PS, RS),
   ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Dim, U_or_E, PS1),
   ((ppl_@CLASS@_is_empty(PS)
     ; ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_2_test1(PS, PS1, RS))
   ->
     fail ; true)
  ).

ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_2_test1(PS, PS1, []) :-
  (
   ppl_@CLASS@_equals_@CLASS@(PS, PS1),
   ppl_delete_@CLASS@(PS),
   ppl_delete_@CLASS@(PS1)
  ).
ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_2_test1(PS, PS1, [R | RS]) :-
  (
     ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize(PS1, R),
     ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_2_test1(PS, PS1, RS)
  ).

')

m4_define(`ppl_@CLASS@_@BINOP@_code',
`
ppl_@CLASS@_@BINOP@_2_test :-
  (
   member(TEST_DATA1, [test00, test01, test02, test03,
          test04, test05, test06]),
   ppl_dimension_test_data(TEST_DATA1, space_dimension, Dim),
   member(TEST_DATA2, [test00, test02, test03, test04, test05, test06]),
   ppl_dimension_test_data(TEST_DATA2, space_dimension, Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA1, PS1),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA2, PS2),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA2, PS2a),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS1, PS1_Copy),
     ppl_@CLASS@_@BINOP@(PS1_Copy, PS2),
     ppl_@CLASS@_equals_@CLASS@(PS2, PS2a),
     (@BINOP@ == intersection_assign
     ->
       ppl_@CLASS@_contains_@CLASS@(PS1, PS1_Copy),
       ppl_@CLASS@_contains_@CLASS@(PS2, PS1_Copy)
     ;
       (@BINOP@ == difference_assign ; @BINOP@ == poly_difference_assign
       ->
         ppl_@CLASS@_contains_@CLASS@(PS1, PS1_Copy)
       ;
         (@BINOP@ == time_elapse_assign
         ->
           (\+ ppl_@CLASS@_is_empty(PS2)
           ->
             ppl_@CLASS@_contains_@CLASS@(PS1_Copy, PS1)
           ;
             ppl_@CLASS@_is_empty(PS1_Copy)
           )
         ;
           (@BINOP@ == concatenate_assign
           ->
             ppl_@CLASS@_space_dimension(PS1, Dim1),
             ppl_@CLASS@_space_dimension(PS2, Dim2),
             Dim_Conc is Dim1 + Dim2,
             ppl_@CLASS@_space_dimension(PS1_Copy, Dim_Conc)
           ;
             member(@BINOP@,
                           [upper_bound_assign,
                            poly_hull_assign,
                            join_assign,
                            bds_hull_assign,
                            oct_hull_assign]),
             ppl_@CLASS@_contains_@CLASS@(PS1_Copy, PS1),
             ppl_@CLASS@_contains_@CLASS@(PS1_Copy, PS2)
           )
         )
       )
     ),
     ppl_@CLASS@_OK(PS1),
     ppl_@CLASS@_OK(PS1_Copy),
     ppl_@CLASS@_OK(PS2),
     ppl_delete_@CLASS@(PS1),
     ppl_delete_@CLASS@(PS1_Copy),
     ppl_delete_@CLASS@(PS2),
     ppl_delete_@CLASS@(PS2a)
   ->
     fail ; true)
 ).

')

m4_define(`ppl_@CLASS@_@BINMINOP@_code',
`
ppl_@CLASS@_@BINMINOP@_2_test :-
  (
   member(TEST_DATA1, [test00, test01, test02, test03,
          test04, test05, test06]),
   ppl_dimension_test_data(TEST_DATA1, space_dimension, Dim),
   member(TEST_DATA2, [test00, test02, test03, test04, test05, test06]),
   ppl_dimension_test_data(TEST_DATA2, space_dimension, Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA1, PS1),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA2, PS2),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA2, PS2a),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS1, PS1_Copy),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS1, PS1_Copy_n),
     (@BINMINOP@ == intersection_assign_and_minimize
     ->
       ppl_@CLASS@_intersection_assign(PS1_Copy_n, PS2),
       (\+ ppl_@CLASS@_is_empty(PS1_Copy_n)
       ->
         ppl_@CLASS@_@BINMINOP@(PS1_Copy, PS2),
         \+ ppl_@CLASS@_is_empty(PS1_Copy),
         ppl_@CLASS@_equals_@CLASS@(PS2, PS2a),
         ppl_@CLASS@_contains_@CLASS@(PS1, PS1_Copy),
         ppl_@CLASS@_contains_@CLASS@(PS2, PS1_Copy)
       ;
         \+ ppl_@CLASS@_@BINMINOP@(PS1_Copy, PS2)
       )
     ;
       member(@BINMINOP@,
                     [upper_bound_assign_and_minimize,
                      poly_hull_assign_and_minimize,
                      join_assign_and_minimize,
                      bds_hull_assign_and_minimize,
                      oct_hull_assign_and_minimize]),
       ppl_@CLASS@_upper_bound_assign(PS1_Copy_n, PS2),
       (\+ ppl_@CLASS@_is_empty(PS1_Copy_n)
       ->
         ppl_@CLASS@_@BINMINOP@(PS1_Copy, PS2),
         \+ ppl_@CLASS@_is_empty(PS1_Copy),
         ppl_@CLASS@_contains_@CLASS@(PS1_Copy, PS1),
         ppl_@CLASS@_contains_@CLASS@(PS1_Copy, PS2)
       ;
         \+ ppl_@CLASS@_@BINMINOP@(PS1_Copy, PS2)
       )
     ),
     ppl_@CLASS@_OK(PS1),
     ppl_@CLASS@_OK(PS1_Copy),
     ppl_@CLASS@_OK(PS2),
     ppl_delete_@CLASS@(PS1),
     ppl_delete_@CLASS@(PS1_Copy),
     ppl_delete_@CLASS@(PS1_Copy_n),
     ppl_delete_@CLASS@(PS2),
     ppl_delete_@CLASS@(PS2a)
   ->
     fail ; true)
 ).

')

dnl ppl_@CLASS@_@BINMINOP@/2 +simple,
dnl ppl_@CLASS@_@AFFIMAGE@/4 *nofail +simple,
dnl ppl_@CLASS@_bounded_@AFFIMAGE@/5 *nofail +shape -wr_shape,
dnl ppl_@CLASS@_generalized_@AFFIMAGE@/5 +shape,
dnl ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs/4 +shape,
dnl ppl_Grid_generalized_@AFFIMAGE@/6 +grid,
dnl ppl_Grid_generalized_@AFFIMAGE@_lhs_rhs/5 +grid,
dnl ppl_@CLASS@_@WIDEN@_widening_assign_with_tokens/4 +simple,
dnl ppl_@CLASS@_@WIDEN@_widening_assign/2 *nofail +simple,
dnl ppl_@CLASS@_@LIMITEDBOUNDED@_@EXTRAPOLATION@_extrapolation_assign_with_tokens/5 +simple,
dnl ppl_@CLASS@_@LIMITEDBOUNDED@_@EXTRAPOLATION@_extrapolation_assign/3 *nofail +simple,
dnl ppl_BD_Shape_CC76_extrapolation_assign_with_tokens/4 -bd_shape,
dnl ppl_BD_Shape_CC76_extrapolation_assign/2 *nofail -bd_shape,
dnl ppl_BD_Shape_CC76_narrowing_assign/2 -bd_shape,
dnl ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@/2 *nofail +simple_pps,
dnl ppl_@CLASS@_remove_space_dimensions/2 +simple_pps,
dnl ppl_@CLASS@_remove_higher_space_dimensions/2 *nofail +simple_pps,
dnl ppl_@CLASS@_expand_space_dimension/3 *nofail +simple -octagonal_shape,
dnl ppl_@CLASS@_fold_space_dimensions/3  +simple -octagonal_shape,
dnl ppl_@CLASS@_map_space_dimensions/2 +simple_pps

m4_divert`'dnl
