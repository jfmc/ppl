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
ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS, Dim) :-
  (clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Dim, universe, PS),
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

ppl_@CLASS@_comparison_check(geometrically_covers, _PS1, _PS2, _).

ppl_@CLASS@_comparison_check(geometrically_equals, _PS1, _PS2, _).

')

m4_define(`m4_add_wdn_exn_class_code', `dnl
ppl_@CLASS@_wdn_exn_check_code(PS1, PS1_Copy, PS2, PS2_Copy) :-
  (
     ppl_@CLASS@_contains_@CLASS@(PS1, PS1_Copy),
     ppl_@CLASS@_contains_@CLASS@(PS1, PS2),
     ppl_@CLASS@_equals_@CLASS@(PS2, PS2_Copy),
     ppl_@CLASS@_OK(PS1),
     ppl_@CLASS@_OK(PS2),
     ppl_delete_@CLASS@(PS1),
     ppl_delete_@CLASS@(PS1_Copy),
     ppl_delete_@CLASS@(PS2),
     ppl_delete_@CLASS@(PS2_Copy)
  ).

ppl_@CLASS@_wdn_exn_with_tokens_check_code(PS1, PS1a, PS1_Copy, PS2, PS2_Copy,
                                           T, T1) :-
  (
     ppl_@CLASS@_contains_@CLASS@(PS1, PS1_Copy),
     ppl_@CLASS@_contains_@CLASS@(PS1a, PS1_Copy),
     ppl_@CLASS@_contains_@CLASS@(PS1, PS2),
     ppl_@CLASS@_contains_@CLASS@(PS1a, PS1),
     ppl_@CLASS@_equals_@CLASS@(PS2, PS2_Copy),
     (T == 1
     ->
       ppl_@CLASS@_equals_@CLASS@(PS1, PS1a)
     ;
       T == 0,
       ppl_@CLASS@_equals_@CLASS@(PS1, PS1_Copy)
     ),
     T1 == 0,
     ppl_@CLASS@_OK(PS1),
     ppl_@CLASS@_OK(PS1a),
     ppl_@CLASS@_OK(PS2),
     ppl_delete_@CLASS@(PS1),
     ppl_delete_@CLASS@(PS1a),
     ppl_delete_@CLASS@(PS1_Copy),
     ppl_delete_@CLASS@(PS2),
     ppl_delete_@CLASS@(PS2_Copy)
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
%%    ppl_@CLASS@_equals_@CLASS@(PS1, PS1a),
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
    clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(3, empty, PS1),
    ppl_@CLASS@_swap(PS, PS1),
    (predicate_exists(ppl_@CLASS@_is_empty)
    ->
      ppl_@CLASS@_is_empty(PS),
      ppl_@CLASS@_is_universe(PS1)
    ;
      (predicate_exists(ppl_@CLASS@_geometrically_covers_@CLASS@)
      ->
        ppl_@CLASS@_geometrically_covers_@CLASS@(PS1, PS)
      ;
        true
      )
    ),
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
   choose_test(TEST_DATA, Space_Dim),
    (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS, Space_Dim),
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
   choose_test(TEST_DATA, Space_Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS, Space_Dim),
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
   choose_test(TEST_DATA, Space_Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS, Space_Dim),
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

m4_define(`ppl_@CLASS@_size_code',
`
ppl_@CLASS@_size_2_test :-
  (
   choose_test(TEST_DATA, _Space_Dim),
   ppl_build_test_data(TEST_DATA, t_@TOPOLOGY@, @CONSTRAINER@s, RS),
   (
     clean_ppl_new_@TOPOLOGY@@CLASS@_from_@CONSTRAINER@s(RS, PPS),
     ppl_@CLASS@_size(PPS, S),
     S == 1,
     ppl_@CLASS@_begin_iterator(PPS, It),
     ppl_@CLASS@_drop_disjunct(PPS, It),
     ppl_@CLASS@_size(PPS, 0),
     ppl_@CLASS@_delete_iterator(It),
     ppl_delete_@CLASS@(PPS)
   ->
     fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_begin_iterator_code',
`
ppl_@CLASS@_begin_iterator_2_test :-
  (
   choose_test(TEST_DATA, _Space_Dim),
   ppl_build_test_data(TEST_DATA, t_@TOPOLOGY@, @CONSTRAINER@s, RS),
   (
     clean_ppl_new_@TOPOLOGY@@CLASS@_from_@CONSTRAINER@s(RS, PPS),
     ppl_@CLASS@_begin_iterator(PPS, It_begin),
     ppl_@CLASS@_end_iterator(PPS, It_end),
     ppl_@CLASS@_OK(PPS),
     ppl_@CLASS@_delete_iterator(It_begin),
     ppl_@CLASS@_delete_iterator(It_end),
     ppl_delete_@CLASS@(PPS)
   ->
     fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_end_iterator_code',
`
ppl_@CLASS@_begin_iterator_2_test :-
  (
   (
     clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(1, empty, PPS),
     ppl_@CLASS@_size(PPS, S),
     (S > 0
     ->
       ppl_@CLASS@_begin_iterator(PPS, It),
       ppl_@CLASS@_drop_disjunct(PPS, It)
     ;
       true
     ),
     S = 0,
     ppl_@CLASS@_begin_iterator(PPS, It_begin),
     ppl_@CLASS@_end_iterator(PPS, It_end),
     ppl_@CLASS@_iterator_equals_iterator(It_begin, It_end),
     ppl_@CLASS@_OK(PPS),
     ppl_@CLASS@_delete_iterator(It_begin),
     ppl_@CLASS@_delete_iterator(It_end),
     ppl_delete_@CLASS@(PPS)
   ->
     fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_iterator_equals_iterator_code',
`
ppl_@CLASS@_iterator_equals_iterator_2_test :-
  (
   choose_test(TEST_DATA, _Space_Dim),
   ppl_build_test_data(TEST_DATA, t_@TOPOLOGY@, @CONSTRAINER@s, RS),
   (
     clean_ppl_new_@TOPOLOGY@@CLASS@_from_@CONSTRAINER@s(RS, PPS),
     ppl_@CLASS@_begin_iterator(PPS, It),
     ppl_@CLASS@_begin_iterator(PPS, It_begin),
     ppl_@CLASS@_iterator_equals_iterator(It, It_begin),
     ppl_@CLASS@_OK(PPS),
     ppl_@CLASS@_delete_iterator(It),
     ppl_@CLASS@_delete_iterator(It_begin),
     ppl_delete_@CLASS@(PPS)
   ->
     fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_increment_iterator_code',
`
ppl_@CLASS@_increment_iterator_1_test :-
  (
   choose_test(TEST_DATA, _Space_Dim),
   ppl_build_test_data(TEST_DATA, t_@TOPOLOGY@, @CONSTRAINER@s, RS),
   (
     clean_ppl_new_@TOPOLOGY@@CLASS@_from_@CONSTRAINER@s(RS, PPS),
     ppl_@CLASS@_begin_iterator(PPS, Itb),
     ppl_@CLASS@_begin_iterator(PPS, It_begin),
     ppl_@CLASS@_size(PPS, S),
     (S > 0
     ->
       ppl_@CLASS@_increment_iterator(Itb),
       ppl_@CLASS@_decrement_iterator(Itb)
     ),
     ppl_@CLASS@_iterator_equals_iterator(Itb, It_begin),
     ppl_@CLASS@_end_iterator(PPS, Ite),
     ppl_@CLASS@_end_iterator(PPS, It_end),
     (S > 0
     ->
       ppl_@CLASS@_decrement_iterator(Ite),
       ppl_@CLASS@_increment_iterator(Ite)
     ),
     ppl_@CLASS@_iterator_equals_iterator(Ite, It_end),
     ppl_@CLASS@_OK(PPS),
     ppl_@CLASS@_delete_iterator(Itb),
     ppl_@CLASS@_delete_iterator(It_begin),
     ppl_@CLASS@_delete_iterator(Ite),
     ppl_@CLASS@_delete_iterator(It_end),
     ppl_delete_@CLASS@(PPS)
   ->
     fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_drop_disjunct_code',
`
ppl_@CLASS@_drop_disjunct_2_test :-
  (
   TEST_DATA = test06, TEST_DATA1 = test07,
   ppl_build_test_data(TEST_DATA, t_@TOPOLOGY@, @CONSTRAINER@s, RS),
   ppl_build_test_data(TEST_DATA1, t_@TOPOLOGY@, @CONSTRAINER@s, RS1),
   (
     clean_ppl_new_@TOPOLOGY@@CLASS@_from_@CONSTRAINER@s(RS, PPS),
     clean_ppl_new_@ALT_DISJUNCT@_from_@CONSTRAINER@s(RS1, PS),
     ppl_@CLASS@_add_disjunct(PPS, PS),
     ppl_@CLASS@_size(PPS, S),
     S > 1,
     ppl_@CLASS@_begin_iterator(PPS, It),
     ppl_@CLASS@_increment_iterator(It),
     ppl_@CLASS@_drop_disjunct(PPS, It),
     S1 is S - 1,
     ppl_@CLASS@_size(PPS, S1),
     ppl_@CLASS@_decrement_iterator(It),
     ppl_@CLASS@_drop_disjunct(PPS, It),
     S2 is S1 - 1,
     ppl_@CLASS@_size(PPS, S2),
     ppl_@CLASS@_OK(PPS),
     ppl_@CLASS@_delete_iterator(It),
     ppl_delete_@CLASS@(PPS),
     ppl_delete_@DISJUNCT@(PS)
   ->
     fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_get_disjunct_code',
`
ppl_@CLASS@_get_disjunct_2_test :-
  (
   all_tests(Space_Dim, Tests),
   (
     clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Space_Dim, empty, PPS),
     ppl_@CLASS@_get_disjunct_2_test1(PPS, Tests),
     ppl_@CLASS@_begin_iterator(PPS, It),
     ppl_@CLASS@_end_iterator(PPS, It_end),
     ppl_@CLASS@_get_disjunct_2_test2(PPS, It, It_end, Space_Dim),
     ppl_@CLASS@_OK(PPS),
     ppl_@CLASS@_delete_iterator(It),
     ppl_@CLASS@_delete_iterator(It_end),
     ppl_delete_@CLASS@(PPS)
   ->
     fail ; true)
  ).

ppl_@CLASS@_get_disjunct_2_test1(_, []).
ppl_@CLASS@_get_disjunct_2_test1(PPS, [Test|Tests]) :-
  (
   ppl_build_test_data(Test, t_@TOPOLOGY@, @CONSTRAINER@s, RS),
   clean_ppl_new_@ALT_DISJUNCT@_from_@CONSTRAINER@s(RS, PS),
   ppl_@CLASS@_add_disjunct(PPS, PS),
   ppl_delete_@DISJUNCT@(PS),
   ppl_@CLASS@_get_disjunct_2_test1(PPS, Tests),
   !
  ).

ppl_@CLASS@_get_disjunct_2_test2(PPS, It, It_end, Space_Dim) :-
  (
   (ppl_@CLASS@_iterator_equals_iterator(It, It_end)
   ->
     true
   ;
     ppl_@CLASS@_get_disjunct(It, PS),
     ppl_@CLASS@_increment_iterator(It),
     ppl_@DISJUNCT@_OK(PS),
     ppl_@DISJUNCT@_space_dimension(PS, D),
     D = Space_Dim,
     ppl_@CLASS@_get_disjunct_2_test2(PPS, It, It_end, Space_Dim)
   )
  ).

')

m4_define(`ppl_@CLASS@_relation_with_@RELATION_REPRESENT@_code',
`
ppl_@CLASS@_relation_with_@RELATION_REPRESENT@_3_test :-
  (
   choose_test(TEST_DATA, Space_Dim),
   (
     class_@CLASS@ \= class_BD_Shape_int8_t,
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS, Space_Dim),
     ppl_relation_test_data(TEST_DATA, @RELATION_REPRESENT@, R, Rel_Expected),
     ppl_@CLASS@_relation_with_@RELATION_REPRESENT@(PS, R, Rel),
     Rel = Rel_Expected,
     ppl_delete_@CLASS@(PS)
   ->
     fail ; (class_@CLASS@ == class_BD_Shape_int8_t -> fail ; true))
  ).

')

m4_define(`ppl_@CLASS@_get_bounding_box_code',
`
ppl_@CLASS@_get_bounding_box_3_test :-
  (
   choose_test(TEST_DATA, Space_Dim),
   (CC = any ; CC = simplex ; CC = polynomial),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS, Space_Dim),
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
   choose_test(TEST_DATA, Space_Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS, Space_Dim),
     ppl_@CLASS@_get_covering_box(PS, Box),
     (predicate_exists(ppl_new_@TOPOLOGY@@CLASS@_from_covering_box)
     ->
       clean_ppl_new_@TOPOLOGY@@CLASS@_from_covering_box(Box, PS1),
       ppl_@CLASS@_get_covering_box(PS1, Box1),
       clean_ppl_new_@CLASS@_from_covering_box(Box1, PS2),
       ppl_@CLASS@_space_dimension(PS2, Space_Dim),
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
   choose_test(TEST_DATA, Space_Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS, Space_Dim),
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
   choose_test(TEST_DATA, Space_Dim),
   TEST_DATA = test04,
   \+ TEST_DATA = test00, \+ TEST_DATA = test02,
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS, Space_Dim),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS1, Space_Dim),
     ppl_@CLASS@_@SIMPLIFY@(PS),
     ppl_@CLASS@_OK(PS),
     (predicate_exists(ppl_@CLASS@_contains_@CLASS@)
     ->
       ppl_@CLASS@_contains_@CLASS@(PS, PS1)
     ;
       true
     ),
     (predicate_exists(ppl_@CLASS@_geometrically_equals_@CLASS@)
     ->
      %%% FIXME: There is a bug in the C++ system here.
      %% nl, ppl_@CLASS@_ascii_dump(PS),
      %% nl, ppl_@CLASS@_ascii_dump(PS1),
      %% ppl_@CLASS@_geometrically_equals_@CLASS@(PS, PS1),
      true
     ;
       true
     ),
     ppl_delete_@CLASS@(PS1),
     ppl_delete_@CLASS@(PS)
   ->
    fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_code',
`
ppl_@CLASS@_bounds_from_@ABOVEBELOW@_2_test :-
  (
   choose_test(TEST_DATA, Space_Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS, Space_Dim),
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

dnl FIXME:: The test fails for BD_Shape.
m4_define(`ppl_@CLASS@_@MAXMIN@_codeXXXXX',
`
ppl_@CLASS@_@MAXMIN@_5_test :-
  (
   choose_test(TEST_DATA, Space_Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS, Space_Dim),
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

dnl FIXME:: The test fails for BD_Shape.
m4_define(`ppl_@CLASS@_@MAXMIN@_with_point_codeXXXXX',
`
ppl_@CLASS@_@MAXMIN@_with_point_6_test :-
  (
   choose_test(TEST_DATA, Space_Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS, Space_Dim),
     ppl_maxmin_test_data(TEST_DATA, t_@TOPOLOGY@, @CONSTRAINER@, @MAXMIN@,
                          LE, Nexptd, Dexptd, Bexptd, Gexptd, SuccessFlag),
     (SuccessFlag == true
     ->
       (ppl_@CLASS@_@MAXMIN@_with_point(PS, LE, N, D, B, G),
        B == Bexptd, N == Nexptd, D == Dexptd,
        (predicate_exists(ppl_@CLASS@_add_@GENERATOR@)
        ->
          clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Space_Dim,
                                                             empty, PSG),
          clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Space_Dim,
                                                             empty, PSGexptd),
          (G =.. [closure_point|CP]
          ->
            Gexptd =.. [closure_point|CPexptd],
            G_Point =.. [point|CP],
            Gexptd_Point =.. [point|CPexptd],
            ppl_@CLASS@_add_@GENERATOR@(PSG, G_Point),
            ppl_@CLASS@_add_@GENERATOR@(PSGexptd, Gexptd_Point)
          ;
            (ppl_@CLASS@_add_@GENERATOR@(PSG, G),
             ppl_@CLASS@_add_@GENERATOR@(PSGexptd, Gexptd))
          ),
          ppl_@CLASS@_equals_@CLASS@(PSG, PSGexptd),
          ppl_delete_@CLASS@(PSG),
          ppl_delete_@CLASS@(PSGexptd)
        ;
          true
        ))
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
   choose_2_tests(TEST_DATA1, TEST_DATA2, Space_Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA1, PS1, Space_Dim),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA2, PS2, Space_Dim),
     (ppl_@CLASS@_@COMPARISON@_@CLASS@(PS1, PS2)
     ->
       ppl_@CLASS@_comparison_check(@COMPARISON@, PS1, PS2, true)
     ;
       ppl_@CLASS@_comparison_check(@COMPARISON@, PS1, PS2, false)
     ),
     ppl_@CLASS@_OK(PS1),
     ppl_@CLASS@_OK(PS2),
     ppl_delete_@CLASS@(PS1),
     ppl_delete_@CLASS@(PS2)
   ->
     fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_code',
`
:- discontiguous(ppl_@CLASS@_add_@ADD_REPRESENT@_2_test1/3).

ppl_@CLASS@_add_@ADD_REPRESENT@_2_test :-
  (
   choose_test(TEST_DATA, Space_Dim),
   ppl_build_test_data(TEST_DATA, t_@TOPOLOGY@, @ADD_REPRESENT@s, RS),
   ppl_initial_test_system(@ADD_REPRESENT@, U_or_E),
   ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Space_Dim, U_or_E, PS),
   ppl_@CLASS@_add_@ADD_REPRESENT@s(PS, RS),
   ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Space_Dim, U_or_E, PS1),
   (ppl_@CLASS@_add_@ADD_REPRESENT@_2_test1(PS, PS1, RS)
   ->
     fail ; true)
  ).

ppl_@CLASS@_add_@ADD_REPRESENT@_2_test1(PS, PS1, []) :-
  (
   (predicate_exists(ppl_@CLASS@_equals_@CLASS@)
      ->
     ppl_@CLASS@_equals_@CLASS@(PS, PS1)
   ;
     true
   ),
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
   choose_test(TEST_DATA, Space_Dim),
   (
     ppl_build_test_data(TEST_DATA, t_@TOPOLOGY@, @ADD_REPRESENT@s, RS),
     ppl_initial_test_system(@ADD_REPRESENT@, U_or_E),
     clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Space_Dim,
                                                          U_or_E, PS),
     ppl_@CLASS@_add_@ADD_REPRESENT@s(PS, RS),
     clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Space_Dim,
                                                        U_or_E, PS1),
     (predicate_exists(ppl_@CLASS@_is_empty)
     ->
       (\+ ppl_@CLASS@_is_empty(PS)
       ->
         ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_2_test1(PS, PS1, RS)
       ;
         true
       )
     ;
       true
     ),
     ppl_delete_@CLASS@(PS),
     ppl_delete_@CLASS@(PS1)
   ->
     fail ; true)
  ).

ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_2_test1(PS, PS1, []) :-
  (
   (predicate_exists(ppl_@CLASS@_equals_@CLASS@)
   ->
     ppl_@CLASS@_equals_@CLASS@(PS, PS1)
   ;
     true
   )
  ).
ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_2_test1(PS, PS1, [R | RS]) :-
  (
   ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize(PS1, R),
   ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_2_test1(PS, PS1, RS)
  ).

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_code',
`
ppl_@CLASS@_add_@ADD_REPRESENT@s_2_test :-
  (
   choose_2_tests(TEST_DATA1, TEST_DATA2, Space_Dim),
   (
     ppl_build_test_data(TEST_DATA1, t_@TOPOLOGY@, @ADD_REPRESENT@s, RS),
     ppl_build_test_data(TEST_DATA2, t_@TOPOLOGY@, @ADD_REPRESENT@s, RS1),
     ppl_initial_test_system(@ADD_REPRESENT@, U_or_E),
     ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Space_Dim, U_or_E, PS),
     ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Space_Dim, U_or_E, PS1),
     ppl_@CLASS@_add_@ADD_REPRESENT@s(PS, RS),
     ppl_@CLASS@_add_@ADD_REPRESENT@s(PS, RS1),
     ppl_@CLASS@_add_@ADD_REPRESENT@s(PS1, RS1),
     ppl_@CLASS@_add_@ADD_REPRESENT@s(PS1, RS),
     (predicate_exists(ppl_@CLASS@_equals_@CLASS@)
     ->
       ppl_@CLASS@_equals_@CLASS@(PS, PS1)
     ;
       true
     ),
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
   choose_2_tests(TEST_DATA1, TEST_DATA2, Dim),
   (
     ppl_build_test_data(TEST_DATA1, t_@TOPOLOGY@, @ADD_REPRESENT@s, RS),
     ppl_build_test_data(TEST_DATA2, t_@TOPOLOGY@, @ADD_REPRESENT@s, RS1),
     ppl_initial_test_system(@ADD_REPRESENT@, U_or_E),
     ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Dim, U_or_E, PS),
     ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Dim, U_or_E, PS1),
     ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Dim, U_or_E, PS_min),
     ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Dim, U_or_E, PS1_min),
     ppl_@CLASS@_add_@ADD_REPRESENT@s(PS, RS),
     ppl_@CLASS@_add_@ADD_REPRESENT@s(PS, RS1),
     ppl_@CLASS@_add_@ADD_REPRESENT@s(PS_min, RS),
     (predicate_exists(ppl_@CLASS@_is_empty)
     ->
       (ppl_@CLASS@_is_empty(PS)
       ->
         \+ ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize(PS_min, RS1),
         ppl_@CLASS@_add_@ADD_REPRESENT@s(PS_min, RS1)
       ;
         ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize(PS_min, RS1)
       )
     ;
       true
     ),
     ppl_@CLASS@_add_@ADD_REPRESENT@s(PS1, RS1),
     ppl_@CLASS@_add_@ADD_REPRESENT@s(PS1, RS),
     ppl_@CLASS@_add_@ADD_REPRESENT@s(PS1_min, RS1),
     (predicate_exists(ppl_@CLASS@_is_empty)
     ->
       (ppl_@CLASS@_is_empty(PS1)
       ->
         \+ ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize(PS1_min, RS),
         ppl_@CLASS@_add_@ADD_REPRESENT@s(PS1_min, RS)
       ;
         ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize(PS1_min, RS)
       )
     ;
       true
     ),
     (predicate_exists(ppl_@CLASS@_equals_@CLASS@)
     ->
       ppl_@CLASS@_equals_@CLASS@(PS_min, PS1_min),
       ppl_@CLASS@_equals_@CLASS@(PS, PS1_min),
       ppl_@CLASS@_equals_@CLASS@(PS1, PS1_min)
     ;
       true
     ),
     ppl_delete_@CLASS@(PS),
     ppl_delete_@CLASS@(PS_min),
     ppl_delete_@CLASS@(PS1),
     ppl_delete_@CLASS@(PS1_min)
   ->
     fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_add_disjunct_code',
`
ppl_@CLASS@_add_disjunct_2_test :-
  (
   choose_2_tests(TEST_DATA1, TEST_DATA2, Space_Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA1, PPS1, Space_Dim),
     ppl_@ALT_DISJUNCT@_build_test_object(TEST_DATA2, PS2, Space_Dim),
     ppl_@CLASS@_add_disjunct(PPS1, PS2),
     ppl_@CLASS@_OK(PPS1),
     ppl_delete_@CLASS@(PPS1),
     ppl_delete_Polyhedron(PS2)
   ->
     fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_@BINOP@_code',
`
ppl_@CLASS@_@BINOP@_2_test :-
  (
   choose_2_tests(TEST_DATA1, TEST_DATA2, Space_Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA1, PS1, Space_Dim),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA2, PS2, Space_Dim),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA2, PS2a, Space_Dim),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS1, PS1_Copy),
     ppl_@CLASS@_@BINOP@(PS1_Copy, PS2),
     (predicate_exists(ppl_@CLASS@_equals_@CLASS@)
     ->
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
       )
     ;
       true
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
   choose_2_tests(TEST_DATA1, TEST_DATA2, Space_Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA1, PS1, Space_Dim),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA2, PS2, Space_Dim),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA2, PS2a, Space_Dim),
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

m4_define(`ppl_@CLASS@_@AFFIMAGE@_code',
`ppl_@CLASS@_@AFFIMAGE@_4_test :-
  (
   choose_test(TEST_DATA, Space_Dim),
   Space_Dim > 0,
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS, Space_Dim),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS_Copy, Space_Dim),
     make_vars(Space_Dim, [Var| _Var_List]),
     ppl_@CLASS@_@AFFIMAGE@(PS, Var, Var + 5, 1),
     ppl_@CLASS@_@AFFIMAGE@(PS, Var, Var - 5, 1),
     (@AFFIMAGE@ == affine_image
     ->
       ppl_@CLASS@_@AFFIMAGE@(PS, Var, 3*Var, 1),
       ppl_@CLASS@_@AFFIMAGE@(PS, Var, Var, 3)
     ;
       ppl_@CLASS@_@AFFIMAGE@(PS, Var, Var, 3),
       ppl_@CLASS@_@AFFIMAGE@(PS, Var, 3*Var, 1)
     ),
     ppl_@CLASS@_equals_@CLASS@(PS, PS_Copy),
     ppl_@CLASS@_OK(PS),
     ppl_delete_@CLASS@(PS),
     ppl_delete_@CLASS@(PS_Copy)
   ->
     fail ; true)
 ).

')

m4_define(`ppl_@CLASS@_bounded_@AFFIMAGE@_code',
`
ppl_@CLASS@_bounded_@AFFIMAGE@_5_test :-
  (
   choose_test(TEST_DATA, Space_Dim),
   ppl_dimension_test_data(TEST_DATA, space_dimension, Space_Dim),
   Space_Dim > 0,
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS, Space_Dim),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS_Copy, Space_Dim),
     make_vars(Space_Dim, [Var| _Var_List]),
     ppl_@CLASS@_bounded_@AFFIMAGE@(PS, Var, Var, 2*Var, 3),
     ppl_@CLASS@_bounded_@AFFIMAGE@(PS, Var, 3*Var, 3*Var, 1),
     ppl_@CLASS@_OK(PS),
     ppl_@CLASS@_bounded_@AFFIMAGE@(PS_Copy, Var, 3*Var, 3*Var, 1),
     ppl_@CLASS@_bounded_@AFFIMAGE@(PS_Copy, Var, Var, 2*Var, 3),
     ppl_@CLASS@_equals_@CLASS@(PS, PS_Copy),
     ppl_delete_@CLASS@(PS_Copy),
     ppl_delete_@CLASS@(PS)
   ->
     fail ; true)
 ).

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE_code',
`
ppl_@CLASS@_generalized_@AFFIMAGE@_5_test :-
  (
   choose_test(TEST_DATA, Space_Dim),
   Space_Dim > 0,
   (t_@TOPOLOGY@ == t_NNC_
   ->
     member(Op, [>=, =<, =, >, <])
   ;
     member(Op, [>=, =<, =])
   ),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS, Space_Dim),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS_Copy, Space_Dim),
     make_vars(Space_Dim, [Var| _Var_List]),
     ppl_@CLASS@_generalized_@AFFIMAGE@(PS, Var, Op, 2*Var, 3),
     ppl_@CLASS@_generalized_@AFFIMAGE@(PS, Var, Op, Var + 2, 1),
     ppl_@CLASS@_OK(PS),
     ppl_@CLASS@_generalized_@AFFIMAGE@(PS_Copy, Var, Op, Var + 2, 1),
     ppl_@CLASS@_generalized_@AFFIMAGE@(PS_Copy, Var, Op, 2*Var, 3),
     ppl_@CLASS@_equals_@CLASS@(PS, PS_Copy),
     ppl_@CLASS@_OK(PS_Copy),
     ppl_delete_@CLASS@(PS_Copy),
     ppl_delete_@CLASS@(PS)
   ->
     fail ; true)
 ).

')

m4_define(`ppl_Grid_generalized_@AFFIMAGE@_code',
`
ppl_Grid_generalized_@AFFIMAGE@_6_test :-
  (
   choose_test(TEST_DATA, Space_Dim),
   Space_Dim > 0,
   (
     ppl_Grid_build_test_object(TEST_DATA, PS, Space_Dim),
     ppl_Grid_build_test_object(TEST_DATA, PS_Copy, Space_Dim),
     make_vars(Space_Dim, [Var| _Var_List]),
     ppl_Grid_generalized_@AFFIMAGE@(PS, Var, =, 2*Var, 3, 5),
     ppl_Grid_generalized_@AFFIMAGE@(PS, Var, =, Var + 2, 1, 0),
     ppl_Grid_OK(PS),
     ppl_Grid_generalized_@AFFIMAGE@(PS_Copy, Var, =, Var + 2, 1, 0),
     ppl_Grid_generalized_@AFFIMAGE@(PS_Copy, Var, =, 2*Var, 3, 5),
%%     ppl_Grid_equals_Grid(PS, PS_Copy),
     ppl_@CLASS@_OK(PS_Copy),
     ppl_delete_Grid(PS_Copy),
     ppl_delete_Grid(PS)
   ->
     fail ; true)
 ).

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_code',
`
ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_4_test :-
  (
   choose_test(TEST_DATA, Space_Dim),
   Space_Dim > 0,
   (t_@TOPOLOGY@ == t_NNC_
   ->
     member(Op, [>=, =<, =, >, <])
   ;
     member(Op, [>=, =<, =])
   ),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS, Space_Dim),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS_Copy, Space_Dim),
     make_vars(Space_Dim, [Var| _Var_List]),
     ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs(PS, 2*Var, Op, 2*(Var + 2)),
     ppl_@CLASS@_OK(PS),
     ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs(PS_Copy, Var, Op, Var + 2),
     ppl_@CLASS@_equals_@CLASS@(PS, PS_Copy),
     ppl_@CLASS@_OK(PS_Copy),
     ppl_delete_@CLASS@(PS_Copy),
     ppl_delete_@CLASS@(PS)
   ->
     fail ; true)
 ).

')

m4_define(`ppl_Grid_generalized_@AFFIMAGE@_lhs_rhs_code',
`
ppl_Grid_generalized_@AFFIMAGE@_lhs_rhs_5_test :-
  (
   choose_test(TEST_DATA, Space_Dim),
   Space_Dim > 0,
   (
     ppl_Grid_build_test_object(TEST_DATA, PS, Space_Dim),
     ppl_Grid_build_test_object(TEST_DATA, PS_Copy, Space_Dim),
     make_vars(Space_Dim, [Var| _Var_List]),
     ppl_Grid_generalized_@AFFIMAGE@_lhs_rhs(PS, Var + 2, =, 2*Var, 5),
     ppl_Grid_generalized_@AFFIMAGE@_lhs_rhs(PS, 1 - Var, =, Var + 2, 0),
     ppl_Grid_OK(PS),
     ppl_Grid_generalized_@AFFIMAGE@_lhs_rhs(PS_Copy,
                                                1 - Var, =, Var + 2, 0),
     ppl_Grid_generalized_@AFFIMAGE@_lhs_rhs(PS_Copy,
                                                Var + 2, =, 2*Var, 5),
%%     ppl_Grid_equals_Grid(PS, PS_Copy),
     ppl_Grid_OK(PS_Copy),
     ppl_delete_Grid(PS_Copy),
     ppl_delete_Grid(PS)
   ->
     fail ; true)
 ).

')

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_code',
`
ppl_@CLASS@_@WIDEN@_widening_assign_2_test :-
  (
   choose_2_tests(TEST_DATA1, TEST_DATA2, Space_Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA1, PS1, Space_Dim),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA2, PS2, Space_Dim),
     ppl_@CLASS@_upper_bound_assign(PS1, PS2),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS1, PS1_Copy),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS2, PS2_Copy),
     ppl_@CLASS@_@WIDEN@_widening_assign(PS1, PS2),
     ppl_@CLASS@_wdn_exn_check_code(PS1, PS1_Copy, PS2, PS2_Copy)
   ->
     fail ; true)
 ).

')

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_with_tokens_code',
`
ppl_@CLASS@_@WIDEN@_widening_assign_with_tokens_4_test :-
  (
   choose_2_tests(TEST_DATA1, TEST_DATA2, Space_Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA1, PS1, Space_Dim),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA2, PS2, Space_Dim),
     ppl_@CLASS@_upper_bound_assign(PS1, PS2),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS1, PS1_Copy),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS2, PS2_Copy),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS1, PS1a),
     ppl_@CLASS@_@WIDEN@_widening_assign_with_tokens(PS1, PS2, 1, T),
     ppl_@CLASS@_@WIDEN@_widening_assign_with_tokens(PS1a, PS2, 0, T1),
     ppl_@CLASS@_wdn_exn_with_tokens_check_code(PS1, PS1a, PS1_Copy,
                                                PS2, PS2_Copy, T, T1)
   ->
     fail ; true)
 ).

')

m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_code',
`
ppl_@CLASS@_@LIMITEDBOUNDED@_@EXTRAPOLATION@_extrapolation_assign_3_test :-
  (
   choose_2_tests(TEST_DATA1, TEST_DATA2, Dim),
   Dim > 0,
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA1, PS1, Dim),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA2, PS2, Dim),
     ppl_@CLASS@_upper_bound_assign(PS1, PS2),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS1, PS1_Copy),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS2, PS2_Copy),
     make_vars(Dim, [Var|_]),
     ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign(
           PS1, PS2, [Var = 1]),
     ppl_@CLASS@_wdn_exn_check_code(PS1, PS1_Copy, PS2, PS2_Copy)
   ->
     fail ; true)
 ).

')

m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_with_tokens_code',
`
ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_with_tokens_5_test :-
  (
   choose_2_tests(TEST_DATA1, TEST_DATA2, Dim),
   Dim > 0,
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA1, PS1, Dim),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA2, PS2, Dim),
     ppl_@CLASS@_upper_bound_assign(PS1, PS2),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS1, PS1_Copy),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS2, PS2_Copy),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS1, PS1a),
     make_vars(Dim, [Var|_]),
     ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_with_tokens(
                                                  PS1, PS2, [Var = 1], 1, T),
     ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_with_tokens(
                                                  PS1a, PS2, [Var = 1], 0, T1),
     ppl_@CLASS@_wdn_exn_with_tokens_check_code(PS1, PS1a, PS1_Copy,
                                                PS2, PS2_Copy, T, T1)
   ->
     fail ; true)
 ).

')

m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_extrapolation_assign_code',
`
ppl_@CLASS@_@EXTRAPOLATION@_extrapolation_assign_2_test :-
  (
   choose_2_tests(TEST_DATA1, TEST_DATA2, Space_Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA1, PS1, Space_Dim),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA2, PS2, Space_Dim),
     ppl_@CLASS@_upper_bound_assign(PS1, PS2),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS1, PS1_Copy),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS2, PS2_Copy),
     ppl_@CLASS@_@EXTRAPOLATION@_extrapolation_assign(PS1, PS2),
     ppl_@CLASS@_wdn_exn_check_code(PS1, PS1_Copy, PS2, PS2_Copy)
   ->
     fail ; true)
 ).

')

m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_extrapolation_assign_with_tokens_code',
`
ppl_@CLASS@_@EXTRAPOLATION@_extrapolation_assign_with_tokens_4_test :-
  (
   choose_2_tests(TEST_DATA1, TEST_DATA2, Space_Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA1, PS1, Space_Dim),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA2, PS2, Space_Dim),
     ppl_@CLASS@_upper_bound_assign(PS1, PS2),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS1, PS1_Copy),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS2, PS2_Copy),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS1, PS1a),
     ppl_@CLASS@_@EXTRAPOLATION@_extrapolation_assign_with_tokens(PS1, PS2,
                                                                  1, T),
     ppl_@CLASS@_@EXTRAPOLATION@_extrapolation_assign_with_tokens(PS1a, PS2,
                                                                  0, T1),
     ppl_@CLASS@_wdn_exn_with_tokens_check_code(PS1, PS1a, PS1_Copy,
                                                PS2, PS2_Copy, T, T1)
   ->
     fail ; true)
 ).

')

m4_define(`ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign_code',
`
ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign_2_test :-
  (
   choose_2_tests(TEST_DATA1, TEST_DATA2, Space_Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA1, PS1, Space_Dim),
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA2, PS2, Space_Dim),
     ppl_@CLASS@_upper_bound_assign(PS1, PS2),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS1, PS1_Copy),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS2, PS2_Copy),
     ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign(PS2, PS1),
     ppl_@CLASS@_contains_@CLASS@(PS2, PS2_Copy),
     ppl_@CLASS@_contains_@CLASS@(PS1, PS2),
     ppl_@CLASS@_equals_@CLASS@(PS1, PS1_Copy),
     ppl_@CLASS@_OK(PS1),
     ppl_@CLASS@_OK(PS2),
     ppl_delete_@CLASS@(PS1),
     ppl_delete_@CLASS@(PS1_Copy),
     ppl_delete_@CLASS@(PS2),
     ppl_delete_@CLASS@(PS2_Copy)
   ->
     fail ; true)
 ).

')

m4_define(`ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@_code',
`
ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@_2_test :-
  (
    (
      clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(1, universe, PS),
      ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@(PS, 1),
      ppl_@CLASS@_OK(PS),
      ppl_@CLASS@_space_dimension(PS, 2),
      (@EMBEDPROJECT@ == and_embed
      ->
        clean_ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(2, universe, PS1)
      ;
        make_vars(2, [Var0, Var1]),
        clean_ppl_new_@TOPOLOGY@@CLASS@_from_@CONSTRAINER@s(
                            [Var0 = Var0, Var1 = 0], PS1)
      ),
      (predicate_exists(ppl_@CLASS@_equals_@CLASS@)
      ->
        ppl_@CLASS@_equals_@CLASS@(PS, PS1),
        ppl_@CLASS@_OK(PS1),
        ppl_@CLASS@_OK(PS)
      ;
        true
      ),
      ppl_delete_@CLASS@(PS1),
      ppl_delete_@CLASS@(PS)

   ->
    fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_remove_higher_space_dimensions_code',
`
ppl_@CLASS@_remove_higher_space_dimensions_2_test :-
  (
   choose_test(TEST_DATA, Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS, Dim),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS, PS_Copy),
     ppl_@CLASS@_add_space_dimensions_and_embed(PS, 1),
     ppl_@CLASS@_OK(PS),
     ppl_@CLASS@_space_dimension(PS, Dim1),
     Dim1 is Dim + 1,
     ppl_@CLASS@_remove_higher_space_dimensions(PS, Dim),
     ppl_@CLASS@_space_dimension(PS, Dim),
     (predicate_exists(ppl_@CLASS@_equals_@CLASS@)
     ->
       ppl_@CLASS@_equals_@CLASS@(PS, PS_Copy)
     ;
       true
     ),
     ppl_@CLASS@_remove_higher_space_dimensions(PS, 0),
     ppl_@CLASS@_space_dimension(PS, 0),
     ppl_@CLASS@_OK(PS),
     ppl_delete_@CLASS@(PS),
     ppl_delete_@CLASS@(PS_Copy)
   ->
    fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_remove_space_dimensions_code',
`
ppl_@CLASS@_remove_space_dimensions_2_test :-
  (
   choose_test(TEST_DATA, Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS, Dim),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS, PS_Copy),
     ppl_@CLASS@_add_space_dimensions_and_embed(PS, 1),
     Dim1 is Dim + 1,
     ppl_@CLASS@_remove_space_dimensions(PS, []),
     ppl_@CLASS@_space_dimension(PS, Dim1),
     make_vars(Dim1, Var_List),
     append(_, [Var], Var_List),
     ppl_@CLASS@_remove_space_dimensions(PS, [Var]),
     (predicate_exists(ppl_@CLASS@_equals_@CLASS@)
     ->
       ppl_@CLASS@_equals_@CLASS@(PS, PS_Copy)
     ;
       true
     ),
     ppl_@CLASS@_OK(PS),
     ppl_delete_@CLASS@(PS),
     ppl_delete_@CLASS@(PS_Copy)
   ->
    fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_expand_space_dimension_code',
`
ppl_@CLASS@_expand_space_dimension_3_test :-
  (
   choose_test(TEST_DATA, Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS, Dim),
     ppl_@CLASS@_add_space_dimensions_and_embed(PS, 1),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS, PS_Copy),
     Dim1 is Dim + 1,
     make_vars(1, [Var]),
     ppl_@CLASS@_expand_space_dimension(PS, Var, 0),
     ppl_@CLASS@_equals_@CLASS@(PS, PS_Copy),
     make_vars(Dim1, Var_List),
     append(_, [Var1], Var_List),
     ppl_@CLASS@_expand_space_dimension(PS, Var1, 1),
     Dim2 is Dim1 + 1,
     ppl_@CLASS@_space_dimension(PS, Dim2),
     ppl_@CLASS@_remove_higher_space_dimensions(PS, Dim1),
     ppl_@CLASS@_equals_@CLASS@(PS, PS_Copy),
     ppl_@CLASS@_OK(PS),
     ppl_delete_@CLASS@(PS),
     ppl_delete_@CLASS@(PS_Copy)
   ->
    fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_fold_space_dimensions_code',
`
ppl_@CLASS@_fold_space_dimensions_3_test :-
  (
   choose_test(TEST_DATA, Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS, Dim),
     ppl_@CLASS@_add_space_dimensions_and_embed(PS, 1),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS, PS_Copy),
     Dim1 is Dim + 1,
     make_vars(Dim1, [Var | _]),
     ppl_@CLASS@_fold_space_dimensions(PS, [], Var),
     ppl_@CLASS@_equals_@CLASS@(PS, PS_Copy),
     ppl_@CLASS@_add_space_dimensions_and_embed(PS, 1),
     Dim2 is Dim1 + 1,
     make_vars(Dim2, Var_List),
     append(_, [Var1], Var_List),
     ppl_@CLASS@_fold_space_dimensions(PS, [Var], Var1),
     ppl_@CLASS@_space_dimension(PS, Dim1),
     ppl_@CLASS@_OK(PS),
     ppl_delete_@CLASS@(PS),
     ppl_delete_@CLASS@(PS_Copy)
   ->
    fail ; true)
  ).

')

m4_define(`ppl_@CLASS@_map_space_dimensions_code',
`
ppl_@CLASS@_map_space_dimensions_2_test :-
  (
   choose_test(TEST_DATA, Dim),
   (
     ppl_@TOPOLOGY@@CLASS@_build_test_object(TEST_DATA, PS, Dim),
     ppl_@CLASS@_add_space_dimensions_and_embed(PS, 1),
     ppl_@CLASS@_add_space_dimensions_and_project(PS, 1),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS, PS_Copy),
     ppl_new_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@(PS, PSa),
     Dim1 is Dim + 2,
     ppl_@CLASS@_map_space_dimensions(PSa, []),
     ppl_@CLASS@_space_dimension(PSa, 0),
     make_vars(Dim1, Var_List),
     append(_, [Var, Var1], Var_List),
     make_map_vars(Dim, Var_Map_List),
     append(Var_Map_List, [Var-Var1, Var1-Var], Var_Map_List1),
     ppl_@CLASS@_map_space_dimensions(PS, Var_Map_List1),
     ppl_@CLASS@_map_space_dimensions(PS, Var_Map_List1),
     ppl_@CLASS@_space_dimension(PS, Dim1),
     (predicate_exists(ppl_@CLASS@_equals_@CLASS@)
     ->
       ppl_@CLASS@_equals_@CLASS@(PS, PS_Copy)
     ;
       true
     ),
     ppl_@CLASS@_OK(PS),
     ppl_delete_@CLASS@(PS),
     ppl_delete_@CLASS@(PSa),
     ppl_delete_@CLASS@(PS_Copy)
   ->
    fail ; true)
  ).

')

dnl ppl_@CLASS@_@NARROWING@_narrowing_assign/2,

m4_divert`'dnl
