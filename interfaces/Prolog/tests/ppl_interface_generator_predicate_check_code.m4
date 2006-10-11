m4_define(`dnl', `m4_dnl')
dnl This file contains the schematic tests for the Prolog interface predicates.
dnl
m4_define(`m4_add_extra_class_code', `dnl
m4_ifelse(m4_cplusplus_class$1, Polyhedron,
  `
ppl_new_Polyhedron_from_space_dimension(Dim, UorE, PS) :-
    ppl_new_C_Polyhedron_from_space_dimension(Dim, UorE, PS).
')

ppl_cleanup_@CLASS@(_).
ppl_cleanup_@CLASS@(P) :-
  (ppl_delete_@CLASS@(P), fail).

ppl_cleanup_all_@CLASS@([]).
ppl_cleanup_all_@CLASS@([_|_]).
ppl_cleanup_all_@CLASS@([P|Ps]) :-
  delete_all_ppl_all_@CLASS@([P|Ps]).

ppl_delete_all_@CLASS@([]).
ppl_delete_all_@CLASS@([P|Ps]) :-
  (ppl_delete_@CLASS@(P),
  ppl_delete_all_@CLASS@(Ps)).

')

dnl Note that to avoid m4 treating commas as m4 argument separators,
dnl all tests must be between `(' and `)'.
m4_divert(-1)
m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_code',
`
ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_test :-
  (
   ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(0, empty, PS),
   ppl_delete_@CLASS@(PS)
  ->
   fail ; true).

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@_code',
`
ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@_test :-
  (
   ppl_new_@INTOPOLOGY@@FRIEND@_from_space_dimension(0, universe, PS),
   ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@(PS, PS1),
   ppl_delete_@FRIEND@(PS),
   ppl_delete_@CLASS@(PS1)
  ->
   fail ; true).

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_code',
`
ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_test :-
  (
   (TEST_DATA = 1 ; TEST_DATA = 2),
   (
    ppl_@BUILD_REPRESENT@s_test_data(TEST_DATA, Space_Dim, RS1, _),
    ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s(RS1, PS1),
    ppl_@ADD_REPRESENT@s_test_data(TEST_DATA,
                                   Space_Dim, RS1a, Universe_or_Empty1a),
    ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(Space_Dim,
                                                   Universe_or_Empty1a,
                                                   PS1a),
    ppl_@CLASS@_add_@ADD_REPRESENT@s(PS1a, RS1a),
    ppl_@CLASS@_equals_@CLASS@(PS1, PS1a),
    ppl_delete_@CLASS@(PS1),
    ppl_delete_@CLASS@(PS1a)
   ->
    fail ; true)
  ).

')


m4_divert`'dnl
