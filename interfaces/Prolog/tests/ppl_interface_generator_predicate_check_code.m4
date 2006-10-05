m4_define(`dnl', `m4_dnl')
dnl This file contains the schematic tests for the Prolog interface predicates.
dnl
m4_define(`m4_add_extra_class_code', `dnl
m4_ifelse(m4_cplusplus_class$1, Polyhedron,
  `
ppl_new_Polyhedron_from_space_dimension(Dim, UorE, PS) :-
    ppl_new_C_Polyhedron_from_space_dimension(Dim, UorE, PS).
')
')

dnl Note that to avoid m4 treating commas as m4 argument separators,
dnl all tests must be between `(' and `)'.
m4_divert(-1)
m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_code',
`
ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_test(ok) :-
  (
  ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(0, empty, PS),
  ppl_delete_@CLASS@(PS),
  !).
ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_test(notok).

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@_code',
`
ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@_test(ok) :-
  (
  ppl_new_@INTOPOLOGY@@FRIEND@_from_space_dimension(0, universe, PS),
  ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@(PS, PS1),
  ppl_delete_@FRIEND@(PS),
  ppl_delete_@CLASS@(PS1),
  !).
ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@_test(notok).

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_code',
`
ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_test(ok) :-
  (make_vars(1, Vs),
  ppl_@BUILD_REPRESENT@s_test_data(1, CS1, Vs),
  ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s(CS1, PS1),
  ppl_delete_@CLASS@(PS1),
  ppl_@BUILD_REPRESENT@s_test_data(2, CS2, Vs),
  ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s(CS2, PS2),
  ppl_delete_@CLASS@(PS2),
  !).
ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_test(notok).

')


m4_divert`'dnl
