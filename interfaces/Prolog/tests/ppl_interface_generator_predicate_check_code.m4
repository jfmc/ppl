m4_define(`dnl', `m4_dnl')
dnl This file contains the schematic tests for the Prolog interface predicates.
dnl
dnl Note that to avoid m4 treating commas as m4 argument separators,
dnl all tests must be between `(' and `)'.
dnl
dnl First we define some test data.
dnl
m4_define(`m4_test_data_code',
`
ppl_constraints_test_data(1, CS, _) :-
  CS = [].

ppl_constraints_test_data(2, CS, [A|_]) :-
  CS = [A = 0].

ppl_congruences_test_data(1, CS, [_]) :-
  CS = [].

ppl_congruence_system_test_data(2, CS, [A]) :-
  CS = [A = 0].

ppl_generator_system_test_data(1, GS, [A]) :-
  GS = [point, line(A)].

ppl_generator_system_test_data(2, GS, [A]) :-
  GS = [point(A)].

ppl_grid_generator_system_test_data(1, GS, [A]) :-
  GS = [grid_point, grid_line(A)].

ppl_grid_generator_system_test_data(2, GS, [A]) :-
  GS = [grid_point(A)].

')

m4_define(`m4_var_list_code',
`m4_changequote(`{{', `}}')
% make_var_list(+I,+Dimension,?Variable_List)
% constructs a list of variables with indices from I to Dimension - 1.
% It is assumed that I =< Dimension.

make_vars(Dim, Var_List) :-
  make_var_list(0, Dim, Var_List).
make_var_list(Dim, Dim, []) :- !.
make_var_list(I, Dim, ['$VAR'(I)|Var_List]) :-
  (I1 is I + 1,
  make_var_list(I1, Dim, Var_List)).

m4_changequote`'dnl
')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_code',
`
ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_test :-
  (
  ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(0, empty, PS),
  ppl_delete_@CLASS@(PS),
  !).

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@_code',
`
ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@_test :-
  (
  ppl_new_@INTOPOLOGY@@FRIEND@_from_space_dimension(0, universe, PS),
  ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@(PS, PS1),
  ppl_delete_@FRIEND@(PS),
  ppl_delete_@CLASS@(PS1),
  !).

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@REPRESENT@s_code',
`
ppl_new_@TOPOLOGY@@CLASS@_from_@REPRESENT@s_test :-
  (make_vars(1, Vs),
  ppl_@REPRESENT@s_test_data(1, CS, Vs),
  ppl_new_@TOPOLOGY@@CLASS@_from_@REPRESENT@s(CS, PS1),
  ppl_delete_@CLASS@(PS1),
  ppl_@REPRESENT@s_test_data(2, CS, Vs),
  ppl_new_@TOPOLOGY@@CLASS@_from_@REPRESENT@s(CS, PS2),
  ppl_delete_@CLASS@(PS2),
  !).

')


m4_divert`'dnl
