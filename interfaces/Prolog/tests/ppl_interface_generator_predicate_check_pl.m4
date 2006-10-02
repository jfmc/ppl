dnl This file generates ppl_predicate_check.pl.
/* Prolog code for checking all predicates.  -*- C++ -*-
include(`ppl_interface_generator_copyright')dnl
*/

dnl Include files defining macros needed to generate the
dnl definition of the top-level predicate that calls the indiviual tests.
include(`ppl_interface_generator_common.m4')dnl
include(`ppl_interface_generator_prolog_dat.m4')dnl

dnl define check_all_predicates:
dnl check_all_predicates :-
check_all :-
  ppl_initialize
divert(1)`'dnl
,
  ppl_finalise.
dnl
dnl the actual tests go here.
divert(2)`'dnl
dnl
dnl Generate a list of calls to all predicates
dnl.
include(`ppl_interface_generator_prolog_systems.m4')`'dnl
define(`m4_extension', `ifelse($4, 0, , `COMMA
')  ($1_test) -> true ;
    true')`'dnl
patsubst(m4_all_classes_code, COMMA, `,')`'dnl
undivert(1)`'dnl
divert`'dnl
dnl Generate the tests.
dnl
include(`ppl_interface_generator_common.m4')dnl
include(`ppl_interface_generator_predicate_check_code.m4')dnl
dnl Define a default test.
undefine(`default_code')`'dnl
define(`default_code', `$1_test.

')`'dnl
m4_all_classes_code`'dnl
dnl
m4_var_list_code`'dnl
m4_test_data_code`'dnl
dnl End of file generation.
undivert(2)`'dnl
divert`'dnl
