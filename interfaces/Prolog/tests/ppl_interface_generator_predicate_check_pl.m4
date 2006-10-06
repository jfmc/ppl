m4_define(`dnl', `m4_dnl')
dnl This file generates ppl_predicate_check.pl.
/* Prolog code for checking all predicates.  -*- C++ -*-
m4_include(`ppl_interface_generator_copyright')dnl
*/

dnl Include files defining macros needed to generate the
dnl definition of the top-level predicate that calls the indiviual tests.

dnl define check_all_predicates:
dnl check_all_predicates :-
check_all :-
  ppl_initialize,
m4_divert(1)`'dnl
  ppl_finalize.
dnl
dnl the actual tests go here.
m4_divert(2)`'dnl
dnl
dnl Generate a list of calls to all predicates
dnl
m4_include(`ppl_interface_generator_prolog_dat.m4')dnl
m4_include(`ppl_interface_generator_prolog_systems.m4')dnl
m4_include(`ppl_interface_generator_common.m4')`'dnl
m4_pushdef(`m4_extension', `dnl
m4_ifelse($4, 0, , `COMMA
')  ((\+$1_test(ok),
    $1_test(notok))
      -> write_error($1)
      ;  true)')`'dnl
dnl
m4_patsubst(m4_library_names_to_code(0, m4_library_predicate_list),
            COMMA, `,')`'dnl
m4_patsubst(m4_all_code, COMMA, `,')`'dnl
`,'
m4_undivert(1)`'dnl
m4_divert`'dnl
dnl Generate the tests.
dnl
m4_popdef(`m4_extension')`'dnl
m4_include(`ppl_interface_generator_predicate_check_code.m4')`'dnl
dnl Define a default test.
m4_pushdef(`m4_default_code', `$1_test(ok).

')`'dnl
dnl m4_pre_extra_class_code(Class_Counter, Class_Kind)
dnl Prefix extra code for each class.
m4_define(`m4_pre_extra_class_code', `dnl
m4_add_extra_class_code($1)`'dnl
')`'dnl
m4_patsubst(m4_library_names_to_code(0, m4_library_predicate_list), COMMA, `,')`'dnl
m4_all_code`'dnl
dnl
m4_popdef(`m4_default_code')`'dnl
m4_changequote(`@<<@',`@>>@')@<<@@>>@dnl
m4_include(
  @<<@ppl_interface_generator_predicate_check_extra_code@>>@)@<<@@>>@dnl
m4_changequote@<<@@>>@dnl
dnl End of file generation.
m4_undivert(2)`'dnl
m4_divert`'dnl
