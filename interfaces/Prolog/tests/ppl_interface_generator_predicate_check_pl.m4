m4_define(`dnl', `m4_dnl')
dnl This file generates ppl_predicate_check.pl.
/* Prolog code for checking all predicates.  -*- C++ -*-
m4_include(`ppl_interface_generator_copyright')dnl
*/`'dnl
dnl
dnl ==================================================================
dnl Common files are included here
dnl ==================================================================
dnl
m4_include(`ppl_interface_generator_common.m4')`'dnl
m4_include(`ppl_interface_generator_prolog_dat.m4')dnl
m4_include(`ppl_interface_generator_predicate_check_code.m4')`'dnl
dnl
dnl ==================================================================
dnl Macros needed to generate all the test code, both for the library
dnl and the class dependent predicates
dnl ==================================================================
dnl
m4_define(`m4_filter_code', `dnl
m4_ifelse($#, 0, , $#, 1, , $#, 2,
  `m4_ifelse(`$2', `', keep,
    `m4_ifelse(m4_index($1, `m4_regexp($2, `ppl_[^ /]+', `\&')'), -1,
      keep, throw)')',
  `m4_ifelse(m4_index($1, m4_regexp($2, `ppl_[^ /]+', `\&')), -1,
    `m4_filter_code($1, m4_shift(m4_shift($@)))', throw)')')`'dnl
dnl
m4_define(`m4_check_test_usability', `dnl
m4_filter_code(m4_indir($1_code),
  m4_filter_all_procedures($2, 0, m4_procedure_list))')`'dnl
dnl
dnl ==================================================================
dnl The top level call is a call to a test for each predicate
dnl ==================================================================
dnl
check_all :-
  ppl_initialize,
m4_divert(1)`'dnl
  ppl_finalize.

dnl
dnl ==================================================================
dnl Any required declarations for the tests go here
dnl ==================================================================
dnl
m4_divert(2)`'dnl
dnl
dnl ==================================================================
dnl Tests for the library predicates go here
dnl ==================================================================
dnl
m4_divert(3)`'dnl
dnl
dnl ==================================================================
dnl Tests for the class dependent predicates go here
dnl ==================================================================
dnl
m4_divert(4)`'dnl
dnl
dnl ==================================================================
dnl Test data and other generic code goes here
dnl ==================================================================
dnl
m4_divert(5)`'dnl
dnl
dnl ==================================================================
dnl Generate code for divert(1), the top-level call
dnl ==================================================================
dnl
dnl -----------------------------------------------------------------
dnl Extra files and definitions for divert(1)
dnl -----------------------------------------------------------------
m4_include(`ppl_interface_generator_prolog_systems.m4')dnl
m4_define(`m4_start1', 0)`'dnl
m4_pushdef(`m4_extension', `dnl
m4_ifdef(`$1_code',
         `m4_ifelse(m4_check_test_usability($1, $5), keep,
                    `m4_ifelse(m4_start1, 0,
                      `m4_undefine(`m4_start1')', `
')'  ((\+$1_test(ok)COMMA
    $1_test(notok))
      -> write_error($1)
      ;  true)COMMA
)')`'dnl
')`'dnl
dnl
dnl -----------------------------------------------------------------
dnl Main calls to macros to generate code for divert(1)
dnl -----------------------------------------------------------------
m4_patsubst(m4_library_names_to_code(0, m4_library_predicate_list),
            COMMA, `,')`'dnl
m4_patsubst(m4_all_code, COMMA, `,')`'dnl
m4_undivert(1)`'dnl
m4_divert`'dnl
dnl
dnl ==================================================================
dnl Generate code for divert(2), the declarations
dnl ==================================================================
dnl
dnl -----------------------------------------------------------------
dnl Extra definitions for divert(2)
dnl -----------------------------------------------------------------
m4_popdef(`m4_extension')`'dnl
m4_pushdef(`m4_extension', `dnl
m4_ifdef(`$1_code',
         `m4_ifelse(m4_check_test_usability($1, $5), keep,
:- discontiguous($1_test/1).)')
')`'dnl
dnl -----------------------------------------------------------------
dnl Main call to macros to generate code for divert(2)
dnl -----------------------------------------------------------------
m4_all_code`'dnl
dnl
m4_undivert(2)`'dnl
m4_divert`'dnl
dnl
dnl ==================================================================
dnl Generate code for divert(3), the library predicate tests
dnl ==================================================================
dnl
dnl -----------------------------------------------------------------
dnl Extra definitions for divert(3)
dnl -----------------------------------------------------------------
m4_popdef(`m4_extension')`'dnl
m4_ifdef(`$1_code',
`m4_ifelse(m4_check_test_usability($1, $5), keep, m4_indir(`$1_code'))',
         `m4_default_code($1)')`'dnl
dnl Define a default test.
m4_pushdef(`m4_default_code', `')`'dnl
dnl
dnl -----------------------------------------------------------------
dnl Main call to macros to generate code for divert(3)
dnl -----------------------------------------------------------------
m4_patsubst(m4_library_names_to_code(0, m4_library_predicate_list),
  COMMA, `,')`'dnl
dnl
m4_undivert(3)`'dnl
m4_divert`'dnl
dnl
dnl ==================================================================
dnl Generate code for divert(4), the class dependent predicate tests
dnl ==================================================================
dnl
dnl -----------------------------------------------------------------
dnl Extra definitions for divert(4)
dnl -----------------------------------------------------------------
dnl m4_pre_extra_class_code(Class_Counter, Class_Kind)
dnl Prefix extra code for each class.
m4_define(`m4_pre_extra_class_code', `dnl
m4_add_extra_class_code($1)`'dnl
')`'dnl
m4_include(`ppl_interface_generator_predicate_check_code.m4')`'dnl
dnl
m4_pushdef(`m4_default_code', `')`'dnl
dnl
m4_define(`m4_extension', `dnl
m4_ifdef(`$1_code',
`m4_ifelse(m4_check_test_usability($1, $5), keep, m4_indir(`$1_code'))',
         `m4_default_code($1)')`'dnl
')
dnl
dnl -----------------------------------------------------------------
dnl Main call to macros to generate code for divert(4)
dnl -----------------------------------------------------------------
m4_all_code`'dnl
dnl
m4_undivert(4)`'dnl
m4_divert`'dnl
dnl
dnl ==================================================================
dnl Generate code for divert(5), the test data and similar generic code
dnl ==================================================================
dnl
dnl -----------------------------------------------------------------
dnl Extra definitions for divert(5)
dnl -----------------------------------------------------------------
m4_popdef(`m4_default_code')`'dnl
dnl
dnl -----------------------------------------------------------------
dnl Main call to macros to generate code for divert(5)
dnl -----------------------------------------------------------------
m4_changequote(`@<<@',`@>>@')@<<@@>>@dnl
m4_include(
  @<<@ppl_interface_generator_predicate_check_extra_code@>>@)@<<@@>>@dnl
m4_changequote`'dnl
m4_undivert(5)`'dnl
