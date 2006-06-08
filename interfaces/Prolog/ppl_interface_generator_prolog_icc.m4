dnl This file generates ppl_prolog.icc.
dnl
dnl Include files defining macros that generate the non-fixed part.
include(`ppl_interface_generator_prolog_icc_code.m4')dnl
include(`ppl_interface_generator_common.m4')dnl
include(`ppl_interface_generator_prolog_dat.m4')dnl
dnl
divert(-1)dnl

dnl m4_add_term_to_class_handle_code(Class, CPP_Class)
dnl
dnl Adds the code to convert a term to a Class handle.
define(`m4_add_term_to_class_handle_code', `dnl
m4_replace_class_patterns($1, $2, m4_term_to_class_handle_code)dnl
')

dnl m4_add_bop_assign_code(Class, CPP_Class)
dnl
dnl Adds the extra code used by the binary operators.
define(`m4_add_bop_assign_code', `dnl
m4_replace_class_patterns($1, $2, bop_assign_code)dnl
')

dnl m4_get_num_widenexps(Class_Kind)
dnl
dnl Get the number of widenings for the provided Class_Kind.
define(`get_num_widenexps', `dnl
m4_ifndef(num_`$1'_widenexps,
          m4_ifndef(num_widenexps, 0))dnl
')

dnl m4_add_widening_extrapolation_code(Class, CPP_Class, Class_Kind)
dnl
dnl Adds the extra code used by the widening and extrapolation predicates.
define(`m4_add_widening_extrapolation_code', `dnl
ifelse(get_num_widenexps($3), 0, ,
  `m4_replace_class_patterns($1, $2,
                             m4_replace_pattern($3,
                                                widening_extrapolation_code,
                                                constrainer))')dnl
')

dnl m4_pre_extra_class_code(Class, CPP_Class, Class_Kind)
dnl Prefix extra code for each class.
define(`m4_pre_extra_class_code', `dnl
m4_add_term_to_class_handle_code($1, $2)`'dnl
m4_add_bop_assign_code($1, $2)`'dnl
m4_add_widening_extrapolation_code($1, $2, $3)`'dnl
')

divert`'dnl
dnl
dnl Output the fixed preamble.
include(`ppl_interface_generator_prolog_icc_preamble')
dnl
dnl Generate the non-fixed part of the file.
m4_all_classes_code`'dnl
dnl
dnl End of file generation.

