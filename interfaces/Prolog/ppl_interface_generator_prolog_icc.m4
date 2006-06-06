dnl This file generates ppl_prolog.icc.
dnl
include(`ppl_interface_generator_prolog_icc_code.m4')dnl
include(`ppl_interface_generator_common.m4')dnl
include(`ppl_interface_generator_prolog_dat.m4')dnl
divert(-1)dnl

# add_widening_extrapolation_code
#
# adds the extra code used by the widening and extrapolation predicate code
# only if needed for that class. Takes care to set class name and
# dependent schemas in this code.
define(`m4_add_widening_extrapolation_code',
  `define(`num_widenexps',
     m4_ifndef(num_`'m4_class`'_widenexps, 0))dnl
ifelse(num_widenexps, 0, ,
  m4_replace_class_patterns(m4_replace_pattern(constrainer,
    widening_extrapolation_code)))')

# add_bop_assign_code
#
# adds the extra code used by the binary operator predicate code
# only if needed for that class.
define(`m4_add_bop_assign_code',
  `define(`num_binops',
        m4_ifndef(num_`'m4_class`'_binops, m4_ifndef(num_`'binops, 0)))dnl
ifelse(num_binops, 0, , m4_replace_class_patterns(bop_assign_code))')

define(`m4_add_term_to_class_handle_code',
  `m4_replace_class_patterns(m4_term_to_class_handle_code)')

# m4_extra_class_code
#
# Extra code needed for the class in addition to the user-interface
# procedures.
define(`m4_pre_extra_class_code',
`m4_add_term_to_class_handle_code`'dnl
m4_add_bop_assign_code`'dnl
m4_add_widening_extrapolation_code')

divert`'dnl
include(`ppl_interface_generator_prolog_icc_preamble')dnl
m4_all_classes_code`'dnl

