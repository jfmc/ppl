ifelse(
        This file is to generate ppl_prolog.icc in the
        interfaces/Prolog directory.
      )dnl
include(`ppl_interface_generator_prolog_icc_code.m4')dnl
include(`ppl_interface_generator_common.m4')dnl
include(`ppl_interface_generator_prolog_dat.m4')dnl
divert(-1)dnl

# add_widening_extrapolation_code
#
# adds the extra code used by the widening and extrapolation predicate code
# only if needed for that class. Takes care to set class name and
# dependent schemas in this code.
define(`add_widening_extrapolation_code',
  `define(`num_widenexps',
     m4_ifndef(num_`'class`'_widenexps, 0))dnl
ifelse(num_widenexps, 0, ,
  m4_set_class(m4_set_string(rstrct, widening_extrapolation_code)))')

# add_bop_assign_code
#
# adds the extra code used by the binary operator predicate code
# only if needed for that class.
define(`add_bop_assign_code',
  `define(`num_binops',
        m4_ifndef(num_`'class`'_binops, m4_ifndef(num_`'binops, 0)))dnl
ifelse(num_binops, 0, , m4_set_class(bop_assign_code))')

# ppl_prolog_icc_code
#
# For each recognised class in the "classes" list,
# takes main predicate input list and each predicate is checked
# to see if there is a macro with "_code" extension that defines the code.
# Then a macro sets the class and other schematic components.
define(`ppl_prolog_icc_code',
  `m4_forloop(`ind', 1, m4_num_possible_classes,
    `dnl
define(`class', Class`'ind)dnl
ifelse(index(m4_classes, class), -1, ,
`add_bop_assign_code`'dnl
add_widening_extrapolation_code`'dnl
m4_set_class(m4_procedure_names_to_code(m4_filter(class_predicate_list)))')')')

divert`'dnl
include(`ppl_interface_generator_prolog_icc_preamble')dnl
ppl_prolog_icc_code()dnl
