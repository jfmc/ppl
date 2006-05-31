dnl This file is to generate ppl_c.cc.
dnl
dnl Include files defining macros that generate the non-fixed part.
include(`ppl_interface_generator_c_cc_code.m4')dnl
include(`ppl_interface_generator_common.m4')dnl
include(`ppl_interface_generator_c_dat.m4')dnl
divert(-1)dnl

dnl m4_ppl_c_cc_code
dnl
dnl For each recognised class in the "classes" list,
dnl takes main predicate input list and each predicate is checked
dnl to see if there is a macro with "_code" extension that defines the code.
dnl Then a macro sets the class and other schematic components.
define(`m4_ppl_c_cc_code',
  `m4_forloop(`ind', 1, m4_num_possible_classes,
    `dnl
define(`class', Class`'ind)dnl
ifelse(index(m4_classes, class), -1, ,
`m4_set_class(m4_procedure_names_to_code(m4_filter(class_predicate_list)))'
)')')

divert`'dnl
dnl Output the fixed preamble.
include(`ppl_interface_generator_c_cc_preamble')
dnl Generate the non-fixed postamble.
m4_ppl_c_cc_code()dnl
