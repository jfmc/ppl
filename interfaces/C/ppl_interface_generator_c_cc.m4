dnl This file is to generate ppl_c.cc.
dnl
dnl Include files defining macros that generate the non-fixed part.
include(`ppl_interface_generator_c_cc_code.m4')dnl
include(`ppl_interface_generator_common.m4')dnl
include(`ppl_interface_generator_c_dat.m4')dnl
divert(-1)dnl

dnl m4_pre_extra_class_code(Class, CPP_Class, Class_Kind)
dnl Prefix extra code for each class.
define(`m4_pre_extra_class_code', `dnl
/* Interfacing m4_interface_class$1 */

DECLARE_CONVERSIONS(m4_interface_class$1, m4_cplusplus_class$1)

')dnl

dnl m4_pre_extra_class_code(Class, CPP_Class, Class_Kind)
dnl Postfix extra code for each class.
define(`m4_post_extra_class_code', `dnl
DEFINE_OUTPUT_FUNCTIONS(m4_interface_class$1)

')dnl

divert`'dnl
dnl
dnl Output the fixed preamble.
include(`ppl_interface_generator_c_cc_preamble')
dnl
dnl Generate the non-fixed part of the file.
m4_all_classes_code`'dnl
dnl
dnl End of file generation.
