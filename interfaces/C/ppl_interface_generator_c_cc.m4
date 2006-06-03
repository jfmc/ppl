dnl This file is to generate ppl_c.cc.
dnl
dnl Include files defining macros that generate the non-fixed part.
include(`ppl_interface_generator_c_cc_code.m4')dnl
include(`ppl_interface_generator_common.m4')dnl
include(`ppl_interface_generator_c_dat.m4')dnl
divert(-1)dnl

# m4_extra_class_code
define(`m4_pre_extra_class_code', `dnl
DECLARE_CONVERSIONS(m4_class)

')dnl

define(`m4_post_extra_class_code', `dnl
DEFINE_PRINT_FUNCTIONS(m4_class)

')dnl

divert`'dnl
dnl Output the fixed preamble.
include(`ppl_interface_generator_c_cc_preamble')
dnl Generate the non-fixed postamble.
m4_all_classes_code`'dnl
