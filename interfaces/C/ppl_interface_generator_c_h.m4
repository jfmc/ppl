m4_define(`dnl', `m4_dnl')
dnl This file is to generate ppl_c.h.
dnl
dnl Include files defining macros that generate the non-fixed part.
m4_include(`ppl_interface_generator_c_h_code.m4')dnl
m4_include(`ppl_interface_generator_common.m4')dnl
m4_include(`ppl_interface_generator_c_dat.m4')dnl
dnl
m4_divert(-1)dnl

dnl m4_pre_extra_class_code(Class, CPP_Class, Class_Kind)
dnl Prefix extra code for each class.
define(`m4_pre_extra_class_code', `dnl

PPL_TYPE_DECLARATION(m4_interface_class$1);

/*! \name Functions Related to m4_interface_class$1 */
/*@{*/

')dnl

dnl m4_post_extra_class_code(Class, CPP_Class, Class_Kind)
dnl Postfix extra code for each class.
define(`m4_post_extra_class_code', `dnl
PPL_DECLARE_OUTPUT_FUNCTIONS(m4_interface_class$1)

/*@}*/ /* Functions Related to m4_interface_class$1 */
')

m4_divert`'dnl
dnl
dnl Output the fixed preamble.
m4_include(`ppl_interface_generator_c_h_preamble')
dnl
dnl Generate the non-fixed part of the file.
m4_all_classes_code`'dnl
dnl
dnl Generate the fixed postamble.

#ifdef __cplusplus
} /* extern "C" */
#endif

#undef PPL_TYPE_DECLARATION
#undef PPL_DECLARE_PRINT_FUNCTIONS
#undef PPL_DECLARE_ASCII_DUMP_FUNCTIONS
#undef PPL_DECLARE_OUTPUT_FUNCTIONS
#undef PPL_PROTO

/*@}*/ /* \defgroup PPL_C_interface */

#endif /* !defined(PPL_ppl_c_h) */
dnl
dnl End of file generation.
