dnl This file is to generate ppl_c.h.
dnl
dnl Include files defining macros that generate the non-fixed part.
include(`ppl_interface_generator_c_h_code.m4')dnl
include(`ppl_interface_generator_common.m4')dnl
include(`ppl_interface_generator_c_dat.m4')dnl
divert(-1)dnl

dnl Prefix extra code for each class.
define(`m4_pre_extra_class_code', `dnl

PPL_TYPE_DECLARATION(m4_class);

/*! \name Functions Related to m4_class */
/*@{*/

')dnl

dnl Postfix extra code for each class.
define(`m4_post_extra_class_code', `dnl
PPL_DECLARE_PRINT_FUNCTIONS(m4_class)

/*@}*/ /* Functions Related to m4_class */
')

divert`'dnl
dnl
dnl Output the fixed preamble.
include(`ppl_interface_generator_c_h_preamble')
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
#undef PPL_PROTO

/*@}*/ /* \defgroup PPL_C_interface */

#endif /* !defined(PPL_ppl_c_h) */
dnl End of file generation.
