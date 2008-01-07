m4_define(`dnl', `m4_dnl')
dnl Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>
dnl
dnl This file is part of the Parma Polyhedra Library (PPL).
dnl
dnl The PPL is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by the
dnl Free Software Foundation; either version 3 of the License, or (at your
dnl option) any later version.
dnl
dnl The PPL is distributed in the hope that it will be useful, but WITHOUT
dnl ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
dnl FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
dnl for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software Foundation,
dnl Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.
dnl
dnl For the most up-to-date information see the Parma Polyhedra Library
dnl site: http://www.cs.unipr.it/ppl/ .

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
m4_define(`m4_pre_extra_class_code', `dnl

PPL_TYPE_DECLARATION(m4_interface_class$1);

/*! \name Functions Related to m4_interface_class$1 */
/*@{*/

')dnl

dnl m4_post_extra_class_code(Class, CPP_Class, Class_Kind)
dnl Postfix extra code for each class.
m4_define(`m4_post_extra_class_code', `dnl
PPL_DECLARE_OUTPUT_FUNCTIONS(m4_interface_class$1)

/*@}*/ /* Functions Related to m4_interface_class$1 */
')

m4_divert`'dnl
dnl
dnl Output the fixed preamble.
dnl As the preamble has quotes, first change the quote characters.
m4_changequote(`@<<@',`@>>@')@<<@@>>@dnl
m4_include(@<<@ppl_interface_generator_c_h_preamble@>>@)@<<@@>>@dnl
dnl Change the quote characters back to the standard.
m4_changequote`'dnl
dnl
dnl Generate the non-fixed part of the file.
m4_all_code`'dnl
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
