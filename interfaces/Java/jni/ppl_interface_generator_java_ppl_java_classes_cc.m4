m4_define(`dnl', `m4_dnl')`'dnl
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

dnl This file is to generate CLASS.java.
dnl
dnl Include files defining macros that generate the non-fixed part.
m4_divert(-1)dnl
m4_include(`ppl_interface_generator_java_ppl_java_classes_cc_code.m4')dnl
m4_include(`ppl_interface_generator_common.m4')dnl
m4_include(`ppl_interface_generator_common_dat.m4')dnl
m4_include(`ppl_interface_generator_java_procedure_generators.m4')dnl
dnl

dnl m4_pre_extra_class_code(Class, CPP_Class, Class_Kind)
dnl Prefix extra code for each class.
m4_define(`m4_pre_extra_class_code', `dnl
m4_define(`m4_this_class', `m4_interface_class$1')`'dnl
`#'include "ppl_java_`'m4_this_class`'.h"
m4_ifelse(m4_this_class, Polyhedron,
`#'include "ppl_java_C_Polyhedron.h"
`#'include "ppl_java_NNC_Polyhedron.h"
)`'dnl
m4_undefine(`m4_this_class')
using namespace Parma_Polyhedra_Library;
')

dnl m4_post_extra_class_code(Class, CPP_Class, Class_Kind)
dnl Postfix extra code for each class.
m4_define(`m4_post_extra_class_code', `')

m4_divert`'dnl
`#'include "ppl_java_common.hh"
dnl Generate the non-fixed part of the file.
m4_all_code`'dnl
dnl
dnl End of file generation.
