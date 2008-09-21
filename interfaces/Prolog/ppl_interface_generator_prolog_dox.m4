m4_define(`dnl', `m4_dnl')`'dnl
m4_divert(-1)

dnl This m4 file generates the file Prolog_interface.dox

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

m4_define(`m4_documentation_generation', `')

dnl Define the classes to be documented.
m4_define(`m4_interface_classes_names', `Polyhedron@Grid@Octagonal_Shape_mpz_class@BD_Shape_double@Float_Box@@Pointset_Powerset_C_Polyhedron@Direct_Product_C_Polyhedron_Grid')
m4_define(`m4_cplusplus_classes_names', `Polyhedron@Grid@Octagonal_Shape<mpz_class>@BD_Shape<double>@Float_Box@Pointset_Powerset<C_Polyhedron>@Direct_Product<C_Polyhedron,Grid>')

dnl Include files defining macros that generate the non-fixed part.
m4_include(`ppl_interface_generator_prolog_dox_code.m4')
m4_include(`ppl_interface_generator_prolog_procedure_generators.m4')

m4_divert`'dnl
m4_include(`ppl_prolog_sysindep_dox')
m4_divert(-1)`'dnl

dnl m4_pre_extra_class_code(Class_Counter)
dnl Prefix extra code for each class.
m4_pushdef(`m4_one_class_code', `m4_interface_class$1 <BR>
')
m4_divert`'dnl
dnl -----------------------------------------------------------------
dnl Output all the classes.
dnl -----------------------------------------------------------------
\anchor generated_predicates
<H1>PPL Domains Generated for the Prolog Interface</H1>
Here are some example PPL domains: <BR>
m4_all_code
Next are lists of all the supported predicates for these domains: <BR>
m4_popdef(`m4_one_class_code')

dnl m4_pre_extra_class_code(Class_Counter)
dnl Prefix extra code for each class.
m4_define(`m4_pre_extra_class_code', `dnl
\anchor m4_interface_class$1`'_predicates
<H2>m4_interface_class`'$1 Predicates List</H2>
')
m4_define(`m4_post_extra_class_code', `dnl
')

m4_divert`'dnl
m4_all_code`'dnl
m4_include(`ppl_prolog_sysdep_dox')
m4_popdef(`m4_interface_classes_names')
m4_popdef(`m4_cplusplus_classes_names')

dnl
dnl End of file generation.
