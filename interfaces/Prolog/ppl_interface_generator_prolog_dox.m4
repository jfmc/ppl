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
dnl m4_define(`m4_config_indep_only', `')
m4_define(`m4_config_dep_only', `')

dnl -----------------------------------------------------------------
dnl Include files defining macros that generate the non-fixed part.
dnl -----------------------------------------------------------------
m4_include(`ppl_interface_generator_prolog_procedure_generators.m4')
m4_include(`ppl_interface_generator_prolog_dox_code.m4')

m4_ifdef(`m4_config_dep_only', `', `dnl
m4_define(`m4_interface_classes_names', `Polyhedron@Pointset_Powerset_C_Polyhedron@Pointset_Powerset_NNC_Polyhedron')
m4_define(`m4_cplusplus_classes_names', `Polyhedron@Pointset_Powerset<C_Polyhedron>@Pointset_Powerset<NNC_Polyhedron>')`'dnl
')

m4_define(`m4_interface_classes_names', `Polyhedron@Pointset_Powerset_C_Polyhedron@Pointset_Powerset_NNC_Polyhedron')
m4_define(`m4_cplusplus_classes_names', `Polyhedron@Pointset_Powerset<C_Polyhedron>@Pointset_Powerset<NNC_Polyhedron>')`'dnl


dnl -----------------------------------------------------------------
dnl Initialize the class definitions
dnl as we do not use m4_all_code to generate the documentation
dnl -----------------------------------------------------------------
m4_init_class_definitions
m4_define(`m4_all_categories', `dnl
m4_define(`m4_ext', convert)`'dnl
m4_one_subcategory
m4_define(`m4_ext', destruct)`'dnl
m4_one_subcategory
m4_category_const
m4_define(`m4_ext', testpoly)`'dnl
m4_one_subcategory
m4_define(`m4_ext', constpoly)`'dnl
m4_one_subcategory
m4_define(`m4_ext', dump)`'dnl
m4_one_subcategory
m4_category_nonconst
m4_define(`m4_ext', addto)`'dnl
m4_one_subcategory
m4_define(`m4_ext', trans)`'dnl
m4_one_subcategory
m4_define(`m4_ext', binop)`'dnl
m4_one_subcategory
m4_define(`m4_ext', widen)`'dnl
m4_one_subcategory
m4_category_varspace
m4_define(`m4_ext', concat)`'dnl
m4_one_subcategory
m4_define(`m4_ext', spacemod)`'dnl
m4_one_subcategory
')

dnl -----------------------------------------------------------------
dnl System Dependent Documentation.
dnl -----------------------------------------------------------------
m4_divert`'dnl
m4_ifdef(`m4_config_dep_only', `',
                   `m4_include(`ppl_prolog_sysindep_dox')')
m4_divert(-1)`'dnl

dnl -----------------------------------------------------------------
dnl ********************* THE POLYHEDRON DOMAIN. ********************
dnl -----------------------------------------------------------------
m4_divert`'dnl
\anchor Polyhedron_predicates
<H1>Predicates for C Polyhedra</H1>
Here we provide a short description for each of the predicates
available for the domain of C polyhedra.
Note that predicates for other domains will follow a similar pattern.

dnl m4_all_categories

dnl -----------------------------------------------------------------
dnl *** The constructor, copy, convert and descruct predicates. ***
dnl -----------------------------------------------------------------
m4_category_constr_destr
m4_divert(-1)

dnl -----------------------------------------------------------------
dnl The constructor predicates.
dnl -----------------------------------------------------------------
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_build_doc', `m4_indir(`$1_build_doc')', `')
')
m4_divert`'dnl
m4_subcategory_build
m4_ifdef(m4_interface_class`'1,
`m4_patsubst(m4_one_class_code(1), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl -----------------------------------------------------------------
dnl The copy and conversion constructor predicates.
dnl -----------------------------------------------------------------
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_convert_doc', `m4_indir(`$1_convert_doc')', `')
')
m4_divert`'dnl
m4_subcategory_convert
m4_ifdef(m4_interface_class`'1,
`m4_patsubst(m4_one_class_code(1), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl -----------------------------------------------------------------
dnl The destructor predicate
dnl -----------------------------------------------------------------
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_destruct_doc', `m4_indir(`$1_destruct_doc')', `')
')
m4_divert`'dnl
m4_subcategory_destruct
m4_ifdef(m4_interface_class`'1,
`m4_patsubst(m4_one_class_code(1), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl -----------------------------------------------------------------
dnl *** Predicates that do not modify the polyhedron. ***
dnl -----------------------------------------------------------------
m4_divert`'dnl
m4_category_const
m4_divert(-1)

dnl -----------------------------------------------------------------
dnl Predicates that test a property
dnl -----------------------------------------------------------------
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_testpoly_doc', `m4_indir(`$1_testpoly_doc')', `')
')
m4_divert`'dnl
m4_subcategory_testpoly
m4_ifdef(m4_interface_class`'1,
`m4_patsubst(m4_one_class_code(1), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl -----------------------------------------------------------------
dnl Predicates that get information about the polyhedron.
dnl -----------------------------------------------------------------
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_constpoly_doc', `m4_indir(`$1_constpoly_doc')', `')
')
m4_divert`'dnl
m4_subcategory_constpoly
m4_ifdef(m4_interface_class`'1,
`m4_patsubst(m4_one_class_code(1), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl -----------------------------------------------------------------
dnl Predicate that dumps.
dnl -----------------------------------------------------------------
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_dump_doc', `m4_indir(`$1_dump_doc')', `')
')
m4_divert`'dnl
m4_subcategory_dump
m4_ifdef(m4_interface_class`'1,
`m4_patsubst(m4_one_class_code(1), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl -----------------------------------------------------------------
dnl *** Space dimension preserving predicates that may modify the ***
dnl *** polyhedron.                                               ***
dnl -----------------------------------------------------------------
m4_divert`'dnl
m4_category_nonconst
m4_divert(-1)

dnl -----------------------------------------------------------------
dnl Predicates that may change the polyhedron by adding
dnl to the polyhedron's constraint or generator descriptions.
dnl -----------------------------------------------------------------
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_addto_doc', `m4_indir(`$1_addto_doc')', `')
')
m4_divert`'dnl
m4_subcategory_addto
m4_ifdef(m4_interface_class`'1,
`m4_patsubst(m4_one_class_code(1), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl -----------------------------------------------------------------
dnl Predicates that transform a polyhedron.
dnl -----------------------------------------------------------------
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_trans_doc', `m4_indir(`$1_trans_doc')', `')
')
m4_divert`'dnl
m4_subcategory_trans
m4_ifdef(m4_interface_class`'1,
`m4_patsubst(m4_one_class_code(1), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl -----------------------------------------------------------------
dnl Predicates that act as binary operators.
dnl -----------------------------------------------------------------
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_binop_doc', `m4_indir(`$1_binop_doc')', `')
')
m4_divert`'dnl
m4_subcategory_binop
m4_ifdef(m4_interface_class`'1,
`m4_patsubst(m4_one_class_code(1), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl -----------------------------------------------------------------
dnl Predicates for widening and extrapolation.
dnl -----------------------------------------------------------------
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_widen_doc', `m4_indir(`$1_widen_doc')', `')
')
m4_divert`'dnl
m4_subcategory_widen
m4_ifdef(m4_interface_class`'1,
`m4_translit(m4_one_class_code(1), `|&', `\,')
')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl -----------------------------------------------------------------
dnl Predicates that may modify the dimension of the vector space.
dnl -----------------------------------------------------------------
m4_divert`'dnl
m4_category_varspace
m4_divert(-1)

dnl -----------------------------------------------------------------
dnl Concatenate predicate.
dnl -----------------------------------------------------------------
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_concat_doc', `m4_indir(`$1_concat_doc')', `')
')
m4_divert`'dnl
m4_subcategory_concat
m4_ifdef(m4_interface_class`'1,
`m4_patsubst(m4_one_class_code(1), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl -----------------------------------------------------------------
dnl Space modification predicates.
dnl -----------------------------------------------------------------
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_spacemod_doc', `m4_indir(`$1_spacemod_doc')', `')
')
m4_divert`'dnl
m4_subcategory_spacemod
m4_ifdef(m4_interface_class`'1,
`m4_patsubst(m4_one_class_code(1), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl -----------------------------------------------------------------
dnl ***************** OTHER PPL DOMAINS *****************************
dnl -----------------------------------------------------------------
m4_divert`'dnl
\anchor other_domains
<H1>Ad hoc Predicates for Other Domains</H1>
dnl
dnl -----------------------------------------------------------------
dnl *** Pointset powerset domains.                               ***
dnl -----------------------------------------------------------------
m4_category_pps
m4_divert(-1)

dnl -----------------------------------------------------------------
dnl Predicates for pointset powerset iterators.
dnl -----------------------------------------------------------------
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_pps_iter_doc', `m4_indir(`$1_pps_iter_doc')', `')
')
m4_divert`'dnl
m4_subcategory_pps_iter
m4_ifdef(m4_interface_class`'2,
`m4_patsubst(m4_one_class_code(2), @COMMA@, `,')')`'dnl
m4_divert(-1)
m4_popdef(`m4_extension')

dnl -----------------------------------------------------------------
dnl Other ad hoc predicates for the pointset_powerset domains.
dnl -----------------------------------------------------------------
m4_pushdef(`m4_extension',
  `m4_ifdef(`$1_pps_other_doc', `m4_indir(`$1_pps_other_doc')', `')
')
m4_divert`'dnl
m4_subcategory_pps_other
m4_ifdef(m4_interface_class`'2,
`m4_patsubst(m4_one_class_code(2), @COMMA@, `,')')`'dnl
m4_popdef(`m4_extension')

dnl -----------------------------------------------------------------
dnl SYSTEM DEPENDENT DOCUMENTATION.
dnl -----------------------------------------------------------------
m4_ifdef(`m4_config_dep_only', `',
                   `m4_include(ppl_prolog_sysdep_dox)')
m4_popdef(`m4_interface_classes_names')
m4_popdef(`m4_cplusplus_classes_names')
dnl
dnl End of file generation.
