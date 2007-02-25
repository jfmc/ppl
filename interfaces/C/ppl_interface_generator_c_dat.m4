dnl Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>
dnl
dnl This file is part of the Parma Polyhedra Library (PPL).
dnl
dnl The PPL is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by the
dnl Free Software Foundation; either version 2 of the License, or (at your
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

m4_divert(-1)

dnl Include the language independent `dat' information.
m4_include(`ppl_interface_generator_common_dat.m4')

dnl m4_procedure_list
dnl This class using patterns wherever possible.
dnl Which classes the schema applies to is determined by +/-group_name.
dnl The group_names represent sets of PPL classes +group1 and -group2.
dnl These are defined in ../ppl_interface_generator_common_dat.m4.
dnl The actual classes the schema applies to is the set difference
dnl +group1 \ -group2 where a missing +group1 or -group2 is
dnl assumed to be the empty set.
dnl Where "@CLASS@" is replaced by the class name, then that class only
dnl is applicable for that schema.
dnl
dnl Note that the code for the schema "<name>_code" must be defined
dnl in the ppl_interface_generator_*_code.m4 file.
dnl The <name> must be exactly as written here.
dnl
m4_define(`m4_procedure_list', `dnl
ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension +simple,
ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@CLASS@ +simple,
ppl_new_@TOPOLOGY@@CLASS@_from_@UBUILD_REPRESENT@_System +simple,
ppl_new_@TOPOLOGY@@CLASS@_recycle_@UBUILD_REPRESENT@_System +simple,
ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@ +simple -wr_shape,
ppl_delete_@CLASS@ +simple,
ppl_assign_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@ +simple,
ppl_@CLASS@_@DIMENSION@ +simple,
ppl_@CLASS@_@GET_REPRESENT@s +simple,
ppl_@CLASS@_minimized_@GET_REPRESENT@s +simple,
ppl_@CLASS@_relation_with_@URELATION_REPRESENT@ +simple,
ppl_@CLASS@_shrink_@BOX@ +shape,
ppl_Grid_get_bounding_box/3 +grid,
ppl_Grid_get_covering_box/2 +grid,
ppl_@CLASS@_@HAS_PROPERTY@ +simple,
ppl_@CLASS@_topological_closure_assign +simple,
ppl_@CLASS@_bounds_from_@ABOVEBELOW@ +simple -wr_shape,
ppl_@CLASS@_@MAXMIN@ +simple -wr_shape,
ppl_@CLASS@_@MAXMIN@_with_point +simple -wr_shape,
ppl_@CLASS@_@COMPARISON@_@CLASS@ +simple,
ppl_@CLASS@_equals_@CLASS@ +simple,
ppl_@CLASS@_OK +simple,
ppl_@CLASS@_add_@ADD_REPRESENT@ +simple,
ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize +simple,
ppl_@CLASS@_add_@ADD_REPRESENT@s +simple,
ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize +simple,
ppl_@CLASS@_add_recycled_@ADD_REPRESENT@ +simple -octagonal_shape,
ppl_@CLASS@_add_recycled_@ADD_REPRESENT@_and_minimize +simple -octagonal_shape,
ppl_@CLASS@_add_recycled_@ADD_REPRESENT@s +simple -octagonal_shape,
ppl_@CLASS@_add_recycled_@ADD_REPRESENT@s_and_minimize +simple -octagonal_shape,
ppl_@CLASS@_@BINOP@ +simple,
ppl_@CLASS@_@BINOPPMIN@ +simple,
ppl_@CLASS@_@AFFIMAGE@ +simple,
ppl_@CLASS@_bounded_@AFFIMAGE@ +shape -wr_shape,
ppl_@CLASS@_generalized_@AFFIMAGE@ +shape,
ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs +shape,
ppl_Grid_generalized_@AFFIMAGE@ +grid,
ppl_Grid_generalized_@AFFIMAGE@_lhs_rhs +grid,
ppl_@CLASS@_@WIDEN@_widening_assign_with_tokens +simple,
ppl_@CLASS@_@WIDEN@_widening_assign +simple,
ppl_@CLASS@_limited_@WIDENEXPN@_extrapolation_assign_with_tokens +simple,
ppl_@CLASS@_limited_@WIDENEXPN@_extrapolation_assign +simple,
ppl_@CLASS@_bounded_@WIDENEXPN@_extrapolation_assign_with_tokens +simple -grid -wr_shape,
ppl_@CLASS@_bounded_@WIDENEXPN@_extrapolation_assign +simple -grid -wr_shape,
ppl_@CLASS@_@EXTRAPOLATION@_extrapolation_assign_with_tokens/4 +wr_shape,
ppl_@CLASS@_@EXTRAPOLATION@_extrapolation_assign/2 *nofail +wr_shape,
ppl_@CLASS@_@EXTRAPOLATION@_narrowing_assign,
ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@ +simple,
ppl_@CLASS@_remove_space_dimensions +simple,
ppl_@CLASS@_remove_higher_space_dimensions +simple,
ppl_@CLASS@_expand_space_dimension +simple -wr_shape,
ppl_@CLASS@_fold_space_dimensions +simple -wr_shape,
ppl_@CLASS@_map_space_dimensions +simple
')

m4_divert`'dnl

