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
ppl_Grid_get_bounding_box/2 +grid,
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
ppl_@CLASS@_add_recycled_@ADD_REPRESENT@ +simple,
ppl_@CLASS@_add_recycled_@ADD_REPRESENT@_and_minimize +simple,
ppl_@CLASS@_add_recycled_@ADD_REPRESENT@s +simple,
ppl_@CLASS@_add_recycled_@ADD_REPRESENT@s_and_minimize +simple,
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
ppl_@CLASS@_limited_@EXTRAPOLATION@_extrapolation_assign_with_tokens +simple,
ppl_@CLASS@_limited_@EXTRAPOLATION@_extrapolation_assign +simple,
ppl_@CLASS@_bounded_@EXTRAPOLATION@_extrapolation_assign_with_tokens +simple -grid -wr_shape,
ppl_@CLASS@_bounded_@EXTRAPOLATION@_extrapolation_assign +simple -grid -wr_shape,
ppl_BD_Shape_CC76_narrowing_assign,
ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@ +simple,
ppl_@CLASS@_remove_space_dimensions +simple,
ppl_@CLASS@_remove_higher_space_dimensions +simple,
ppl_@CLASS@_expand_space_dimension +simple -wr_shape,
ppl_@CLASS@_fold_space_dimensions +simple -wr_shape,
ppl_@CLASS@_map_space_dimensions +simple
')

m4_divert`'dnl

