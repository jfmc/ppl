divert(-1)

dnl Include the language independent `dat' information.
include(`ppl_interface_generator_common_dat.m4')

dnl m4_procedure_list
dnl This class using patterns wherever possible.
dnl Which classes the schema applies to is determined by the following codes:
dnl If code is POINTS = the point-domain classes ie grid and polyhedra classes;
dnl            All = all classes
dnl            SHAPE = the polyhedra-shape classes;
dnl A class name with an "X" in front means it is not included.
dnl Where "4CLASS4" is replaced by the class name, then that class only
dnl is applicable for that schema.
dnl
dnl Note that the code for the schema "<name>_code" must be defined
dnl in the ppl_interface_generator_*_code.m4 file.
dnl The <name> must be exactly as written here.
dnl
define(`m4_procedure_list', `dnl
ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension +all,
ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@CLASS@ +all,
ppl_new_@TOPOLOGY@@CLASS@_from_@UREPRESENT@_System +all,
ppl_new_@TOPOLOGY@@CLASS@_recycle_@UREPRESENT@_System +all,
ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@ +all -bd_shape,
ppl_delete_@CLASS@ +all,
ppl_assign_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@ +all,
ppl_@CLASS@_@DIMENSION@ +all,
ppl_@CLASS@_@DESCRIBE@s +all,
ppl_@CLASS@_minimized_@DESCRIBE@s +all,
ppl_@CLASS@_relation_with_@UDESCRIBE@ +all,
ppl_@CLASS@_shrink_@BOX@ +shape,
ppl_Grid_get_bounding_box/2 +grid,
ppl_Grid_get_covering_box/2 +grid,
ppl_@CLASS@_is_@STATE@ +all,
ppl_@CLASS@_topological_closure_assign +all,
ppl_@CLASS@_bounds_from_@ABOVEBELOW@ +all -bd_shape,
ppl_@CLASS@_@MAXMIN@ +all -bd_shape,
ppl_@CLASS@_@MAXMIN@_with_point +all -bd_shape,
ppl_@CLASS@_@COMPARISON@_@CLASS@ +all,
ppl_@CLASS@_equals_@CLASS@ +all,
ppl_@CLASS@_OK +all,
ppl_@CLASS@_add_@REPRESENT@ +all,
ppl_@CLASS@_add_@REPRESENT@_and_minimize +all,
ppl_@CLASS@_add_@REPRESENT@s +all,
ppl_@CLASS@_add_@REPRESENT@s_and_minimize +all,
ppl_@CLASS@_add_recycled_@REPRESENT@ +all,
ppl_@CLASS@_add_recycled_@REPRESENT@_and_minimize +all,
ppl_@CLASS@_add_recycled_@REPRESENT@s +all,
ppl_@CLASS@_add_recycled_@REPRESENT@s_and_minimize +all,
ppl_@CLASS@_@BINOP@ +all,
ppl_@CLASS@_@BINOPPMIN@ +all,
ppl_@CLASS@_@AFFIMAGE@ +all,
ppl_@CLASS@_bounded_@AFFIMAGE@ +shape -bd_shape,
ppl_@CLASS@_generalized_@AFFIMAGE@ +shape,
ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs +shape,
ppl_Grid_generalized_@AFFIMAGE@ +grid,
ppl_Grid_generalized_@AFFIMAGE@_lhs_rhs +grid,
ppl_@CLASS@_@WIDENEXP@_widening_assign_with_tokens +all,
ppl_@CLASS@_@WIDENEXP@_widening_assign +all,
ppl_@CLASS@_limited_@WIDENEXP@_extrapolation_assign_with_tokens +all,
ppl_@CLASS@_limited_@WIDENEXP@_extrapolation_assign +all,
ppl_@CLASS@_bounded_@WIDENEXP@_extrapolation_assign_with_tokens +all -grid -bd_shape,
ppl_@CLASS@_bounded_@WIDENEXP@_extrapolation_assign +all -grid -bd_shape,
ppl_BD_Shape_CC76_narrowing_assign,
ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@ +all,
ppl_@CLASS@_remove_space_dimensions +all,
ppl_@CLASS@_remove_higher_space_dimensions +all,
ppl_@CLASS@_expand_space_dimension +all,
ppl_@CLASS@_fold_space_dimensions +all,
ppl_@CLASS@_map_space_dimensions +all
')

divert`'dnl

