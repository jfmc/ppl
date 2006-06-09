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
ppl_new_4TOPOLOGY44CLASS4_from_space_dimension +all,
ppl_new_4TOPOLOGY44CLASS4_from_4INTOPOLOGY44CLASS4 +all,
ppl_new_4TOPOLOGY44CLASS4_from_4UREPRESENT4_System +all,
ppl_new_4TOPOLOGY44CLASS4_recycle_4UREPRESENT4_System +all,
ppl_new_4TOPOLOGY44CLASS4_from_4BOX4 +all -bd_shape,
ppl_delete_4CLASS4 +all,
ppl_assign_4TOPOLOGY44CLASS4_from_4TOPOLOGY44CLASS4 +all,
ppl_4CLASS4_4DIMENSION4 +all,
ppl_4CLASS4_4DESCRIBE4s +all,
ppl_4CLASS4_minimized_4DESCRIBE4s +all,
ppl_4CLASS4_relation_with_4UDESCRIBE4 +all,
ppl_4CLASS4_shrink_4BOX4 +shape,
ppl_Grid_get_4BOX4 +grid,
ppl_4CLASS4_is_4STATE4 +all,
ppl_4CLASS4_topological_closure_assign +all,
ppl_4CLASS4_bounds_from_4ABOVEBELOW4 +all -bd_shape,
ppl_4CLASS4_4MAXMIN4 +all -bd_shape,
ppl_4CLASS4_4MAXMIN4_with_point +all -bd_shape,
ppl_4CLASS4_4COMPARISON4_4CLASS4 +all,
ppl_4CLASS4_equals_4CLASS4 +all,
ppl_4CLASS4_OK +all,
ppl_4CLASS4_add_4REPRESENT4 +all,
ppl_4CLASS4_add_4REPRESENT4_and_minimize +all,
ppl_4CLASS4_add_4REPRESENT4s +all,
ppl_4CLASS4_add_4REPRESENT4s_and_minimize +all,
ppl_4CLASS4_add_recycled_4REPRESENT4 +all,
ppl_4CLASS4_add_recycled_4REPRESENT4_and_minimize +all,
ppl_4CLASS4_add_recycled_4REPRESENT4s +all,
ppl_4CLASS4_add_recycled_4REPRESENT4s_and_minimize +all,
ppl_4CLASS4_4BINOP4 +all,
ppl_4CLASS4_4BINOPPMIN4 +all,
ppl_4CLASS4_4AFFIMAGE4 +all,
ppl_4CLASS4_bounded_4AFFIMAGE4 +shape -bd_shape,
ppl_4CLASS4_generalized_4AFFIMAGE4 +shape,
ppl_4CLASS4_generalized_4AFFIMAGE4_lhs_rhs +shape,
ppl_Grid_generalized_4AFFIMAGE4 +grid,
ppl_Grid_generalized_4AFFIMAGE4_lhs_rhs +grid,
ppl_4CLASS4_4WIDENEXP4_widening_assign_with_tokens +all,
ppl_4CLASS4_4WIDENEXP4_widening_assign +all,
ppl_4CLASS4_limited_4WIDENEXP4_extrapolation_assign_with_tokens +all,
ppl_4CLASS4_limited_4WIDENEXP4_extrapolation_assign +all,
ppl_4CLASS4_bounded_4WIDENEXP4_extrapolation_assign_with_tokens +all -grid -bd_shape,
ppl_4CLASS4_bounded_4WIDENEXP4_extrapolation_assign +all -grid -bd_shape,
ppl_BD_Shape_CC76_narrowing_assign,
ppl_4CLASS4_add_space_dimensions_4EMBEDPROJECT4 +all,
ppl_4CLASS4_remove_space_dimensions +all,
ppl_4CLASS4_remove_higher_space_dimensions +all,
ppl_4CLASS4_expand_space_dimension +all,
ppl_4CLASS4_fold_space_dimensions +all,
ppl_4CLASS4_map_space_dimensions +all
')

divert`'dnl

