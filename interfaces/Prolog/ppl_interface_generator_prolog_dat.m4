divert(-1)

dnl Include the language independent `dat' information.
include(`ppl_interface_generator_common_dat.m4')

dnl Library predicate list.
define(`m4_library_predicate_list', `dnl
ppl_version_major/1
ppl_version_minor/1
ppl_version_revision/1
ppl_version_beta/1
ppl_version/1
ppl_banner/1
ppl_max_space_dimension/1
ppl_Coefficient_is_bounded/0
ppl_Coefficient_max/1
ppl_Coefficient_min/1
ppl_initialize/0 nofail
ppl_finalize/0 nofail
ppl_set_timeout_exception_atom/1 nofail
ppl_timeout_exception_atom/1
ppl_set_timeout/1 nofail
ppl_reset_timeout/0 nofail
ppl_new_LP_Problem_trivial/1
ppl_new_LP_Problem/4
ppl_new_LP_Problem_from_LP_Problem/2
ppl_LP_Problem_swap/2 nofail
ppl_delete_LP_Problem/1 nofail
ppl_LP_Problem_space_dimension/2
ppl_LP_Problem_OK/1
ppl_LP_Problem_constraints/2
ppl_LP_Problem_objective_function/2
ppl_LP_Problem_optimization_mode/2
ppl_LP_Problem_clear/1
ppl_LP_Problem_add_constraint/2
ppl_LP_Problem_add_constraints/2
ppl_LP_Problem_set_objective_function/2
ppl_LP_Problem_set_optimization_mode/2
ppl_LP_Problem_is_satisfiable/1
ppl_LP_Problem_solve/2
ppl_LP_Problem_feasible_point/2
ppl_LP_Problem_optimizing_point/2
ppl_LP_Problem_optimal_value/3
ppl_LP_Problem_evaluate_objective_function/4
')

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
ppl_new_4TOPOLOGY44CLASS4_from_space_dimension/3 POINTS
ppl_new_4TOPOLOGY44CLASS4_from_4INTOPOLOGY44CLASS4/2 All
ppl_new_4TOPOLOGY44CLASS4_from_4REPRESENT4s/2 POINTS
ppl_new_4TOPOLOGY44CLASS4_from_4BOX4/2 POINTS XBD_Shape
ppl_4CLASS4_swap/2 nofail All
ppl_delete_4CLASS4/1 All nofail
ppl_4CLASS4_4DIMENSION4/2 All
ppl_4CLASS4_get_4DESCRIBE4s/2 POINTS
ppl_4CLASS4_get_minimized_4DESCRIBE4s/2 POINTS XBD_Shape
ppl_4CLASS4_relation_with_4DESCRIBE4/3 POINTS
ppl_4CLASS4_get_4BOX4/3 SHAPE XBD_Shape
ppl_Grid_get_4BOX4/2
ppl_4CLASS4_is_4STATE4/1 POINTS
ppl_4CLASS4_topological_closure_assign/1 nofail POINTS XBD_Shape
ppl_4CLASS4_bounds_from_4ABOVEBELOW4/2 POINTS XBD_Shape
ppl_4CLASS4_4MAXMIN4/5 POINTS XBD_Shape
ppl_4CLASS4_4MAXMIN4_with_point/6 POINTS XBD_Shape
ppl_4CLASS4_4COMPARISON4_4CLASS4/2 POINTS
ppl_4CLASS4_equals_4CLASS4/2 POINTS
ppl_4CLASS4_OK/1 All
ppl_4CLASS4_add_4REPRESENT4/2 nofail POINTS
ppl_4CLASS4_add_4REPRESENT4_and_minimize/2 POINTS XBD_Shape
ppl_4CLASS4_add_4REPRESENT4s/2 nofail POINTS
ppl_4CLASS4_add_4REPRESENT4s_and_minimize/2 POINTS XBD_Shape
ppl_4CLASS4_4BINOP4/2 nofail POINTS
ppl_4CLASS4_4BINMINOP4/2 POINTS XBD_Shape
ppl_4CLASS4_4AFFIMAGE4/4 nofail POINTS
ppl_4CLASS4_bounded_4AFFIMAGE4/5 nofail SHAPE XBD_Shape
ppl_4CLASS4_generalized_4AFFIMAGE4/5 SHAPE XBD_Shape
ppl_4CLASS4_generalized_4AFFIMAGE4_lhs_rhs/4 SHAPE XBD_Shape
ppl_Grid_generalized_4AFFIMAGE4/5
ppl_Grid_generalized_4AFFIMAGE4_lhs_rhs/4
ppl_4CLASS4_4WIDENEXP4_widening_assign_with_tokens/4 POINTS XBD_Shape
ppl_4CLASS4_4WIDENEXP4_widening_assign/2 nofail POINTS XBD_Shape
ppl_4CLASS4_limited_4WIDENEXP4_extrapolation_assign_with_tokens/5 POINTS XBD_Shape
ppl_4CLASS4_limited_4WIDENEXP4_extrapolation_assign/3 nofail POINTS XBD_Shape
ppl_4CLASS4_bounded_4WIDENEXP4_extrapolation_assign_with_tokens/5 SHAPE XBD_Shape
ppl_4CLASS4_bounded_4WIDENEXP4_extrapolation_assign/3 nofail SHAPE XBD_Shape
ppl_BD_Shape_CC76_extrapolation_assign_with_tokens/4 XBD_Shape
ppl_BD_Shape_CC76_extrapolation_assign/2 nofail XBD_Shape
ppl_BD_Shape_CC76_narrowing_assign/2 XBD_Shape
ppl_4CLASS4_add_space_dimensions_4EMBEDPROJECT4/2 nofail POINTS
ppl_4CLASS4_remove_space_dimensions/2 POINTS
ppl_4CLASS4_remove_higher_space_dimensions/2 nofail POINTS
ppl_4CLASS4_expand_space_dimension/3 nofail POINTS XBD_Shape
ppl_4CLASS4_fold_space_dimensions/3  POINTS XBD_Shape
ppl_4CLASS4_map_space_dimensions/2 POINTS
')

divert`'dnl
