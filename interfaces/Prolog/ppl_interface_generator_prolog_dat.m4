m4_divert(-1)
m4_define(`dnl', `m4_dnl')
dnl
dnl Include the language independent `dat' information.
m4_include(`ppl_interface_generator_common_dat.m4')`'dnl

dnl Library predicate list.
m4_define(`m4_library_predicate_list', `dnl
ppl_version_major/1,
ppl_version_minor/1,
ppl_version_revision/1,
ppl_version_beta/1,
ppl_version/1,
ppl_banner/1,
ppl_max_space_dimension/1,
ppl_Coefficient_is_bounded/0,
ppl_Coefficient_max/1,
ppl_Coefficient_min/1,
ppl_initialize/0 *nofail,
ppl_finalize/0 *nofail,
ppl_set_timeout_exception_atom/1 *nofail,
ppl_timeout_exception_atom/1,
ppl_set_timeout/1 *nofail,
ppl_reset_timeout/0 *nofail,
ppl_new_MIP_Problem_from_space_dimension/2,
ppl_new_MIP_Problem/5,
ppl_new_MIP_Problem_from_MIP_Problem/2,
ppl_MIP_Problem_swap/2 *nofail,
ppl_delete_MIP_Problem/1 *nofail,
ppl_MIP_Problem_space_dimension/2,
ppl_MIP_Problem_integer_space_dimensions/2,
ppl_MIP_Problem_constraints/2,
ppl_MIP_Problem_objective_function/2,
ppl_MIP_Problem_optimization_mode/2,
ppl_MIP_Problem_clear/1,
ppl_MIP_Problem_add_space_dimensions_and_embed/2,
ppl_MIP_Problem_add_to_integer_space_dimensions/2,
ppl_MIP_Problem_add_constraint/2,
ppl_MIP_Problem_add_constraints/2,
ppl_MIP_Problem_set_objective_function/2,
ppl_MIP_Problem_set_optimization_mode/2,
ppl_MIP_Problem_is_satisfiable/1,
ppl_MIP_Problem_solve/2,
ppl_MIP_Problem_feasible_point/2,
ppl_MIP_Problem_optimizing_point/2,
ppl_MIP_Problem_optimal_value/3,
ppl_MIP_Problem_evaluate_objective_function/4,
ppl_MIP_Problem_OK/1`'dnl
')`'dnl
dnl
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
ppl_delete_@CLASS@/1 *nofail +simple_pps,
ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension/3 +simple_pps,
ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@/2 +simple_pps,
ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s/2 +simple_pps,
ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@/2 +simple -bd_shape,
ppl_@CLASS@_swap/2 *nofail +simple_pps,
ppl_@CLASS@_@DIMENSION@/2 +simple_pps,
ppl_@CLASS@_get_@GET_REPRESENT@s/2 +simple,
ppl_@CLASS@_get_minimized_@GET_REPRESENT@s/2 +simple,
ppl_@CLASS@_get_disjuncts/2 +polyhedra_powerset,
ppl_@CLASS@_relation_with_@RELATION_REPRESENT@/3 +simple,
ppl_@CLASS@_get_@BOX@/3 +shape,
ppl_Grid_get_@BOX@/2 +grid,
ppl_@CLASS@_@HAS_PROPERTY@/1 +simple,
ppl_@CLASS@_@SIMPLIFY@/1 *nofail +simple_pps -bd_shape,
ppl_@CLASS@_bounds_from_@ABOVEBELOW@/2 +simple -bd_shape,
ppl_@CLASS@_@MAXMIN@/5 +simple -bd_shape,
ppl_@CLASS@_@MAXMIN@_with_point/6 +simple -bd_shape,
ppl_@CLASS@_@COMPARISON@_@CLASS@/2 +simple_pps -bd_shape,
ppl_@CLASS@_equals_@CLASS@/2 +simple,
ppl_@CLASS@_OK/1 +simple_pps,
ppl_@CLASS@_add_disjunct/2 *nofail +polyhedra_powerset,
ppl_@CLASS@_add_@ADD_REPRESENT@/2 *nofail +simple_pps,
ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize/2 +simple_pps,
ppl_@CLASS@_add_@ADD_REPRESENT@s/2 *nofail +simple_pps,
ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize/2 +simple_pps,
ppl_@CLASS@_@BINOP@/2 *nofail +simple,
ppl_@CLASS@_@BINMINOP@/2 +simple,
ppl_@CLASS@_@AFFIMAGE@/4 *nofail +simple,
ppl_@CLASS@_bounded_@AFFIMAGE@/5 *nofail +shape -bd_shape,
ppl_@CLASS@_generalized_@AFFIMAGE@/5 +shape,
ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs/4 +shape,
ppl_Grid_generalized_@AFFIMAGE@/6 +grid,
ppl_Grid_generalized_@AFFIMAGE@_lhs_rhs/5 +grid,
ppl_@CLASS@_@WIDEN@_widening_assign_with_tokens/4 +simple,
ppl_@CLASS@_@WIDEN@_widening_assign/2 *nofail +simple,
ppl_@CLASS@_@LIMITEDBOUNDED@_@EXTRAPOLATION@_extrapolation_assign_with_tokens/5 +simple,
ppl_@CLASS@_@LIMITEDBOUNDED@_@EXTRAPOLATION@_extrapolation_assign/3 *nofail +simple,
ppl_BD_Shape_CC76_extrapolation_assign_with_tokens/4 -bd_shape,
ppl_BD_Shape_CC76_extrapolation_assign/2 *nofail -bd_shape,
ppl_BD_Shape_CC76_narrowing_assign/2 -bd_shape,
ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@/2 *nofail +simple_pps,
ppl_@CLASS@_remove_space_dimensions/2 +simple_pps,
ppl_@CLASS@_remove_higher_space_dimensions/2 *nofail +simple_pps,
ppl_@CLASS@_expand_space_dimension/3 *nofail +simple -bd_shape,
ppl_@CLASS@_fold_space_dimensions/3  +simple -bd_shape,
ppl_@CLASS@_map_space_dimensions/2 +simple_pps
')`'dnl
dnl
m4_divert`'dnl
