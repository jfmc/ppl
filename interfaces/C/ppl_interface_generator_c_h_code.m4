divert(-1)dnl

define(`ppl_new_TOPOLOGY_M4_CLASS_from_space_dimension_code',
`int
ppl_new_TOPOLOGY_M4_CLASS_from_space_dimension
PPL_PROTO((ppl_M4_CLASS_t* pph, ppl_dimension_type d, int empty));

')

define(`ppl_new_TOPOLOGY_M4_CLASS_from_INTOPOLOGY_M4_CLASS_code',
`int
ppl_new_TOPOLOGY_M4_CLASS_from_INTOPOLOGY_M4_CLASS
PPL_PROTO((ppl_M4_CLASS_t* pph, ppl_const_M4_CLASS_t ph));

')

define(`ppl_new_TOPOLOGY_M4_CLASS_from_UALT_REPRESENT_System_code',
`int
ppl_new_TOPOLOGY_M4_CLASS_from_UALT_REPRESENT_System
PPL_PROTO((ppl_M4_CLASS_t* pph, ppl_const_UALT_REPRESENT_System_t cs));

')

define(`ppl_new_TOPOLOGY_M4_CLASS_recycle_UALT_REPRESENT_System_code',
`int
ppl_new_TOPOLOGY_M4_CLASS_recycle_UALT_REPRESENT_System
PPL_PROTO((ppl_M4_CLASS_t* pph, ppl_UALT_REPRESENT_System_t cs));

')

define(`ppl_new_TOPOLOGY_M4_CLASS_from_BOX_code',
`int
ppl_new_TOPOLOGY_M4_CLASS_from_bounding_box
PPL_PROTO((ppl_M4_CLASS_t* pph,
	   ppl_dimension_type (*space_dimension)(void),
	   int (*is_empty)(void),
	   int (*get_lower_bound)(ppl_dimension_type k, int closed,
				  ppl_Coefficient_t n,
				  ppl_Coefficient_t d),
	   int (*get_upper_bound)(ppl_dimension_type k, int closed,
				  ppl_Coefficient_t n,
				  ppl_Coefficient_t d)));

')

define(`ppl_delete_M4_CLASS_code',
`int
ppl_delete_M4_CLASS PPL_PROTO((ppl_const_M4_CLASS_t ph));

')

define(`ppl_assign_TOPOLOGY_M4_CLASS_from_TOPOLOGY_M4_CLASS_code',
`int
ppl_assign_TOPOLOGY_M4_CLASS_from_TOPOLOGY_M4_CLASS
PPL_PROTO((ppl_M4_CLASS_t dst, ppl_const_M4_CLASS_t src));

')

define(`ppl_M4_CLASS_DIM_code',
`int
ppl_M4_CLASS_DIM
PPL_PROTO((ppl_const_M4_CLASS_t ph, ppl_dimension_type* m));

')

define(`ppl_M4_CLASS_DESCRIBEs_code',
`int
ppl_M4_CLASS_DESCRIBEs
PPL_PROTO((ppl_const_M4_CLASS_t ph,
           ppl_const_UALT_DESCRIBE_System_t* pcs));

')

define(`ppl_M4_CLASS_minimized_DESCRIBEs_code',
`int
ppl_M4_CLASS_minimized_DESCRIBEs
PPL_PROTO((ppl_const_M4_CLASS_t ph,
           ppl_const_UALT_DESCRIBE_System_t* pcs));

')

define(`ppl_M4_CLASS_relation_with_UALT_DESCRIBE_code',
`int
ppl_M4_CLASS_relation_with_UALT_DESCRIBE
PPL_PROTO((ppl_const_M4_CLASS_t ph,
           ppl_const_UALT_DESCRIBE_t c));

')

define(`ppl_M4_CLASS_is_STATE_code',
`int
ppl_M4_CLASS_is_STATE PPL_PROTO((ppl_const_M4_CLASS_t ph));

')

define(`ppl_M4_CLASS_bounds_from_ABOVEBELOW_code',
`int
ppl_M4_CLASS_bounds_from_ABOVEBELOW
PPL_PROTO((ppl_const_M4_CLASS_t ph,
           ppl_const_Linear_Expression_t le));

')

define(`ppl_M4_CLASS_MAXMIN_code',
`int
ppl_M4_CLASS_MAXMIN
PPL_PROTO((ppl_const_M4_CLASS_t ph,
           ppl_const_Linear_Expression_t le,
           ppl_Coefficient_t ext_n,
           ppl_Coefficient_t ext_d,
           int* poptimum,
           ppl_Generator_t point));

')

define(`ppl_M4_CLASS_shrink_BOX_code',
`int
ppl_M4_CLASS_shrink_BOX
PPL_PROTO((ppl_const_M4_CLASS_t ph,
           unsigned int complexity,
           void (*set_empty)(void),
           void (*raise_lower_bound)(ppl_dimension_type k, int closed,
                                     ppl_const_Coefficient_t n,
                                     ppl_const_Coefficient_t d),
           void (*lower_upper_bound)(ppl_dimension_type k, int closed,
                                     ppl_const_Coefficient_t n,
                                     ppl_const_Coefficient_t d)));

')

define(`ppl_M4_CLASS_COMPARISON_M4_CLASS_code',
`int
ppl_M4_CLASS_COMPARISON_M4_CLASS
PPL_PROTO((ppl_const_M4_CLASS_t x,
           ppl_const_M4_CLASS_t y));

')

define(`ppl_M4_CLASS_equals_M4_CLASS_code',
`int
ppl_M4_CLASS_equals_M4_CLASS
PPL_PROTO((ppl_const_M4_CLASS_t x,
           ppl_const_M4_CLASS_t y));

')

define(`ppl_M4_CLASS_OK_code',
`int
ppl_M4_CLASS_OK PPL_PROTO((ppl_const_M4_CLASS_t ph));

')

define(`ppl_M4_CLASS_topological_closure_assign_code',
`int
ppl_M4_CLASS_topological_closure_assign PPL_PROTO((ppl_M4_CLASS_t ph));

')

define(`ppl_M4_CLASS_BINOP_code',
`int
ppl_M4_CLASS_BINOP
PPL_PROTO((ppl_M4_CLASS_t x,
           ppl_const_M4_CLASS_t y));

')

define(`ppl_M4_CLASS_BPMIN_code',
`int
ppl_M4_CLASS_BPMIN
PPL_PROTO((ppl_M4_CLASS_t x,
           ppl_const_M4_CLASS_t y));

')

define(`ppl_M4_CLASS_add_REPRESENT_code',
`int
ppl_M4_CLASS_add_REPRESENT
PPL_PROTO((ppl_M4_CLASS_t ph,
           ppl_const_UALT_REPRESENT_t c));

')

define(`ppl_M4_CLASS_add_REPRESENT_and_minimize_code',
`int
ppl_M4_CLASS_add_REPRESENT_and_minimize
PPL_PROTO((ppl_M4_CLASS_t ph,
           ppl_const_UALT_REPRESENT_t c));

')

define(`ppl_M4_CLASS_add_REPRESENTs_code',
`int
ppl_M4_CLASS_add_REPRESENTs
PPL_PROTO((ppl_M4_CLASS_t ph,
           ppl_const_UALT_REPRESENT_System_t cs));

')

define(`ppl_M4_CLASS_add_REPRESENTs_and_minimize_code',
`int
ppl_M4_CLASS_add_REPRESENTs_and_minimize
PPL_PROTO((ppl_M4_CLASS_t ph,
           ppl_const_UALT_REPRESENT_System_t cs));

')

define(`ppl_M4_CLASS_add_recycled_REPRESENTs_code',
`int
ppl_M4_CLASS_add_recycled_REPRESENTs
PPL_PROTO((ppl_M4_CLASS_t ph,
           ppl_UALT_REPRESENT_System_t cs));

')

define(`ppl_M4_CLASS_add_recycled_REPRESENTs_and_minimize_code',
`int
ppl_M4_CLASS_add_recycled_REPRESENTs_and_minimize
PPL_PROTO((ppl_M4_CLASS_t ph,
           ppl_UALT_REPRESENT_System_t cs));

')

define(`ppl_M4_CLASS_AFFIM_code',
`int
ppl_M4_CLASS_AFFIM
PPL_PROTO((ppl_M4_CLASS_t ph,
           ppl_dimension_type var,
           ppl_const_Linear_Expression_t le,
           ppl_const_Coefficient_t d));

')

define(`ppl_M4_CLASS_bounded_AFFIM_code',
`int
ppl_M4_CLASS_bounded_AFFIM
PPL_PROTO((ppl_M4_CLASS_t ph,
           ppl_dimension_type var,
           ppl_const_Linear_Expression_t lb,
           ppl_const_Linear_Expression_t ub,
           ppl_const_Coefficient_t d));

')

define(`ppl_M4_CLASS_generalized_AFFIM_code',
`int
ppl_M4_CLASS_generalized_AFFIM
PPL_PROTO((ppl_M4_CLASS_t ph,
           ppl_dimension_type var,
           enum ppl_enum_Constraint_Type relsym,
           ppl_const_Linear_Expression_t le,
           ppl_const_Coefficient_t d));

')

define(`ppl_M4_CLASS_generalized_AFFIM_lhs_rhs_code',
`int
ppl_M4_CLASS_generalized_AFFIM_lhs_rhs
PPL_PROTO((ppl_M4_CLASS_t ph,
           ppl_const_Linear_Expression_t lhs,
           enum ppl_enum_Constraint_Type relsym,
           ppl_const_Linear_Expression_t rhs));

')

define(`ppl_M4_CLASS_WIDENEXP_widening_assign_with_tokens_code',
`int
ppl_M4_CLASS_WIDENEXP_widening_assign_with_tokens
PPL_PROTO((ppl_M4_CLASS_t x,
           ppl_const_M4_CLASS_t y,
           unsigned* tp));

')

define(`ppl_M4_CLASS_WIDENEXP_widening_assign_code',
`int
ppl_M4_CLASS_WIDENEXP_widening_assign
PPL_PROTO((ppl_M4_CLASS_t x,
           ppl_const_M4_CLASS_t y));

')

define(`ppl_M4_CLASS_limited_WIDENEXP_extrapolation_assign_with_tokens_code',
`int
ppl_M4_CLASS_limited_WIDENEXP_extrapolation_assign_with_tokens
PPL_PROTO((ppl_M4_CLASS_t x,
           ppl_const_M4_CLASS_t y,
           ppl_const_Constraint_System_t cs,
           unsigned* tp));

')

define(`ppl_M4_CLASS_limited_WIDENEXP_extrapolation_assign_code',
`int
ppl_M4_CLASS_limited_WIDENEXP_extrapolation_assign
PPL_PROTO((ppl_M4_CLASS_t x,
           ppl_const_M4_CLASS_t y,
           ppl_const_Constraint_System_t cs));

')

define(`ppl_M4_CLASS_bounded_WIDENEXP_extrapolation_assign_with_tokens_code',
`int
ppl_M4_CLASS_bounded_WIDENEXP_extrapolation_assign_with_tokens
PPL_PROTO((ppl_M4_CLASS_t x,
           ppl_const_M4_CLASS_t y,
           ppl_const_Constraint_System_t cs,
           unsigned* tp));

')

define(`ppl_M4_CLASS_bounded_WIDENEXP_extrapolation_assign_code',
`int
ppl_M4_CLASS_bounded_WIDENEXP_extrapolation_assign
PPL_PROTO((ppl_M4_CLASS_t x,
           ppl_const_M4_CLASS_t y,
           ppl_const_Constraint_System_t cs));

')

dnl FIXME: why is this different from other BINOP's?
define(`ppl_M4_CLASS_concatenate_assign_code'
`int
ppl_M4_CLASS_concatenate_assign
PPL_PROTO((ppl_M4_CLASS_t x,
           ppl_const_M4_CLASS_t y));

')

define(`ppl_M4_CLASS_add_space_dimensions_EMBEDPROJECT_code'
`int
ppl_M4_CLASS_add_space_dimensions_EMBEDPROJECT
PPL_PROTO((ppl_M4_CLASS_t ph,
           ppl_dimension_type d));

')

define(`ppl_M4_CLASS_remove_space_dimensions_code'
`int
ppl_M4_CLASS_remove_space_dimensions
PPL_PROTO((ppl_M4_CLASS_t ph,
           ppl_dimension_type ds[],
           size_t n));

')

define(`ppl_M4_CLASS_remove_higher_space_dimensions_code'
`int
ppl_M4_CLASS_remove_higher_space_dimensions
PPL_PROTO((ppl_M4_CLASS_t ph,
           ppl_dimension_type d));

')

define(`ppl_M4_CLASS_map_space_dimensions_code'
`int
ppl_M4_CLASS_map_space_dimensions
PPL_PROTO((ppl_M4_CLASS_t ph,
           ppl_dimension_type maps[],
           size_t n));

')

define(`ppl_M4_CLASS_expand_space_dimension_code'
`int
ppl_M4_CLASS_expand_space_dimension
PPL_PROTO((ppl_M4_CLASS_t ph,
           ppl_dimension_type d,
           ppl_dimension_type m));

')

define(`ppl_M4_CLASS_fold_space_dimensions_code'
`int
ppl_M4_CLASS_fold_space_dimensions
PPL_PROTO((ppl_M4_CLASS_t ph,
           ppl_dimension_type ds[],
           size_t n,
           ppl_dimension_type d));

')

divert`'dnl
