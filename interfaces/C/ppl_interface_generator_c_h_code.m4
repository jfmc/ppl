divert(-1)dnl

define(`ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_code',
`int
ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension
PPL_PROTO((ppl_@CLASS@_t* pph, ppl_dimension_type d, int empty));

')

define(`ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@CLASS@_code',
`int
ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@CLASS@
PPL_PROTO((ppl_@CLASS@_t* pph, ppl_const_@CLASS@_t ph));

')

define(`ppl_new_@TOPOLOGY@@CLASS@_from_@UREPRESENT@_System_code',
`int
ppl_new_@TOPOLOGY@@CLASS@_from_@UREPRESENT@_System
PPL_PROTO((ppl_@CLASS@_t* pph, ppl_const_@UREPRESENT@_System_t cs));

')

define(`ppl_new_@TOPOLOGY@@CLASS@_recycle_@UREPRESENT@_System_code',
`int
ppl_new_@TOPOLOGY@@CLASS@_recycle_@UREPRESENT@_System
PPL_PROTO((ppl_@CLASS@_t* pph, ppl_@UREPRESENT@_System_t cs));

')

define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@_code',
`int
ppl_new_@TOPOLOGY@@CLASS@_from_bounding_box
PPL_PROTO((ppl_@CLASS@_t* pph,
	   ppl_dimension_type (*space_dimension)(void),
	   int (*is_empty)(void),
	   int (*get_lower_bound)(ppl_dimension_type k, int closed,
				  ppl_Coefficient_t n,
				  ppl_Coefficient_t d),
	   int (*get_upper_bound)(ppl_dimension_type k, int closed,
				  ppl_Coefficient_t n,
				  ppl_Coefficient_t d)));

')

define(`ppl_delete_@CLASS@_code',
`int
ppl_delete_@CLASS@ PPL_PROTO((ppl_const_@CLASS@_t ph));

')

define(`ppl_assign_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@_code',
`int
ppl_assign_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@
PPL_PROTO((ppl_@CLASS@_t dst, ppl_const_@CLASS@_t src));

')

define(`ppl_@CLASS@_@DIMENSION@_code',
`int
ppl_@CLASS@_@DIMENSION@
PPL_PROTO((ppl_const_@CLASS@_t ph, ppl_dimension_type* m));

')

define(`ppl_@CLASS@_@DESCRIBE@s_code',
`int
ppl_@CLASS@_@DESCRIBE@s
PPL_PROTO((ppl_const_@CLASS@_t ph,
           ppl_const_@UDESCRIBE@_System_t* pcs));

')

define(`ppl_@CLASS@_minimized_@DESCRIBE@s_code',
`int
ppl_@CLASS@_minimized_@DESCRIBE@s
PPL_PROTO((ppl_const_@CLASS@_t ph,
           ppl_const_@UDESCRIBE@_System_t* pcs));

')

define(`ppl_@CLASS@_relation_with_@UDESCRIBE@_code',
`int
ppl_@CLASS@_relation_with_@UDESCRIBE@
PPL_PROTO((ppl_const_@CLASS@_t ph,
           ppl_const_@UDESCRIBE@_t c));

')

define(`ppl_@CLASS@_is_@STATE@_code',
`int
ppl_@CLASS@_is_@STATE@ PPL_PROTO((ppl_const_@CLASS@_t ph));

')

define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_code',
`int
ppl_@CLASS@_bounds_from_@ABOVEBELOW@
PPL_PROTO((ppl_const_@CLASS@_t ph,
           ppl_const_Linear_Expression_t le));

')

define(`ppl_@CLASS@_@MAXMIN@_code',
`int
ppl_@CLASS@_@MAXMIN@
PPL_PROTO((ppl_const_@CLASS@_t ph,
           ppl_const_Linear_Expression_t le,
           ppl_Coefficient_t ext_n,
           ppl_Coefficient_t ext_d,
           int* poptimum,
           ppl_Generator_t point));

')

define(`ppl_@CLASS@_shrink_@BOX@_code',
`int
ppl_@CLASS@_shrink_@BOX@
PPL_PROTO((ppl_const_@CLASS@_t ph,
           unsigned int complexity,
           void (*set_empty)(void),
           void (*raise_lower_bound)(ppl_dimension_type k, int closed,
                                     ppl_const_Coefficient_t n,
                                     ppl_const_Coefficient_t d),
           void (*lower_upper_bound)(ppl_dimension_type k, int closed,
                                     ppl_const_Coefficient_t n,
                                     ppl_const_Coefficient_t d)));

')

define(`ppl_@CLASS@_@COMPARISON@_@CLASS@_code',
`int
ppl_@CLASS@_@COMPARISON@_@CLASS@
PPL_PROTO((ppl_const_@CLASS@_t x,
           ppl_const_@CLASS@_t y));

')

define(`ppl_@CLASS@_equals_@CLASS@_code',
`int
ppl_@CLASS@_equals_@CLASS@
PPL_PROTO((ppl_const_@CLASS@_t x,
           ppl_const_@CLASS@_t y));

')

define(`ppl_@CLASS@_OK_code',
`int
ppl_@CLASS@_OK PPL_PROTO((ppl_const_@CLASS@_t ph));

')

define(`ppl_@CLASS@_topological_closure_assign_code',
`int
ppl_@CLASS@_topological_closure_assign PPL_PROTO((ppl_@CLASS@_t ph));

')

define(`ppl_@CLASS@_@BINOP@_code',
`int
ppl_@CLASS@_@BINOP@
PPL_PROTO((ppl_@CLASS@_t x,
           ppl_const_@CLASS@_t y));

')

define(`ppl_@CLASS@_@BINMINOP@_code',
`int
ppl_@CLASS@_@BINMINOP@
PPL_PROTO((ppl_@CLASS@_t x,
           ppl_const_@CLASS@_t y));

')

define(`ppl_@CLASS@_add_@REPRESENT@_code',
`int
ppl_@CLASS@_add_@REPRESENT@
PPL_PROTO((ppl_@CLASS@_t ph,
           ppl_const_@UREPRESENT@_t c));

')

define(`ppl_@CLASS@_add_@REPRESENT@_and_minimize_code',
`int
ppl_@CLASS@_add_@REPRESENT@_and_minimize
PPL_PROTO((ppl_@CLASS@_t ph,
           ppl_const_@UREPRESENT@_t c));

')

define(`ppl_@CLASS@_add_@REPRESENT@s_code',
`int
ppl_@CLASS@_add_@REPRESENT@s
PPL_PROTO((ppl_@CLASS@_t ph,
           ppl_const_@UREPRESENT@_System_t cs));

')

define(`ppl_@CLASS@_add_@REPRESENT@s_and_minimize_code',
`int
ppl_@CLASS@_add_@REPRESENT@s_and_minimize
PPL_PROTO((ppl_@CLASS@_t ph,
           ppl_const_@UREPRESENT@_System_t cs));

')

define(`ppl_@CLASS@_add_recycled_@REPRESENT@s_code',
`int
ppl_@CLASS@_add_recycled_@REPRESENT@s
PPL_PROTO((ppl_@CLASS@_t ph,
           ppl_@UREPRESENT@_System_t cs));

')

define(`ppl_@CLASS@_add_recycled_@REPRESENT@s_and_minimize_code',
`int
ppl_@CLASS@_add_recycled_@REPRESENT@s_and_minimize
PPL_PROTO((ppl_@CLASS@_t ph,
           ppl_@UREPRESENT@_System_t cs));

')

define(`ppl_@CLASS@_@AFFIMAGE@_code',
`int
ppl_@CLASS@_@AFFIMAGE@
PPL_PROTO((ppl_@CLASS@_t ph,
           ppl_dimension_type var,
           ppl_const_Linear_Expression_t le,
           ppl_const_Coefficient_t d));

')

define(`ppl_@CLASS@_bounded_@AFFIMAGE@_code',
`int
ppl_@CLASS@_bounded_@AFFIMAGE@
PPL_PROTO((ppl_@CLASS@_t ph,
           ppl_dimension_type var,
           ppl_const_Linear_Expression_t lb,
           ppl_const_Linear_Expression_t ub,
           ppl_const_Coefficient_t d));

')

define(`ppl_@CLASS@_generalized_@AFFIMAGE@_code',
`int
ppl_@CLASS@_generalized_@AFFIMAGE@
PPL_PROTO((ppl_@CLASS@_t ph,
           ppl_dimension_type var,
           enum ppl_enum_Constraint_Type relsym,
           ppl_const_Linear_Expression_t le,
           ppl_const_Coefficient_t d));

')

define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_code',
`int
ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs
PPL_PROTO((ppl_@CLASS@_t ph,
           ppl_const_Linear_Expression_t lhs,
           enum ppl_enum_Constraint_Type relsym,
           ppl_const_Linear_Expression_t rhs));

')

define(`ppl_@CLASS@_@WIDENEXP@_widening_assign_with_tokens_code',
`int
ppl_@CLASS@_@WIDENEXP@_widening_assign_with_tokens
PPL_PROTO((ppl_@CLASS@_t x,
           ppl_const_@CLASS@_t y,
           unsigned* tp));

')

define(`ppl_@CLASS@_@WIDENEXP@_widening_assign_code',
`int
ppl_@CLASS@_@WIDENEXP@_widening_assign
PPL_PROTO((ppl_@CLASS@_t x,
           ppl_const_@CLASS@_t y));

')

define(`ppl_@CLASS@_limited_@WIDENEXP@_extrapolation_assign_with_tokens_code',
`int
ppl_@CLASS@_limited_@WIDENEXP@_extrapolation_assign_with_tokens
PPL_PROTO((ppl_@CLASS@_t x,
           ppl_const_@CLASS@_t y,
           ppl_const_Constraint_System_t cs,
           unsigned* tp));

')

define(`ppl_@CLASS@_limited_@WIDENEXP@_extrapolation_assign_code',
`int
ppl_@CLASS@_limited_@WIDENEXP@_extrapolation_assign
PPL_PROTO((ppl_@CLASS@_t x,
           ppl_const_@CLASS@_t y,
           ppl_const_Constraint_System_t cs));

')

define(`ppl_@CLASS@_bounded_@WIDENEXP@_extrapolation_assign_with_tokens_code',
`int
ppl_@CLASS@_bounded_@WIDENEXP@_extrapolation_assign_with_tokens
PPL_PROTO((ppl_@CLASS@_t x,
           ppl_const_@CLASS@_t y,
           ppl_const_Constraint_System_t cs,
           unsigned* tp));

')

define(`ppl_@CLASS@_bounded_@WIDENEXP@_extrapolation_assign_code',
`int
ppl_@CLASS@_bounded_@WIDENEXP@_extrapolation_assign
PPL_PROTO((ppl_@CLASS@_t x,
           ppl_const_@CLASS@_t y,
           ppl_const_Constraint_System_t cs));

')

define(`ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@_code',
`int
ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@
PPL_PROTO((ppl_@CLASS@_t ph,
           ppl_dimension_type d));

')

define(`ppl_@CLASS@_remove_space_dimensions_code',
`int
ppl_@CLASS@_remove_space_dimensions
PPL_PROTO((ppl_@CLASS@_t ph,
           ppl_dimension_type ds[],
           size_t n));

')

define(`ppl_@CLASS@_remove_higher_space_dimensions_code',
`int
ppl_@CLASS@_remove_higher_space_dimensions
PPL_PROTO((ppl_@CLASS@_t ph,
           ppl_dimension_type d));

')

define(`ppl_@CLASS@_map_space_dimensions_code',
`int
ppl_@CLASS@_map_space_dimensions
PPL_PROTO((ppl_@CLASS@_t ph,
           ppl_dimension_type maps[],
           size_t n));

')

define(`ppl_@CLASS@_expand_space_dimension_code',
`int
ppl_@CLASS@_expand_space_dimension
PPL_PROTO((ppl_@CLASS@_t ph,
           ppl_dimension_type d,
           ppl_dimension_type m));

')

define(`ppl_@CLASS@_fold_space_dimensions_code',
`int
ppl_@CLASS@_fold_space_dimensions
PPL_PROTO((ppl_@CLASS@_t ph,
           ppl_dimension_type ds[],
           size_t n,
           ppl_dimension_type d));

')

divert`'dnl
