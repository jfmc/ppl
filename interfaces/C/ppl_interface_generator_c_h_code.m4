divert(-1)dnl

define(`ppl_new_4TOPOLOGY44CLASS4_from_space_dimension_code',
`int
ppl_new_4TOPOLOGY44CLASS4_from_space_dimension
PPL_PROTO((ppl_4CLASS4_t* pph, ppl_dimension_type d, int empty));

')

define(`ppl_new_4TOPOLOGY44CLASS4_from_4INTOPOLOGY44CLASS4_code',
`int
ppl_new_4TOPOLOGY44CLASS4_from_4INTOPOLOGY44CLASS4
PPL_PROTO((ppl_4CLASS4_t* pph, ppl_const_4CLASS4_t ph));

')

define(`ppl_new_4TOPOLOGY44CLASS4_from_4UREPRESENT4_System_code',
`int
ppl_new_4TOPOLOGY44CLASS4_from_4UREPRESENT4_System
PPL_PROTO((ppl_4CLASS4_t* pph, ppl_const_4UREPRESENT4_System_t cs));

')

define(`ppl_new_4TOPOLOGY44CLASS4_recycle_4UREPRESENT4_System_code',
`int
ppl_new_4TOPOLOGY44CLASS4_recycle_4UREPRESENT4_System
PPL_PROTO((ppl_4CLASS4_t* pph, ppl_4UREPRESENT4_System_t cs));

')

define(`ppl_new_4TOPOLOGY44CLASS4_from_4BOX4_code',
`int
ppl_new_4TOPOLOGY44CLASS4_from_bounding_box
PPL_PROTO((ppl_4CLASS4_t* pph,
	   ppl_dimension_type (*space_dimension)(void),
	   int (*is_empty)(void),
	   int (*get_lower_bound)(ppl_dimension_type k, int closed,
				  ppl_Coefficient_t n,
				  ppl_Coefficient_t d),
	   int (*get_upper_bound)(ppl_dimension_type k, int closed,
				  ppl_Coefficient_t n,
				  ppl_Coefficient_t d)));

')

define(`ppl_delete_4CLASS4_code',
`int
ppl_delete_4CLASS4 PPL_PROTO((ppl_const_4CLASS4_t ph));

')

define(`ppl_assign_4TOPOLOGY44CLASS4_from_4TOPOLOGY44CLASS4_code',
`int
ppl_assign_4TOPOLOGY44CLASS4_from_4TOPOLOGY44CLASS4
PPL_PROTO((ppl_4CLASS4_t dst, ppl_const_4CLASS4_t src));

')

define(`ppl_4CLASS4_4DIMENSION4_code',
`int
ppl_4CLASS4_4DIMENSION4
PPL_PROTO((ppl_const_4CLASS4_t ph, ppl_dimension_type* m));

')

define(`ppl_4CLASS4_4DESCRIBE4s_code',
`int
ppl_4CLASS4_4DESCRIBE4s
PPL_PROTO((ppl_const_4CLASS4_t ph,
           ppl_const_4UDESCRIBE4_System_t* pcs));

')

define(`ppl_4CLASS4_minimized_4DESCRIBE4s_code',
`int
ppl_4CLASS4_minimized_4DESCRIBE4s
PPL_PROTO((ppl_const_4CLASS4_t ph,
           ppl_const_4UDESCRIBE4_System_t* pcs));

')

define(`ppl_4CLASS4_relation_with_4UDESCRIBE4_code',
`int
ppl_4CLASS4_relation_with_4UDESCRIBE4
PPL_PROTO((ppl_const_4CLASS4_t ph,
           ppl_const_4UDESCRIBE4_t c));

')

define(`ppl_4CLASS4_is_4STATE4_code',
`int
ppl_4CLASS4_is_4STATE4 PPL_PROTO((ppl_const_4CLASS4_t ph));

')

define(`ppl_4CLASS4_bounds_from_4ABOVEBELOW4_code',
`int
ppl_4CLASS4_bounds_from_4ABOVEBELOW4
PPL_PROTO((ppl_const_4CLASS4_t ph,
           ppl_const_Linear_Expression_t le));

')

define(`ppl_4CLASS4_4MAXMIN4_code',
`int
ppl_4CLASS4_4MAXMIN4
PPL_PROTO((ppl_const_4CLASS4_t ph,
           ppl_const_Linear_Expression_t le,
           ppl_Coefficient_t ext_n,
           ppl_Coefficient_t ext_d,
           int* poptimum,
           ppl_Generator_t point));

')

define(`ppl_4CLASS4_shrink_4BOX4_code',
`int
ppl_4CLASS4_shrink_4BOX4
PPL_PROTO((ppl_const_4CLASS4_t ph,
           unsigned int complexity,
           void (*set_empty)(void),
           void (*raise_lower_bound)(ppl_dimension_type k, int closed,
                                     ppl_const_Coefficient_t n,
                                     ppl_const_Coefficient_t d),
           void (*lower_upper_bound)(ppl_dimension_type k, int closed,
                                     ppl_const_Coefficient_t n,
                                     ppl_const_Coefficient_t d)));

')

define(`ppl_4CLASS4_4COMPARISON4_4CLASS4_code',
`int
ppl_4CLASS4_4COMPARISON4_4CLASS4
PPL_PROTO((ppl_const_4CLASS4_t x,
           ppl_const_4CLASS4_t y));

')

define(`ppl_4CLASS4_equals_4CLASS4_code',
`int
ppl_4CLASS4_equals_4CLASS4
PPL_PROTO((ppl_const_4CLASS4_t x,
           ppl_const_4CLASS4_t y));

')

define(`ppl_4CLASS4_OK_code',
`int
ppl_4CLASS4_OK PPL_PROTO((ppl_const_4CLASS4_t ph));

')

define(`ppl_4CLASS4_topological_closure_assign_code',
`int
ppl_4CLASS4_topological_closure_assign PPL_PROTO((ppl_4CLASS4_t ph));

')

define(`ppl_4CLASS4_4BINOP4_code',
`int
ppl_4CLASS4_4BINOP4
PPL_PROTO((ppl_4CLASS4_t x,
           ppl_const_4CLASS4_t y));

')

define(`ppl_4CLASS4_4BINMINOP4_code',
`int
ppl_4CLASS4_4BINMINOP4
PPL_PROTO((ppl_4CLASS4_t x,
           ppl_const_4CLASS4_t y));

')

define(`ppl_4CLASS4_add_4REPRESENT4_code',
`int
ppl_4CLASS4_add_4REPRESENT4
PPL_PROTO((ppl_4CLASS4_t ph,
           ppl_const_4UREPRESENT4_t c));

')

define(`ppl_4CLASS4_add_4REPRESENT4_and_minimize_code',
`int
ppl_4CLASS4_add_4REPRESENT4_and_minimize
PPL_PROTO((ppl_4CLASS4_t ph,
           ppl_const_4UREPRESENT4_t c));

')

define(`ppl_4CLASS4_add_4REPRESENT4s_code',
`int
ppl_4CLASS4_add_4REPRESENT4s
PPL_PROTO((ppl_4CLASS4_t ph,
           ppl_const_4UREPRESENT4_System_t cs));

')

define(`ppl_4CLASS4_add_4REPRESENT4s_and_minimize_code',
`int
ppl_4CLASS4_add_4REPRESENT4s_and_minimize
PPL_PROTO((ppl_4CLASS4_t ph,
           ppl_const_4UREPRESENT4_System_t cs));

')

define(`ppl_4CLASS4_add_recycled_4REPRESENT4s_code',
`int
ppl_4CLASS4_add_recycled_4REPRESENT4s
PPL_PROTO((ppl_4CLASS4_t ph,
           ppl_4UREPRESENT4_System_t cs));

')

define(`ppl_4CLASS4_add_recycled_4REPRESENT4s_and_minimize_code',
`int
ppl_4CLASS4_add_recycled_4REPRESENT4s_and_minimize
PPL_PROTO((ppl_4CLASS4_t ph,
           ppl_4UREPRESENT4_System_t cs));

')

define(`ppl_4CLASS4_4AFFIMAGE4_code',
`int
ppl_4CLASS4_4AFFIMAGE4
PPL_PROTO((ppl_4CLASS4_t ph,
           ppl_dimension_type var,
           ppl_const_Linear_Expression_t le,
           ppl_const_Coefficient_t d));

')

define(`ppl_4CLASS4_bounded_4AFFIMAGE4_code',
`int
ppl_4CLASS4_bounded_4AFFIMAGE4
PPL_PROTO((ppl_4CLASS4_t ph,
           ppl_dimension_type var,
           ppl_const_Linear_Expression_t lb,
           ppl_const_Linear_Expression_t ub,
           ppl_const_Coefficient_t d));

')

define(`ppl_4CLASS4_generalized_4AFFIMAGE4_code',
`int
ppl_4CLASS4_generalized_4AFFIMAGE4
PPL_PROTO((ppl_4CLASS4_t ph,
           ppl_dimension_type var,
           enum ppl_enum_Constraint_Type relsym,
           ppl_const_Linear_Expression_t le,
           ppl_const_Coefficient_t d));

')

define(`ppl_4CLASS4_generalized_4AFFIMAGE4_lhs_rhs_code',
`int
ppl_4CLASS4_generalized_4AFFIMAGE4_lhs_rhs
PPL_PROTO((ppl_4CLASS4_t ph,
           ppl_const_Linear_Expression_t lhs,
           enum ppl_enum_Constraint_Type relsym,
           ppl_const_Linear_Expression_t rhs));

')

define(`ppl_4CLASS4_4WIDENEXP4_widening_assign_with_tokens_code',
`int
ppl_4CLASS4_4WIDENEXP4_widening_assign_with_tokens
PPL_PROTO((ppl_4CLASS4_t x,
           ppl_const_4CLASS4_t y,
           unsigned* tp));

')

define(`ppl_4CLASS4_4WIDENEXP4_widening_assign_code',
`int
ppl_4CLASS4_4WIDENEXP4_widening_assign
PPL_PROTO((ppl_4CLASS4_t x,
           ppl_const_4CLASS4_t y));

')

define(`ppl_4CLASS4_limited_4WIDENEXP4_extrapolation_assign_with_tokens_code',
`int
ppl_4CLASS4_limited_4WIDENEXP4_extrapolation_assign_with_tokens
PPL_PROTO((ppl_4CLASS4_t x,
           ppl_const_4CLASS4_t y,
           ppl_const_Constraint_System_t cs,
           unsigned* tp));

')

define(`ppl_4CLASS4_limited_4WIDENEXP4_extrapolation_assign_code',
`int
ppl_4CLASS4_limited_4WIDENEXP4_extrapolation_assign
PPL_PROTO((ppl_4CLASS4_t x,
           ppl_const_4CLASS4_t y,
           ppl_const_Constraint_System_t cs));

')

define(`ppl_4CLASS4_bounded_4WIDENEXP4_extrapolation_assign_with_tokens_code',
`int
ppl_4CLASS4_bounded_4WIDENEXP4_extrapolation_assign_with_tokens
PPL_PROTO((ppl_4CLASS4_t x,
           ppl_const_4CLASS4_t y,
           ppl_const_Constraint_System_t cs,
           unsigned* tp));

')

define(`ppl_4CLASS4_bounded_4WIDENEXP4_extrapolation_assign_code',
`int
ppl_4CLASS4_bounded_4WIDENEXP4_extrapolation_assign
PPL_PROTO((ppl_4CLASS4_t x,
           ppl_const_4CLASS4_t y,
           ppl_const_Constraint_System_t cs));

')

define(`ppl_4CLASS4_add_space_dimensions_4EMBEDPROJECT4_code',
`int
ppl_4CLASS4_add_space_dimensions_4EMBEDPROJECT4
PPL_PROTO((ppl_4CLASS4_t ph,
           ppl_dimension_type d));

')

define(`ppl_4CLASS4_remove_space_dimensions_code',
`int
ppl_4CLASS4_remove_space_dimensions
PPL_PROTO((ppl_4CLASS4_t ph,
           ppl_dimension_type ds[],
           size_t n));

')

define(`ppl_4CLASS4_remove_higher_space_dimensions_code',
`int
ppl_4CLASS4_remove_higher_space_dimensions
PPL_PROTO((ppl_4CLASS4_t ph,
           ppl_dimension_type d));

')

define(`ppl_4CLASS4_map_space_dimensions_code',
`int
ppl_4CLASS4_map_space_dimensions
PPL_PROTO((ppl_4CLASS4_t ph,
           ppl_dimension_type maps[],
           size_t n));

')

define(`ppl_4CLASS4_expand_space_dimension_code',
`int
ppl_4CLASS4_expand_space_dimension
PPL_PROTO((ppl_4CLASS4_t ph,
           ppl_dimension_type d,
           ppl_dimension_type m));

')

define(`ppl_4CLASS4_fold_space_dimensions_code',
`int
ppl_4CLASS4_fold_space_dimensions
PPL_PROTO((ppl_4CLASS4_t ph,
           ppl_dimension_type ds[],
           size_t n,
           ppl_dimension_type d));

')

divert`'dnl
