divert(-1)dnl

define(`ppl_new_4TOPOLOGY_44CLASS4_from_space_dimension_code',
`int
ppl_new_4TOPOLOGY_44CLASS4_from_space_dimension
(ppl_4CLASS4_t* pph,
 ppl_dimension_type d,
 int empty) try {
  *pph = to_nonconst(new 4TOPOLOGY_44CLASS4(d, empty ? EMPTY : UNIVERSE));
  return 0;
}
CATCH_ALL

')

define(`ppl_new_4TOPOLOGY_44CLASS4_from_4INTOPOLOGY_44CLASS4_code',
`int
ppl_new_4TOPOLOGY_44CLASS4_from_4INTOPOLOGY_44CLASS4
(ppl_4CLASS4_t* pph,
 ppl_const_4CLASS4_t ph) try {
  const 4INTOPOLOGY_44CLASS4& phh
    = *static_cast<const 4INTOPOLOGY_44CLASS4*>(to_const(ph));
  *pph = to_nonconst(new 4TOPOLOGY_44CLASS4(phh));
  return 0;
}
CATCH_ALL;

')

define(`ppl_new_4TOPOLOGY_44CLASS4_from_4UALT_REPRESENT4_System_code',
`int
ppl_new_4TOPOLOGY_44CLASS4_from_4UALT_REPRESENT4_System
(ppl_4CLASS4_t* pph, ppl_const_4UALT_REPRESENT4_System_t cs) try {
  const 4UALT_REPRESENT4_System& ccs = *to_const(cs);
  *pph = to_nonconst(new 4TOPOLOGY_44CLASS4(ccs));
  return 0;
}
CATCH_ALL

')

define(`ppl_new_4TOPOLOGY_44CLASS4_recycle_4UALT_REPRESENT4_System_code',
`int
ppl_new_4TOPOLOGY_44CLASS4_recycle_4UALT_REPRESENT4_System
(ppl_4CLASS4_t* pph, ppl_4UALT_REPRESENT4_System_t cs) try {
  4UALT_REPRESENT4_System& ccs = *to_nonconst(cs);
  *pph = to_nonconst(new 4TOPOLOGY_44CLASS4(ccs));
  return 0;
}
CATCH_ALL

')

define(`ppl_new_4TOPOLOGY_44CLASS4_from_4BOX4_code',
`int
ppl_new_4TOPOLOGY_44CLASS4_from_4BOX4
(ppl_4CLASS4_t* pph,
 ppl_dimension_type (*space_dimension)(void),
 int (*is_empty)(void),
 int (*get_lower_bound)(ppl_dimension_type k, int closed,
			ppl_Coefficient_t n,
			ppl_Coefficient_t d),
 int (*get_upper_bound)(ppl_dimension_type k, int closed,
			ppl_Coefficient_t n,
			ppl_Coefficient_t d)) try {
  C_Build_Box cbbox(space_dimension, is_empty,
		    get_lower_bound, get_upper_bound);
  *pph = to_nonconst(new 4TOPOLOGY_44CLASS4(cbbox, From_Bounding_Box()));
  return 0;
}
CATCH_ALL

')

define(`ppl_delete_4CLASS4_code',
`int
ppl_delete_4CLASS4(ppl_const_4CLASS4_t ph) try {
  delete to_const(ph);
  return 0;
}
CATCH_ALL

')

define(`ppl_assign_4TOPOLOGY_44CLASS4_from_4TOPOLOGY_44CLASS4_code',
`int
ppl_assign_4TOPOLOGY_44CLASS4_from_4TOPOLOGY_44CLASS4
(ppl_4CLASS4_t dst,
 ppl_const_4CLASS4_t src) try {
  const 4TOPOLOGY_44CLASS4& ssrc
    = *static_cast<const 4TOPOLOGY_44CLASS4*>(to_const(src));
  4TOPOLOGY_44CLASS4& ddst = *static_cast<TOPOLOGY_44CLASS4*>(to_nonconst(dst));
  ddst = ssrc;
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_4DIMENSION4_code',
`int
ppl_4CLASS4_4DIMENSION4
(ppl_const_4CLASS4_t ph,
 ppl_dimension_type* m) try {
  *m = to_const(ph)->4DIMENSION4();
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_DESCRIBEs_code',
`int
ppl_4CLASS4_DESCRIBEs
(ppl_const_4CLASS4_t ph,
 ppl_const_4UALT_DESCRIBE_System_t* pcs) try {
  const 4CLASS4& pph = *to_const(ph);
  const 4UALT_DESCRIBE_System& cs = pph.DESCRIBEs();
  *pcs = to_const(&cs);
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_minimized_DESCRIBEs_code',
`int
ppl_4CLASS4_minimized_DESCRIBEs
(ppl_const_4CLASS4_t ph,
 ppl_const_4UALT_DESCRIBE_System_t* pcs) try {
  const 4CLASS4& pph = *to_const(ph);
  const 4UALT_DESCRIBE_System& cs = pph.minimized_DESCRIBEs();
  *pcs = to_const(&cs);
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_relation_with_4UALT_DESCRIBE_code',
`int
ppl_4CLASS4_relation_with_4UALT_DESCRIBE
(ppl_const_4CLASS4_t ph,
 ppl_const_4UALT_DESCRIBE_t c) try {
  const 4CLASS4& pph = *to_const(ph);
  const 4UALT_DESCRIBE& cc = *to_const(c);
  return pph.relation_with(cc).get_flags();
}
CATCH_ALL

')

define(`ppl_4CLASS4_is_4STATE4_code',
`int
ppl_4CLASS4_is_4STATE4(ppl_const_4CLASS4_t ph) try {
  const 4CLASS4& pph = *to_const(ph);
  return pph.is_4STATE4() ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_bounds_from_4ABOVEBELOW4_code',
`int
ppl_4CLASS4_bounds_from_4ABOVEBELOW4
(ppl_const_4CLASS4_t ph,
 ppl_const_Linear_Expression_t le) try {
  const 4CLASS4& pph = *to_const(ph);
  const Linear_Expression& lle = *to_const(le);
  return pph.bounds_from_4ABOVEBELOW4(lle) ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_4MAXMIN4_code',
`int
ppl_4CLASS4_4MAXMIN4
(ppl_const_4CLASS4_t ph,
 ppl_const_Linear_Expression_t le,
 ppl_Coefficient_t sup_n,
 ppl_Coefficient_t sup_d,
 int* poptimum,
 ppl_4UGENERATOR4_t point) try {
  const 4CLASS4& pph = *to_const(ph);
  const Linear_Expression& lle = *to_const(le);
  Coefficient& ssup_n = *to_nonconst(sup_n);
  Coefficient& ssup_d = *to_nonconst(sup_d);
  4UGENERATOR4& ppoint = *to_nonconst(point);
  bool optimum;
  bool ok = pph.4MAXMIN4(lle, ssup_n, ssup_d, optimum, ppoint);
  if (ok)
    *poptimum = optimum ? 1 : 0;
  return ok ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_shrink_4BOX4_code',
`int
ppl_4CLASS4_shrink_4BOX4
(ppl_const_4CLASS4_t ph,
 unsigned int complexity,
 void (*set_empty)(void),
 void (*raise_lower_bound)(ppl_dimension_type k, int closed,
			   ppl_const_Coefficient_t n,
			   ppl_const_Coefficient_t d),
 void (*lower_upper_bound)(ppl_dimension_type k, int closed,
			   ppl_const_Coefficient_t n,
			   ppl_const_Coefficient_t d)) try {
  if (complexity != POLYNOMIAL_COMPLEXITY
      && complexity != SIMPLEX_COMPLEXITY
      && complexity != ANY_COMPLEXITY)
    return PPL_ERROR_INVALID_ARGUMENT;

  const 4CLASS4& pph = *to_const(ph);
  C_Shrink_Box csbox(set_empty, raise_lower_bound, lower_upper_bound);
  pph.shrink_4BOX4(csbox, Complexity_Class(complexity));
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_4COMPARISON4_4CLASS4_code',
`int
ppl_4CLASS4_4COMPARISON4_4CLASS4
(ppl_const_4CLASS4_t x,
 ppl_const_4CLASS4_t y) try {
  const 4CLASS4& xx = *to_const(x);
  const 4CLASS4& yy = *to_const(y);
  return xx.4COMPARISON4(yy) ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_equals_4CLASS4_code',
`int
ppl_4CLASS4_equals_4CLASS4
(ppl_const_4CLASS4_t x,
 ppl_const_4CLASS4_t y) try {
  const 4CLASS4& xx = *to_const(x);
  const 4CLASS4& yy = *to_const(y);
  return (xx == yy) ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_OK_code',
`int
ppl_4CLASS4_OK(ppl_const_4CLASS4_t ph) try {
  return to_const(ph)->OK() ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_topological_closure_assign_code',
`int
ppl_4CLASS4_topological_closure_assign(ppl_4CLASS4_t ph) try {
  4CLASS4& pph = *to_nonconst(ph);
  pph.topological_closure_assign();
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_4BINOP4_code',
`int
ppl_4CLASS4_4BINOP4
(ppl_4CLASS4_t x,
 ppl_const_4CLASS4_t y) try {
  4CLASS4& xx = *to_nonconst(x);
  const 4CLASS4& yy = *to_const(y);
  xx.4BINOP4(yy);
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_4BINMINOP4_code',
`int
ppl_4CLASS4_4BINMINOP4
(ppl_4CLASS4_t x,
 ppl_const_4CLASS4_t y) try {
  4CLASS4& xx = *to_nonconst(x);
  const 4CLASS4& yy = *to_const(y);
  return xx.4BINMINOP4(yy) ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_add_4REPRESENT4_code',
`int
ppl_4CLASS4_add_4REPRESENT4
(ppl_4CLASS4_t ph,
 ppl_const_4UALT_REPRESENT4_t c) try {
  4CLASS4& pph = *to_nonconst(ph);
  const 4UALT_REPRESENT4& cc = *to_const(c);
  pph.add_4REPRESENT4(cc);
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_add_4REPRESENT4_and_minimize_code',
`int
ppl_4CLASS4_add_4REPRESENT4_and_minimize
(ppl_4CLASS4_t ph,
 ppl_const_4UALT_REPRESENT4_t c) try {
  4CLASS4& pph = *to_nonconst(ph);
  const 4UALT_REPRESENT4& cc = *to_const(c);
  return pph.add_4REPRESENT4_and_minimize(cc) ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_add_4REPRESENT4s_code',
`int
ppl_4CLASS4_add_4REPRESENT4s
(ppl_4CLASS4_t ph,
 ppl_const_4UALT_REPRESENT4_System_t cs) try {
  4CLASS4& pph = *to_nonconst(ph);
  const 4UALT_REPRESENT4_System& ccs = *to_const(cs);
  pph.add_4REPRESENT4s(ccs);
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_add_4REPRESENT4s_and_minimize_code',
`int
ppl_4CLASS4_add_4REPRESENT4s_and_minimize
(ppl_4CLASS4_t ph,
 ppl_const_4UALT_REPRESENT4_System_t cs) try {
  4CLASS4& pph = *to_nonconst(ph);
  const 4UALT_REPRESENT4_System& ccs = *to_const(cs);
  return pph.add_4REPRESENT4s_and_minimize(ccs) ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_add_recycled_4REPRESENT4s_code',
`int
ppl_4CLASS4_add_recycled_4REPRESENT4s
(ppl_4CLASS4_t ph,
 ppl_4UALT_REPRESENT4_System_t cs) try {
  4CLASS4& pph = *to_nonconst(ph);
  4UALT_REPRESENT4_System& ccs = *to_nonconst(cs);
  pph.add_recycled_4REPRESENT4s(ccs);
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_add_recycled_4REPRESENT4s_and_minimize_code',
`int
ppl_4CLASS4_add_recycled_4REPRESENT4s_and_minimize
(ppl_4CLASS4_t ph,
 ppl_4UALT_REPRESENT4_System_t cs) try {
  4CLASS4& pph = *to_nonconst(ph);
  4UALT_REPRESENT4_System& ccs = *to_nonconst(cs);
  return pph.add_recycled_4REPRESENT4s_and_minimize(ccs) ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_4AFFIMAGE4_code',
`int
ppl_4CLASS4_4AFFIMAGE4
(ppl_4CLASS4_t ph,
 ppl_dimension_type var,
 ppl_const_Linear_Expression_t le,
 ppl_const_Coefficient_t d) try {
  4CLASS4& pph = *to_nonconst(ph);
  const Linear_Expression& lle = *to_const(le);
  const Coefficient& dd = *to_const(d);
  pph.4AFFIMAGE4(Variable(var), lle, dd);
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_bounded_4AFFIMAGE4_code',
`int
ppl_4CLASS4_bounded_4AFFIMAGE4
(ppl_4CLASS4_t ph,
 ppl_dimension_type var,
 ppl_const_Linear_Expression_t lb,
 ppl_const_Linear_Expression_t ub,
 ppl_const_Coefficient_t d) try {
  4CLASS4& pph = *to_nonconst(ph);
  const Linear_Expression& llb = *to_const(lb);
  const Linear_Expression& uub = *to_const(ub);
  const Coefficient& dd = *to_const(d);
  pph.bounded_4AFFIMAGE4(Variable(var), llb, uub, dd);
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_generalized_4AFFIMAGE4_code',
`int
ppl_4CLASS4_generalized_4AFFIMAGE4
(ppl_4CLASS4_t ph,
 ppl_dimension_type var,
 enum ppl_enum_Constraint_Type relsym,
 ppl_const_Linear_Expression_t le,
 ppl_const_Coefficient_t d) try {
  4CLASS4& pph = *to_nonconst(ph);
  const Linear_Expression& lle = *to_const(le);
  const Coefficient& dd = *to_const(d);
  pph.generalized_4AFFIMAGE4(Variable(var), relation_symbol(relsym), lle,
			       dd);
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_generalized_4AFFIMAGE4_lhs_rhs_code',
`int
ppl_4CLASS4_generalized_4AFFIMAGE4_lhs_rhs
(ppl_4CLASS4_t ph,
 ppl_const_Linear_Expression_t lhs,
 enum ppl_enum_Constraint_Type relsym,
 ppl_const_Linear_Expression_t rhs) try {
  4CLASS4& pph = *to_nonconst(ph);
  const Linear_Expression& llhs = *to_const(lhs);
  const Linear_Expression& rrhs = *to_const(rhs);
  pph.generalized_4AFFIMAGE4(llhs, relation_symbol(relsym), rrhs);
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_4WIDENEXP4_widening_assign_with_tokens_code',
`int
ppl_4CLASS4_4WIDENEXP4_widening_assign_with_tokens
(ppl_4CLASS4_t x,
 ppl_const_4CLASS4_t y,
 unsigned* tp) try {
  4CLASS4& xx = *to_nonconst(x);
  const 4CLASS4& yy = *to_const(y);
  xx.4WIDENEXP4_widening_assign(yy, tp);
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_4WIDENEXP4_widening_assign_code',
`int
ppl_4CLASS4_4WIDENEXP4_widening_assign
(ppl_4CLASS4_t x,
 ppl_const_4CLASS4_t y) try {
  return ppl_4CLASS4_4WIDENEXP4_widening_assign_with_tokens(x, y, 0);
}
CATCH_ALL

')

define(`ppl_4CLASS4_limited_4WIDENEXP4_extrapolation_assign_with_tokens_code',
`int
ppl_4CLASS4_limited_4WIDENEXP4_extrapolation_assign_with_tokens
(ppl_4CLASS4_t x,
 ppl_const_4CLASS4_t y,
 ppl_const_4UCONSTRAINER4_System_t cs,
 unsigned* tp) try {
  4CLASS4& xx = *to_nonconst(x);
  const 4CLASS4& yy = *to_const(y);
  const 4UCONSTRAINER4_System& ccs = *to_const(cs);
  xx.limited_4WIDENEXP4_extrapolation_assign(yy, ccs, tp);
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_limited_4WIDENEXP4_extrapolation_assign_code',
`int
ppl_4CLASS4_limited_4WIDENEXP4_extrapolation_assign
(ppl_4CLASS4_t x,
 ppl_const_4CLASS4_t y,
 ppl_const_4UCONSTRAINER4_System_t cs) try {
  return ppl_4CLASS4_limited_4WIDENEXP4_extrapolation_assign_with_tokens(x, y,
									cs, 0);
}
CATCH_ALL

')

define(`ppl_4CLASS4_bounded_4WIDENEXP4_extrapolation_assign_with_tokens_code',
`int
ppl_4CLASS4_bounded_4WIDENEXP4_extrapolation_assign_with_tokens
(ppl_4CLASS4_t x,
 ppl_const_4CLASS4_t y,
 ppl_const_4UCONSTRAINER4_System_t cs,
 unsigned* tp) try {
  4CLASS4& xx = *to_nonconst(x);
  const 4CLASS4& yy = *to_const(y);
  const 4UCONSTRAINER4_System& ccs = *to_const(cs);
  xx.bounded_4WIDENEXP4_extrapolation_assign(yy, ccs, tp);
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_bounded_4WIDENEXP4_extrapolation_assign_code',
`int
ppl_4CLASS4_bounded_4WIDENEXP4_extrapolation_assign
(ppl_4CLASS4_t x,
 ppl_const_4CLASS4_t y,
 ppl_const_4UCONSTRAINER4_System_t cs) try {
  return ppl_4CLASS4_bounded_4WIDENEXP4_extrapolation_assign_with_tokens(x, y,
									cs, 0);
}
CATCH_ALL

')

dnl FIXME: why is this different from other BINOP's?
define(`ppl_4CLASS4_concatenate_assign_code'
`int
ppl_4CLASS4_concatenate_assign
(ppl_4CLASS4_t x,
 ppl_const_4CLASS4_t y) try {
  4CLASS4& xx = *to_nonconst(x);
  const 4CLASS4& yy = *to_const(y);
  xx.concatenate_assign(yy);
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_add_space_dimensions_4EMBEDPROJECT4_code'
`int
ppl_4CLASS4_add_space_dimensions_4EMBEDPROJECT4
(ppl_4CLASS4_t ph,
 ppl_dimension_type d) try {
  4CLASS4& pph = *to_nonconst(ph);
  pph.add_space_dimensions_4EMBEDPROJECT4(d);
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_remove_space_dimensions_code'
`int
ppl_4CLASS4_remove_space_dimensions
(ppl_4CLASS4_t ph,
 ppl_dimension_type ds[],
 size_t n) try {
  4CLASS4& pph = *to_nonconst(ph);
  Variables_Set to_be_removed;
  for (ppl_dimension_type i = n; i-- > 0; )
    to_be_removed.insert(Variable(ds[i]));
  pph.remove_space_dimensions(to_be_removed);
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_remove_higher_space_dimensions_code'
`int
ppl_4CLASS4_remove_higher_space_dimensions
(ppl_4CLASS4_t ph,
 ppl_dimension_type d) try {
  4CLASS4& pph = *to_nonconst(ph);
  pph.remove_higher_space_dimensions(d);
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_map_space_dimensions_code'
`int
ppl_4CLASS4_map_space_dimensions
(ppl_4CLASS4_t ph,
 ppl_dimension_type maps[],
 size_t n) try {
  4CLASS4& pph = *to_nonconst(ph);
  PIFunc pifunc(maps, n);
  pph.map_space_dimensions(pifunc);
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_expand_space_dimension_code'
`int
ppl_4CLASS4_expand_space_dimension
(ppl_4CLASS4_t ph,
 ppl_dimension_type d,
 ppl_dimension_type m) try {
  4CLASS4& pph = *to_nonconst(ph);
  pph.expand_space_dimension(Variable(d), m);
  return 0;
}
CATCH_ALL

')

define(`ppl_4CLASS4_fold_space_dimensions_code'
`int
ppl_4CLASS4_fold_space_dimensions
(ppl_4CLASS4_t ph,
 ppl_dimension_type ds[],
 size_t n,
 ppl_dimension_type d) try {
  4CLASS4& pph = *to_nonconst(ph);
  Variables_Set to_be_folded;
  for (ppl_dimension_type i = n; i-- > 0; )
    to_be_folded.insert(Variable(ds[i]));
  pph.fold_space_dimensions(to_be_folded, Variable(d));
  return 0;
}
CATCH_ALL

')

divert`'dnl
