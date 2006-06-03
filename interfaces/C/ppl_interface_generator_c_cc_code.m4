divert(-1)dnl

define(`ppl_new_TOPOLOGY_M4_CLASS_from_space_dimension_code',
`int
ppl_new_TOPOLOGY_M4_CLASS_from_space_dimension
(ppl_M4_CLASS_t* pph,
 ppl_dimension_type d) try {
  *pph = to_nonconst(new TOPOLOGY_M4_CLASS(d, UNIVERSE));
  return 0;
}
CATCH_ALL

')

define(`ppl_new_TOPOLOGY_M4_CLASS_empty_from_space_dimension_code',
`int
ppl_new_TOPOLOGY_M4_CLASS_empty_from_space_dimension
(ppl_M4_CLASS_t* pph,
 ppl_dimension_type d) try {
  *pph = to_nonconst(new TOPOLOGY_M4_CLASS(d, EMPTY));
  return 0;
}
CATCH_ALL

')

define(`ppl_new_TOPOLOGY_M4_CLASS_from_INTOPOLOGY_M4_CLASS_code',
`int
ppl_new_TOPOLOGY_M4_CLASS_from_INTOPOLOGY_M4_CLASS
(ppl_M4_CLASS_t* pph,
 ppl_const_M4_CLASS_t ph) try {
  const INTOPOLOGY_M4_CLASS& phh
    = *static_cast<const INTOPOLOGY_M4_CLASS*>(to_const(ph));
  *pph = to_nonconst(new TOPOLOGY_M4_CLASS(phh));
  return 0;
}
CATCH_ALL;

')

define(`ppl_new_TOPOLOGY_M4_CLASS_from_UALT_REPRESENT_System_code',
`int
ppl_new_TOPOLOGY_M4_CLASS_from_UALT_REPRESENT_System
(ppl_M4_CLASS_t* pph, ppl_const_UALT_REPRESENT_System_t cs) try {
  const UALT_REPRESENT_System& ccs = *to_const(cs);
  *pph = to_nonconst(new TOPOLOGY_M4_CLASS(ccs));
  return 0;
}
CATCH_ALL

')

define(`ppl_new_TOPOLOGY_M4_CLASS_recycle_UALT_REPRESENT_System_code',
`int
ppl_new_TOPOLOGY_M4_CLASS_recycle_UALT_REPRESENT_System
(ppl_M4_CLASS_t* pph, ppl_UALT_REPRESENT_System_t cs) try {
  UALT_REPRESENT_System& ccs = *to_nonconst(cs);
  *pph = to_nonconst(new TOPOLOGY_M4_CLASS(ccs));
  return 0;
}
CATCH_ALL

')

define(`ppl_new_TOPOLOGY_M4_CLASS_from_BOX_code',
`int
ppl_new_TOPOLOGY_M4_CLASS_from_BOX
(ppl_M4_CLASS_t* pph,
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
  *pph = to_nonconst(new TOPOLOGY_M4_CLASS(cbbox, From_Bounding_Box()));
  return 0;
}
CATCH_ALL

')

define(`ppl_delete_M4_CLASS_code',
`int
ppl_delete_M4_CLASS(ppl_const_M4_CLASS_t ph) try {
  delete to_const(ph);
  return 0;
}
CATCH_ALL

')

define(`ppl_assign_TOPOLOGY_M4_CLASS_from_TOPOLOGY_M4_CLASS_code',
`int
ppl_assign_TOPOLOGY_M4_CLASS_from_TOPOLOGY_M4_CLASS
(ppl_M4_CLASS_t dst,
 ppl_const_M4_CLASS_t src) try {
  const TOPOLOGY_M4_CLASS& ssrc
    = *static_cast<const TOPOLOGY_M4_CLASS*>(to_const(src));
  TOPOLOGY_M4_CLASS& ddst = *static_cast<TOPOLOGY_M4_CLASS*>(to_nonconst(dst));
  ddst = ssrc;
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_DIM_code',
`int
ppl_M4_CLASS_DIM
(ppl_const_M4_CLASS_t ph,
 ppl_dimension_type* m) try {
  *m = to_const(ph)->DIM();
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_DESCRIBEs_code',
`int
ppl_M4_CLASS_DESCRIBEs
(ppl_const_M4_CLASS_t ph,
 ppl_const_UALT_DESCRIBE_System_t* pcs) try {
  const M4_CLASS& pph = *to_const(ph);
  const UALT_DESCRIBE_System& cs = pph.DESCRIBEs();
  *pcs = to_const(&cs);
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_minimized_DESCRIBEs_code',
`int
ppl_M4_CLASS_minimized_DESCRIBEs
(ppl_const_M4_CLASS_t ph,
 ppl_const_UALT_DESCRIBE_System_t* pcs) try {
  const M4_CLASS& pph = *to_const(ph);
  const UALT_DESCRIBE_System& cs = pph.minimized_DESCRIBEs();
  *pcs = to_const(&cs);
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_relation_with_UALT_DESCRIBE_code',
`int
ppl_M4_CLASS_relation_with_UALT_DESCRIBE
(ppl_const_M4_CLASS_t ph,
 ppl_const_UALT_DESCRIBE_t c) try {
  const M4_CLASS& pph = *to_const(ph);
  const UALT_DESCRIBE& cc = *to_const(c);
  return pph.relation_with(cc).get_flags();
}
CATCH_ALL

')

define(`ppl_M4_CLASS_is_STATE_code',
`int
ppl_M4_CLASS_is_STATE(ppl_const_M4_CLASS_t ph) try {
  const M4_CLASS& pph = *to_const(ph);
  return pph.is_STATE() ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_bounds_from_ABOVEBELOW_code',
`int
ppl_M4_CLASS_bounds_from_ABOVEBELOW
(ppl_const_M4_CLASS_t ph,
 ppl_const_Linear_Expression_t le) try {
  const M4_CLASS& pph = *to_const(ph);
  const Linear_Expression& lle = *to_const(le);
  return pph.bounds_from_ABOVEBELOW(lle) ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_MAXMIN_code',
`int
ppl_M4_CLASS_MAXMIN
(ppl_const_M4_CLASS_t ph,
 ppl_const_Linear_Expression_t le,
 ppl_Coefficient_t sup_n,
 ppl_Coefficient_t sup_d,
 int* poptimum,
 ppl_UALT_GNRT_t point) try {
  const M4_CLASS& pph = *to_const(ph);
  const Linear_Expression& lle = *to_const(le);
  Coefficient& ssup_n = *to_nonconst(sup_n);
  Coefficient& ssup_d = *to_nonconst(sup_d);
  UALT_GNRT& ppoint = *to_nonconst(point);
  bool optimum;
  bool ok = pph.MAXMIN(lle, ssup_n, ssup_d, optimum, ppoint);
  if (ok)
    *poptimum = optimum ? 1 : 0;
  return ok ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_shrink_BOX_code',
`int
ppl_M4_CLASS_shrink_BOX
(ppl_const_M4_CLASS_t ph,
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

  const M4_CLASS& pph = *to_const(ph);
  C_Shrink_Box csbox(set_empty, raise_lower_bound, lower_upper_bound);
  pph.shrink_BOX(csbox, Complexity_Class(complexity));
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_COMPARISON_M4_CLASS_code',
`int
ppl_M4_CLASS_COMPARISON_M4_CLASS
(ppl_const_M4_CLASS_t x,
 ppl_const_M4_CLASS_t y) try {
  const M4_CLASS& xx = *to_const(x);
  const M4_CLASS& yy = *to_const(y);
  return xx.COMPARISON(yy) ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_equals_M4_CLASS_code',
`int
ppl_M4_CLASS_equals_M4_CLASS
(ppl_const_M4_CLASS_t x,
 ppl_const_M4_CLASS_t y) try {
  const M4_CLASS& xx = *to_const(x);
  const M4_CLASS& yy = *to_const(y);
  return (xx == yy) ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_OK_code',
`int
ppl_M4_CLASS_OK(ppl_const_M4_CLASS_t ph) try {
  return to_const(ph)->OK() ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_topological_closure_assign_code',
`int
ppl_M4_CLASS_topological_closure_assign(ppl_M4_CLASS_t ph) try {
  M4_CLASS& pph = *to_nonconst(ph);
  pph.topological_closure_assign();
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_BINOP_code',
`int
ppl_M4_CLASS_BINOP
(ppl_M4_CLASS_t x,
 ppl_const_M4_CLASS_t y) try {
  M4_CLASS& xx = *to_nonconst(x);
  const M4_CLASS& yy = *to_const(y);
  xx.BINOP(yy);
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_BPMIN_code',
`int
ppl_M4_CLASS_BPMIN
(ppl_M4_CLASS_t x,
 ppl_const_M4_CLASS_t y) try {
  M4_CLASS& xx = *to_nonconst(x);
  const M4_CLASS& yy = *to_const(y);
  return xx.BPMIN(yy) ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_add_REPRESENT_code',
`int
ppl_M4_CLASS_add_REPRESENT
(ppl_M4_CLASS_t ph,
 ppl_const_UALT_REPRESENT_t c) try {
  M4_CLASS& pph = *to_nonconst(ph);
  const UALT_REPRESENT& cc = *to_const(c);
  pph.add_REPRESENT(cc);
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_add_REPRESENT_and_minimize_code',
`int
ppl_M4_CLASS_add_REPRESENT_and_minimize
(ppl_M4_CLASS_t ph,
 ppl_const_UALT_REPRESENT_t c) try {
  M4_CLASS& pph = *to_nonconst(ph);
  const UALT_REPRESENT& cc = *to_const(c);
  return pph.add_REPRESENT_and_minimize(cc) ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_add_REPRESENTs_code',
`int
ppl_M4_CLASS_add_REPRESENTs
(ppl_M4_CLASS_t ph,
 ppl_const_UALT_REPRESENT_System_t cs) try {
  M4_CLASS& pph = *to_nonconst(ph);
  const UALT_REPRESENT_System& ccs = *to_const(cs);
  pph.add_REPRESENTs(ccs);
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_add_REPRESENTs_and_minimize_code',
`int
ppl_M4_CLASS_add_REPRESENTs_and_minimize
(ppl_M4_CLASS_t ph,
 ppl_const_UALT_REPRESENT_System_t cs) try {
  M4_CLASS& pph = *to_nonconst(ph);
  const UALT_REPRESENT_System& ccs = *to_const(cs);
  return pph.add_REPRESENTs_and_minimize(ccs) ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_add_recycled_REPRESENTs_code',
`int
ppl_M4_CLASS_add_recycled_REPRESENTs
(ppl_M4_CLASS_t ph,
 ppl_UALT_REPRESENT_System_t cs) try {
  M4_CLASS& pph = *to_nonconst(ph);
  UALT_REPRESENT_System& ccs = *to_nonconst(cs);
  pph.add_recycled_REPRESENTs(ccs);
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_add_recycled_REPRESENTs_and_minimize_code',
`int
ppl_M4_CLASS_add_recycled_REPRESENTs_and_minimize
(ppl_M4_CLASS_t ph,
 ppl_UALT_REPRESENT_System_t cs) try {
  M4_CLASS& pph = *to_nonconst(ph);
  UALT_REPRESENT_System& ccs = *to_nonconst(cs);
  return pph.add_recycled_REPRESENTs_and_minimize(ccs) ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_AFFIM_code',
`int
ppl_M4_CLASS_AFFIM
(ppl_M4_CLASS_t ph,
 ppl_dimension_type var,
 ppl_const_Linear_Expression_t le,
 ppl_const_Coefficient_t d) try {
  M4_CLASS& pph = *to_nonconst(ph);
  const Linear_Expression& lle = *to_const(le);
  const Coefficient& dd = *to_const(d);
  pph.AFFIM(Variable(var), lle, dd);
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_bounded_AFFIM_code',
`int
ppl_M4_CLASS_bounded_AFFIM
(ppl_M4_CLASS_t ph,
 ppl_dimension_type var,
 ppl_const_Linear_Expression_t lb,
 ppl_const_Linear_Expression_t ub,
 ppl_const_Coefficient_t d) try {
  M4_CLASS& pph = *to_nonconst(ph);
  const Linear_Expression& llb = *to_const(lb);
  const Linear_Expression& uub = *to_const(ub);
  const Coefficient& dd = *to_const(d);
  pph.bounded_AFFIM(Variable(var), llb, uub, dd);
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_generalized_AFFIM_code',
`int
ppl_M4_CLASS_generalized_AFFIM
(ppl_M4_CLASS_t ph,
 ppl_dimension_type var,
 enum ppl_enum_Constraint_Type relsym,
 ppl_const_Linear_Expression_t le,
 ppl_const_Coefficient_t d) try {
  M4_CLASS& pph = *to_nonconst(ph);
  const Linear_Expression& lle = *to_const(le);
  const Coefficient& dd = *to_const(d);
  pph.generalized_AFFIM(Variable(var), relation_symbol(relsym), lle,
			       dd);
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_generalized_AFFIM_lhs_rhs_code',
`int
ppl_M4_CLASS_generalized_AFFIM_lhs_rhs
(ppl_M4_CLASS_t ph,
 ppl_const_Linear_Expression_t lhs,
 enum ppl_enum_Constraint_Type relsym,
 ppl_const_Linear_Expression_t rhs) try {
  M4_CLASS& pph = *to_nonconst(ph);
  const Linear_Expression& llhs = *to_const(lhs);
  const Linear_Expression& rrhs = *to_const(rhs);
  pph.generalized_AFFIM(llhs, relation_symbol(relsym), rrhs);
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_WIDENEXP_widening_assign_with_tokens_code',
`int
ppl_M4_CLASS_WIDENEXP_widening_assign_with_tokens
(ppl_M4_CLASS_t x,
 ppl_const_M4_CLASS_t y,
 unsigned* tp) try {
  M4_CLASS& xx = *to_nonconst(x);
  const M4_CLASS& yy = *to_const(y);
  xx.WIDENEXP_widening_assign(yy, tp);
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_WIDENEXP_widening_assign_code',
`int
ppl_M4_CLASS_WIDENEXP_widening_assign
(ppl_M4_CLASS_t x,
 ppl_const_M4_CLASS_t y) try {
  return ppl_M4_CLASS_WIDENEXP_widening_assign_with_tokens(x, y, 0);
}
CATCH_ALL

')

define(`ppl_M4_CLASS_limited_WIDENEXP_extrapolation_assign_with_tokens_code',
`int
ppl_M4_CLASS_limited_WIDENEXP_extrapolation_assign_with_tokens
(ppl_M4_CLASS_t x,
 ppl_const_M4_CLASS_t y,
 ppl_const_UALT_RSTRCT_System_t cs,
 unsigned* tp) try {
  M4_CLASS& xx = *to_nonconst(x);
  const M4_CLASS& yy = *to_const(y);
  const UALT_RSTRCT_System& ccs = *to_const(cs);
  xx.limited_WIDENEXP_extrapolation_assign(yy, ccs, tp);
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_limited_WIDENEXP_extrapolation_assign_code',
`int
ppl_M4_CLASS_limited_WIDENEXP_extrapolation_assign
(ppl_M4_CLASS_t x,
 ppl_const_M4_CLASS_t y,
 ppl_const_UALT_RSTRCT_System_t cs) try {
  return ppl_M4_CLASS_limited_WIDENEXP_extrapolation_assign_with_tokens(x, y,
									cs, 0);
}
CATCH_ALL

')

define(`ppl_M4_CLASS_bounded_WIDENEXP_extrapolation_assign_with_tokens_code',
`int
ppl_M4_CLASS_bounded_WIDENEXP_extrapolation_assign_with_tokens
(ppl_M4_CLASS_t x,
 ppl_const_M4_CLASS_t y,
 ppl_const_UALT_RSTRCT_System_t cs,
 unsigned* tp) try {
  M4_CLASS& xx = *to_nonconst(x);
  const M4_CLASS& yy = *to_const(y);
  const UALT_RSTRCT_System& ccs = *to_const(cs);
  xx.bounded_WIDENEXP_extrapolation_assign(yy, ccs, tp);
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_bounded_WIDENEXP_extrapolation_assign_code',
`int
ppl_M4_CLASS_bounded_WIDENEXP_extrapolation_assign
(ppl_M4_CLASS_t x,
 ppl_const_M4_CLASS_t y,
 ppl_const_UALT_RSTRCT_System_t cs) try {
  return ppl_M4_CLASS_bounded_WIDENEXP_extrapolation_assign_with_tokens(x, y,
									cs, 0);
}
CATCH_ALL

')

dnl FIXME: why is this different from other BINOP's?
define(`ppl_M4_CLASS_concatenate_assign_code'
`int
ppl_M4_CLASS_concatenate_assign
(ppl_M4_CLASS_t x,
 ppl_const_M4_CLASS_t y) try {
  M4_CLASS& xx = *to_nonconst(x);
  const M4_CLASS& yy = *to_const(y);
  xx.concatenate_assign(yy);
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_add_space_dimensions_EMBEDPROJECT_code'
`int
ppl_M4_CLASS_add_space_dimensions_EMBEDPROJECT
(ppl_M4_CLASS_t ph,
 ppl_dimension_type d) try {
  M4_CLASS& pph = *to_nonconst(ph);
  pph.add_space_dimensions_EMBEDPROJECT(d);
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_remove_space_dimensions_code'
`int
ppl_M4_CLASS_remove_space_dimensions
(ppl_M4_CLASS_t ph,
 ppl_dimension_type ds[],
 size_t n) try {
  M4_CLASS& pph = *to_nonconst(ph);
  Variables_Set to_be_removed;
  for (ppl_dimension_type i = n; i-- > 0; )
    to_be_removed.insert(Variable(ds[i]));
  pph.remove_space_dimensions(to_be_removed);
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_remove_higher_space_dimensions_code'
`int
ppl_M4_CLASS_remove_higher_space_dimensions
(ppl_M4_CLASS_t ph,
 ppl_dimension_type d) try {
  M4_CLASS& pph = *to_nonconst(ph);
  pph.remove_higher_space_dimensions(d);
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_map_space_dimensions_code'
`int
ppl_M4_CLASS_map_space_dimensions
(ppl_M4_CLASS_t ph,
 ppl_dimension_type maps[],
 size_t n) try {
  M4_CLASS& pph = *to_nonconst(ph);
  PIFunc pifunc(maps, n);
  pph.map_space_dimensions(pifunc);
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_expand_space_dimension_code'
`int
ppl_M4_CLASS_expand_space_dimension
(ppl_M4_CLASS_t ph,
 ppl_dimension_type d,
 ppl_dimension_type m) try {
  M4_CLASS& pph = *to_nonconst(ph);
  pph.expand_space_dimension(Variable(d), m);
  return 0;
}
CATCH_ALL

')

define(`ppl_M4_CLASS_fold_space_dimensions_code'
`int
ppl_M4_CLASS_fold_space_dimensions
(ppl_M4_CLASS_t ph,
 ppl_dimension_type ds[],
 size_t n,
 ppl_dimension_type d) try {
  M4_CLASS& pph = *to_nonconst(ph);
  Variables_Set to_be_folded;
  for (ppl_dimension_type i = n; i-- > 0; )
    to_be_folded.insert(Variable(ds[i]));
  pph.fold_space_dimensions(to_be_folded, Variable(d));
  return 0;
}
CATCH_ALL

')

divert`'dnl
