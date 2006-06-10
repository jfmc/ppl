divert(-1)dnl

define(`ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_code',
`int
ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension
(ppl_@CLASS@_t* pph,
 ppl_dimension_type d,
 int empty) try {
  *pph = to_nonconst(new @TOPOLOGY@@CPP_CLASS@(d, empty ? EMPTY : UNIVERSE));
  return 0;
}
CATCH_ALL

')

define(`ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@CLASS@_code',
`int
ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@CLASS@
(ppl_@CLASS@_t* pph,
 ppl_const_@CLASS@_t ph) try {
  const @INTOPOLOGY@@CPP_CLASS@& phh
    = *static_cast<const @INTOPOLOGY@@CPP_CLASS@*>(to_const(ph));
  *pph = to_nonconst(new @TOPOLOGY@@CPP_CLASS@(phh));
  return 0;
}
CATCH_ALL;

')

define(`ppl_new_@TOPOLOGY@@CLASS@_from_@UREPRESENT@_System_code',
`int
ppl_new_@TOPOLOGY@@CLASS@_from_@UREPRESENT@_System
(ppl_@CLASS@_t* pph, ppl_const_@UREPRESENT@_System_t cs) try {
  const @UREPRESENT@_System& ccs = *to_const(cs);
  *pph = to_nonconst(new @TOPOLOGY@@CPP_CLASS@(ccs));
  return 0;
}
CATCH_ALL

')

define(`ppl_new_@TOPOLOGY@@CLASS@_recycle_@UREPRESENT@_System_code',
`int
ppl_new_@TOPOLOGY@@CLASS@_recycle_@UREPRESENT@_System
(ppl_@CLASS@_t* pph, ppl_@UREPRESENT@_System_t cs) try {
  @UREPRESENT@_System& ccs = *to_nonconst(cs);
  *pph = to_nonconst(new @TOPOLOGY@@CPP_CLASS@(ccs));
  return 0;
}
CATCH_ALL

')

define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@_code',
`int
ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@
(ppl_@CLASS@_t* pph,
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
  *pph = to_nonconst(new @TOPOLOGY@@CPP_CLASS@(cbbox, From_Bounding_Box()));
  return 0;
}
CATCH_ALL

')

define(`ppl_delete_@CLASS@_code',
`int
ppl_delete_@CLASS@(ppl_const_@CLASS@_t ph) try {
  delete to_const(ph);
  return 0;
}
CATCH_ALL

')

define(`ppl_assign_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@_code',
`int
ppl_assign_@TOPOLOGY@@CLASS@_from_@TOPOLOGY@@CLASS@
(ppl_@CLASS@_t dst,
 ppl_const_@CLASS@_t src) try {
  const @TOPOLOGY@@CPP_CLASS@& ssrc
    = *static_cast<const @TOPOLOGY@@CPP_CLASS@*>(to_const(src));
  @TOPOLOGY@@CPP_CLASS@& ddst
    = *static_cast<@TOPOLOGY@@CPP_CLASS@*>(to_nonconst(dst));
  ddst = ssrc;
  return 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_@DIMENSION@_code',
`int
ppl_@CLASS@_@DIMENSION@
(ppl_const_@CLASS@_t ph,
 ppl_dimension_type* m) try {
  *m = to_const(ph)->@DIMENSION@();
  return 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_@DESCRIBE@s_code',
`int
ppl_@CLASS@_@DESCRIBE@s
(ppl_const_@CLASS@_t ph,
 ppl_const_@UDESCRIBE@_System_t* pcs) try {
  const @CPP_CLASS@& pph = *to_const(ph);
  const @UDESCRIBE@_System& cs = pph.@ALT_DESCRIBE@s();
  *pcs = to_const(&cs);
  return 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_minimized_@DESCRIBE@s_code',
`int
ppl_@CLASS@_minimized_@DESCRIBE@s
(ppl_const_@CLASS@_t ph,
 ppl_const_@UDESCRIBE@_System_t* pcs) try {
  const @CPP_CLASS@& pph = *to_const(ph);
  const @UDESCRIBE@_System& cs = pph.minimized_@ALT_DESCRIBE@s();
  *pcs = to_const(&cs);
  return 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_relation_with_@UDESCRIBE@_code',
`int
ppl_@CLASS@_relation_with_@UDESCRIBE@
(ppl_const_@CLASS@_t ph,
 ppl_const_@UDESCRIBE@_t c) try {
  const @CPP_CLASS@& pph = *to_const(ph);
  const @UDESCRIBE@& cc = *to_const(c);
  return pph.relation_with(cc).get_flags();
}
CATCH_ALL

')

define(`ppl_@CLASS@_is_@STATE@_code',
`int
ppl_@CLASS@_is_@STATE@(ppl_const_@CLASS@_t ph) try {
  const @CPP_CLASS@& pph = *to_const(ph);
  return pph.is_@STATE@() ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_code',
`int
ppl_@CLASS@_bounds_from_@ABOVEBELOW@
(ppl_const_@CLASS@_t ph,
 ppl_const_Linear_Expression_t le) try {
  const @CPP_CLASS@& pph = *to_const(ph);
  const Linear_Expression& lle = *to_const(le);
  return pph.bounds_from_@ABOVEBELOW@(lle) ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_@MAXMIN@_code',
`int
ppl_@CLASS@_@MAXMIN@
(ppl_const_@CLASS@_t ph,
 ppl_const_Linear_Expression_t le,
 ppl_Coefficient_t sup_n,
 ppl_Coefficient_t sup_d,
 int* poptimum,
 ppl_@UGENERATOR@_t point) try {
  const @CPP_CLASS@& pph = *to_const(ph);
  const Linear_Expression& lle = *to_const(le);
  Coefficient& ssup_n = *to_nonconst(sup_n);
  Coefficient& ssup_d = *to_nonconst(sup_d);
  @UGENERATOR@& ppoint = *to_nonconst(point);
  bool optimum;
  bool ok = pph.@MAXMIN@(lle, ssup_n, ssup_d, optimum, ppoint);
  if (ok)
    *poptimum = optimum ? 1 : 0;
  return ok ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_shrink_@BOX@_code',
`int
ppl_@CLASS@_shrink_@BOX@
(ppl_const_@CLASS@_t ph,
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

  const @CPP_CLASS@& pph = *to_const(ph);
  C_Shrink_Box csbox(set_empty, raise_lower_bound, lower_upper_bound);
  pph.shrink_@BOX@(csbox, Complexity_Class(complexity));
  return 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_@COMPARISON@_@CLASS@_code',
`int
ppl_@CLASS@_@COMPARISON@_@CLASS@
(ppl_const_@CLASS@_t x,
 ppl_const_@CLASS@_t y) try {
  const @CPP_CLASS@& xx = *to_const(x);
  const @CPP_CLASS@& yy = *to_const(y);
  return xx.@COMPARISON@(yy) ? 1 : 0;
  return xx.@COMPARISON@(yy) ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_equals_@CLASS@_code',
`int
ppl_@CLASS@_equals_@CLASS@
(ppl_const_@CLASS@_t x,
 ppl_const_@CLASS@_t y) try {
  const @CPP_CLASS@& xx = *to_const(x);
  const @CPP_CLASS@& yy = *to_const(y);
  return (xx == yy) ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_OK_code',
`int
ppl_@CLASS@_OK(ppl_const_@CLASS@_t ph) try {
  return to_const(ph)->OK() ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_topological_closure_assign_code',
`int
ppl_@CLASS@_topological_closure_assign(ppl_@CLASS@_t ph) try {
  @CPP_CLASS@& pph = *to_nonconst(ph);
  pph.topological_closure_assign();
  return 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_@BINOP@_code',
`int
ppl_@CLASS@_@BINOP@
(ppl_@CLASS@_t x,
 ppl_const_@CLASS@_t y) try {
  @CPP_CLASS@& xx = *to_nonconst(x);
  const @CPP_CLASS@& yy = *to_const(y);
  xx.@BINOP@(yy);
  return 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_@BINMINOP@_code',
`int
ppl_@CLASS@_@BINMINOP@
(ppl_@CLASS@_t x,
 ppl_const_@CLASS@_t y) try {
  @CPP_CLASS@& xx = *to_nonconst(x);
  const @CPP_CLASS@& yy = *to_const(y);
  return xx.@BINMINOP@(yy) ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_add_@REPRESENT@_code',
`int
ppl_@CLASS@_add_@REPRESENT@
(ppl_@CLASS@_t ph,
 ppl_const_@UREPRESENT@_t c) try {
  @CPP_CLASS@& pph = *to_nonconst(ph);
  const @UREPRESENT@& cc = *to_const(c);
  pph.add_@ALT_REPRESENT@(cc);
  return 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_add_@REPRESENT@_and_minimize_code',
`int
ppl_@CLASS@_add_@REPRESENT@_and_minimize
(ppl_@CLASS@_t ph,
 ppl_const_@UREPRESENT@_t c) try {
  @CPP_CLASS@& pph = *to_nonconst(ph);
  const @UREPRESENT@& cc = *to_const(c);
  return pph.add_@ALT_REPRESENT@_and_minimize(cc) ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_add_@REPRESENT@s_code',
`int
ppl_@CLASS@_add_@REPRESENT@s
(ppl_@CLASS@_t ph,
 ppl_const_@UREPRESENT@_System_t cs) try {
  @CPP_CLASS@& pph = *to_nonconst(ph);
  const @UREPRESENT@_System& ccs = *to_const(cs);
  pph.add_@ALT_REPRESENT@s(ccs);
  return 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_add_@REPRESENT@s_and_minimize_code',
`int
ppl_@CLASS@_add_@REPRESENT@s_and_minimize
(ppl_@CLASS@_t ph,
 ppl_const_@UREPRESENT@_System_t cs) try {
  @CPP_CLASS@& pph = *to_nonconst(ph);
  const @UREPRESENT@_System& ccs = *to_const(cs);
  return pph.add_@ALT_REPRESENT@s_and_minimize(ccs) ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_add_recycled_@REPRESENT@s_code',
`int
ppl_@CLASS@_add_recycled_@REPRESENT@s
(ppl_@CLASS@_t ph,
 ppl_@UREPRESENT@_System_t cs) try {
  @CPP_CLASS@& pph = *to_nonconst(ph);
  @UREPRESENT@_System& ccs = *to_nonconst(cs);
  pph.add_recycled_@ALT_REPRESENT@s(ccs);
  return 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_add_recycled_@REPRESENT@s_and_minimize_code',
`int
ppl_@CLASS@_add_recycled_@REPRESENT@s_and_minimize
(ppl_@CLASS@_t ph,
 ppl_@UREPRESENT@_System_t cs) try {
  @CPP_CLASS@& pph = *to_nonconst(ph);
  @UREPRESENT@_System& ccs = *to_nonconst(cs);
  return pph.add_recycled_@ALT_REPRESENT@s_and_minimize(ccs) ? 1 : 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_@AFFIMAGE@_code',
`int
ppl_@CLASS@_@AFFIMAGE@
(ppl_@CLASS@_t ph,
 ppl_dimension_type var,
 ppl_const_Linear_Expression_t le,
 ppl_const_Coefficient_t d) try {
  @CPP_CLASS@& pph = *to_nonconst(ph);
  const Linear_Expression& lle = *to_const(le);
  const Coefficient& dd = *to_const(d);
  pph.@AFFIMAGE@(Variable(var), lle, dd);
  return 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_bounded_@AFFIMAGE@_code',
`int
ppl_@CLASS@_bounded_@AFFIMAGE@
(ppl_@CLASS@_t ph,
 ppl_dimension_type var,
 ppl_const_Linear_Expression_t lb,
 ppl_const_Linear_Expression_t ub,
 ppl_const_Coefficient_t d) try {
  @CPP_CLASS@& pph = *to_nonconst(ph);
  const Linear_Expression& llb = *to_const(lb);
  const Linear_Expression& uub = *to_const(ub);
  const Coefficient& dd = *to_const(d);
  pph.bounded_@AFFIMAGE@(Variable(var), llb, uub, dd);
  return 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_generalized_@AFFIMAGE@_code',
`int
ppl_@CLASS@_generalized_@AFFIMAGE@
(ppl_@CLASS@_t ph,
 ppl_dimension_type var,
 enum ppl_enum_Constraint_Type relsym,
 ppl_const_Linear_Expression_t le,
 ppl_const_Coefficient_t d) try {
  @CPP_CLASS@& pph = *to_nonconst(ph);
  const Linear_Expression& lle = *to_const(le);
  const Coefficient& dd = *to_const(d);
  pph.generalized_@AFFIMAGE@
    (Variable(var), relation_symbol(relsym), lle, dd);
  return 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_code',
`int
ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs
(ppl_@CLASS@_t ph,
 ppl_const_Linear_Expression_t lhs,
 enum ppl_enum_Constraint_Type relsym,
 ppl_const_Linear_Expression_t rhs) try {
  @CPP_CLASS@& pph = *to_nonconst(ph);
  const Linear_Expression& llhs = *to_const(lhs);
  const Linear_Expression& rrhs = *to_const(rhs);
  pph.generalized_@AFFIMAGE@(llhs, relation_symbol(relsym), rrhs);
  return 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_@WIDENEXP@_widening_assign_with_tokens_code',
`int
ppl_@CLASS@_@WIDENEXP@_widening_assign_with_tokens
(ppl_@CLASS@_t x,
 ppl_const_@CLASS@_t y,
 unsigned* tp) try {
  @CPP_CLASS@& xx = *to_nonconst(x);
  const @CPP_CLASS@& yy = *to_const(y);
  xx.@WIDENEXP@_widening_assign(yy, tp);
  return 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_@WIDENEXP@_widening_assign_code',
`int
ppl_@CLASS@_@WIDENEXP@_widening_assign
(ppl_@CLASS@_t x,
 ppl_const_@CLASS@_t y) try {
  return ppl_@CLASS@_@WIDENEXP@_widening_assign_with_tokens(x, y, 0);
}
CATCH_ALL

')

define(`ppl_@CLASS@_limited_@WIDENEXP@_extrapolation_assign_with_tokens_code',
`int
ppl_@CLASS@_limited_@WIDENEXP@_extrapolation_assign_with_tokens
(ppl_@CLASS@_t x,
 ppl_const_@CLASS@_t y,
 ppl_const_@UCONSTRAINER@_System_t cs,
 unsigned* tp) try {
  @CPP_CLASS@& xx = *to_nonconst(x);
  const @CPP_CLASS@& yy = *to_const(y);
  const @UCONSTRAINER@_System& ccs = *to_const(cs);
  xx.limited_@WIDENEXP@_extrapolation_assign(yy, ccs, tp);
  return 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_limited_@WIDENEXP@_extrapolation_assign_code',
`int
ppl_@CLASS@_limited_@WIDENEXP@_extrapolation_assign
(ppl_@CLASS@_t x,
 ppl_const_@CLASS@_t y,
 ppl_const_@UCONSTRAINER@_System_t cs) try {
  return
    ppl_@CLASS@_limited_@WIDENEXP@_extrapolation_assign_with_tokens
      (x, y, cs, 0);
}
CATCH_ALL

')

define(`ppl_@CLASS@_bounded_@WIDENEXP@_extrapolation_assign_with_tokens_code',
`int
ppl_@CLASS@_bounded_@WIDENEXP@_extrapolation_assign_with_tokens
(ppl_@CLASS@_t x,
 ppl_const_@CLASS@_t y,
 ppl_const_@UCONSTRAINER@_System_t cs,
 unsigned* tp) try {
  @CPP_CLASS@& xx = *to_nonconst(x);
  const @CPP_CLASS@& yy = *to_const(y);
  const @UCONSTRAINER@_System& ccs = *to_const(cs);
  xx.bounded_@WIDENEXP@_extrapolation_assign(yy, ccs, tp);
  return 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_bounded_@WIDENEXP@_extrapolation_assign_code',
`int
ppl_@CLASS@_bounded_@WIDENEXP@_extrapolation_assign
(ppl_@CLASS@_t x,
 ppl_const_@CLASS@_t y,
 ppl_const_@UCONSTRAINER@_System_t cs) try {
  return
    ppl_@CLASS@_bounded_@WIDENEXP@_extrapolation_assign_with_tokens
      (x, y, cs, 0);
}
CATCH_ALL

')

define(`ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@_code'
`int
ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@
(ppl_@CLASS@_t ph,
 ppl_dimension_type d) try {
  @CPP_CLASS@& pph = *to_nonconst(ph);
  pph.add_space_dimensions_@EMBEDPROJECT@(d);
  return 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_remove_space_dimensions_code'
`int
ppl_@CLASS@_remove_space_dimensions
(ppl_@CLASS@_t ph,
 ppl_dimension_type ds[],
 size_t n) try {
  @CPP_CLASS@& pph = *to_nonconst(ph);
  Variables_Set to_be_removed;
  for (ppl_dimension_type i = n; i-- > 0; )
    to_be_removed.insert(Variable(ds[i]));
  pph.remove_space_dimensions(to_be_removed);
  return 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_remove_higher_space_dimensions_code'
`int
ppl_@CLASS@_remove_higher_space_dimensions
(ppl_@CLASS@_t ph,
 ppl_dimension_type d) try {
  @CPP_CLASS@& pph = *to_nonconst(ph);
  pph.remove_higher_space_dimensions(d);
  return 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_map_space_dimensions_code'
`int
ppl_@CLASS@_map_space_dimensions
(ppl_@CLASS@_t ph,
 ppl_dimension_type maps[],
 size_t n) try {
  @CPP_CLASS@& pph = *to_nonconst(ph);
  PIFunc pifunc(maps, n);
  pph.map_space_dimensions(pifunc);
  return 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_expand_space_dimension_code'
`int
ppl_@CLASS@_expand_space_dimension
(ppl_@CLASS@_t ph,
 ppl_dimension_type d,
 ppl_dimension_type m) try {
  @CPP_CLASS@& pph = *to_nonconst(ph);
  pph.expand_space_dimension(Variable(d), m);
  return 0;
}
CATCH_ALL

')

define(`ppl_@CLASS@_fold_space_dimensions_code'
`int
ppl_@CLASS@_fold_space_dimensions
(ppl_@CLASS@_t ph,
 ppl_dimension_type ds[],
 size_t n,
 ppl_dimension_type d) try {
  @CPP_CLASS@& pph = *to_nonconst(ph);
  Variables_Set to_be_folded;
  for (ppl_dimension_type i = n; i-- > 0; )
    to_be_folded.insert(Variable(ds[i]));
  pph.fold_space_dimensions(to_be_folded, Variable(d));
  return 0;
}
CATCH_ALL

')

divert`'dnl
