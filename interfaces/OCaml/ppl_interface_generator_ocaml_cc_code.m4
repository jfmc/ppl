m4_divert(-1)dnl
m4_define(`m4_access_class_code',
`dnl
//! Give access to the embedded @CLASS@* in \p v.
inline @CPP_CLASS@*&
p_@CLASS@_val(value v) {
  return *reinterpret_cast<@CPP_CLASS@**>(Data_custom_val(v));
}

void
custom_@CLASS@_finalize(value v) {
  std::cerr << "About to delete a polyhedron " << *p_@CLASS@_val(v)
	    << std::endl;
  delete p_@CLASS@_val(v);
}

static struct custom_operations @CLASS@_custom_operations = {
  "it.unipr.cs.ppl" "." PPL_VERSION "." "@CLASS@"@COMMA@
  custom_@CLASS@_finalize@COMMA@
  custom_compare_default@COMMA@
  custom_hash_default@COMMA@
  custom_serialize_default@COMMA@
  custom_deserialize_default
};

inline value
val_p_@CLASS@(const @CPP_CLASS@& ph) {
  value v = caml_alloc_custom(&@CLASS@_custom_operations,
			      sizeof(@CPP_CLASS@*), 0, 1);
  p_@CLASS@_val(v) = const_cast<@CPP_CLASS@*>(&ph);
  return(v);
}

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_code',
`dnl
extern "C"
CAMLprim value
ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension(value d) try {
  CAMLparam1(d);
  int dd = Int_val(d);
  if (dd < 0)
    abort();
  CAMLreturn(val_p_@CLASS@(*new @TOPOLOGY@@CPP_CLASS@(dd)));
}
CATCH_ALL

')

m4_define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s_code',
`dnl
extern "C"
CAMLprim value
ppl_new_@TOPOLOGY@@CLASS@_from_@BUILD_REPRESENT@s(value cl) try {
  CAMLparam1(cl);
  @UBUILD_REPRESENT@_System cs = build_ppl_@UBUILD_REPRESENT@_System(cl);
  CAMLreturn(val_p_@CLASS@(*new @TOPOLOGY@@CPP_CLASS@(cs)));
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_relation_with_@RELATION_REPRESENT@_code',
`dnl
extern "C"
CAMLprim value
ppl_@CLASS@_relation_with_@RELATION_REPRESENT@(value ph, value c) try {
  CAMLparam2(ph, c);
  const @CPP_CLASS@& pph = *p_@CLASS@_val(ph);
  @URELATION_REPRESENT@ ppl_c = build_ppl_@URELATION_REPRESENT@(c);
  Poly_@UALT_RELATION_REPRESENT@_Relation r = pph.relation_with(ppl_c);
  CAMLreturn(build_ocaml_poly_@ALT_RELATION_REPRESENT@_relation(r));
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_@DIMENSION@_code',
`dnl
extern "C"
CAMLprim value
ppl_@CLASS@_@DIMENSION@(value ph) try {
  CAMLparam1(ph);
  const @CPP_CLASS@& pph = *p_@CLASS@_val(ph);
  dimension_type d = pph.@DIMENSION@();
  if (d > INT_MAX)
    abort();
  CAMLreturn(Val_int(d));
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_@HAS_PROPERTY@_code',
`dnl
extern "C"
CAMLprim value
ppl_@CLASS@_@HAS_PROPERTY@(value ph) try {
  CAMLparam1(ph);
  const @CPP_CLASS@& pph = *p_@CLASS@_val(ph);
  CAMLreturn(Val_bool(pph.@HAS_PROPERTY@()));
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_code',
`dnl
extern "C"
CAMLprim value
ppl_@CLASS@_bounds_from_@ABOVEBELOW@(value ph, value le) try {
  CAMLparam2(ph, le);
  const @CPP_CLASS@& pph = *p_@CLASS@_val(ph);
  Linear_Expression ple = build_ppl_Linear_Expression(le);
  CAMLreturn(Val_bool(pph.bounds_from_@ABOVEBELOW@(ple)));
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_code',
`dnl
extern "C"
void
ppl_@CLASS@_add_@ADD_REPRESENT@(value ph, value c) try {
  CAMLparam2(ph, c);
  @CPP_CLASS@& pph = *p_@CLASS@_val(ph);
  @UADD_REPRESENT@ pc = build_ppl_@UADD_REPRESENT@(c);
  pph.add_@ADD_REPRESENT@(pc);
  CAMLreturn0;
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize_code',
`dnl
extern "C"
CAMLprim value
ppl_@CLASS@_add_@ADD_REPRESENT@_and_minimize(value ph, value c) try {
  CAMLparam2(ph, c);
  @CPP_CLASS@& pph = *p_@CLASS@_val(ph);
  @UADD_REPRESENT@ pc = build_ppl_@UADD_REPRESENT@(c);
  CAMLreturn(Val_bool(pph.add_@ADD_REPRESENT@_and_minimize(pc)));
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_code',
`dnl
extern "C"
void
ppl_@CLASS@_add_@ADD_REPRESENT@s(value ph, value cs) try {
  CAMLparam2(ph, cs);
  @CPP_CLASS@& pph = *p_@CLASS@_val(ph);
  @UADD_REPRESENT@_System pcs = build_ppl_@UADD_REPRESENT@_System(cs);
  pph.add_@ADD_REPRESENT@s(pcs);
  CAMLreturn0;
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize_code',
`dnl
extern "C"
CAMLprim value
ppl_@CLASS@_add_@ADD_REPRESENT@s_and_minimize(value ph, value cs) try {
  CAMLparam2(ph, cs);
  @CPP_CLASS@& pph = *p_@CLASS@_val(ph);
  @UADD_REPRESENT@_System pcs = build_ppl_@UADD_REPRESENT@_System(cs);
  CAMLreturn(Val_bool(pph.add_@ADD_REPRESENT@s_and_minimize(pcs)));
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_@COMPARISON@_@CLASS@_code',
`dnl
extern "C"
CAMLprim value
ppl_@CLASS@_@COMPARISON@_@CLASS@(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  @CPP_CLASS@& pph1 = *p_@CLASS@_val(ph1);
  @CPP_CLASS@& pph2 = *p_@CLASS@_val(ph2);
  CAMLreturn(Val_bool(pph1.@COMPARISON@(pph2)));
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_@BINOP@_code',
`dnl
extern "C"
void
ppl_@CLASS@_@BINOP@(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  @CPP_CLASS@& pph1 = *p_@CLASS@_val(ph1);
  @CPP_CLASS@& pph2 = *p_@CLASS@_val(ph2);
  pph1.@BINOP@(pph2);
  CAMLreturn0;
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_@BINMINOP@_code',
`dnl
extern "C"
CAMLprim value
ppl_@CLASS@_@BINMINOP@(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  @CPP_CLASS@& pph1 = *p_@CLASS@_val(ph1);
  @CPP_CLASS@& pph2 = *p_@CLASS@_val(ph2);
  CAMLreturn(Val_bool(pph1.@BINMINOP@(pph2)));
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@_code',
`dnl
extern "C"
void
ppl_@CLASS@_ppl_@CLASS@_add_space_dimensions_and_embed(value ph,
							     value d) try {
  CAMLparam2(ph, d);
  int dd = Int_val(d);
  if (dd < 0)
    abort();
  @CPP_CLASS@& pph = *p_@CLASS@_val(ph);
  pph.add_space_dimensions_and_embed(dd);
  CAMLreturn0;
							     }
CATCH_ALL

')

m4_define(`ppl_@CLASS@_remove_higher_space_dimensions_code',
`dnl
extern "C"
void
ppl_@CLASS@_ppl_@CLASS@_remove_higher_space_dimensions(value ph,
							     value d) try {
  CAMLparam2(ph, d);
  int dd = Int_val(d);
  if (dd < 0)
    abort();
  @CPP_CLASS@& pph = *p_@CLASS@_val(ph);
  pph.remove_higher_space_dimensions(dd);
  CAMLreturn0;
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_expand_space_dimensions_code',
`dnl
extern "C"
void
ppl_@CLASS@_ppl_@CLASS@_expand_space_dimension(value ph,
						     value var_index,
						     value m) try {
  CAMLparam3(ph, var_index, m);
  int c_var_index = Int_val(var_index);
  if (c_var_index < 0)
    abort();
  int c_m = Int_val(m);
  if (c_m < 0)
    abort();
  @CPP_CLASS@& pph = *p_@CLASS@_val(ph);
  pph.expand_space_dimension(build_ppl_Variable(c_var_index), c_m);
  CAMLreturn0;
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_remove_space_dimensions_code',
`dnl
extern "C"
void
ppl_@CLASS@_remove_space_dimensions(value ph, value caml_vset) try {
  CAMLparam2(ph, caml_vset);
  @CPP_CLASS@& pph = *p_@CLASS@_val(ph);
  pph.remove_space_dimensions(build_ppl_Variables_Set(caml_vset));
  CAMLreturn0;
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_fold_space_dimensions_code',
`dnl
extern "C"
void
ppl_@CLASS@_fold_space_dimensions(value ph, value caml_vset, value caml_dim)
  try {
  CAMLparam1(ph);
  dimension_type ppl_dim = Int_val(caml_dim);
  @CPP_CLASS@& pph = *p_@CLASS@_val(ph);
  Variables_Set ppl_vset;
  if (Int_val(caml_vset) == 0)
    CAMLreturn0;
  while (true) {
    ppl_vset.insert(Int_val(Field(caml_vset, 0)));
    if (Int_val(Field(caml_vset, 1)) == 0)
      break;
    caml_vset = Field(caml_vset, 1);
  }
  pph.fold_space_dimensions(ppl_vset, Variable(ppl_dim));
  CAMLreturn0;
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_map_space_dimensions_code',
`dnl
extern "C"
void
ppl_@CLASS@_map_space_dimensions(value ph, value caml_mapped_dims) try {
  CAMLparam2(ph, caml_mapped_dims);
  @CPP_CLASS@& pph = *p_@CLASS@_val(ph);
  PFunc pfunc;
  while (caml_mapped_dims != Val_int(0)) {
    Int_val(Field(Field(caml_mapped_dims, 0),0));
    pfunc.insert(Int_val(Field(Field(caml_mapped_dims, 0),0)),
		 Int_val(Field(Field(caml_mapped_dims, 0),1)));
    caml_mapped_dims = Field(caml_mapped_dims, 1);
  }
  pph.map_space_dimensions(pfunc);
  CAMLreturn0;
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_get_@GET_REPRESENT@s_code',
`dnl
extern "C"
CAMLprim value
ppl_@CLASS@_get_@GET_REPRESENT@s(value ph) try {
  CAMLparam1(ph);
  @CPP_CLASS@& pph = *p_@CLASS@_val(ph);
  CAMLreturn(build_caml_@GET_REPRESENT@_system(pph.@GET_REPRESENT@s()));
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_get_minimized_@GET_REPRESENT@s_code',
`dnl
extern "C"
CAMLprim value
ppl_@CLASS@_get_minimized_@GET_REPRESENT@s(value ph) try {
  CAMLparam1(ph);
  @CPP_CLASS@& pph = *p_@CLASS@_val(ph);
  CAMLreturn(build_caml_@GET_REPRESENT@_system(pph.minimized_@GET_REPRESENT@s()));
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_bounded_@AFFIMAGE@_code',
`dnl
extern "C"
void
ppl_@CLASS@_bounded_@AFFIMAGE@(value ph, value var, value lb_expr,
				    value ub_expr, value coeff) try {
  CAMLparam5(ph, var, lb_expr, ub_expr, coeff);
  @CPP_CLASS@& pph = *p_@CLASS@_val(ph);
  pph.bounded_@AFFIMAGE@(build_ppl_Variable(Val_int(var)),
			   build_ppl_Linear_Expression(lb_expr),
 			   build_ppl_Linear_Expression(ub_expr),
 			   build_ppl_Coefficient(coeff));
  CAMLreturn0;
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_@AFFIMAGE@_code',
`dnl
extern "C"
void
ppl_@CLASS@_@AFFIMAGE@(value ph, value var, value expr,
			    value coeff) try {
  CAMLparam4(ph, var, expr, coeff);
  @CPP_CLASS@& pph = *p_@CLASS@_val(ph);
  build_ppl_Linear_Expression(expr);
  pph.@AFFIMAGE@(build_ppl_Variable(var),
		   build_ppl_Linear_Expression(expr),
		   build_ppl_Coefficient(coeff));
  CAMLreturn0;
			    }
CATCH_ALL

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_code',
`dnl
extern "C"
void
ppl_@CLASS@_generalized_@AFFIMAGE@1(value ph, value le1, value rel_sym,
					 value le2) try {
  CAMLparam4(ph, le1, rel_sym, le2);
  build_ppl_relsym(rel_sym);
  @CPP_CLASS@& pph = *p_@CLASS@_val(ph);
  pph.generalized_@AFFIMAGE@(build_ppl_Linear_Expression(le1),
			       build_ppl_relsym(rel_sym),
			       build_ppl_Linear_Expression(le2));
  CAMLreturn0;
 }
CATCH_ALL

')

m4_define(`ppl_@CLASS@_generalized_@AFFIMAGE@_code',
`dnl
extern "C"
void
ppl_@CLASS@_generalized_@AFFIMAGE@2(value ph, value int_val,
					 value rel_sym,
					 value le, value caml_coeff) try {
  CAMLparam5(ph, int_val, rel_sym, le, caml_coeff);
  @CPP_CLASS@& pph = *p_@CLASS@_val(ph);
  pph.generalized_@AFFIMAGE@(build_ppl_Variable(int_val),
			       build_ppl_relsym(rel_sym),
			       build_ppl_Linear_Expression(le),
			       build_ppl_Coefficient(caml_coeff));
  CAMLreturn0;
 }
CATCH_ALL

')

m4_define(`ppl_@CLASS@_@WIDEN@_widening_assign_code',
`dnl
extern "C"
CAMLprim value ppl_@CLASS@_@WIDEN@_widening_assign(value ph1, value ph2,
						     value integer) try {
  CAMLparam3(ph1, ph2, integer);
  @CPP_CLASS@& pph1 = *p_@CLASS@_val(ph1);
  @CPP_CLASS@& pph2 = *p_@CLASS@_val(ph2);
  // FIXME: ensure that the input parameter is positive.
  unsigned int cpp_int = Val_int(integer);
  pph1.@WIDEN@_widening_assign(pph2, &cpp_int);
  CAMLreturn(Int_val(cpp_int));
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign_code',
`dnl
extern "C"
CAMLprim value
ppl_@CLASS@_@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign(value ph1,
						   value ph2,
						   value caml_cs,
						   value integer) try {
  CAMLparam4(ph1, ph2, caml_cs, integer);
  @CPP_CLASS@& pph1 = *p_@CLASS@_val(ph1);
  @CPP_CLASS@& pph2 = *p_@CLASS@_val(ph2);
  @UCONSTRAINER@_System ppl_cs = build_ppl_@UCONSTRAINER@_System(caml_cs);
  // FIXME: ensure that the input parameter is positive.
  unsigned int cpp_int = Val_int(integer);
  pph1.@LIMITEDBOUNDED@_@WIDENEXPN@_extrapolation_assign(pph2, ppl_cs, &cpp_int);
  CAMLreturn(Int_val(cpp_int));
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_@MAXMIN@_code',
`dnl
extern "C"
CAMLprim value
ppl_@CLASS@_@MAXMIN@(value ph, value caml_le) try {
  CAMLparam2(ph, caml_le);
  Coefficient num@COMMA@ den = 0;
  bool is_supremum = false;
  @UGENERATOR@ g = @POINT@();
  @CPP_CLASS@& pph = *p_@CLASS@_val(ph);
  bool ppl_return_value = pph.@MAXMIN@(build_ppl_Linear_Expression(caml_le),
				      num, den, is_supremum, g);
  value caml_return_value = caml_alloc(5,0);
  Field(caml_return_value, 0) = Val_bool(ppl_return_value);
  Field(caml_return_value, 1) = build_caml_coefficient(num);
  Field(caml_return_value, 2) = build_caml_coefficient(den);
  Field(caml_return_value, 3) = Val_bool(is_supremum);
  Field(caml_return_value, 4) = build_caml_@GENERATOR@(g);
  CAMLreturn(caml_return_value);
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_OK_code',
`dnl
extern "C"
CAMLprim value
ppl_@CLASS@_OK(value ph) try {
  CAMLparam1(ph);
  @CPP_CLASS@& pph = *p_@CLASS@_val(ph);
  CAMLreturn(Bool_val(pph.OK()));
}
CATCH_ALL

')

m4_define(`ppl_@CLASS@_swap_code',
`dnl
extern "C"
void
ppl_@CLASS@_swap(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  @CPP_CLASS@& pph1 = *p_@CLASS@_val(ph1);
  @CPP_CLASS@& pph2 = *p_@CLASS@_val(ph2);
  pph1.swap(pph2);
  CAMLreturn0;
}
CATCH_ALL

')
