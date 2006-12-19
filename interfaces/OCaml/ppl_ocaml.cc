

#include "ppl_ocaml_globals.cc"
//! Give access to the embedded Polyhedron* in \p v.
inline Polyhedron*&
p_Polyhedron_val(value v) {
  return *reinterpret_cast<Polyhedron**>(Data_custom_val(v));
}

void
custom_Polyhedron_finalize(value v) {
  std::cerr << "About to delete a polyhedron " << *p_Polyhedron_val(v)
	    << std::endl;
  delete p_Polyhedron_val(v);
}

static struct custom_operations Polyhedron_custom_operations = {
  "it.unipr.cs.ppl" "." PPL_VERSION "." "Polyhedron",
  custom_Polyhedron_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

inline value
val_p_Polyhedron(const Polyhedron& ph) {
  value v = caml_alloc_custom(&Polyhedron_custom_operations,
			      sizeof(Polyhedron*), 0, 1);
  p_Polyhedron_val(v) = const_cast<Polyhedron*>(&ph);
  return(v);
}


extern "C"
CAMLprim value
ppl_new_C_Polyhedron_from_space_dimension(value d) try {
  CAMLparam1(d);
  int dd = Int_val(d);
  if (dd < 0)
    abort();
  CAMLreturn(val_p_Polyhedron(*new C_Polyhedron(dd)));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_new_NNC_Polyhedron_from_space_dimension(value d) try {
  CAMLparam1(d);
  int dd = Int_val(d);
  if (dd < 0)
    abort();
  CAMLreturn(val_p_Polyhedron(*new NNC_Polyhedron(dd)));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_new_C_Polyhedron_from_constraints(value cl) try {
  CAMLparam1(cl);
  Constraint_System cs = build_ppl_Constraint_System(cl);
  CAMLreturn(val_p_Polyhedron(*new C_Polyhedron(cs)));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_new_NNC_Polyhedron_from_constraints(value cl) try {
  CAMLparam1(cl);
  Constraint_System cs = build_ppl_Constraint_System(cl);
  CAMLreturn(val_p_Polyhedron(*new NNC_Polyhedron(cs)));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_new_C_Polyhedron_from_generators(value cl) try {
  CAMLparam1(cl);
  Generator_System cs = build_ppl_Generator_System(cl);
  CAMLreturn(val_p_Polyhedron(*new C_Polyhedron(cs)));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_new_NNC_Polyhedron_from_generators(value cl) try {
  CAMLparam1(cl);
  Generator_System cs = build_ppl_Generator_System(cl);
  CAMLreturn(val_p_Polyhedron(*new NNC_Polyhedron(cs)));
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_swap(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  pph1.swap(pph2);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_space_dimension(value ph) try {
  CAMLparam1(ph);
  const Polyhedron& pph = *p_Polyhedron_val(ph);
  dimension_type d = pph.space_dimension();
  if (d > INT_MAX)
    abort();
  CAMLreturn(Val_int(d));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_affine_dimension(value ph) try {
  CAMLparam1(ph);
  const Polyhedron& pph = *p_Polyhedron_val(ph);
  dimension_type d = pph.affine_dimension();
  if (d > INT_MAX)
    abort();
  CAMLreturn(Val_int(d));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_get_constraints(value ph) try {
  CAMLparam1(ph);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(build_caml_constraint_system(pph.constraints()));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_get_generators(value ph) try {
  CAMLparam1(ph);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(build_caml_generator_system(pph.generators()));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_get_congruences(value ph) try {
  CAMLparam1(ph);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(build_caml_congruence_system(pph.congruences()));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_get_minimized_constraints(value ph) try {
  CAMLparam1(ph);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(build_caml_constraint_system(pph.minimized_constraints()));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_get_minimized_generators(value ph) try {
  CAMLparam1(ph);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(build_caml_generator_system(pph.minimized_generators()));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_get_minimized_congruences(value ph) try {
  CAMLparam1(ph);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(build_caml_congruence_system(pph.minimized_congruences()));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_relation_with_constraint(value ph, value c) try {
  CAMLparam2(ph, c);
  const Polyhedron& pph = *p_Polyhedron_val(ph);
  Constraint ppl_c = build_ppl_Constraint(c);
  Poly_Con_Relation r = pph.relation_with(ppl_c);
  CAMLreturn(build_ocaml_poly_con_relation(r));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_relation_with_generator(value ph, value c) try {
  CAMLparam2(ph, c);
  const Polyhedron& pph = *p_Polyhedron_val(ph);
  Generator ppl_c = build_ppl_Generator(c);
  Poly_Gen_Relation r = pph.relation_with(ppl_c);
  CAMLreturn(build_ocaml_poly_gen_relation(r));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_is_empty(value ph) try {
  CAMLparam1(ph);
  const Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(Val_bool(pph.is_empty()));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_is_universe(value ph) try {
  CAMLparam1(ph);
  const Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(Val_bool(pph.is_universe()));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_is_bounded(value ph) try {
  CAMLparam1(ph);
  const Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(Val_bool(pph.is_bounded()));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_contains_integer_point(value ph) try {
  CAMLparam1(ph);
  const Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(Val_bool(pph.contains_integer_point()));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_is_topologically_closed(value ph) try {
  CAMLparam1(ph);
  const Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(Val_bool(pph.is_topologically_closed()));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_is_discrete(value ph) try {
  CAMLparam1(ph);
  const Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(Val_bool(pph.is_discrete()));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_bounds_from_above(value ph, value le) try {
  CAMLparam2(ph, le);
  const Polyhedron& pph = *p_Polyhedron_val(ph);
  Linear_Expression ple = build_ppl_Linear_Expression(le);
  CAMLreturn(Val_bool(pph.bounds_from_above(ple)));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_bounds_from_below(value ph, value le) try {
  CAMLparam2(ph, le);
  const Polyhedron& pph = *p_Polyhedron_val(ph);
  Linear_Expression ple = build_ppl_Linear_Expression(le);
  CAMLreturn(Val_bool(pph.bounds_from_below(ple)));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_maximize(value ph, value caml_le) try {
  CAMLparam2(ph, caml_le);
  Coefficient num, den = 0;
  bool is_supremum = false;
  Generator g = point();
  Polyhedron& pph = *p_Polyhedron_val(ph);
  bool ppl_return_value = pph.maximize(build_ppl_Linear_Expression(caml_le),
				      num, den, is_supremum, g);
  value caml_return_value = caml_alloc(5,0);
  Field(caml_return_value, 0) = Val_bool(ppl_return_value);
  Field(caml_return_value, 1) = build_caml_coefficient(num);
  Field(caml_return_value, 2) = build_caml_coefficient(den);
  Field(caml_return_value, 3) = Val_bool(is_supremum);
  Field(caml_return_value, 4) = build_caml_generator(g);
  CAMLreturn(caml_return_value);
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_minimize(value ph, value caml_le) try {
  CAMLparam2(ph, caml_le);
  Coefficient num, den = 0;
  bool is_supremum = false;
  Generator g = point();
  Polyhedron& pph = *p_Polyhedron_val(ph);
  bool ppl_return_value = pph.minimize(build_ppl_Linear_Expression(caml_le),
				      num, den, is_supremum, g);
  value caml_return_value = caml_alloc(5,0);
  Field(caml_return_value, 0) = Val_bool(ppl_return_value);
  Field(caml_return_value, 1) = build_caml_coefficient(num);
  Field(caml_return_value, 2) = build_caml_coefficient(den);
  Field(caml_return_value, 3) = Val_bool(is_supremum);
  Field(caml_return_value, 4) = build_caml_generator(g);
  CAMLreturn(caml_return_value);
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_contains_Polyhedron(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  CAMLreturn(Val_bool(pph1.contains(pph2)));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_strictly_contains_Polyhedron(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  CAMLreturn(Val_bool(pph1.strictly_contains(pph2)));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_is_disjoint_from_Polyhedron(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  CAMLreturn(Val_bool(pph1.is_disjoint_from(pph2)));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_OK(value ph) try {
  CAMLparam1(ph);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  CAMLreturn(Bool_val(pph.OK()));
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_add_constraint(value ph, value c) try {
  CAMLparam2(ph, c);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  Constraint pc = build_ppl_Constraint(c);
  pph.add_constraint(pc);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_add_generator(value ph, value c) try {
  CAMLparam2(ph, c);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  Generator pc = build_ppl_Generator(c);
  pph.add_generator(pc);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_add_constraint_and_minimize(value ph, value c) try {
  CAMLparam2(ph, c);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  Constraint pc = build_ppl_Constraint(c);
  CAMLreturn(Val_bool(pph.add_constraint_and_minimize(pc)));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_add_generator_and_minimize(value ph, value c) try {
  CAMLparam2(ph, c);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  Generator pc = build_ppl_Generator(c);
  CAMLreturn(Val_bool(pph.add_generator_and_minimize(pc)));
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_add_constraints(value ph, value cs) try {
  CAMLparam2(ph, cs);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  Constraint_System pcs = build_ppl_Constraint_System(cs);
  pph.add_constraints(pcs);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_add_generators(value ph, value cs) try {
  CAMLparam2(ph, cs);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  Generator_System pcs = build_ppl_Generator_System(cs);
  pph.add_generators(pcs);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_add_constraints_and_minimize(value ph, value cs) try {
  CAMLparam2(ph, cs);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  Constraint_System pcs = build_ppl_Constraint_System(cs);
  CAMLreturn(Val_bool(pph.add_constraints_and_minimize(pcs)));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_add_generators_and_minimize(value ph, value cs) try {
  CAMLparam2(ph, cs);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  Generator_System pcs = build_ppl_Generator_System(cs);
  CAMLreturn(Val_bool(pph.add_generators_and_minimize(pcs)));
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_intersection_assign(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  pph1.intersection_assign(pph2);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_upper_bound_assign(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  pph1.upper_bound_assign(pph2);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_difference_assign(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  pph1.difference_assign(pph2);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_concatenate_assign(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  pph1.concatenate_assign(pph2);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_time_elapse_assign(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  pph1.time_elapse_assign(pph2);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_poly_hull_assign(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  pph1.poly_hull_assign(pph2);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_poly_difference_assign(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  pph1.poly_difference_assign(pph2);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_intersection_assign_and_minimize(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  CAMLreturn(Val_bool(pph1.intersection_assign_and_minimize(pph2)));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_poly_hull_assign_and_minimize(value ph1, value ph2) try {
  CAMLparam2(ph1, ph2);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  CAMLreturn(Val_bool(pph1.poly_hull_assign_and_minimize(pph2)));
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_affine_image(value ph, value var, value expr,
			    value coeff) try {
  CAMLparam4(ph, var, expr, coeff);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  build_ppl_Linear_Expression(expr);
  pph.affine_image(build_ppl_Variable(var),
		   build_ppl_Linear_Expression(expr),
		   build_ppl_Coefficient(coeff));
  CAMLreturn0;
			    }
CATCH_ALL

extern "C"
void
ppl_Polyhedron_affine_preimage(value ph, value var, value expr,
			    value coeff) try {
  CAMLparam4(ph, var, expr, coeff);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  build_ppl_Linear_Expression(expr);
  pph.affine_preimage(build_ppl_Variable(var),
		   build_ppl_Linear_Expression(expr),
		   build_ppl_Coefficient(coeff));
  CAMLreturn0;
			    }
CATCH_ALL

extern "C"
void
ppl_Polyhedron_bounded_affine_image(value ph, value var, value lb_expr,
				    value ub_expr, value coeff) try {
  CAMLparam5(ph, var, lb_expr, ub_expr, coeff);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  pph.bounded_affine_image(build_ppl_Variable(Val_int(var)),
			   build_ppl_Linear_Expression(lb_expr),
 			   build_ppl_Linear_Expression(ub_expr),
 			   build_ppl_Coefficient(coeff));
  CAMLreturn0;
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_bounded_affine_preimage(value ph, value var, value lb_expr,
				    value ub_expr, value coeff) try {
  CAMLparam5(ph, var, lb_expr, ub_expr, coeff);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  pph.bounded_affine_preimage(build_ppl_Variable(Val_int(var)),
			   build_ppl_Linear_Expression(lb_expr),
 			   build_ppl_Linear_Expression(ub_expr),
 			   build_ppl_Coefficient(coeff));
  CAMLreturn0;
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_generalized_affine_image2(value ph, value int_val,
					 value rel_sym,
					 value le, value caml_coeff) try {
  CAMLparam5(ph, int_val, rel_sym, le, caml_coeff);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  pph.generalized_affine_image(build_ppl_Variable(int_val),
			       build_ppl_relsym(rel_sym),
			       build_ppl_Linear_Expression(le),
			       build_ppl_Coefficient(caml_coeff));
  CAMLreturn0;
 }
CATCH_ALL

extern "C"
void
ppl_Polyhedron_generalized_affine_preimage2(value ph, value int_val,
					 value rel_sym,
					 value le, value caml_coeff) try {
  CAMLparam5(ph, int_val, rel_sym, le, caml_coeff);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  pph.generalized_affine_preimage(build_ppl_Variable(int_val),
			       build_ppl_relsym(rel_sym),
			       build_ppl_Linear_Expression(le),
			       build_ppl_Coefficient(caml_coeff));
  CAMLreturn0;
 }
CATCH_ALL

extern "C"
void
ppl_Polyhedron_generalized_affine_image1(value ph, value le1, value rel_sym,
					 value le2) try {
  CAMLparam4(ph, le1, rel_sym, le2);
  build_ppl_relsym(rel_sym);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  pph.generalized_affine_image(build_ppl_Linear_Expression(le1),
			       build_ppl_relsym(rel_sym),
			       build_ppl_Linear_Expression(le2));
  CAMLreturn0;
 }
CATCH_ALL

extern "C"
void
ppl_Polyhedron_generalized_affine_preimage1(value ph, value le1, value rel_sym,
					 value le2) try {
  CAMLparam4(ph, le1, rel_sym, le2);
  build_ppl_relsym(rel_sym);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  pph.generalized_affine_preimage(build_ppl_Linear_Expression(le1),
			       build_ppl_relsym(rel_sym),
			       build_ppl_Linear_Expression(le2));
  CAMLreturn0;
 }
CATCH_ALL

extern "C"
CAMLprim value ppl_Polyhedron_BHRZ03_widening_assign(value ph1, value ph2,
						     value integer) try {
  CAMLparam3(ph1, ph2, integer);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  // FIXME: ensure that the input parameter is positive.
  unsigned int cpp_int = Val_int(integer);
  pph1.BHRZ03_widening_assign(pph2, &cpp_int);
  CAMLreturn(Int_val(cpp_int));
}
CATCH_ALL

extern "C"
CAMLprim value ppl_Polyhedron_H79_widening_assign(value ph1, value ph2,
						     value integer) try {
  CAMLparam3(ph1, ph2, integer);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  // FIXME: ensure that the input parameter is positive.
  unsigned int cpp_int = Val_int(integer);
  pph1.H79_widening_assign(pph2, &cpp_int);
  CAMLreturn(Int_val(cpp_int));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_limited_BHRZ03_extrapolation_assign(value ph1,
						   value ph2,
						   value caml_cs,
						   value integer) try {
  CAMLparam4(ph1, ph2, caml_cs, integer);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  Constraint_System ppl_cs = build_ppl_Constraint_System(caml_cs);
  // FIXME: ensure that the input parameter is positive.
  unsigned int cpp_int = Val_int(integer);
  pph1.limited_BHRZ03_extrapolation_assign(pph2, ppl_cs, &cpp_int);
  CAMLreturn(Int_val(cpp_int));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_limited_H79_extrapolation_assign(value ph1,
						   value ph2,
						   value caml_cs,
						   value integer) try {
  CAMLparam4(ph1, ph2, caml_cs, integer);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  Constraint_System ppl_cs = build_ppl_Constraint_System(caml_cs);
  // FIXME: ensure that the input parameter is positive.
  unsigned int cpp_int = Val_int(integer);
  pph1.limited_H79_extrapolation_assign(pph2, ppl_cs, &cpp_int);
  CAMLreturn(Int_val(cpp_int));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign(value ph1,
						   value ph2,
						   value caml_cs,
						   value integer) try {
  CAMLparam4(ph1, ph2, caml_cs, integer);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  Constraint_System ppl_cs = build_ppl_Constraint_System(caml_cs);
  // FIXME: ensure that the input parameter is positive.
  unsigned int cpp_int = Val_int(integer);
  pph1.bounded_BHRZ03_extrapolation_assign(pph2, ppl_cs, &cpp_int);
  CAMLreturn(Int_val(cpp_int));
}
CATCH_ALL

extern "C"
CAMLprim value
ppl_Polyhedron_bounded_H79_extrapolation_assign(value ph1,
						   value ph2,
						   value caml_cs,
						   value integer) try {
  CAMLparam4(ph1, ph2, caml_cs, integer);
  Polyhedron& pph1 = *p_Polyhedron_val(ph1);
  Polyhedron& pph2 = *p_Polyhedron_val(ph2);
  Constraint_System ppl_cs = build_ppl_Constraint_System(caml_cs);
  // FIXME: ensure that the input parameter is positive.
  unsigned int cpp_int = Val_int(integer);
  pph1.bounded_H79_extrapolation_assign(pph2, ppl_cs, &cpp_int);
  CAMLreturn(Int_val(cpp_int));
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_ppl_Polyhedron_add_space_dimensions_and_embed(value ph,
							     value d) try {
  CAMLparam2(ph, d);
  int dd = Int_val(d);
  if (dd < 0)
    abort();
  Polyhedron& pph = *p_Polyhedron_val(ph);
  pph.add_space_dimensions_and_embed(dd);
  CAMLreturn0;
							     }
CATCH_ALL

extern "C"
void
ppl_Polyhedron_remove_space_dimensions(value ph, value caml_vset) try {
  CAMLparam2(ph, caml_vset);
  Polyhedron& pph = *p_Polyhedron_val(ph);
  pph.remove_space_dimensions(build_ppl_Variables_Set(caml_vset));
  CAMLreturn0;
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_ppl_Polyhedron_remove_higher_space_dimensions(value ph,
							     value d) try {
  CAMLparam2(ph, d);
  int dd = Int_val(d);
  if (dd < 0)
    abort();
  Polyhedron& pph = *p_Polyhedron_val(ph);
  pph.remove_higher_space_dimensions(dd);
  CAMLreturn0;
}
CATCH_ALL

extern "C"
void
ppl_Polyhedron_fold_space_dimensions(value ph, value caml_vset, value caml_dim)
  try {
  CAMLparam1(ph);
  dimension_type ppl_dim = Int_val(caml_dim);
  Polyhedron& pph = *p_Polyhedron_val(ph);
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

extern "C"
void
ppl_Polyhedron_map_space_dimensions(value ph, value caml_mapped_dims) try {
  CAMLparam2(ph, caml_mapped_dims);
  Polyhedron& pph = *p_Polyhedron_val(ph);
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



