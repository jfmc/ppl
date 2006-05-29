divert(-1)dnl

define(`ppl_new_TOPOLOGY_CLASS_from_space_dimension_code',
`extern "C" Prolog_foreign_return_type
ppl_new_TOPOLOGY_CLASS_from_space_dimension(Prolog_term_ref t_nd,
					    Prolog_term_ref t_uoe,
					    Prolog_term_ref t_ph) {
  try {
    CLASS* ph;
    Prolog_atom uoe = term_to_universe_or_empty(t_uoe);

    if (uoe == a_empty)
        ph = new TOPOLOGY_CLASS(term_to_unsigned<dimension_type>(t_nd),
			      EMPTY);
      else
        ph = new TOPOLOGY_CLASS(term_to_unsigned<dimension_type>(t_nd));


    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}
')

define(`ppl_new_TOPOLOGY_CLASS_from_INTOPOLOGY_CLASS_code',
`extern "C" Prolog_foreign_return_type
ppl_new_TOPOLOGY_CLASS_from_INTOPOLOGY_CLASS(Prolog_term_ref t_ph_source,
				       Prolog_term_ref t_ph) {
  try {
    CLASS* ph;
    const INTOPOLOGY_CLASS* ph_source
	= static_cast<const INTOPOLOGY_CLASS*>
	(term_to_cLASS_handle(t_ph_source));
    CHECK(ph_source);
        ph = new TOPOLOGY_CLASS(*ph_source);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}
')

define(`ppl_new_TOPOLOGY_CLASS_from_REPRESENTs_code',
`extern "C" Prolog_foreign_return_type
ppl_new_TOPOLOGY_CLASS_from_REPRESENTs(Prolog_term_ref t_clist,
				      Prolog_term_ref t_ph) {
  try {
    UALT_REPRESENT_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_ALT_REPRESENT(c));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist);

    CLASS* ph;
    ph = new TOPOLOGY_CLASS(cs);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}
')

define(`ppl_new_TOPOLOGY_CLASS_from_BOX_code',
`extern "C" Prolog_foreign_return_type
ppl_new_TOPOLOGY_CLASS_from_BOX(Prolog_term_ref t_bb,
				       Prolog_term_ref t_ph) {
  try {
    // Compute the space dimension.
    Prolog_term_ref t_l = Prolog_new_term_ref();
    Prolog_term_ref t_interval = Prolog_new_term_ref();
    Prolog_put_term(t_l, t_bb);
    dimension_type dimension;
    for (dimension = 0; Prolog_is_cons(t_l); ++dimension)
      Prolog_get_cons(t_l, t_interval, t_l);

    // Check the list is properly terminated.
    check_nil_terminating(t_l);

    Bounding_Box bbox(dimension);
    // Set bbox to reflect its Prolog representation.
    for (dimension_type i = 0; i < dimension; ++i) {
      Prolog_get_cons(t_bb, t_interval, t_bb);
      // An interval is either the atom empty or of the form
      // i(Lower_Bound, Upper_Bound).
      if (Prolog_is_atom(t_interval)) {
	Prolog_atom name;
	if (Prolog_get_atom_name(t_interval, &name) && name == a_empty) {
	  bbox.set_empty();
	  continue;
	}
	else
	  return PROLOG_FAILURE;
      }

      if (!Prolog_is_compound(t_interval))
	return PROLOG_FAILURE;

      Prolog_atom functor;
      int arity;
      Prolog_get_compound_name_arity(t_interval, &functor, &arity);
      if (arity != 2 || functor != a_i)
	return PROLOG_FAILURE;

      bool finite;
      bool closed;
      Coefficient n;
      Coefficient d;
      Prolog_term_ref t_bound = Prolog_new_term_ref();

      // Get and raise the lower bound.
      Prolog_get_arg(1, t_interval, t_bound);
      if (!term_to_boundary(t_bound, LOWER, finite, closed, n, d))
	return PROLOG_FAILURE;
      if (finite)
	bbox.raise_lower_bound(i, closed, n, d);

      // Get and lower the upper bound.
      Prolog_get_arg(2, t_interval, t_bound);
      if (!term_to_boundary(t_bound, UPPER, finite, closed, n, d))
	return PROLOG_FAILURE;
      if (finite)
	bbox.lower_upper_bound(i, closed, n, d);
    }

    CLASS* ph;
    ph = new TOPOLOGY_CLASS(bbox, From_`'UBOX());
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, ph);
    if (Prolog_unify(t_ph, tmp)) {
      REGISTER(ph);
      return PROLOG_SUCCESS;
    }
    else
      delete ph;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_swap_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_swap(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  try {
    CLASS* lhs = term_to_cLASS_handle(t_lhs);
    CLASS* rhs = term_to_cLASS_handle(t_rhs);
    CHECK(lhs);
    CHECK(rhs);
    lhs->swap(*rhs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_delete_CLASS_code',
`extern "C" Prolog_foreign_return_type
ppl_delete_CLASS(Prolog_term_ref t_ph) {
  try {
    const CLASS* ph = term_to_cLASS_handle(t_ph);
    UNREGISTER(ph);
    delete ph;
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_DIM_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_DIM(Prolog_term_ref t_ph, Prolog_term_ref t_sd) {
  try {
    const CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);
    if (unify_ulong(t_sd, ph->DIM()))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_get_DESCRIBEs_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_get_DESCRIBEs(Prolog_term_ref t_ph,
			      Prolog_term_ref t_glist) {
  try {
    const CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    const UALT_DESCRIBE_System& gs = ph->DESCRIBEs();
    for (UALT_DESCRIBE_System::const_iterator i = gs.begin(),
	   gs_end = gs.end(); i != gs_end; ++i)
      Prolog_construct_cons(tail, ALT_DESCRIBE_term(*i), tail);

    if (Prolog_unify(t_glist, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_get_minimized_DESCRIBEs_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_get_minimized_DESCRIBEs(Prolog_term_ref t_ph,
					Prolog_term_ref t_glist) {
  try {
    const CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    const UALT_DESCRIBE_System& gs = ph->minimized_DESCRIBEs();
    for (UALT_DESCRIBE_System::const_iterator i = gs.begin(),
	   gs_end = gs.end(); i != gs_end; ++i)
      Prolog_construct_cons(tail, ALT_DESCRIBE_term(*i), tail);

    if (Prolog_unify(t_glist, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_relation_with_DESCRIBE_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_relation_with_DESCRIBE(Prolog_term_ref t_ph,
					Prolog_term_ref t_c,
					Prolog_term_ref t_r) {
  try {
    CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);
relation_with_ALT_DESCRIBE_code`'
    if (Prolog_unify(t_r, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`relation_with_constraint_code', `
    Poly_Con_Relation r = ph->relation_with(build_constraint(t_c));

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    while (r != Poly_Con_Relation::nothing()) {
      if (r.implies(Poly_Con_Relation::is_disjoint())) {
        Prolog_term_ref t_dis = Prolog_new_term_ref();
	Prolog_put_atom(t_dis, a_is_disjoint);
        Prolog_construct_cons(tail, t_dis, tail);
	r = r - Poly_Con_Relation::is_disjoint();
      }
      else if (r.implies(Poly_Con_Relation::strictly_intersects())) {
        Prolog_term_ref t_sin = Prolog_new_term_ref();
	Prolog_put_atom(t_sin, a_strictly_intersects);
        Prolog_construct_cons(tail, t_sin, tail);
	r = r - Poly_Con_Relation::strictly_intersects();
      }
      else if (r.implies(Poly_Con_Relation::is_included())) {
        Prolog_term_ref t_inc = Prolog_new_term_ref();
	Prolog_put_atom(t_inc, a_is_included);
        Prolog_construct_cons(tail, t_inc, tail);
	r = r - Poly_Con_Relation::is_included();
      }
      else if (r.implies(Poly_Con_Relation::saturates())) {
        Prolog_term_ref t_sat = Prolog_new_term_ref();
	Prolog_put_atom(t_sat, a_saturates);
        Prolog_construct_cons(tail, t_sat, tail);
	r = r - Poly_Con_Relation::saturates();
      }
    }
')

define(`relation_with_congruence_code', `
    Poly_Con_Relation r = ph->relation_with(build_congruence(t_c));

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    while (r != Poly_Con_Relation::nothing()) {
      if (r.implies(Poly_Con_Relation::is_disjoint())) {
        Prolog_term_ref t_dis = Prolog_new_term_ref();
	Prolog_put_atom(t_dis, a_is_disjoint);
        Prolog_construct_cons(tail, t_dis, tail);
	r = r - Poly_Con_Relation::is_disjoint();
      }
      else if (r.implies(Poly_Con_Relation::strictly_intersects())) {
        Prolog_term_ref t_sin = Prolog_new_term_ref();
	Prolog_put_atom(t_sin, a_strictly_intersects);
        Prolog_construct_cons(tail, t_sin, tail);
	r = r - Poly_Con_Relation::strictly_intersects();
      }
      else if (r.implies(Poly_Con_Relation::is_included())) {
        Prolog_term_ref t_inc = Prolog_new_term_ref();
	Prolog_put_atom(t_inc, a_is_included);
        Prolog_construct_cons(tail, t_inc, tail);
	r = r - Poly_Con_Relation::is_included();
      }
    }
')

define(`relation_with_generator_code', `
    Poly_Gen_Relation r = ph->relation_with(build_generator(t_c));

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    while (r != Poly_Gen_Relation::nothing()) {
      if (r.implies(Poly_Gen_Relation::subsumes())) {
        Prolog_term_ref t_sub = Prolog_new_term_ref();
	Prolog_put_atom(t_sub, a_subsumes);
        Prolog_construct_cons(tail, t_sub, tail);
	r = r - Poly_Gen_Relation::subsumes();
      }
    }
')

define(`relation_with_grid_generator_code', `
    Poly_Gen_Relation r = ph->relation_with(build_grid_generator(t_c));

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    while (r != Poly_Gen_Relation::nothing()) {
      if (r.implies(Poly_Gen_Relation::subsumes())) {
        Prolog_term_ref t_sub = Prolog_new_term_ref();
	Prolog_put_atom(t_sub, a_subsumes);
        Prolog_construct_cons(tail, t_sub, tail);
	r = r - Poly_Gen_Relation::subsumes();
      }
    }
')

define(`ppl_Grid_get_BOX_code',
`extern "C" Prolog_foreign_return_type
ppl_Grid_get_BOX(Prolog_term_ref t_ph,
				Prolog_term_ref t_bb) {
  try {
    CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);

    dimension_type dimension = ph->space_dimension();
    Bounding_Box bbox(dimension);
    ph->ALT_BOX(bbox);
    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    for (dimension_type i = dimension; i-- > 0; )
      Prolog_construct_cons(tail, interval_term(bbox[i]), tail);
    if (Prolog_unify(t_bb, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_get_BOX_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_get_BOX(Prolog_term_ref t_ph,
				Prolog_term_ref t_cc,
				Prolog_term_ref t_bb) {
  try {
    CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);

    Prolog_atom p_cc = term_to_complexity_class(t_cc);
    Complexity_Class cc;
    if (p_cc == a_polynomial)
      cc = POLYNOMIAL_COMPLEXITY;
    else if (p_cc == a_simplex)
      cc = SIMPLEX_COMPLEXITY;
    else
      cc = ANY_COMPLEXITY;

    dimension_type dimension = ph->space_dimension();
    Bounding_Box bbox(dimension);
    ph->shrink_BOX(bbox, cc);
    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    for (dimension_type i = dimension; i-- > 0; )
      Prolog_construct_cons(tail, interval_term(bbox[i]), tail);
    if (Prolog_unify(t_bb, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_is_STATE_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_is_STATE(Prolog_term_ref t_ph) {
  try {
    const CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);
    if (ph->is_STATE())
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_topological_closure_assign_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_topological_closure_assign(Prolog_term_ref t_ph) {
  try {
    CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);
    ph->topological_closure_assign();
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_bounds_from_ABOVEBELOW_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_bounds_from_ABOVEBELOW(Prolog_term_ref t_ph,
				 Prolog_term_ref t_expr) {
  try {
    CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);
    Linear_Expression l = build_linear_expression(t_expr);
    if (ph->bounds_from_ABOVEBELOW(l))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_MAXMIN_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_MAXMIN(Prolog_term_ref t_ph,
			Prolog_term_ref t_le_expr,
                        Prolog_term_ref t_n,
                        Prolog_term_ref t_d,
                        Prolog_term_ref t_maxmin) {
 try {
    const CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);
    const Linear_Expression le = build_linear_expression(t_le_expr);
    Coefficient n;
    Coefficient d;
    bool maxmin;
    if (ph->MAXMIN(le, n, d, maxmin)) {
      Prolog_term_ref t = Prolog_new_term_ref();
      Prolog_atom a = (maxmin ? a_true : a_false);
      Prolog_put_atom(t, a);
      if (Prolog_unify(t_n, Coefficient_to_integer_term(n))
	  && Prolog_unify(t_d, Coefficient_to_integer_term(d))
	  && Prolog_unify(t_maxmin, t))
	return PROLOG_SUCCESS;
    }
 }
 CATCH_ALL;
}
')

define(`ppl_CLASS_MAXMIN_with_point_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_MAXMIN_with_point(Prolog_term_ref t_ph,
				   Prolog_term_ref t_le_expr,
				   Prolog_term_ref t_n,
				   Prolog_term_ref t_d,
				   Prolog_term_ref t_maxmin,
				   Prolog_term_ref t_g) {
 try {
    const CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);
    const Linear_Expression le = build_linear_expression(t_le_expr);
    Coefficient n;
    Coefficient d;
    bool maxmin;
    UALT_GNRT g(ALT_POINT());
    if (ph->MAXMIN(le, n, d, maxmin, g)) {
      Prolog_term_ref t = Prolog_new_term_ref();
      Prolog_atom a = (maxmin ? a_true : a_false);
      Prolog_put_atom(t, a);
      if (Prolog_unify(t_n, Coefficient_to_integer_term(n))
	  && Prolog_unify(t_d, Coefficient_to_integer_term(d))
	  && Prolog_unify(t_maxmin, t)
	  && Prolog_unify(t_g, ALT_GNRT_term(g)))
	return PROLOG_SUCCESS;
    }
 }
 CATCH_ALL;
}
')

define(`ppl_CLASS_COMPARISON_CLASS_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_COMPARISON_CLASS(Prolog_term_ref t_lhs,
				   Prolog_term_ref t_rhs) {
  try {
    const CLASS* lhs = term_to_cLASS_handle(t_lhs);
    const CLASS* rhs = term_to_cLASS_handle(t_rhs);
    CHECK(lhs);
    CHECK(rhs);
    if (lhs->COMPARISON(*rhs))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_equals_CLASS_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_equals_CLASS(Prolog_term_ref t_lhs,
				 Prolog_term_ref t_rhs) {
  try {
    const CLASS* lhs = term_to_cLASS_handle(t_lhs);
    const CLASS* rhs = term_to_cLASS_handle(t_rhs);
    CHECK(lhs);
    CHECK(rhs);
    if (*lhs == *rhs)
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_OK_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_OK(Prolog_term_ref t_ph) {
  try {
    const CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);
    if (ph->OK())
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_add_REPRESENT_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_add_REPRESENT(Prolog_term_ref t_ph, Prolog_term_ref t_c) {
  try {
    CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);
    ph->add_REPRESENT(build_ALT_REPRESENT(t_c));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_add_REPRESENT_and_minimize_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_add_REPRESENT_and_minimize(Prolog_term_ref t_ph,
					   Prolog_term_ref t_c) {
  try {
    CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);
    if (ph->add_REPRESENT_and_minimize(build_ALT_REPRESENT(t_c)))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_add_REPRESENTs_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_add_REPRESENTs(Prolog_term_ref t_ph,
			       Prolog_term_ref t_clist) {
  try {
    CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);
    UALT_REPRESENT_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_ALT_REPRESENT(c));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist);

    ph->add_REPRESENTs(cs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_add_REPRESENTs_and_minimize_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_add_REPRESENTs_and_minimize(Prolog_term_ref t_ph,
					    Prolog_term_ref t_clist) {
  try {
    CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);
    UALT_REPRESENT_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_ALT_REPRESENT(c));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist);

    if (ph->add_REPRESENTs_and_minimize(cs))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`bop_assign_code',
`namespace {

Prolog_foreign_return_type
bop_assign(Prolog_term_ref t_lhs,
	   Prolog_term_ref t_rhs,
	   void (CLASS::* bop_assign)(const CLASS&)) {
  try {
    CLASS* lhs = term_to_cLASS_handle(t_lhs);
    const CLASS* rhs = term_to_cLASS_handle(t_rhs);
    CHECK(lhs);
    CHECK(rhs);
    (lhs->*bop_assign)(*rhs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

Prolog_foreign_return_type
bop_assign_and_minimize(Prolog_term_ref t_lhs,
			Prolog_term_ref t_rhs,
			bool (CLASS::*
			      bop_assign_and_minimize)(const CLASS&)) {
  try {
    CLASS* lhs = term_to_cLASS_handle(t_lhs);
    const CLASS* rhs = term_to_cLASS_handle(t_rhs);
    CHECK(lhs);
    CHECK(rhs);
    if ((lhs->*bop_assign_and_minimize)(*rhs))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

} // namespace

')

define(`ppl_CLASS_BINOP_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_BINOP(Prolog_term_ref t_lhs,
						Prolog_term_ref t_rhs) {
  return bop_assign(t_lhs, t_rhs,
		    &CLASS::BINOP);
}
')

define(`ppl_CLASS_BPMIN_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_BPMIN(Prolog_term_ref t_lhs,
						Prolog_term_ref t_rhs) {
  return bop_assign_and_minimize(t_lhs, t_rhs,
		    &CLASS::BPMIN);
}
')

define(`ppl_CLASS_AFFIM_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_AFFIM(Prolog_term_ref t_ph, Prolog_term_ref t_v,
			    Prolog_term_ref t_le, Prolog_term_ref t_d) {
  try {
    CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);
    ph->AFFIM(term_to_Variable(t_v),
		     build_linear_expression(t_le),
		     term_to_Coefficient(t_d));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_Grid_generalized_AFFIM_code',
`extern "C" Prolog_foreign_return_type
ppl_Grid_generalized_AFFIM(Prolog_term_ref t_ph,
					Prolog_term_ref t_v,
					Prolog_term_ref t_le,
					Prolog_term_ref t_d,
					Prolog_term_ref t_m) {
  try {
    Grid* ph = term_to_grid_handle(t_ph);
    CHECK(ph);
    ph->generalized_AFFIM(term_to_Variable(t_v),
				 build_linear_expression(t_le),
				 term_to_Coefficient(t_d),
				 term_to_Coefficient(t_m));
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_generalized_AFFIM_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_generalized_AFFIM(Prolog_term_ref t_ph,
					Prolog_term_ref t_v,
					Prolog_term_ref t_r,
					Prolog_term_ref t_le,
					Prolog_term_ref t_d) {
  try {
    CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);
    Relation_Symbol r = term_to_relation_symbol(t_r);
    ph->generalized_AFFIM(term_to_Variable(t_v),
				 r,
				 build_linear_expression(t_le),
				 term_to_Coefficient(t_d));
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_Grid_generalized_AFFIM_lhs_rhs_code',
`extern "C" Prolog_foreign_return_type
ppl_Grid_generalized_AFFIM_lhs_rhs(Prolog_term_ref t_ph,
						Prolog_term_ref t_lhs,
						Prolog_term_ref t_rhs,
						Prolog_term_ref t_m) {
  try {
    Grid* ph = term_to_grid_handle(t_ph);
    CHECK(ph);
    ph->generalized_AFFIM(build_linear_expression(t_lhs),
				 build_linear_expression(t_rhs),
				 term_to_Coefficient(t_m));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_generalized_AFFIM_lhs_rhs_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_generalized_AFFIM_lhs_rhs(Prolog_term_ref t_ph,
						Prolog_term_ref t_lhs,
						Prolog_term_ref t_r,
						Prolog_term_ref t_rhs) {
  try {
    CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);
    Relation_Symbol r = term_to_relation_symbol(t_r);
    ph->generalized_AFFIM(build_linear_expression(t_lhs),
				 r,
				 build_linear_expression(t_rhs));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_bounded_AFFIM_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_bounded_AFFIM(Prolog_term_ref t_ph,
				    Prolog_term_ref t_v,
				    Prolog_term_ref t_lb_le,
				    Prolog_term_ref t_ub_le,
				    Prolog_term_ref t_d) {
  try {
    CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);
    ph->bounded_AFFIM(term_to_Variable(t_v),
			     build_linear_expression(t_lb_le),
			     build_linear_expression(t_ub_le),
			     term_to_Coefficient(t_d));
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`widening_extrapolation_code',
`namespace {

Prolog_foreign_return_type
widening_assign(Prolog_term_ref t_lhs,
		Prolog_term_ref t_rhs,
		void (CLASS::* widening_assign)(const CLASS&,
						     unsigned* tp)) {
  try {
    CLASS* lhs = term_to_cLASS_handle(t_lhs);
    const CLASS* rhs = term_to_cLASS_handle(t_rhs);
    CHECK(lhs);
    CHECK(rhs);
    (lhs->*widening_assign)(*rhs, 0);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

Prolog_foreign_return_type
widening_assign_with_tokens(Prolog_term_ref t_lhs,
			    Prolog_term_ref t_rhs,
			    Prolog_term_ref t_ti,
			    Prolog_term_ref t_to,
			    void (CLASS::*
				  widening_assign)(const CLASS&,
						   unsigned* tp)) {
  try {
    CLASS* lhs = term_to_cLASS_handle(t_lhs);
    const CLASS* rhs = term_to_cLASS_handle(t_rhs);
    CHECK(lhs);
    CHECK(rhs);
    unsigned t = term_to_unsigned<unsigned>(t_ti);
    (lhs->*widening_assign)(*rhs, &t);
      if (unify_long(t_to, t))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

Prolog_foreign_return_type
limited_extrapolation_assign(Prolog_term_ref t_lhs,
			 Prolog_term_ref t_rhs,
			 Prolog_term_ref t_clist,
			 void (CLASS::*
			       limited_extrap_assign)(const CLASS&,
						      const UALT_RSTRCT_System&,
						      unsigned* tp)) {
  try {
    CLASS* lhs = term_to_cLASS_handle(t_lhs);
    const CLASS* rhs = term_to_cLASS_handle(t_rhs);
    CHECK(lhs);
    CHECK(rhs);
    UALT_RSTRCT_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
     cs.insert(build_RSTRCT(c));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist);

    (lhs->*limited_extrap_assign)(*rhs, cs, 0);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

Prolog_foreign_return_type
limited_extrapolation_assign_with_tokens(Prolog_term_ref t_lhs,
			 Prolog_term_ref t_rhs,
			 Prolog_term_ref t_clist,
	  		 Prolog_term_ref t_ti,
	  		 Prolog_term_ref t_to,
			 void (CLASS::*
			       limited_extrap_assign)(const CLASS&,
						      const UALT_RSTRCT_System&,
						      unsigned* tp)) {
  try {
    CLASS* lhs = term_to_cLASS_handle(t_lhs);
    const CLASS* rhs = term_to_cLASS_handle(t_rhs);
    CHECK(lhs);
    CHECK(rhs);
    UALT_RSTRCT_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_RSTRCT(c));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist);

    unsigned t = term_to_unsigned<unsigned>(t_ti);
   (lhs->*limited_extrap_assign)(*rhs, cs, &t);
      if (unify_long(t_to, t))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

} // namespace
')

define(`ppl_CLASS_WIDENEXP_widening_assign_with_tokens_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_WIDENEXP_widening_assign_with_tokens(Prolog_term_ref t_lhs,
					       Prolog_term_ref t_rhs,
					       Prolog_term_ref t_ti,
					       Prolog_term_ref t_to) {
  return widening_assign_with_tokens(t_lhs, t_rhs, t_ti, t_to,
				    &CLASS::WIDENEXP_widening_assign);
}
')

define(`ppl_CLASS_WIDENEXP_widening_assign_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_WIDENEXP_widening_assign(Prolog_term_ref t_lhs,
				   Prolog_term_ref t_rhs) {
  return widening_assign(t_lhs, t_rhs, &CLASS::WIDENEXP_widening_assign);
}
')

define(`ppl_CLASS_limited_WIDENEXP_extrapolation_assign_with_tokens_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_limited_WIDENEXP_extrapolation_assign_with_tokens(
                                                Prolog_term_ref t_lhs,
						Prolog_term_ref t_rhs,
						Prolog_term_ref t_clist,
				                Prolog_term_ref t_ti,
				                Prolog_term_ref t_to) {
  return limited_extrapolation_assign_with_tokens(t_lhs,
				      t_rhs,
				      t_clist,
				      t_ti,
				      t_to,
				      &CLASS::
				      limited_WIDENEXP_extrapolation_assign);
}
')

define(`ppl_CLASS_limited_WIDENEXP_extrapolation_assign_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_limited_WIDENEXP_extrapolation_assign(Prolog_term_ref t_lhs,
						Prolog_term_ref t_rhs,
						Prolog_term_ref t_clist) {
  return limited_extrapolation_assign(t_lhs,
				      t_rhs,
				      t_clist,
				      &CLASS::
				      limited_WIDENEXP_extrapolation_assign);
}
')

define(`ppl_CLASS_bounded_WIDENEXP_extrapolation_assign_with_tokens_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_bounded_WIDENEXP_extrapolation_assign_with_tokens(
                                                   Prolog_term_ref t_lhs,
						   Prolog_term_ref t_rhs,
						   Prolog_term_ref t_clist,
				                   Prolog_term_ref t_ti,
				                   Prolog_term_ref t_to) {
  return limited_extrapolation_assign_with_tokens(t_lhs,
				      t_rhs,
				      t_clist,
				      t_ti,
				      t_to,
				      &CLASS::
				      bounded_WIDENEXP_extrapolation_assign);
}
')

define(`ppl_CLASS_bounded_WIDENEXP_extrapolation_assign_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_bounded_WIDENEXP_extrapolation_assign(Prolog_term_ref t_lhs,
						   Prolog_term_ref t_rhs,
						   Prolog_term_ref t_clist) {
  return limited_extrapolation_assign(t_lhs,
				      t_rhs,
				      t_clist,
				      &CLASS::
				      bounded_WIDENEXP_extrapolation_assign);
}
')

define(`ppl_CLASS_add_space_dimensions_and_project_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_add_space_dimensions_and_project(Prolog_term_ref t_ph,
						Prolog_term_ref t_nnd) {
  try {
    CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);
    dimension_type d = term_to_unsigned<dimension_type>(t_nnd);
      ph->add_space_dimensions_and_project(d);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_add_space_dimensions_and_embed_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_add_space_dimensions_and_embed(Prolog_term_ref t_ph,
					      Prolog_term_ref t_nnd) {
  try {
    CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);
    dimension_type d = term_to_unsigned<dimension_type>(t_nnd);
    ph->add_space_dimensions_and_embed(d);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_concatenate_assign_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_concatenate_assign(Prolog_term_ref t_lhs,
				  Prolog_term_ref t_rhs) {
  try {
    CLASS* lhs = term_to_cLASS_handle(t_lhs);
    const CLASS* rhs = term_to_cLASS_handle(t_rhs);
    CHECK(lhs);
    CHECK(rhs);
    lhs->concatenate_assign(*rhs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_remove_space_dimensions_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_remove_space_dimensions(Prolog_term_ref t_ph,
				       Prolog_term_ref t_vlist) {
  try {
    CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);
    Variables_Set dead_variables;
    Prolog_term_ref v = Prolog_new_term_ref();
    while (Prolog_is_cons(t_vlist)) {
      Prolog_get_cons(t_vlist, v, t_vlist);
      dead_variables.insert(term_to_Variable(v));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_vlist);

    ph->remove_space_dimensions(dead_variables);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_remove_higher_space_dimensions_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_remove_higher_space_dimensions(Prolog_term_ref t_ph,
					      Prolog_term_ref t_nd) {
  try {
    CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);
    ph->remove_higher_space_dimensions(term_to_unsigned<dimension_type>(t_nd));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_expand_space_dimension_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_expand_space_dimension(Prolog_term_ref t_ph,
				      Prolog_term_ref t_v,
				      Prolog_term_ref t_nd) {
  try {
    CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);
    ph->expand_space_dimension(term_to_Variable(t_v),
			       term_to_unsigned<dimension_type>(t_nd));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_fold_space_dimensions_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_fold_space_dimensions(Prolog_term_ref t_ph,
				     Prolog_term_ref t_vlist,
				     Prolog_term_ref t_v) {
  try {
    CLASS* ph = term_to_cLASS_handle(t_ph);
    CHECK(ph);
    Variables_Set fold_variables;
    Prolog_term_ref v = Prolog_new_term_ref();
    while (Prolog_is_cons(t_vlist)) {
      Prolog_get_cons(t_vlist, v, t_vlist);
      fold_variables.insert(term_to_Variable(v));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_vlist);

    ph->fold_space_dimensions(fold_variables, term_to_Variable(t_v));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_CLASS_map_space_dimensions_code',
`extern "C" Prolog_foreign_return_type
ppl_CLASS_map_space_dimensions(Prolog_term_ref t_ph,
				    Prolog_term_ref t_pfunc) {
  try {
    CLASS* ph = term_to_cLASS_handle(t_ph);
    dimension_type space_dim = ph->space_dimension();
    CHECK(ph);
    PFunc pfunc;
    Prolog_term_ref t_pair = Prolog_new_term_ref();
    while (Prolog_is_cons(t_pfunc)) {
      Prolog_get_cons(t_pfunc, t_pair, t_pfunc);
      Prolog_atom functor;
      int arity;
      Prolog_get_compound_name_arity(t_pair, &functor, &arity);
      if (arity != 2 || functor != a_minus)
	return PROLOG_FAILURE;
      Prolog_term_ref t_i = Prolog_new_term_ref();
      Prolog_term_ref t_j = Prolog_new_term_ref();
      Prolog_get_arg(1, t_pair, t_i);
      Prolog_get_arg(2, t_pair, t_j);
      dimension_type i = term_to_Variable(t_i).id();
      dimension_type j = term_to_Variable(t_j).id();
      if (i >= space_dim || !pfunc.insert(i, j))
	return PROLOG_FAILURE;
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_pfunc);

    ph->map_space_dimensions(pfunc);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;

}
')

define(`ppl_new_LP_Problem_trivial_code',
`extern "C" Prolog_foreign_return_type
ppl_new_LP_Problem_trivial(Prolog_term_ref t_lp) {
  try {
    LP_Problem* lp = new LP_Problem;
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, lp);
    if (Prolog_unify(t_lp, tmp)) {
      REGISTER(lp);
      return PROLOG_SUCCESS;
    }
    else
      delete lp;
  }
  CATCH_ALL;
}
')

define(`ppl_new_LP_Problem_code',
`extern "C" Prolog_foreign_return_type
ppl_new_LP_Problem(Prolog_term_ref t_clist,
		   Prolog_term_ref t_le_expr,
		   Prolog_term_ref t_opt,
		   Prolog_term_ref t_lp) {
  try {
    Constraint_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();
    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_constraint(c));
    }
    // Check the list is properly terminated.
    check_nil_terminating(t_clist);

    const Linear_Expression le = build_linear_expression(t_le_expr);
    Prolog_atom opt = term_to_optimization_mode(t_opt);
    Optimization_Mode mode = (opt == a_max) ? MAXIMIZATION : MINIMIZATION;

    LP_Problem* lp = new LP_Problem(cs, le, mode);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, lp);
    if (Prolog_unify(t_lp, tmp)) {
      REGISTER(lp);
      return PROLOG_SUCCESS;
    }
    else
      delete lp;
  }
  CATCH_ALL;
}
')

define(`ppl_LP_Problem_constraints_code',
`extern "C" Prolog_foreign_return_type
ppl_LP_Problem_constraints(Prolog_term_ref t_lp,
			   Prolog_term_ref t_clist) {
  try {
    const LP_Problem* lp = term_to_lp_problem_handle(t_lp);
    CHECK(lp);

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    const Constraint_System& cs = lp->constraints();
    for (Constraint_System::const_iterator i = cs.begin(),
	   cs_end = cs.end(); i != cs_end; ++i)
      Prolog_construct_cons(tail, constraint_term(*i), tail);

    if (Prolog_unify(t_clist, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_LP_Problem_objective_function_code',
`extern "C" Prolog_foreign_return_type
ppl_LP_Problem_objective_function(Prolog_term_ref t_lp,
				  Prolog_term_ref t_le_expr) {
  try {
    const LP_Problem* lp = term_to_lp_problem_handle(t_lp);
    CHECK(lp);

    const Linear_Expression& le = lp->objective_function();
    Prolog_term_ref t = get_linear_expression(le);

    if (Prolog_unify(t_le_expr, t))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_LP_Problem_optimization_mode_code',
`extern "C" Prolog_foreign_return_type
ppl_LP_Problem_optimization_mode(Prolog_term_ref t_lp,
				 Prolog_term_ref t_opt) {
  try {
    LP_Problem* lp = term_to_lp_problem_handle(t_lp);
    CHECK(lp);

    Optimization_Mode mode = lp->optimization_mode();
    Prolog_term_ref t = Prolog_new_term_ref();
    Prolog_atom a = (mode == MAXIMIZATION) ? a_max : a_min;
    Prolog_put_atom(t, a);
    if (Prolog_unify(t_opt, t))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_LP_Problem_clear_code',
`extern "C" Prolog_foreign_return_type
ppl_LP_Problem_clear(Prolog_term_ref t_lp) {
  try {
    LP_Problem* lp = term_to_lp_problem_handle(t_lp);
    CHECK(lp);
    lp->clear();
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_LP_Problem_add_constraint_code',
`extern "C" Prolog_foreign_return_type
ppl_LP_Problem_add_constraint(Prolog_term_ref t_lp, Prolog_term_ref t_c) {
  try {
    LP_Problem* lp = term_to_lp_problem_handle(t_lp);
    CHECK(lp);
    lp->add_constraint(build_constraint(t_c));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_LP_Problem_add_constraints_code',
`extern "C" Prolog_foreign_return_type
ppl_LP_Problem_add_constraints(Prolog_term_ref t_lp,
			       Prolog_term_ref t_clist) {
  try {
    LP_Problem* lp = term_to_lp_problem_handle(t_lp);
    CHECK(lp);
    Constraint_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_constraint(c));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist);

    lp->add_constraints(cs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_LP_Problem_set_objective_function_code',
`extern "C" Prolog_foreign_return_type
ppl_LP_Problem_set_objective_function(Prolog_term_ref t_lp,
				      Prolog_term_ref t_le_expr) {
  try {
    LP_Problem* lp = term_to_lp_problem_handle(t_lp);
    CHECK(lp);
    lp->set_objective_function(build_linear_expression(t_le_expr));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_LP_Problem_set_optimization_mode_code',
`extern "C" Prolog_foreign_return_type
ppl_LP_Problem_set_optimization_mode(Prolog_term_ref t_lp,
				     Prolog_term_ref t_opt) {
  try {
    LP_Problem* lp = term_to_lp_problem_handle(t_lp);
    CHECK(lp);

    Prolog_atom opt = term_to_optimization_mode(t_opt);
    Optimization_Mode mode = (opt == a_max) ? MAXIMIZATION : MINIMIZATION;
    lp->set_optimization_mode(mode);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_LP_Problem_is_satisfiable_code',
`extern "C" Prolog_foreign_return_type
ppl_LP_Problem_is_satisfiable(Prolog_term_ref t_lp) {
  try {
    const LP_Problem* lp = term_to_lp_problem_handle(t_lp);
    CHECK(lp);
    if (lp->is_satisfiable())
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_LP_Problem_solve_code',
`extern "C" Prolog_foreign_return_type
ppl_LP_Problem_solve(Prolog_term_ref t_lp, Prolog_term_ref t_status) {
  try {
    const LP_Problem* lp = term_to_lp_problem_handle(t_lp);
    CHECK(lp);

    Prolog_atom a;
    switch (lp->solve()) {
    case UNFEASIBLE_LP_PROBLEM:
      a = a_unfeasible;
      break;
    case UNBOUNDED_LP_PROBLEM:
      a = a_unbounded;
      break;
    case OPTIMIZED_LP_PROBLEM:
      a = a_optimized;
      break;
    default:
      throw unknown_interface_error("ppl_LP_Problem_solve()");
    }
    Prolog_term_ref t = Prolog_new_term_ref();
    Prolog_put_atom(t, a);
    if (Prolog_unify(t_status, t))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
')

define(`ppl_LP_Problem_feasible_point_code',
`extern "C" Prolog_foreign_return_type
ppl_LP_Problem_feasible_point(Prolog_term_ref t_lp,
			      Prolog_term_ref t_g) {
 try {
    const LP_Problem* lp = term_to_lp_problem_handle(t_lp);
    CHECK(lp);
    const Generator& g = lp->feasible_point();
    if (Prolog_unify(t_g, generator_term(g)))
      return PROLOG_SUCCESS;
 }
 CATCH_ALL;
}
')

define(`ppl_LP_Problem_optimizing_point_code',
`extern "C" Prolog_foreign_return_type
ppl_LP_Problem_optimizing_point(Prolog_term_ref t_lp,
				Prolog_term_ref t_g) {
 try {
    const LP_Problem* lp = term_to_lp_problem_handle(t_lp);
    CHECK(lp);
    const Generator& g = lp->optimizing_point();
    if (Prolog_unify(t_g, generator_term(g)))
      return PROLOG_SUCCESS;
 }
 CATCH_ALL;
}
')

define(`ppl_LP_Problem_optimal_value_code',
`extern "C" Prolog_foreign_return_type
ppl_LP_Problem_optimal_value(Prolog_term_ref t_lp,
			     Prolog_term_ref t_n,
			     Prolog_term_ref t_d) {
 try {
    const LP_Problem* lp = term_to_lp_problem_handle(t_lp);
    CHECK(lp);
    Coefficient n;
    Coefficient d;
    lp->optimal_value(n, d);
    if (Prolog_unify(t_n, Coefficient_to_integer_term(n))
	&& Prolog_unify(t_d, Coefficient_to_integer_term(d)))
      return PROLOG_SUCCESS;
 }
 CATCH_ALL;
}
')

define(`ppl_LP_Problem_evaluate_objective_function_code',
`extern "C" Prolog_foreign_return_type
ppl_LP_Problem_evaluate_objective_function(Prolog_term_ref t_lp,
					   Prolog_term_ref t_g,
					   Prolog_term_ref t_n,
					   Prolog_term_ref t_d) {
 try {
    const LP_Problem* lp = term_to_lp_problem_handle(t_lp);
    CHECK(lp);
    Coefficient n;
    Coefficient d;
    lp->evaluate_objective_function(build_generator(t_g), n, d);
    if (Prolog_unify(t_n, Coefficient_to_integer_term(n))
	&& Prolog_unify(t_d, Coefficient_to_integer_term(d)))
      return PROLOG_SUCCESS;
 }
 CATCH_ALL;
}
')

divert`'dnl
