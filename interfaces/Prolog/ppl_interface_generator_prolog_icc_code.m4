divert(-1)dnl

define(`m4_class_exception_handler_code',
`void
handle_exception(const not_a_@CLASS@_handle& e) {
  Prolog_term_ref found = Prolog_new_term_ref();
  Prolog_construct_compound(found, a_found, e.term());

  Prolog_term_ref expected = Prolog_new_term_ref();
  Prolog_construct_compound(expected, a_expected,
			    Prolog_atom_term_from_string("@CLASS@_handle"));

  Prolog_term_ref where = Prolog_new_term_ref();
  Prolog_construct_compound(where, a_where,
			    Prolog_atom_term_from_string
			    ("term_to_@CLASS@_handle"));

  Prolog_term_ref exception_term = Prolog_new_term_ref();
  Prolog_construct_compound(exception_term, a_ppl_invalid_argument,
			    found, expected, where);
  Prolog_raise_exception(exception_term);
}

')

define(`m4_term_to_class_handle_code', `dnl
namespace {

@CPP_CLASS@*
term_to_@CLASS@_handle(Prolog_term_ref t_ph) {
  if (Prolog_is_address(t_ph)) {
    void* p;
    if (Prolog_get_address(t_ph, &p))
      return static_cast<@CPP_CLASS@*>(p);
  }
  throw ppl_handle_mismatch(t_ph);
}

} // namespace

')

define(`m4_term_to_topology_Polyhedron_handle_code', `dnl
namespace {

inline @TOPOLOGY@Polyhedron*
term_to_@TOPOLOGY@Polyhedron_handle(Prolog_term_ref t_ph) {
  return static_cast<@TOPOLOGY@Polyhedron*>(term_to_Polyhedron_handle(t_ph));
}

} // namespace

')

define(`ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension_code',
`extern "C" Prolog_foreign_return_type
ppl_new_@TOPOLOGY@@CLASS@_from_space_dimension
(Prolog_term_ref t_nd, Prolog_term_ref t_uoe, Prolog_term_ref t_ph) {
  try {
    @TOPOLOGY@@CPP_CLASS@* ph;
    Prolog_atom uoe = term_to_universe_or_empty(t_uoe);

    if (uoe == a_empty)
        ph = new @TOPOLOGY@@CPP_CLASS@(term_to_unsigned<dimension_type>(t_nd),
			      EMPTY);
      else
        ph = new @TOPOLOGY@@CPP_CLASS@(term_to_unsigned<dimension_type>(t_nd));


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

define(`ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@_code',
`extern "C" Prolog_foreign_return_type
ppl_new_@TOPOLOGY@@CLASS@_from_@INTOPOLOGY@@FRIEND@
(Prolog_term_ref t_ph_source, Prolog_term_ref t_ph) {
  try {
    @TOPOLOGY@@CPP_CLASS@* ph;
    const @INTOPOLOGY@@ALT_FRIEND@* ph_source
	= static_cast<const @INTOPOLOGY@@ALT_FRIEND@*>
	(term_to_@FRIEND@_handle(t_ph_source));
    CHECK(ph_source);
        ph = new @TOPOLOGY@@CPP_CLASS@(*ph_source);
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

define(`ppl_new_@TOPOLOGY@@CLASS@_from_@REPRESENT@s_code',
`extern "C" Prolog_foreign_return_type
ppl_new_@TOPOLOGY@@CLASS@_from_@REPRESENT@s
(Prolog_term_ref t_clist, Prolog_term_ref t_ph) {
  try {
    @UREPRESENT@_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_@REPRESENT@(c));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist);

    @TOPOLOGY@@CPP_CLASS@* ph;
    ph = new @TOPOLOGY@@CPP_CLASS@(cs);
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

define(`ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@_code',
`extern "C" Prolog_foreign_return_type
ppl_new_@TOPOLOGY@@CLASS@_from_@BOX@
(Prolog_term_ref t_bb, Prolog_term_ref t_ph) {
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

    @TOPOLOGY@@CPP_CLASS@* ph;
    ph = new @TOPOLOGY@@CPP_CLASS@(bbox, From_`'@UBOX@());
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

define(`ppl_@CLASS@_swap_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_swap(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  try {
    @CPP_CLASS@* lhs = term_to_@CLASS@_handle(t_lhs);
    @CPP_CLASS@* rhs = term_to_@CLASS@_handle(t_rhs);
    CHECK(lhs);
    CHECK(rhs);
    lhs->swap(*rhs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_delete_@CLASS@_code',
`extern "C" Prolog_foreign_return_type
ppl_delete_@CLASS@(Prolog_term_ref t_ph) {
  try {
    const @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    UNREGISTER(ph);
    delete ph;
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_@DIMENSION@_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_@DIMENSION@(Prolog_term_ref t_ph, Prolog_term_ref t_sd) {
  try {
    const @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    CHECK(ph);
    if (unify_ulong(t_sd, ph->@DIMENSION@()))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_get_@DESCRIBE@s_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_get_@DESCRIBE@s
(Prolog_term_ref t_ph, Prolog_term_ref t_glist) {
  try {
    const @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    CHECK(ph);

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    const @UDESCRIBE@_System& gs = ph->@DESCRIBE@s();
    for (@UDESCRIBE@_System::const_iterator i = gs.begin(),
	   gs_end = gs.end(); i != gs_end; ++i)
      Prolog_construct_cons(tail, @DESCRIBE@_term(*i), tail);

    if (Prolog_unify(t_glist, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_get_minimized_@DESCRIBE@s_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_get_minimized_@DESCRIBE@s
(Prolog_term_ref t_ph, Prolog_term_ref t_glist) {
  try {
    const @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    CHECK(ph);

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    const @UDESCRIBE@_System& gs = ph->minimized_@DESCRIBE@s();
    for (@UDESCRIBE@_System::const_iterator i = gs.begin(),
	   gs_end = gs.end(); i != gs_end; ++i)
      Prolog_construct_cons(tail, @DESCRIBE@_term(*i), tail);

    if (Prolog_unify(t_glist, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_get_disjuncts_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_get_disjuncts(Prolog_term_ref t_pps,
                                     Prolog_term_ref t_dlist) {
  try {
    const @CPP_CLASS@* pps = term_to_@CLASS@_handle(t_pps);
    CHECK(pps);

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    for (@CPP_CLASS@::const_iterator i = pps->begin(),
           pps_end = pps->end(); i != pps_end; ++i) {
      Prolog_term_ref t_d = Prolog_new_term_ref();
      Prolog_put_address(t_d,
			 const_cast<void*>(static_cast<const void*>(&(*i))));
      Prolog_construct_cons(tail, t_d, tail);
    }

    if (Prolog_unify(t_dlist, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')


define(`ppl_@CLASS@_relation_with_@DESCRIBE@_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_relation_with_@DESCRIBE@
(Prolog_term_ref t_ph, Prolog_term_ref t_c, Prolog_term_ref t_r) {
  try {
    @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    CHECK(ph);
relation_with_@DESCRIBE@_code`'
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

define(`ppl_Grid_get_bounding_box_code',
`extern "C" Prolog_foreign_return_type
ppl_Grid_get_bounding_box
(Prolog_term_ref t_ph, Prolog_term_ref t_bb) {
  try {
    Grid* ph = term_to_Grid_handle(t_ph);
    CHECK(ph);

    dimension_type dimension = ph->space_dimension();
    Bounding_Box bbox(dimension);
    ph->shrink_bounding_box(bbox);
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

define(`ppl_Grid_get_@BOX@_code',
`extern "C" Prolog_foreign_return_type
ppl_Grid_get_@BOX@
(Prolog_term_ref t_ph, Prolog_term_ref t_bb) {
  try {
    Grid* ph = term_to_Grid_handle(t_ph);
    CHECK(ph);

    dimension_type dimension = ph->space_dimension();
    Bounding_Box bbox(dimension);
    ph->@ALT_BOX@(bbox);
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

define(`ppl_@CLASS@_get_@BOX@_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_get_@BOX@
(Prolog_term_ref t_ph, Prolog_term_ref t_cc, Prolog_term_ref t_bb) {
  try {
    @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
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
    ph->shrink_@BOX@(bbox, cc);
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

define(`ppl_@CLASS@_is_@STATE@_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_is_@STATE@(Prolog_term_ref t_ph) {
  try {
    const @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    CHECK(ph);
    if (ph->is_@STATE@())
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_@SIMPLIFY@_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_@SIMPLIFY@(Prolog_term_ref t_ph) {
  try {
    @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    CHECK(ph);
    ph->@SIMPLIFY@();
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_bounds_from_@ABOVEBELOW@_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_bounds_from_@ABOVEBELOW@
(Prolog_term_ref t_ph, Prolog_term_ref t_expr) {
  try {
    @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    CHECK(ph);
    Linear_Expression l = build_linear_expression(t_expr);
    if (ph->bounds_from_@ABOVEBELOW@(l))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_@MAXMIN@_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_@MAXMIN@
(Prolog_term_ref t_ph, Prolog_term_ref t_le_expr,
 Prolog_term_ref t_n,  Prolog_term_ref t_d,
 Prolog_term_ref t_maxmin) {
 try {
    const @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    CHECK(ph);
    const Linear_Expression le = build_linear_expression(t_le_expr);
    Coefficient n;
    Coefficient d;
    bool maxmin;
    if (ph->@MAXMIN@(le, n, d, maxmin)) {
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

define(`ppl_@CLASS@_@MAXMIN@_with_point_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_@MAXMIN@_with_point
(Prolog_term_ref t_ph, Prolog_term_ref t_le_expr,
 Prolog_term_ref t_n, Prolog_term_ref t_d,
 Prolog_term_ref t_maxmin, Prolog_term_ref t_g) {
 try {
    const @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    CHECK(ph);
    const Linear_Expression le = build_linear_expression(t_le_expr);
    Coefficient n;
    Coefficient d;
    bool maxmin;
    @UGENERATOR@ g(@POINT@());
    if (ph->@MAXMIN@(le, n, d, maxmin, g)) {
      Prolog_term_ref t = Prolog_new_term_ref();
      Prolog_atom a = (maxmin ? a_true : a_false);
      Prolog_put_atom(t, a);
      if (Prolog_unify(t_n, Coefficient_to_integer_term(n))
	  && Prolog_unify(t_d, Coefficient_to_integer_term(d))
	  && Prolog_unify(t_maxmin, t)
	  && Prolog_unify(t_g, @GENERATOR@_term(g)))
	return PROLOG_SUCCESS;
    }
 }
 CATCH_ALL;
}

')

define(`ppl_@CLASS@_@COMPARISON@_@CLASS@_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_@COMPARISON@_@CLASS@
(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  try {
    const @CPP_CLASS@* lhs = term_to_@CLASS@_handle(t_lhs);
    const @CPP_CLASS@* rhs = term_to_@CLASS@_handle(t_rhs);
    CHECK(lhs);
    CHECK(rhs);
    if (lhs->@COMPARISON@(*rhs))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_equals_@CLASS@_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_equals_@CLASS@
(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  try {
    const @CPP_CLASS@* lhs = term_to_@CLASS@_handle(t_lhs);
    const @CPP_CLASS@* rhs = term_to_@CLASS@_handle(t_rhs);
    CHECK(lhs);
    CHECK(rhs);
    if (*lhs == *rhs)
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_OK_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_OK(Prolog_term_ref t_ph) {
  try {
    const @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    CHECK(ph);
    if (ph->OK())
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_add_@REPRESENT@_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_add_@REPRESENT@(Prolog_term_ref t_ph, Prolog_term_ref t_c) {
  try {
    @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    CHECK(ph);
    ph->add_@REPRESENT@(build_@REPRESENT@(t_c));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_add_disjunct_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_add_disjunct(Prolog_term_ref t_ph, Prolog_term_ref t_d) {
  try {
    @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    CHECK(ph);
    @ALT_DISJUNCT@* d = term_to_@DISJUNCT@_handle(t_d);
    CHECK(d);
    ph->add_disjunct(*d);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_add_@REPRESENT@_and_minimize_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_add_@REPRESENT@_and_minimize
(Prolog_term_ref t_ph, Prolog_term_ref t_c) {
  try {
    @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    CHECK(ph);
    if (ph->add_@REPRESENT@_and_minimize(build_@REPRESENT@(t_c)))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_add_@REPRESENT@s_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_add_@REPRESENT@s
(Prolog_term_ref t_ph, Prolog_term_ref t_clist) {
  try {
    @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    CHECK(ph);
    @UREPRESENT@_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_@REPRESENT@(c));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist);

    ph->add_@REPRESENT@s(cs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_add_@REPRESENT@s_and_minimize_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_add_@REPRESENT@s_and_minimize
(Prolog_term_ref t_ph, Prolog_term_ref t_clist) {
  try {
    @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    CHECK(ph);
    @UREPRESENT@_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_@REPRESENT@(c));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist);

    if (ph->add_@REPRESENT@s_and_minimize(cs))
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
	   void (@CPP_CLASS@::* bop_assign)(const @CPP_CLASS@&)) {
  try {
    @CPP_CLASS@* lhs = term_to_@CLASS@_handle(t_lhs);
    const @CPP_CLASS@* rhs = term_to_@CLASS@_handle(t_rhs);
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
			bool (@CPP_CLASS@::*
			      bop_assign_and_minimize)(const @CPP_CLASS@&)) {
  try {
    @CPP_CLASS@* lhs = term_to_@CLASS@_handle(t_lhs);
    const @CPP_CLASS@* rhs = term_to_@CLASS@_handle(t_rhs);
    CHECK(lhs);
    CHECK(rhs);
    if ((lhs->*bop_assign_and_minimize)(*rhs))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

} // namespace

')

define(`ppl_@CLASS@_@BINOP@_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_@BINOP@
(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  return bop_assign(t_lhs, t_rhs,
		    &@CPP_CLASS@::@BINOP@);
}

')

define(`ppl_@CLASS@_@BINMINOP@_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_@BINMINOP@
(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  return bop_assign_and_minimize(t_lhs, t_rhs,
		    &@CPP_CLASS@::@BINMINOP@);
}

')

define(`ppl_@CLASS@_@AFFIMAGE@_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_@AFFIMAGE@
(Prolog_term_ref t_ph,
 Prolog_term_ref t_v, Prolog_term_ref t_le, Prolog_term_ref t_d) {
  try {
    @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    CHECK(ph);
    ph->@AFFIMAGE@(term_to_Variable(t_v),
		     build_linear_expression(t_le),
		     term_to_Coefficient(t_d));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_Grid_generalized_@AFFIMAGE@_code',
`extern "C" Prolog_foreign_return_type
ppl_Grid_generalized_@AFFIMAGE@
(Prolog_term_ref t_ph,
 Prolog_term_ref t_v, Prolog_term_ref t_le, Prolog_term_ref t_d,
 Prolog_term_ref t_m) {
  try {
    Grid* ph = term_to_Grid_handle(t_ph);
    CHECK(ph);
    ph->generalized_@AFFIMAGE@(term_to_Variable(t_v),
				 build_linear_expression(t_le),
				 term_to_Coefficient(t_d),
				 term_to_Coefficient(t_m));
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_generalized_@AFFIMAGE@_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_generalized_@AFFIMAGE@
(Prolog_term_ref t_ph,
 Prolog_term_ref t_v, Prolog_term_ref t_r, Prolog_term_ref t_le,
 Prolog_term_ref t_d) {
  try {
    @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    CHECK(ph);
    Relation_Symbol r = term_to_relation_symbol(t_r);
    ph->generalized_@AFFIMAGE@(term_to_Variable(t_v),
				 r,
				 build_linear_expression(t_le),
				 term_to_Coefficient(t_d));
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_Grid_generalized_@AFFIMAGE@_lhs_rhs_code',
`extern "C" Prolog_foreign_return_type
ppl_Grid_generalized_@AFFIMAGE@_lhs_rhs
(Prolog_term_ref t_ph,
 Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_m) {
  try {
    Grid* ph = term_to_Grid_handle(t_ph);
    CHECK(ph);
    ph->generalized_@AFFIMAGE@(build_linear_expression(t_lhs),
				 build_linear_expression(t_rhs),
				 term_to_Coefficient(t_m));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_generalized_@AFFIMAGE@_lhs_rhs
(Prolog_term_ref t_ph,
 Prolog_term_ref t_lhs, Prolog_term_ref t_r, Prolog_term_ref t_rhs) {
  try {
    @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    CHECK(ph);
    Relation_Symbol r = term_to_relation_symbol(t_r);
    ph->generalized_@AFFIMAGE@(build_linear_expression(t_lhs),
				 r,
				 build_linear_expression(t_rhs));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_bounded_@AFFIMAGE@_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_bounded_@AFFIMAGE@
(Prolog_term_ref t_ph,
 Prolog_term_ref t_v, Prolog_term_ref t_lb_le, Prolog_term_ref t_ub_le,
 Prolog_term_ref t_d) {
  try {
    @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    CHECK(ph);
    ph->bounded_@AFFIMAGE@(term_to_Variable(t_v),
			     build_linear_expression(t_lb_le),
			     build_linear_expression(t_ub_le),
			     term_to_Coefficient(t_d));
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_@WIDEN@_widening_assign_with_tokens_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_@WIDEN@_widening_assign_with_tokens
(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs,
 Prolog_term_ref t_ti, Prolog_term_ref t_to) {
  try {
    @CPP_CLASS@* lhs = term_to_@CLASS@_handle(t_lhs);
    const @CPP_CLASS@* rhs = term_to_@CLASS@_handle(t_rhs);
    CHECK(lhs);
    CHECK(rhs);
    unsigned t = term_to_unsigned<unsigned>(t_ti);
    lhs->@WIDEN@_widening_assign(*rhs, &t);
      if (unify_long(t_to, t))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_@WIDEN@_widening_assign_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_@WIDEN@_widening_assign
(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  try {
    @CPP_CLASS@* lhs = term_to_@CLASS@_handle(t_lhs);
    const @CPP_CLASS@* rhs = term_to_@CLASS@_handle(t_rhs);
    CHECK(lhs);
    CHECK(rhs);
    lhs->@WIDEN@_widening_assign(*rhs, 0);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

dnl FIXME: Polyhedra_Powerset widening = TODO
define(`ppl_@CLASS@_@WIDENEXP@_@BODYWIDENEXP@_widening_assign_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_@WIDENEXP@_@BODYWIDENEXP@_widening_assign
(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  return widening_assign(t_lhs, t_rhs,
                         &@CPP_CLASS@::@WIDENEXP@_widening_assign,
                         &@CPP_BODY@::@BODYWIDENEXP@_widening_assign);
}

')

define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@EXTRAPOLATION@_extrapolation_assign_with_tokens_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_@LIMITEDBOUNDED@_@EXTRAPOLATION@_extrapolation_assign_with_tokens
(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_clist,
 Prolog_term_ref t_ti, Prolog_term_ref t_to) {
  try {
    @CPP_CLASS@* lhs = term_to_@CLASS@_handle(t_lhs);
    const @CPP_CLASS@* rhs = term_to_@CLASS@_handle(t_rhs);
    CHECK(lhs);
    CHECK(rhs);
    @UCONSTRAINER@_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_@CONSTRAINER@(c));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist);

    unsigned t = term_to_unsigned<unsigned>(t_ti);
   lhs->@LIMITEDBOUNDED@_@EXTRAPOLATION@_extrapolation_assign(*rhs, cs, &t);
      if (unify_long(t_to, t))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_@LIMITEDBOUNDED@_@EXTRAPOLATION@_extrapolation_assign_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_@LIMITEDBOUNDED@_@EXTRAPOLATION@_extrapolation_assign
(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_clist) {
  try {
    @CPP_CLASS@* lhs = term_to_@CLASS@_handle(t_lhs);
    const @CPP_CLASS@* rhs = term_to_@CLASS@_handle(t_rhs);
    CHECK(lhs);
    CHECK(rhs);
    @UCONSTRAINER@_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
     cs.insert(build_@CONSTRAINER@(c));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist);

    lhs->@LIMITEDBOUNDED@_@EXTRAPOLATION@_extrapolation_assign(*rhs, cs, 0);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_add_space_dimensions_@EMBEDPROJECT@
(Prolog_term_ref t_ph, Prolog_term_ref t_nnd) {
  try {
    @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    CHECK(ph);
    dimension_type d = term_to_unsigned<dimension_type>(t_nnd);
      ph->add_space_dimensions_@EMBEDPROJECT@(d);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_remove_space_dimensions_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_remove_space_dimensions
(Prolog_term_ref t_ph, Prolog_term_ref t_vlist) {
  try {
    @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
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

define(`ppl_@CLASS@_remove_higher_space_dimensions_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_remove_higher_space_dimensions
(Prolog_term_ref t_ph, Prolog_term_ref t_nd) {
  try {
    @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    CHECK(ph);
    ph->remove_higher_space_dimensions(term_to_unsigned<dimension_type>(t_nd));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_expand_space_dimension_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_expand_space_dimension
(Prolog_term_ref t_ph, Prolog_term_ref t_v, Prolog_term_ref t_nd) {
  try {
    @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
    CHECK(ph);
    ph->expand_space_dimension(term_to_Variable(t_v),
			       term_to_unsigned<dimension_type>(t_nd));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

')

define(`ppl_@CLASS@_fold_space_dimensions_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_fold_space_dimensions
(Prolog_term_ref t_ph, Prolog_term_ref t_vlist, Prolog_term_ref t_v) {
  try {
    @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
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

define(`ppl_@CLASS@_map_space_dimensions_code',
`extern "C" Prolog_foreign_return_type
ppl_@CLASS@_map_space_dimensions
(Prolog_term_ref t_ph, Prolog_term_ref t_pfunc) {
  try {
    @CPP_CLASS@* ph = term_to_@CLASS@_handle(t_ph);
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

divert`'dnl
