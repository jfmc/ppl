
#include "ppl_install.hh"
#include <exception>
#include <stdexcept>
#include <sstream>
#include <climits>

#define TRACK_ALLOCATION 1
#include "track_allocation.hh"

namespace PPL = Parma_Polyhedra_Library;

static void
handle_exception(const integer_out_of_range& e) {
  Prolog_term_ref culprit = Prolog_new_term_ref();
  Prolog_term_ref arg_no = Prolog_new_term_ref();
  Prolog_term_ref expected_domain = Prolog_new_term_ref();
  Prolog_term_ref et = Prolog_new_term_ref();

  Prolog_put_term(culprit, e.term());
  Prolog_put_long(arg_no, 1);
  {
    std::string s;
    std::ostringstream domain(s);
    domain << "[" << LONG_MIN << ", " << LONG_MAX << "]";
    Prolog_put_atom_chars(expected_domain, domain.str().c_str());
  }
  Prolog_construct_compound(et,
			    Prolog_atom_from_string("integer_term_to_Integer"),
			    culprit);
  Prolog_construct_compound(et,
			    Prolog_atom_from_string("domain_error"),
			    et, arg_no, expected_domain, culprit);
  Prolog_raise_exception(et);
}

static void
handle_exception(const not_unsigned_int& e) {
  Prolog_term_ref culprit = Prolog_new_term_ref();
  Prolog_term_ref arg_no = Prolog_new_term_ref();
  Prolog_term_ref expected_domain = Prolog_new_term_ref();
  Prolog_term_ref et = Prolog_new_term_ref();

  Prolog_put_term(culprit, e.term());
  Prolog_put_long(arg_no, 1);
  {
    std::string s;
    std::ostringstream domain(s);
    domain << "[" << 0 << ", " << UINT_MAX << "]";
    Prolog_put_atom_chars(expected_domain, domain.str().c_str());
  }
  Prolog_construct_compound(et,
			    Prolog_atom_from_string("get_unsigned_int/term_to_unsigned_int"),
			   culprit);
  Prolog_construct_compound(et,
			    Prolog_atom_from_string("domain_error"),
			    et, arg_no, expected_domain, culprit);
  Prolog_raise_exception(et);
}

static void
handle_exception(const non_linear& e) {
  Prolog_term_ref culprit = Prolog_new_term_ref();
  Prolog_term_ref arg_no = Prolog_new_term_ref();
  Prolog_term_ref expected_domain = Prolog_new_term_ref();
  Prolog_term_ref et = Prolog_new_term_ref();

  Prolog_put_term(culprit, e.term());
  Prolog_put_long(arg_no, 1);
  Prolog_put_atom_chars(expected_domain, "linear expression or constraint");
  Prolog_construct_compound(et, Prolog_atom_from_string(e.who()), culprit);
  Prolog_construct_compound(et, Prolog_atom_from_string("domain_error"),
			    et, arg_no, expected_domain, culprit);
  Prolog_raise_exception(et);
}

#include <iostream>
using namespace std;

static void
handle_exception(const not_a_variable& e) {
  Prolog_term_ref culprit = Prolog_new_term_ref();
  Prolog_term_ref arg_no = Prolog_new_term_ref();
  Prolog_term_ref expected_domain = Prolog_new_term_ref();
  Prolog_term_ref et = Prolog_new_term_ref();

  Prolog_put_term(culprit, e.term());
  Prolog_put_long(arg_no, 1);
  Prolog_put_atom_chars(expected_domain, "$VAR(integer)");
  Prolog_construct_compound(et, Prolog_atom_from_string("get_variable"),
			    culprit);
  Prolog_construct_compound(et, Prolog_atom_from_string("domain_error"),
			    et, arg_no, expected_domain, culprit);
  Prolog_raise_exception(et);
}

static void
handle_exception(const unknown_interface_error& e) {
  Prolog_term_ref et = Prolog_new_term_ref();
  Prolog_put_atom_chars(et, e.where());
  Prolog_raise_exception(et);
}

static void
handle_exception() {
  Prolog_term_ref et = Prolog_new_term_ref();
  Prolog_put_atom_chars(et, "PPL bug: unknown exception raised");
  Prolog_raise_exception(et);
}

static void
handle_exception(const std::exception& e) {
  Prolog_term_ref et = Prolog_new_term_ref();
  Prolog_put_atom_chars(et, e.what());
  Prolog_raise_exception(et);
}

#define CATCH_INTERNAL \
  catch (const integer_out_of_range& e) { \
    handle_exception(e); \
  } \
  catch (const not_unsigned_int& e) { \
    handle_exception(e); \
  } \
  catch (const non_linear& e) { \
    handle_exception(e); \
  } \
  catch (const not_a_variable& e) { \
    handle_exception(e); \
  } \
  catch (const unknown_interface_error& e) { \
    handle_exception(e); \
  }

#define CATCH_PPL \
  catch (const std::exception& e) { \
    handle_exception(e); \
  } \
  catch (...) { \
    handle_exception(); \
  }

#define CATCH_ALL \
  CATCH_INTERNAL  \
  CATCH_PPL

// For Prolog lists.
static Prolog_atom a_nil;

// For variables.
static Prolog_atom a_dollar_VAR;

// For linear expressions.
static Prolog_atom a_plus;
static Prolog_atom a_minus;
static Prolog_atom a_asterisk;

// For constraints.
static Prolog_atom a_equal;
static Prolog_atom a_greater_than_equal;
static Prolog_atom a_equal_less_than;

// For generators.
static Prolog_atom a_line;
static Prolog_atom a_ray;
static Prolog_atom a_point;

// For the relation between a polyhedron and a constraint.
static Prolog_atom a_is_disjoint;
static Prolog_atom a_strictly_intersects;
static Prolog_atom a_is_included;
static Prolog_atom a_saturates;

// For the relation between a polyhedron and a generator.
static Prolog_atom a_subsumes;

static struct {
  Prolog_atom* p_atom;
  const char* name;
} const prolog_atoms[] = {
  { &a_nil,                 "[]" },

  { &a_dollar_VAR,          "$VAR" },

  { &a_plus,                "+" },
  { &a_minus,               "-" },
  { &a_asterisk,            "*" },

  { &a_equal,               "=" },
  { &a_greater_than_equal,  ">=" },
  { &a_equal_less_than,     "=<" },

  { &a_line,                "line" },
  { &a_ray,                 "ray" },
  { &a_point,               "point" },

  { &a_is_disjoint,         "is_disjoint" },
  { &a_strictly_intersects, "strictly_intersects" },
  { &a_is_included,         "is_included" },
  { &a_saturates,           "saturates" },

  { &a_subsumes,            "subsumes" },
};

static Prolog_term_ref
variable_term(unsigned int varid) {
  Prolog_term_ref v = Prolog_new_term_ref();
  Prolog_put_long(v, varid);
  Prolog_term_ref t = Prolog_new_term_ref();
  Prolog_construct_compound(t, a_dollar_VAR, v);
  return t;
}

static unsigned int
get_unsigned_int(long n) {
  if (n >= 0 && static_cast<unsigned long>(n) <= UINT_MAX)
    return n;
  else {
    Prolog_term_ref n_term = Prolog_new_term_ref();
    Prolog_put_long(n_term, n);
    throw not_unsigned_int(n_term);
  }
}

static unsigned int
term_to_unsigned_int(Prolog_term_ref t) {
  if (Prolog_is_integer(t)) {
    long v;
    if (Prolog_get_long(t, v))
      return get_unsigned_int(v);
  }
  throw not_unsigned_int(t);
}

static PPL::LinExpression
build_lin_expression(Prolog_term_ref t) {
  if (Prolog_is_integer(t))
    return PPL::LinExpression(integer_term_to_Integer(t));
  else if (Prolog_is_compound(t)) {
    Prolog_atom functor;
    int arity;
    Prolog_get_name_arity(t, functor, arity);
    switch (arity) {
    case 1:
      {
	Prolog_term_ref arg = Prolog_new_term_ref();
	Prolog_get_arg(1, t, arg);
	if (functor == a_minus)
	  // Unary minus.
	  return -build_lin_expression(arg);
	else if (functor == a_dollar_VAR)
	  // Variable.
	  return PPL::Variable(term_to_unsigned_int(arg));
      }
      break;
    case 2:
      {
	Prolog_term_ref arg1 = Prolog_new_term_ref();
	Prolog_term_ref arg2 = Prolog_new_term_ref();
	Prolog_get_arg(1, t, arg1);
	Prolog_get_arg(2, t, arg2);
	if (functor == a_plus)
	  // Plus.
	  if (Prolog_is_integer(arg1))
	    return integer_term_to_Integer(arg1) + build_lin_expression(arg2);
	  else if (Prolog_is_integer(arg2))
	    return build_lin_expression(arg1) + integer_term_to_Integer(arg2);
	  else
	    return build_lin_expression(arg1) + build_lin_expression(arg2);
	else if (functor == a_minus)
	  // Minus.
	  if (Prolog_is_integer(arg1))
	    return integer_term_to_Integer(arg1) - build_lin_expression(arg2);
	  else if (Prolog_is_integer(arg2))
	    return build_lin_expression(arg1) - integer_term_to_Integer(arg2);
	  else
	    return build_lin_expression(arg1) - build_lin_expression(arg2);
	else if (functor == a_asterisk)
	  // Times.
	  if (Prolog_is_integer(arg1))
	    return integer_term_to_Integer(arg1) * build_lin_expression(arg2);
	  else if (Prolog_is_integer(arg2))
	    return build_lin_expression(arg1) * integer_term_to_Integer(arg2);
      }
    }
  }
  // Invalid.
  throw non_linear("build_lin_expression", t);
}


static PPL::Constraint
build_constraint(Prolog_term_ref t) {
  if (Prolog_is_compound(t)) {
    Prolog_atom functor;
    int arity;
    Prolog_get_name_arity(t, functor, arity);
    if (arity == 2) {
      Prolog_term_ref arg1 = Prolog_new_term_ref();
      Prolog_term_ref arg2 = Prolog_new_term_ref();
      Prolog_get_arg(1, t, arg1);
      Prolog_get_arg(2, t, arg2);
      if (functor == a_equal)
	// =
	if (Prolog_is_integer(arg1))
	  return integer_term_to_Integer(arg1) == build_lin_expression(arg2);
	else if (Prolog_is_integer(arg2))
	  return build_lin_expression(arg1) == integer_term_to_Integer(arg2);
	else
	  return build_lin_expression(arg1) == build_lin_expression(arg2);
      else if (functor == a_equal_less_than)
	// =<
	if (Prolog_is_integer(arg1))
	  return integer_term_to_Integer(arg1) <= build_lin_expression(arg2);
	else if (Prolog_is_integer(arg2))
	  return build_lin_expression(arg1) <= integer_term_to_Integer(arg2);
	else
	  return build_lin_expression(arg1) <= build_lin_expression(arg2);
      else if (functor == a_greater_than_equal)
	// >=
	if (Prolog_is_integer(arg1))
	  return integer_term_to_Integer(arg1) >= build_lin_expression(arg2);
	else if (Prolog_is_integer(arg2))
	  return build_lin_expression(arg1) >= integer_term_to_Integer(arg2);
	else
	  return build_lin_expression(arg1) >= build_lin_expression(arg2);
    }
  }
  // Invalid.
  throw non_linear("build_constraint", t);
}

static PPL::Generator
build_generator(Prolog_term_ref t) {
  if (Prolog_is_compound(t)) {
    Prolog_atom functor;
    int arity;
    Prolog_get_name_arity(t, functor, arity);
    if (arity == 1) {
      Prolog_term_ref arg = Prolog_new_term_ref();
      Prolog_get_arg(1, t, arg);
      if (functor == a_line)
	return line(build_lin_expression(arg));
      else if (functor == a_ray)
	return ray(build_lin_expression(arg));
      else if (functor == a_point)
	return point(build_lin_expression(arg));
    }
    else if (arity == 2) {
      Prolog_term_ref arg1 = Prolog_new_term_ref();
      Prolog_term_ref arg2 = Prolog_new_term_ref();
      Prolog_get_arg(1, t, arg1);
      Prolog_get_arg(2, t, arg2);
      if (functor == a_point)
	if (Prolog_is_integer(arg2))
	  return point(build_lin_expression(arg1),
		       integer_term_to_Integer(arg2));
    }
  }
  // Invalid.
  throw non_linear("build_generator", t);
}

template <class R>
static Prolog_term_ref
get_lin_expression(const R& r) {
  Prolog_term_ref so_far = Prolog_new_term_ref();
  PPL::Integer coefficient;
  unsigned int varid = 0;
  unsigned int space_dimension = r.space_dimension();
  while (varid < space_dimension
	 && (coefficient = r.coefficient(PPL::Variable(varid))) == 0)
    ++varid;
  if (varid >= space_dimension) {
    Prolog_put_long(so_far, 0);
  }
  else {
    Prolog_construct_compound(so_far, a_asterisk,
			      Integer_to_integer_term(coefficient),
			      variable_term(varid));
    while (true) {
      ++varid;
      while (varid < space_dimension
	     && (coefficient = r.coefficient(PPL::Variable(varid))) == 0)
	++varid;
      if (varid >= space_dimension)
	break;
      else {
	Prolog_term_ref addendum = Prolog_new_term_ref();
	Prolog_construct_compound(addendum, a_asterisk,
				  Integer_to_integer_term(coefficient),
				  variable_term(varid));
	Prolog_term_ref new_so_far = Prolog_new_term_ref();
	Prolog_construct_compound(new_so_far, a_plus,
				  so_far, addendum);
	so_far = new_so_far;
      }
    }
  }
  return so_far;
}

static Prolog_term_ref
constraint_term(const PPL::Constraint& c) {
  Prolog_atom relation = c.is_equality() ? a_equal : a_greater_than_equal;
  Prolog_term_ref t = Prolog_new_term_ref();
  Prolog_construct_compound(t, relation,
			    get_lin_expression(c),
			    Integer_to_integer_term(-c.coefficient()));
  return t;
}

static Prolog_term_ref
generator_term(const PPL::Generator& g) {
  Prolog_term_ref t = Prolog_new_term_ref();
  Prolog_atom constructor;
  switch (g.type()) {
  case PPL::Generator::LINE:
    constructor = a_line;
    break;
  case PPL::Generator::RAY:
    constructor = a_ray;
    break;
  case PPL::Generator::POINT:
    {
      constructor = a_point;
      const PPL::Integer& divisor = g.divisor();
      if (divisor == 1)
	break;
      else {
	Prolog_construct_compound(t, constructor,
				  get_lin_expression(g),
				  Integer_to_integer_term(divisor));
	return t;
      }
    }
  default:
    abort();
  }
  Prolog_construct_compound(t, constructor, get_lin_expression(g));
  return t;
}

static PPL::Variable
get_variable(Prolog_term_ref t) {
  if (Prolog_is_compound(t)) {
    Prolog_atom functor;
    int arity;
    Prolog_get_name_arity(t, functor, arity);
    if (functor == a_dollar_VAR && arity == 1) {
      Prolog_term_ref arg = Prolog_new_term_ref();
      Prolog_get_arg(1, t, arg);
      return PPL::Variable(term_to_unsigned_int(arg));
    }
  }
  throw not_a_variable(t);
}

static inline PPL::Polyhedron*
get_ph_pointer(Prolog_term_ref t_ph) {
  void* p;
  if (Prolog_is_address(t_ph) && Prolog_get_address(t_ph, p))
    return static_cast<PPL::Polyhedron*>(p);
  else
    return 0;
}

extern "C" Prolog_foreign_return_type
ppl_new_polyhedron(Prolog_term_ref t_ph, Prolog_term_ref t_nd) {
  try {
    PPL::Polyhedron* ph
      = new PPL::Polyhedron(term_to_unsigned_int(t_nd));
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
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_new_empty_polyhedron(Prolog_term_ref t_ph, Prolog_term_ref t_nd) {
  try {
    PPL::Polyhedron* ph
      = new PPL::Polyhedron(term_to_unsigned_int(t_nd),
			    PPL::Polyhedron::EMPTY);
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
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_copy_polyhedron(Prolog_term_ref t_source, Prolog_term_ref t_ph) {
  try {
    const PPL::Polyhedron* source_ph = get_ph_pointer(t_source);
    if (source_ph == 0)
      return PROLOG_FAILURE;
    CHECK(source_ph);
    PPL::Polyhedron* ph = new PPL::Polyhedron(*source_ph);
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
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_delete_polyhedron(Prolog_term_ref t_ph) {
  try {
    const PPL::Polyhedron* ph = get_ph_pointer(t_ph);
    if (ph == 0)
      return PROLOG_FAILURE;
    UNREGISTER(ph);
    delete ph;
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_space_dimension(Prolog_term_ref t_ph, Prolog_term_ref t_sd) {
  try {
    const PPL::Polyhedron* ph = get_ph_pointer(t_ph);
    if (ph == 0)
      return PROLOG_FAILURE;
    CHECK(ph);
    size_t sd = ph->space_dimension();
    Prolog_term_ref tmp = Prolog_new_term_ref();
    if (sd <= LONG_MAX
	&& Prolog_put_long(tmp, long(sd)) 
	&& Prolog_unify(t_sd, tmp))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_insert_constraint(Prolog_term_ref t_ph, Prolog_term_ref t_c) {
  try {
    PPL::Polyhedron* ph = get_ph_pointer(t_ph);
    if (ph == 0)
      return PROLOG_FAILURE;
    CHECK(ph);
    ph->insert(build_constraint(t_c));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_insert_generator(Prolog_term_ref t_ph, Prolog_term_ref t_g) {
  try {
    PPL::Polyhedron* ph = get_ph_pointer(t_ph);
    if (ph == 0)
      return PROLOG_FAILURE;
    CHECK(ph);
    ph->insert(build_generator(t_g));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_add_constraints_and_minimize(Prolog_term_ref t_ph,
				 Prolog_term_ref t_clist) {
  try {
    PPL::Polyhedron* ph = get_ph_pointer(t_ph);
    if (ph == 0)
      return PROLOG_FAILURE;
    CHECK(ph);
    PPL::ConSys cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_constraint(c));
    }

    if (ph->add_constraints_and_minimize(cs))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_check_empty(Prolog_term_ref t_ph) {
  try {
    const PPL::Polyhedron* ph = get_ph_pointer(t_ph);
    if (ph == 0)
      return PROLOG_FAILURE;
    CHECK(ph);
    if (ph->check_empty())
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_check_universe(Prolog_term_ref t_ph) {
  try {
    const PPL::Polyhedron* ph = get_ph_pointer(t_ph);
    if (ph == 0)
      return PROLOG_FAILURE;
    CHECK(ph);
    if (ph->check_universe())
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_is_bounded(Prolog_term_ref t_ph) {
  try {
    const PPL::Polyhedron* ph = get_ph_pointer(t_ph);
    if (ph == 0)
      return PROLOG_FAILURE;
    CHECK(ph);
    if (ph->is_bounded())
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_intersection_assign(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  try {
    PPL::Polyhedron* lhs = get_ph_pointer(t_lhs);
    if (lhs == 0)
      return PROLOG_FAILURE;
    const PPL::Polyhedron* rhs = get_ph_pointer(t_rhs);
    if (rhs == 0)
      return PROLOG_FAILURE;
    CHECK(lhs);
    CHECK(rhs);
    lhs->intersection_assign(*rhs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_intersection_assign_and_minimize(Prolog_term_ref t_lhs,
				     Prolog_term_ref t_rhs) {
  try {
    PPL::Polyhedron* lhs = get_ph_pointer(t_lhs);
    if (lhs == 0)
      return PROLOG_FAILURE;
    const PPL::Polyhedron* rhs = get_ph_pointer(t_rhs);
    if (rhs == 0)
      return PROLOG_FAILURE;
    CHECK(lhs);
    CHECK(rhs);
    PPL::Polyhedron& x = *static_cast<PPL::Polyhedron*>(lhs);
    const PPL::Polyhedron& y = *static_cast<const PPL::Polyhedron*>(rhs);
    x.intersection_assign_and_minimize(y);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_convex_hull_assign(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  try {
    PPL::Polyhedron* lhs = get_ph_pointer(t_lhs);
    if (lhs == 0)
      return PROLOG_FAILURE;
    const PPL::Polyhedron* rhs = get_ph_pointer(t_rhs);
    if (rhs == 0)
      return PROLOG_FAILURE;
    CHECK(lhs);
    CHECK(rhs);
    PPL::Polyhedron& x = *static_cast<PPL::Polyhedron*>(lhs);
    const PPL::Polyhedron& y = *static_cast<const PPL::Polyhedron*>(rhs);
    x.convex_hull_assign(y);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_convex_hull_assign_and_minimize(Prolog_term_ref t_lhs,
				    Prolog_term_ref t_rhs) {
  try {
    PPL::Polyhedron* lhs = get_ph_pointer(t_lhs);
    if (lhs == 0)
      return PROLOG_FAILURE;
    const PPL::Polyhedron* rhs = get_ph_pointer(t_rhs);
    if (rhs == 0)
      return PROLOG_FAILURE;
    CHECK(lhs);
    CHECK(rhs);
    PPL::Polyhedron& x = *static_cast<PPL::Polyhedron*>(lhs);
    const PPL::Polyhedron& y = *static_cast<const PPL::Polyhedron*>(rhs);
    x.convex_hull_assign_and_minimize(y);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_convex_difference_assign(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  try {
    PPL::Polyhedron* lhs = get_ph_pointer(t_lhs);
    if (lhs == 0)
      return PROLOG_FAILURE;
    const PPL::Polyhedron* rhs = get_ph_pointer(t_rhs);
    if (rhs == 0)
      return PROLOG_FAILURE;
    CHECK(lhs);
    CHECK(rhs);
    PPL::Polyhedron& x = *static_cast<PPL::Polyhedron*>(lhs);
    const PPL::Polyhedron& y = *static_cast<const PPL::Polyhedron*>(rhs);
    x.convex_difference_assign(y);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_convex_difference_assign_and_minimize(Prolog_term_ref t_lhs,
					  Prolog_term_ref t_rhs) {
  try {
    PPL::Polyhedron* lhs = get_ph_pointer(t_lhs);
    if (lhs == 0)
      return PROLOG_FAILURE;
    const PPL::Polyhedron* rhs = get_ph_pointer(t_rhs);
    if (rhs == 0)
      return PROLOG_FAILURE;
    CHECK(lhs);
    CHECK(rhs);
    PPL::Polyhedron& x = *static_cast<PPL::Polyhedron*>(lhs);
    const PPL::Polyhedron& y = *static_cast<const PPL::Polyhedron*>(rhs);
    x.convex_difference_assign_and_minimize(y);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_widening_assign(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  try {
    PPL::Polyhedron* lhs = get_ph_pointer(t_lhs);
    if (lhs == 0)
      return PROLOG_FAILURE;
    const PPL::Polyhedron* rhs = get_ph_pointer(t_rhs);
    if (rhs == 0)
      return PROLOG_FAILURE;
    CHECK(lhs);
    CHECK(rhs);
    PPL::Polyhedron& x = *static_cast<PPL::Polyhedron*>(lhs);
    const PPL::Polyhedron& y = *static_cast<const PPL::Polyhedron*>(rhs);
    x.widening_assign(y);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_limited_widening_assign(Prolog_term_ref t_lhs, 
                            Prolog_term_ref t_rhs, 
                            Prolog_term_ref t_clist) {
  try {
    PPL::Polyhedron* lhs = get_ph_pointer(t_lhs);
    if (lhs == 0)
      return PROLOG_FAILURE;
    const PPL::Polyhedron* rhs = get_ph_pointer(t_rhs);
    if (rhs == 0)
      return PROLOG_FAILURE;
    CHECK(lhs);
    CHECK(rhs);
    PPL::ConSys cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_constraint(c));
    }
    PPL::Polyhedron& x = *static_cast<PPL::Polyhedron*>(lhs);
    const PPL::Polyhedron& y = *static_cast<const PPL::Polyhedron*>(rhs);
    x.limited_widening_assign(y, cs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_get_constraints(Prolog_term_ref t_ph, Prolog_term_ref t_clist) {
  try {
    const PPL::Polyhedron* ph = get_ph_pointer(t_ph);
    if (ph == 0)
      return PROLOG_FAILURE;
    CHECK(ph);

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    const PPL::ConSys& cs = ph->constraints();
    for (PPL::ConSys::const_iterator i = cs.begin(),
	   cs_end = cs.end(); i != cs_end; ++i)
      Prolog_construct_cons(tail, constraint_term(*i), tail);

    if (Prolog_unify(t_clist, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_get_generators(Prolog_term_ref t_ph, Prolog_term_ref t_glist) {
  try {
    const PPL::Polyhedron* ph = get_ph_pointer(t_ph);
    if (ph == 0)
      return PROLOG_FAILURE;
    CHECK(ph);

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    const PPL::GenSys& gs = ph->generators();
    for (PPL::GenSys::const_iterator i = gs.begin(),
	   gs_end = gs.end(); i != gs_end; ++i)
      Prolog_construct_cons(tail, generator_term(*i), tail);

    if (Prolog_unify(t_glist, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_remove_dimensions(Prolog_term_ref t_ph, Prolog_term_ref t_vlist) {
  try {
    PPL::Polyhedron* ph = get_ph_pointer(t_ph);
    if (ph == 0)
      return PROLOG_FAILURE;
    CHECK(ph);
    std::set<PPL::Variable> dead_variables;
    Prolog_term_ref v = Prolog_new_term_ref();
    while (Prolog_is_cons(t_vlist)) {
      Prolog_get_cons(t_vlist, v, t_vlist);
      dead_variables.insert(get_variable(v));
    }
    ph->remove_dimensions(dead_variables);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_remove_higher_dimensions(Prolog_term_ref t_ph, Prolog_term_ref t_nd) {
  try {
    PPL::Polyhedron* ph = get_ph_pointer(t_ph);
    if (ph == 0)
      return PROLOG_FAILURE;
    CHECK(ph);
    ph->remove_higher_dimensions(term_to_unsigned_int(t_nd));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_add_dimensions_and_project(Prolog_term_ref t_ph, Prolog_term_ref t_nnd) {
  try {
    PPL::Polyhedron* ph = get_ph_pointer(t_ph);
    if (ph == 0)
      return PROLOG_FAILURE;
    CHECK(ph);
    ph->add_dimensions_and_project(term_to_unsigned_int(t_nnd));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_add_dimensions_and_embed(Prolog_term_ref t_ph, Prolog_term_ref t_nnd) {
  try {
    PPL::Polyhedron* ph = get_ph_pointer(t_ph);
    if (ph == 0)
      return PROLOG_FAILURE;
    CHECK(ph);
    ph->add_dimensions_and_embed(term_to_unsigned_int(t_nnd));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_init() {
  for (size_t i = 0; i < sizeof(prolog_atoms)/sizeof(prolog_atoms[0]); ++i) {
    Prolog_atom a = Prolog_atom_from_string(prolog_atoms[i].name);
    *prolog_atoms[i].p_atom = a;
  }
  return PROLOG_SUCCESS;
}

extern "C" Prolog_foreign_return_type
ppl_relation_with_constraint(Prolog_term_ref t_ph, Prolog_term_ref t_c,
			     Prolog_term_ref t_r) {
  try {
    PPL::Polyhedron* ph = get_ph_pointer(t_ph);
    if (ph == 0)
      return PROLOG_FAILURE;
    CHECK(ph);
    PPL::Poly_Con_Relation r = ph->relation_with(build_constraint(t_c));

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    Prolog_term_ref t_a = Prolog_new_term_ref();
    while (r != PPL::Poly_Con_Relation::nothing()) {
      if (r.implies(PPL::Poly_Con_Relation::is_disjoint())) {
	Prolog_put_atom(t_a, a_is_disjoint);
	r = r && !PPL::Poly_Con_Relation::is_disjoint();
      }
      else if (r.implies(PPL::Poly_Con_Relation::strictly_intersects())) {
	Prolog_put_atom(t_a, a_strictly_intersects);
	r = r && !PPL::Poly_Con_Relation::strictly_intersects();
      }
      else if (r.implies(PPL::Poly_Con_Relation::is_included())) {
	Prolog_put_atom(t_a, a_is_included);
	r = r && !PPL::Poly_Con_Relation::is_included();
      }
      else if (r.implies(PPL::Poly_Con_Relation::saturates())) {
	Prolog_put_atom(t_a, a_saturates);
	r = r && !PPL::Poly_Con_Relation::saturates();
      }
      Prolog_construct_cons(tail, t_a, tail);
    }
    if (Prolog_unify(t_r, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_relation_with_generator(Prolog_term_ref t_ph, Prolog_term_ref t_g,
			    Prolog_term_ref t_r) {
  try {
    PPL::Polyhedron* ph = get_ph_pointer(t_ph);
    if (ph == 0)
      return PROLOG_FAILURE;
    CHECK(ph);
    PPL::Poly_Gen_Relation r = ph->relation_with(build_generator(t_g));

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    Prolog_term_ref t_a = Prolog_new_term_ref();
    while (r != PPL::Poly_Gen_Relation::nothing()) {
      if (r.implies(PPL::Poly_Gen_Relation::subsumes())) {
	Prolog_put_atom(t_a, a_subsumes);
	r = r && !PPL::Poly_Gen_Relation::subsumes();
      }
      Prolog_construct_cons(tail, t_a, tail);
    }
    if (Prolog_unify(t_r, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_add_generators_and_minimize(Prolog_term_ref t_ph, 
				 Prolog_term_ref t_glist) {
  try {
    PPL::Polyhedron* ph = get_ph_pointer(t_ph);
    if (ph == 0)
      return PROLOG_FAILURE;
    CHECK(ph);
    PPL::GenSys gs;
    Prolog_term_ref g = Prolog_new_term_ref();

    while (Prolog_is_cons(t_glist)) {
      Prolog_get_cons(t_glist, g, t_glist);
      gs.insert(build_generator(g));
    }

    ph->add_generators_and_minimize(gs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_polyhedron_included_or_equal(Prolog_term_ref t_lhs, 
                                Prolog_term_ref t_rhs) {
  try {
    const PPL::Polyhedron* lhs = get_ph_pointer(t_lhs);
    if (lhs == 0)
      return PROLOG_FAILURE;
    const PPL::Polyhedron* rhs = get_ph_pointer(t_rhs);
    if (rhs == 0)
      return PROLOG_FAILURE;
    CHECK(lhs);
    CHECK(rhs);
    if (*lhs <= *rhs) 
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_polyhedron_equal(Prolog_term_ref t_lhs, 
                    Prolog_term_ref t_rhs) {
  try {
    const PPL::Polyhedron* lhs = get_ph_pointer(t_lhs);
    if (lhs == 0)
      return PROLOG_FAILURE;
    const PPL::Polyhedron* rhs = get_ph_pointer(t_rhs);
    if (rhs == 0)
      return PROLOG_FAILURE;
    CHECK(lhs);
    CHECK(rhs);
    if (*lhs == *rhs)
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_polyhedron_strictly_included(Prolog_term_ref t_lhs, 
                                 Prolog_term_ref t_rhs) {
  try {
    const PPL::Polyhedron* lhs = get_ph_pointer(t_lhs);
    if (lhs == 0)
      return PROLOG_FAILURE;
    const PPL::Polyhedron* rhs = get_ph_pointer(t_rhs);
    if (rhs == 0)
      return PROLOG_FAILURE;
    CHECK(lhs);
    CHECK(rhs);
    if (*lhs < *rhs)
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_affine_preimage(Prolog_term_ref t_ph, Prolog_term_ref t_v, 
                 Prolog_term_ref t_le, Prolog_term_ref t_d) {
  try {
    PPL::Polyhedron* ph = get_ph_pointer(t_ph);
    if (ph == 0)
      return PROLOG_FAILURE;
    CHECK(ph);
    Prolog_term_ref v = Prolog_new_term_ref();
    Prolog_get_arg(1, t_v, v);
    ph->affine_preimage(PPL::Variable(term_to_unsigned_int(v)), 
                     build_lin_expression(t_le),
                     &integer_term_to_Integer(t_d));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}

extern "C" Prolog_foreign_return_type
ppl_affine_image(Prolog_term_ref t_ph, Prolog_term_ref t_v, 
                 Prolog_term_ref t_le, Prolog_term_ref t_d) {
  try {
    PPL::Polyhedron* ph = get_ph_pointer(t_ph);
    if (ph == 0)
      return PROLOG_FAILURE;
    CHECK(ph);
    Prolog_term_ref v = Prolog_new_term_ref();
    Prolog_get_arg(1, t_v, v);
    ph->affine_image(PPL::Variable(term_to_unsigned_int(v)), 
                     build_lin_expression(t_le),
                     &integer_term_to_Integer(t_d));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
  return PROLOG_FAILURE;
}
