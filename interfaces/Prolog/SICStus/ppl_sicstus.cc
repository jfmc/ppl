
#include "ppl_install.hh"
#include <sicstus/sicstus.h>
#include <exception>
#include <stdexcept>
#include <sstream>

namespace PPL = Parma_Polyhedra_Library;

class internal_exception {
private:
  SP_term_ref tr;

public:
  explicit internal_exception(SP_term_ref t)
    : tr(t) {
  }

  virtual ~internal_exception() {
  }

  virtual SP_term_ref term() const {
    return tr;
  }
};

class integer_out_of_range : public internal_exception {
public:
  explicit integer_out_of_range(SP_term_ref t)
    : internal_exception(t) {
  }
};

class non_linear : public internal_exception {
private:
  const char* w;

public:
  explicit non_linear(const char* s, SP_term_ref t)
    : internal_exception(t), w(s) {
  }

  const char* who() const {
    return w;
  }
};

class not_an_integer : public internal_exception {
public:
  explicit not_an_integer(SP_term_ref t)
    : internal_exception(t) {
  }
};

class not_unsigned_int : public internal_exception {
public:
  explicit not_unsigned_int(SP_term_ref t)
    : internal_exception(t) {
  }
};

class not_a_variable : public internal_exception {
public:
  explicit not_a_variable(SP_term_ref t)
    : internal_exception(t) {
  }
};

static void
handle_exception(const integer_out_of_range& e) {
  SP_term_ref culprit = SP_new_term_ref();
  SP_term_ref arg_no = SP_new_term_ref();
  SP_term_ref expected_domain = SP_new_term_ref();
  SP_term_ref et = SP_new_term_ref();

  SP_put_term(culprit, e.term());
  SP_put_integer(arg_no, 1);
  {
    std::string s;
    std::ostringstream domain(s);
    domain << "[" << LONG_MIN << ", " << LONG_MAX << "]";
    SP_put_string(expected_domain, domain.str().c_str());
  }
  SP_cons_functor(et, SP_atom_from_string("integer_term_to_Integer"), 1,
		  culprit);
  SP_cons_functor(et, SP_atom_from_string("domain_error"), 4,
		  et, arg_no, expected_domain, culprit);
  SP_raise_exception(et);
}

static void
handle_exception(const not_unsigned_int& e) {
  SP_term_ref culprit = SP_new_term_ref();
  SP_term_ref arg_no = SP_new_term_ref();
  SP_term_ref expected_domain = SP_new_term_ref();
  SP_term_ref et = SP_new_term_ref();

  SP_put_term(culprit, e.term());
  SP_put_integer(arg_no, 1);
  {
    std::string s;
    std::ostringstream domain(s);
    domain << "[" << 0 << ", " << UINT_MAX << "]";
    SP_put_string(expected_domain, domain.str().c_str());
  }
  SP_cons_functor(et, SP_atom_from_string("get_size_t/term_to_varid"), 1,
		  culprit);
  SP_cons_functor(et, SP_atom_from_string("domain_error"), 4,
		  et, arg_no, expected_domain, culprit);
  SP_raise_exception(et);
}

static void
handle_exception(const non_linear& e) {
  SP_term_ref culprit = SP_new_term_ref();
  SP_term_ref arg_no = SP_new_term_ref();
  SP_term_ref expected_domain = SP_new_term_ref();
  SP_term_ref et = SP_new_term_ref();

  SP_put_term(culprit, e.term());
  SP_put_integer(arg_no, 1);
  SP_put_string(expected_domain, "linear expression or constraint");
  SP_cons_functor(et, SP_atom_from_string(e.who()), 1, culprit);
  SP_cons_functor(et, SP_atom_from_string("domain_error"), 4,
		  et, arg_no, expected_domain, culprit);
  SP_raise_exception(et);
}

static void
handle_exception(const not_a_variable& e) {
  SP_term_ref culprit = SP_new_term_ref();
  SP_term_ref arg_no = SP_new_term_ref();
  SP_term_ref expected_domain = SP_new_term_ref();
  SP_term_ref et = SP_new_term_ref();

  SP_put_term(culprit, e.term());
  SP_put_integer(arg_no, 1);
  SP_put_string(expected_domain, "$VAR(integer)");
  SP_cons_functor(et, SP_atom_from_string("get_variable"), 1, culprit);
  SP_cons_functor(et, SP_atom_from_string("domain_error"), 4,
		  et, arg_no, expected_domain, culprit);
  SP_raise_exception(et);
}

static void
handle_exception() {
  SP_term_ref et = SP_new_term_ref();
  SP_put_string(et, "PPL bug: unknown exception raised");
  SP_raise_exception(et);
}

static void
handle_exception(const std::exception& e) {
  SP_term_ref et = SP_new_term_ref();
  SP_put_string(et, e.what());
  SP_raise_exception(et);
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
static SP_atom a_nil;

// For variables.
static SP_atom a_dollar_VAR;

// For linear expressions.
static SP_atom a_plus;
static SP_atom a_minus;
static SP_atom a_asterisk;

// For constraints.
static SP_atom a_equal;
static SP_atom a_greater_than_equal;
static SP_atom a_equal_less_than;

// For generators.
static SP_atom a_line;
static SP_atom a_ray;
static SP_atom a_vertex;

static struct {
  SP_atom* p_atom;
  const char* name;
} const sp_atoms[] = {
  { &a_nil,                "[]" },

  { &a_dollar_VAR,         "$VAR" },

  { &a_plus,               "+" },
  { &a_minus,              "-" },
  { &a_asterisk,           "*" },

  { &a_equal,              "=" },
  { &a_greater_than_equal, ">=" },
  { &a_equal_less_than,    "=<" },

  { &a_line,               "line" },
  { &a_ray,                "ray" },
  { &a_vertex,             "vertex" },
};

static SP_term_ref
integer_term(const PPL::Integer& n) {
  SP_term_ref t = SP_new_term_ref();
  // FIXME: handle the case where n does not fit into a signed long.
  SP_put_integer(t, to_slong(n));
  return t;
}

static SP_term_ref
variable_term(unsigned int varid) {
  SP_term_ref v = SP_new_term_ref();
  SP_put_integer(v, varid);
  SP_term_ref t = SP_new_term_ref();
  SP_cons_functor(t, a_dollar_VAR, 1, v);
  return t;
}

extern "C" void
ppl_init(int /* when */) {
  for (size_t i = 0; i < sizeof(sp_atoms)/sizeof(sp_atoms[0]); ++i) {
    SP_atom a = SP_atom_from_string(sp_atoms[i].name);
    if (SP_register_atom(a) == 0) {
      SP_term_ref et = SP_new_term_ref();
      SP_put_string(et, "Cannot initialize the PPL interface");
      SP_raise_exception(et);
      return;
    }
    *sp_atoms[i].p_atom = a;
  }
}

extern "C" void
ppl_deinit(int /* when */) {
  for (size_t i = 0; i < sizeof(sp_atoms)/sizeof(sp_atoms[0]); ++i)
    // SP_unregister_atom can fail.
    // We ignore such failures: what else can we do?
    (void) SP_unregister_atom(*sp_atoms[i].p_atom);
}

static size_t
get_size_t(long n) {
  if (n >= 0)
    return n;
  else {
    SP_term_ref n_term = SP_new_term_ref();
    SP_put_integer(n_term, n);
    throw not_unsigned_int(n_term);
  }
}

extern "C" void*
ppl_new_polyhedron(long num_dimensions) {
  try {
    return new PPL::Polyhedron(get_size_t(num_dimensions));
  }
  CATCH_ALL;
  return 0;
}

extern "C" void*
ppl_new_empty_polyhedron(long num_dimensions) {
  try {
    return new PPL::Polyhedron(get_size_t(num_dimensions),
			       PPL::Polyhedron::EMPTY);
  }
  CATCH_ALL;
  return 0;
}

extern "C" void
ppl_delete_polyhedron(void* pp) {
  // If destructors throw it is a catastrophy.
  // Anyway...
  try {
    delete static_cast<PPL::Polyhedron*>(pp);
  }
  CATCH_PPL;
}

extern "C" long
ppl_space_dimension(const void* pp) {
  // Polyhedron::space_dimension() cannot throw.
  return static_cast<const PPL::Polyhedron*>(pp)->space_dimension();
}


static PPL::Integer
integer_term_to_Integer(SP_term_ref t) {
  // FIXME: how can we get the unlimited precision integer?
  assert(SP_is_integer(t));
  long v;
  if (SP_get_integer(t, &v))
    return PPL::Integer(v);
  else
    throw integer_out_of_range(t);
}

static unsigned int
term_to_varid(SP_term_ref t) {
  if (SP_is_integer(t)) {
    long v;
    if (SP_get_integer(t, &v))
      if (v >= 0)
	return v;
  }
  throw not_unsigned_int(t);
}

static PPL::LinExpression
build_lin_expression(SP_term_ref t) {
  if (SP_is_integer(t))
    return PPL::LinExpression(integer_term_to_Integer(t));
  else if (SP_is_compound(t)) {
    SP_atom functor;
    int arity;
    SP_get_functor(t, &functor, &arity);
    switch (arity) {
    case 1:
      {
	SP_term_ref arg = SP_new_term_ref();
	SP_get_arg(1, t, arg);
	if (functor == a_minus)
	  // Unary minus.
	  return -build_lin_expression(arg);
	else if (functor == a_dollar_VAR)
	  // Variable.
	  return PPL::Variable(term_to_varid(arg));
      }
      break;
    case 2:
      {
	SP_term_ref arg1 = SP_new_term_ref();
	SP_term_ref arg2 = SP_new_term_ref();
	SP_get_arg(1, t, arg1);
	SP_get_arg(2, t, arg2);
	if (functor == a_plus)
	  // Plus.
	  if (SP_is_integer(arg1))
	    return integer_term_to_Integer(arg1) + build_lin_expression(arg2);
	  else if (SP_is_integer(arg2))
	    return build_lin_expression(arg1) + integer_term_to_Integer(arg2);
	  else
	    return build_lin_expression(arg1) + build_lin_expression(arg2);
	else if (functor == a_minus)
	  // Minus.
	  if (SP_is_integer(arg1))
	    return integer_term_to_Integer(arg1) - build_lin_expression(arg2);
	  else if (SP_is_integer(arg2))
	    return build_lin_expression(arg1) - integer_term_to_Integer(arg2);
	  else
	    return build_lin_expression(arg1) - build_lin_expression(arg2);
	else if (functor == a_asterisk)
	  // Times.
	  if (SP_is_integer(arg1))
	    return integer_term_to_Integer(arg1) * build_lin_expression(arg2);
	  else if (SP_is_integer(arg2))
	    return build_lin_expression(arg1) * integer_term_to_Integer(arg2);
      }
    }
  }
  // Invalid.
  throw non_linear("build_lin_expression", t);
}


static PPL::Constraint
build_constraint(SP_term_ref t) {
  if (SP_is_compound(t)) {
    SP_atom functor;
    int arity;
    SP_get_functor(t, &functor, &arity);
    if (arity == 2) {
      SP_term_ref arg1 = SP_new_term_ref();
      SP_term_ref arg2 = SP_new_term_ref();
      SP_get_arg(1, t, arg1);
      SP_get_arg(2, t, arg2);
      if (functor == a_equal)
	// ==
	if (SP_is_integer(arg1))
	  return integer_term_to_Integer(arg1) == build_lin_expression(arg2);
	else if (SP_is_integer(arg2))
	  return build_lin_expression(arg1) == integer_term_to_Integer(arg2);
	else
	  return build_lin_expression(arg1) == build_lin_expression(arg2);
      else if (functor == a_equal_less_than)
	// =<
	if (SP_is_integer(arg1))
	  return integer_term_to_Integer(arg1) <= build_lin_expression(arg2);
	else if (SP_is_integer(arg2))
	  return build_lin_expression(arg1) <= integer_term_to_Integer(arg2);
	else
	  return build_lin_expression(arg1) <= build_lin_expression(arg2);
      else if (functor == a_greater_than_equal)
	// >=
	if (SP_is_integer(arg1))
	  return integer_term_to_Integer(arg1) >= build_lin_expression(arg2);
	else if (SP_is_integer(arg2))
	  return build_lin_expression(arg1) >= integer_term_to_Integer(arg2);
	else
	  return build_lin_expression(arg1) >= build_lin_expression(arg2);
    }
  }
  // Invalid.
  throw non_linear("build_constraint", t);
}

extern "C" void
ppl_insert_constraint(void* pp, SP_term_ref t) {
  try {
    static_cast<PPL::Polyhedron*>(pp)->insert(build_constraint(t));
  }
  CATCH_ALL;
}

static PPL::Generator
build_generator(SP_term_ref t) {
  if (SP_is_compound(t)) {
    SP_atom functor;
    int arity;
    SP_get_functor(t, &functor, &arity);
    if (arity == 1) {
      SP_term_ref arg = SP_new_term_ref();
      SP_get_arg(1, t, arg);
      if (functor == a_line)
	return line(build_lin_expression(arg));
      else if (functor == a_ray)
	return ray(build_lin_expression(arg));
      else if (functor == a_vertex)
	return vertex(build_lin_expression(arg));
    }
    else if (arity == 2) {
      SP_term_ref arg1 = SP_new_term_ref();
      SP_term_ref arg2 = SP_new_term_ref();
      SP_get_arg(1, t, arg1);
      SP_get_arg(2, t, arg2);
      if (functor == a_vertex)
	if (SP_is_integer(arg2))
	  return vertex(build_lin_expression(arg1),
			integer_term_to_Integer(arg2));
    }
  }
  // Invalid.
  throw non_linear("build_generator", t);
}

extern "C" void
ppl_insert_generator(void* pp, SP_term_ref t) {
  try {
    static_cast<PPL::Polyhedron*>(pp)->insert(build_generator(t));
  }
  CATCH_ALL;
}

extern "C" long
ppl_check_empty(const void* pp) {
  try {
    return static_cast<const PPL::Polyhedron*>(pp)->check_empty() ? 1 : 0;
  }
  CATCH_ALL;
  return -1;
}

extern "C" void
ppl_intersection_assign(void* pp_lhs, const void* pp_rhs) {
  try {
    static_cast<PPL::Polyhedron*>(pp_lhs)
      ->intersection_assign(*static_cast<const PPL::Polyhedron*>(pp_rhs));
  }
  CATCH_ALL;
}

extern "C" void
ppl_convex_hull_assign(void* pp_lhs, const void* pp_rhs) {
  try {
    static_cast<PPL::Polyhedron*>(pp_lhs)
      ->convex_hull_assign(*static_cast<const PPL::Polyhedron*>(pp_rhs));
  }
  CATCH_ALL;
}

extern "C" void
ppl_widening_assign(void* pp_lhs, const void* pp_rhs) {
  try {
    static_cast<PPL::Polyhedron*>(pp_lhs)
      ->widening_assign(*static_cast<const PPL::Polyhedron*>(pp_rhs));
  }
  CATCH_ALL;
}

template <class R>
static SP_term_ref
get_lin_expression(const R& r) {
  SP_term_ref so_far = SP_new_term_ref();
  PPL::Integer coefficient;
  unsigned int varid = 0;
  unsigned int space_dimension = r.space_dimension();
  while (varid < space_dimension
	 && (coefficient = r.coefficient(PPL::Variable(varid))) == 0)
    ++varid;
  if (varid >= space_dimension) {
    SP_put_integer(so_far, 0);
  }
  else {
    SP_cons_functor(so_far, a_asterisk, 2,
		    integer_term(coefficient), variable_term(varid));
    while (true) {
      ++varid;
      while (varid < space_dimension
	     && (coefficient = r.coefficient(PPL::Variable(varid))) == 0)
	++varid;
      if (varid >= space_dimension)
	break;
      else {
	SP_term_ref addendum = SP_new_term_ref();
	SP_cons_functor(addendum, a_asterisk, 2,
			integer_term(coefficient), variable_term(varid));
	SP_term_ref new_so_far = SP_new_term_ref();
	SP_cons_functor(new_so_far, a_plus, 2,
			so_far, addendum);
	so_far = new_so_far;
      }
    }
  }
  return so_far;
}

static SP_term_ref
false_constraint_term() {
  SP_term_ref zero_times_x = SP_new_term_ref();
  SP_cons_functor(zero_times_x, a_asterisk, 2,
		  integer_term(0), variable_term(0));
  SP_term_ref t = SP_new_term_ref();
  SP_cons_functor(t, a_equal, 2,
		  zero_times_x, integer_term(1));
  return t;
}

static SP_term_ref
constraint_term(const PPL::Constraint& c) {
  SP_atom relation = c.is_equality() ? a_equal : a_greater_than_equal;
  SP_term_ref t = SP_new_term_ref();
  SP_cons_functor(t, relation, 2,
		  get_lin_expression(c), integer_term(-c.coefficient()));
  return t;
}

extern "C" void
ppl_get_constraints(const void* pp, SP_term_ref constraints_list) {
  try {
    SP_term_ref tail = SP_new_term_ref();
    SP_put_atom(tail, a_nil);

    const PPL::Polyhedron& ph = *static_cast<const PPL::Polyhedron*>(pp);

    if (ph.check_empty()) {
      SP_term_ref new_tail = SP_new_term_ref();
      SP_cons_list(new_tail, false_constraint_term(), tail);
      tail = new_tail;
    }
    else {
      const PPL::ConSys& cs = ph.constraints();
      PPL::ConSys::const_iterator i = cs.begin();
      PPL::ConSys::const_iterator cs_end = cs.end();
      while (i != cs_end) {
	const PPL::Constraint& c = *i++;
	SP_term_ref new_tail = SP_new_term_ref();
	SP_cons_list(new_tail, constraint_term(c), tail);
	tail = new_tail;
      }
    }
    SP_put_term(constraints_list, tail);
  }
  CATCH_ALL;
}

static SP_term_ref
get_generator(const PPL::Generator& g) {
  SP_term_ref t = SP_new_term_ref();
  SP_atom constructor;
  switch (g.type()) {
  case PPL::Generator::LINE:
    constructor = a_line;
    break;
  case PPL::Generator::RAY:
    constructor = a_ray;
    break;
  case PPL::Generator::VERTEX:
    {
      constructor = a_vertex;
      const PPL::Integer& divisor = g.divisor();
      if (divisor == 1)
	break;
      else {
	SP_cons_functor(t, constructor, 2,
			get_lin_expression(g), integer_term(divisor));
	return t;
      }
    }
  default:
    abort();
  }
  SP_cons_functor(t, constructor, 1, get_lin_expression(g));
  return t;
}

extern "C" void
ppl_get_generators(const void* pp, SP_term_ref generators_list) {
  try {
    SP_term_ref tail = SP_new_term_ref();
    SP_put_atom(tail, a_nil);

    const PPL::Polyhedron& ph = *static_cast<const PPL::Polyhedron*>(pp);

    if (ph.space_dimension() == 0) {
      // FIXME: what is the right thing to do?
      abort();
    }
    else {
      const PPL::GenSys& gs = ph.generators();
      PPL::GenSys::const_iterator i = gs.begin();
      PPL::GenSys::const_iterator gs_end = gs.end();
      while (i != gs_end) {
	const PPL::Generator& g = *i++;
	SP_term_ref new_tail = SP_new_term_ref();
	SP_cons_list(new_tail, get_generator(g), tail);
	tail = new_tail;
      }
    }
    SP_put_term(generators_list, tail);
  }
  CATCH_ALL;
}

static PPL::Variable
get_variable(SP_term_ref t) {
  if (SP_is_compound(t)) {
    SP_atom functor;
    int arity;
    SP_get_functor(t, &functor, &arity);
    if (functor == a_dollar_VAR && arity == 1) {
      SP_term_ref arg = SP_new_term_ref();
      SP_get_arg(1, t, arg);
      return PPL::Variable(term_to_varid(arg));
    }
  }
  throw not_a_variable(t);
}

extern "C" void
ppl_remove_dimensions(void* pp, SP_term_ref variables_list) {
  try {
    std::set<PPL::Variable> dead_variables;
    SP_term_ref v = SP_new_term_ref();
    while (SP_is_list(variables_list)) {
      SP_get_list(variables_list, v, variables_list);
      dead_variables.insert(get_variable(v));
    }
    static_cast<PPL::Polyhedron*>(pp)->remove_dimensions(dead_variables);
  }
  CATCH_ALL;
}

extern "C" void
ppl_add_dimensions_and_project(void* pp, long num_new_dimensions) {
  try {
    static_cast<PPL::Polyhedron*>(pp)
      ->add_dimensions_and_project(get_size_t(num_new_dimensions));
  }
  CATCH_ALL;
}

extern "C" void
ppl_add_dimensions_and_embed(void* pp, long num_new_dimensions) {
  try {
    static_cast<PPL::Polyhedron*>(pp)
      ->add_dimensions_and_embed(get_size_t(num_new_dimensions));
  }
  CATCH_ALL;
}
