
#include "ppl_install.hh"
#include <exception>
#include <stdexcept>
#include <sstream>

namespace PPL = Parma_Polyhedra_Library;

#ifndef NDEBUG
#include <set>
#include <iostream>

class PolyTracker {
public:
  void insert(const void* pp);
  void check(const void* pp);
  void remove(const void* pp);

  PolyTracker();
  ~PolyTracker();

private:
  typedef std::set<const void*, std::less<const void*> > Set;
  Set s;
};

PolyTracker::PolyTracker() {
}

PolyTracker::~PolyTracker() {
  int n = s.size();
  std::cerr << n << " polyhedra leaked!" << std::endl;
}

void
PolyTracker::insert(const void* pp) {
  std::pair<Set::iterator, bool> stat = s.insert(pp);
  if (!stat.second)
    abort();
}

void
PolyTracker::check(const void* pp) {
  if (s.find(pp) == s.end())
    abort();
}

void
PolyTracker::remove(const void* pp) {
  if (s.erase(pp) != 1)
    abort();
}

static PolyTracker poly_tracker;

#define REGISTER(x) poly_tracker.insert(x)
#define UNREGISTER(x) poly_tracker.remove(x)
#define CHECK(x) poly_tracker.check(x)

#else

#define REGISTER(x)
#define UNREGISTER(x)
#define CHECK(x)

#endif

class internal_exception {
private:
  Prolog_term_ref tr;

public:
  explicit internal_exception(Prolog_term_ref t)
    : tr(t) {
  }

  virtual ~internal_exception() {
  }

  virtual Prolog_term_ref term() const {
    return tr;
  }
};

class integer_out_of_range : public internal_exception {
public:
  explicit integer_out_of_range(Prolog_term_ref t)
    : internal_exception(t) {
  }
};

class non_linear : public internal_exception {
private:
  const char* w;

public:
  explicit non_linear(const char* s, Prolog_term_ref t)
    : internal_exception(t), w(s) {
  }

  const char* who() const {
    return w;
  }
};

class not_an_integer : public internal_exception {
public:
  explicit not_an_integer(Prolog_term_ref t)
    : internal_exception(t) {
  }
};

class not_unsigned_int : public internal_exception {
public:
  explicit not_unsigned_int(Prolog_term_ref t)
    : internal_exception(t) {
  }
};

class not_a_variable : public internal_exception {
public:
  explicit not_a_variable(Prolog_term_ref t)
    : internal_exception(t) {
  }
};

static void
handle_exception(const integer_out_of_range& e) {
  Prolog_term_ref culprit = Prolog_new_term_ref();
  Prolog_term_ref arg_no = Prolog_new_term_ref();
  Prolog_term_ref expected_domain = Prolog_new_term_ref();
  Prolog_term_ref et = Prolog_new_term_ref();

  Prolog_put_term(culprit, e.term());
  Prolog_put_integer(arg_no, 1);
  {
    std::string s;
    std::ostringstream domain(s);
    domain << "[" << LONG_MIN << ", " << LONG_MAX << "]";
    Prolog_put_string(expected_domain, domain.str().c_str());
  }
  SP_cons_functor(et, SP_atom_from_string("integer_term_to_Integer"), 1,
		  culprit);
  SP_cons_functor(et, SP_atom_from_string("domain_error"), 4,
		  et, arg_no, expected_domain, culprit);
  SP_raise_exception(et);
}

static void
handle_exception(const not_unsigned_int& e) {
  Prolog_term_ref culprit = Prolog_new_term_ref();
  Prolog_term_ref arg_no = Prolog_new_term_ref();
  Prolog_term_ref expected_domain = Prolog_new_term_ref();
  Prolog_term_ref et = Prolog_new_term_ref();

  Prolog_put_term(culprit, e.term());
  Prolog_put_integer(arg_no, 1);
  {
    std::string s;
    std::ostringstream domain(s);
    domain << "[" << 0 << ", " << UINT_MAX << "]";
    Prolog_put_string(expected_domain, domain.str().c_str());
  }
  SP_cons_functor(et, SP_atom_from_string("get_size_t/term_to_varid"), 1,
		  culprit);
  SP_cons_functor(et, SP_atom_from_string("domain_error"), 4,
		  et, arg_no, expected_domain, culprit);
  SP_raise_exception(et);
}

static void
handle_exception(const non_linear& e) {
  Prolog_term_ref culprit = Prolog_new_term_ref();
  Prolog_term_ref arg_no = Prolog_new_term_ref();
  Prolog_term_ref expected_domain = Prolog_new_term_ref();
  Prolog_term_ref et = Prolog_new_term_ref();

  Prolog_put_term(culprit, e.term());
  Prolog_put_integer(arg_no, 1);
  Prolog_put_string(expected_domain, "linear expression or constraint");
  SP_cons_functor(et, SP_atom_from_string(e.who()), 1, culprit);
  SP_cons_functor(et, SP_atom_from_string("domain_error"), 4,
		  et, arg_no, expected_domain, culprit);
  SP_raise_exception(et);
}

static void
handle_exception(const not_a_variable& e) {
  Prolog_term_ref culprit = Prolog_new_term_ref();
  Prolog_term_ref arg_no = Prolog_new_term_ref();
  Prolog_term_ref expected_domain = Prolog_new_term_ref();
  Prolog_term_ref et = Prolog_new_term_ref();

  Prolog_put_term(culprit, e.term());
  Prolog_put_integer(arg_no, 1);
  Prolog_put_string(expected_domain, "$VAR(integer)");
  SP_cons_functor(et, SP_atom_from_string("get_variable"), 1, culprit);
  SP_cons_functor(et, SP_atom_from_string("domain_error"), 4,
		  et, arg_no, expected_domain, culprit);
  SP_raise_exception(et);
}

static void
handle_exception() {
  Prolog_term_ref et = Prolog_new_term_ref();
  Prolog_put_string(et, "PPL bug: unknown exception raised");
  SP_raise_exception(et);
}

static void
handle_exception(const std::exception& e) {
  Prolog_term_ref et = Prolog_new_term_ref();
  Prolog_put_string(et, e.what());
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

static Prolog_term_ref
integer_term(const PPL::Integer& n) {
  Prolog_term_ref t = Prolog_new_term_ref();
  // FIXME: handle the case where n does not fit into a signed long.
  if (!n.fits_slong_p())
    abort();
  Prolog_put_integer(t, n.get_si());
  return t;
}

static Prolog_term_ref
variable_term(unsigned int varid) {
  Prolog_term_ref v = Prolog_new_term_ref();
  Prolog_put_integer(v, varid);
  Prolog_term_ref t = Prolog_new_term_ref();
  SP_cons_functor(t, a_dollar_VAR, 1, v);
  return t;
}

extern "C" void
ppl_init(int /* when */) {
  for (size_t i = 0; i < sizeof(sp_atoms)/sizeof(sp_atoms[0]); ++i) {
    SP_atom a = SP_atom_from_string(sp_atoms[i].name);
    if (SP_register_atom(a) == 0) {
      Prolog_term_ref et = Prolog_new_term_ref();
      Prolog_put_string(et, "Cannot initialize the PPL interface");
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
    Prolog_term_ref n_term = Prolog_new_term_ref();
    Prolog_put_integer(n_term, n);
    throw not_unsigned_int(n_term);
  }
}

extern "C" void*
ppl_new_polyhedron(long num_dimensions) {
  try {
    PPL::Polyhedron* ret = new PPL::Polyhedron(get_size_t(num_dimensions));
    REGISTER(ret);
    return ret;
  }
  CATCH_ALL;
  return 0;
}

extern "C" void*
ppl_new_empty_polyhedron(long num_dimensions) {
  try {
    PPL::Polyhedron* ret = new PPL::Polyhedron(get_size_t(num_dimensions),
					       PPL::Polyhedron::EMPTY);
    REGISTER(ret);
    return ret;
  }
  CATCH_ALL;
  return 0;
}

extern "C" void*
ppl_copy_polyhedron(const void* pp) {
  try {
    CHECK(pp);
    PPL::Polyhedron* ret
      = new PPL::Polyhedron(*static_cast<const PPL::Polyhedron*>(pp));
    REGISTER(ret);
    return ret;
  }
  CATCH_ALL;
  return 0;
}

extern "C" void
ppl_delete_polyhedron(void* pp) {
  // If destructors throw it is a catastrophy.
  // Anyway...
  try {
    UNREGISTER(pp);
    delete static_cast<PPL::Polyhedron*>(pp);
  }
  CATCH_PPL;
}

extern "C" long
ppl_space_dimension(const void* pp) {
  CHECK(pp);
  // Polyhedron::space_dimension() cannot throw.
  return static_cast<const PPL::Polyhedron*>(pp)->space_dimension();
}


static PPL::Integer
integer_term_to_Integer(Prolog_term_ref t) {
  // FIXME: how can we get the unlimited precision integer?
  assert(SP_is_integer(t));
  long v;
  if (SP_get_integer(t, &v))
    return PPL::Integer(v);
  else
    throw integer_out_of_range(t);
}

static unsigned int
term_to_varid(Prolog_term_ref t) {
  if (SP_is_integer(t)) {
    long v;
    if (SP_get_integer(t, &v))
      if (v >= 0)
	return v;
  }
  throw not_unsigned_int(t);
}

static PPL::LinExpression
build_lin_expression(Prolog_term_ref t) {
  if (SP_is_integer(t))
    return PPL::LinExpression(integer_term_to_Integer(t));
  else if (SP_is_compound(t)) {
    SP_atom functor;
    int arity;
    SP_get_functor(t, &functor, &arity);
    switch (arity) {
    case 1:
      {
	Prolog_term_ref arg = Prolog_new_term_ref();
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
	Prolog_term_ref arg1 = Prolog_new_term_ref();
	Prolog_term_ref arg2 = Prolog_new_term_ref();
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
build_constraint(Prolog_term_ref t) {
  if (SP_is_compound(t)) {
    SP_atom functor;
    int arity;
    SP_get_functor(t, &functor, &arity);
    if (arity == 2) {
      Prolog_term_ref arg1 = Prolog_new_term_ref();
      Prolog_term_ref arg2 = Prolog_new_term_ref();
      SP_get_arg(1, t, arg1);
      SP_get_arg(2, t, arg2);
      if (functor == a_equal)
	// =
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
ppl_insert_constraint(void* pp, Prolog_term_ref t) {
  try {
    CHECK(pp);
    static_cast<PPL::Polyhedron*>(pp)->insert(build_constraint(t));
  }
  CATCH_ALL;
}

extern "C" long
ppl_add_constraints_and_minimize(void* pp, Prolog_term_ref constraints_list) {
  try {
    CHECK(pp);
    PPL::ConSys cs;
    Prolog_term_ref c = Prolog_new_term_ref();
    while (SP_is_list(constraints_list)) {
      SP_get_list(constraints_list, c, constraints_list);
      cs.insert(build_constraint(c));
    }
    PPL::Polyhedron& ph = *static_cast<PPL::Polyhedron*>(pp);
    return ph.add_constraints_and_minimize(cs) ? 1 : 0;
  }
  CATCH_ALL;
  return -1;
}

static PPL::Generator
build_generator(Prolog_term_ref t) {
  if (SP_is_compound(t)) {
    SP_atom functor;
    int arity;
    SP_get_functor(t, &functor, &arity);
    if (arity == 1) {
      Prolog_term_ref arg = Prolog_new_term_ref();
      SP_get_arg(1, t, arg);
      if (functor == a_line)
	return line(build_lin_expression(arg));
      else if (functor == a_ray)
	return ray(build_lin_expression(arg));
      else if (functor == a_vertex)
	return vertex(build_lin_expression(arg));
    }
    else if (arity == 2) {
      Prolog_term_ref arg1 = Prolog_new_term_ref();
      Prolog_term_ref arg2 = Prolog_new_term_ref();
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
ppl_insert_generator(void* pp, Prolog_term_ref t) {
  try {
    CHECK(pp);
    static_cast<PPL::Polyhedron*>(pp)->insert(build_generator(t));
  }
  CATCH_ALL;
}

extern "C" long
ppl_check_empty(const void* pp) {
  try {
    CHECK(pp);
    return static_cast<const PPL::Polyhedron*>(pp)->check_empty() ? 1 : 0;
  }
  CATCH_ALL;
  return -1;
}

extern "C" void
ppl_intersection_assign(void* pp_lhs, const void* pp_rhs) {
  try {
    CHECK(pp_lhs);
    CHECK(pp_rhs);
    PPL::Polyhedron& x = *static_cast<PPL::Polyhedron*>(pp_lhs);
    const PPL::Polyhedron& y = *static_cast<const PPL::Polyhedron*>(pp_rhs);
    x.intersection_assign(y);
  }
  CATCH_ALL;
}

extern "C" void
ppl_convex_hull_assign(void* pp_lhs, const void* pp_rhs) {
  try {
    CHECK(pp_lhs);
    CHECK(pp_rhs);
    PPL::Polyhedron& x = *static_cast<PPL::Polyhedron*>(pp_lhs);
    const PPL::Polyhedron& y = *static_cast<const PPL::Polyhedron*>(pp_rhs);
    x.convex_hull_assign_and_minimize(y);
  }
  CATCH_ALL;
}

extern "C" void
ppl_widening_assign(void* pp_lhs, const void* pp_rhs) {
  try {
    CHECK(pp_lhs);
    CHECK(pp_rhs);
    static_cast<PPL::Polyhedron*>(pp_lhs)
      ->widening_assign(*static_cast<const PPL::Polyhedron*>(pp_rhs));
  }
  CATCH_ALL;
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
    Prolog_put_integer(so_far, 0);
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
	Prolog_term_ref addendum = Prolog_new_term_ref();
	SP_cons_functor(addendum, a_asterisk, 2,
			integer_term(coefficient), variable_term(varid));
	Prolog_term_ref new_so_far = Prolog_new_term_ref();
	SP_cons_functor(new_so_far, a_plus, 2,
			so_far, addendum);
	so_far = new_so_far;
      }
    }
  }
  return so_far;
}

static Prolog_term_ref
constraint_term(const PPL::Constraint& c) {
  SP_atom relation = c.is_equality() ? a_equal : a_greater_than_equal;
  Prolog_term_ref t = Prolog_new_term_ref();
  SP_cons_functor(t, relation, 2,
		  get_lin_expression(c), integer_term(-c.coefficient()));
  return t;
}

extern "C" void
ppl_get_constraints(const void* pp, Prolog_term_ref constraints_list) {
  try {
    CHECK(pp);
    Prolog_term_ref tail = Prolog_new_term_ref();
    SP_put_atom(tail, a_nil);

    const PPL::Polyhedron& ph = *static_cast<const PPL::Polyhedron*>(pp);
    const PPL::ConSys& cs = ph.constraints();

    for (PPL::ConSys::const_iterator i = cs.begin(),
	   cs_end = cs.end(); i != cs_end; ++i) {
      Prolog_term_ref new_tail = Prolog_new_term_ref();
      SP_cons_list(new_tail, constraint_term(*i), tail);
      tail = new_tail;
    }

    Prolog_put_term(constraints_list, tail);
  }
  CATCH_ALL;
}

static Prolog_term_ref
generator_term(const PPL::Generator& g) {
  Prolog_term_ref t = Prolog_new_term_ref();
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
ppl_get_generators(const void* pp, Prolog_term_ref generators_list) {
  try {
    CHECK(pp);
    Prolog_term_ref tail = Prolog_new_term_ref();
    SP_put_atom(tail, a_nil);

    const PPL::Polyhedron& ph = *static_cast<const PPL::Polyhedron*>(pp);
    const PPL::GenSys& gs = ph.generators();

    for (PPL::GenSys::const_iterator i = gs.begin(),
	   gs_end = gs.end(); i != gs_end; ++i) {
      Prolog_term_ref new_tail = Prolog_new_term_ref();
      SP_cons_list(new_tail, generator_term(*i), tail);
      tail = new_tail;
    }

    Prolog_put_term(generators_list, tail);
  }
  CATCH_ALL;
}

static PPL::Variable
get_variable(Prolog_term_ref t) {
  if (SP_is_compound(t)) {
    SP_atom functor;
    int arity;
    SP_get_functor(t, &functor, &arity);
    if (functor == a_dollar_VAR && arity == 1) {
      Prolog_term_ref arg = Prolog_new_term_ref();
      SP_get_arg(1, t, arg);
      return PPL::Variable(term_to_varid(arg));
    }
  }
  throw not_a_variable(t);
}


extern "C" void
ppl_remove_dimensions(void* pp, Prolog_term_ref variables_list) {
  try {
    CHECK(pp);
    std::set<PPL::Variable> dead_variables;
    Prolog_term_ref v = Prolog_new_term_ref();
    while (SP_is_list(variables_list)) {
      SP_get_list(variables_list, v, variables_list);
      dead_variables.insert(get_variable(v));
    }
    static_cast<PPL::Polyhedron*>(pp)->remove_dimensions(dead_variables);
  }
  CATCH_ALL;
}


extern "C" void
ppl_remove_higher_dimensions(void* pp, long new_dimension) {
  try {
    CHECK(pp);
    static_cast<PPL::Polyhedron*>(pp)
      ->remove_higher_dimensions(get_size_t(new_dimension));
  }
  CATCH_ALL;
}


extern "C" void
ppl_add_dimensions_and_project(void* pp, long num_new_dimensions) {
  try {
    CHECK(pp);
    static_cast<PPL::Polyhedron*>(pp)
      ->add_dimensions_and_project(get_size_t(num_new_dimensions));
  }
  CATCH_ALL;
}

extern "C" void
ppl_add_dimensions_and_embed(void* pp, long num_new_dimensions) {
  try {
    CHECK(pp);
    static_cast<PPL::Polyhedron*>(pp)
      ->add_dimensions_and_embed(get_size_t(num_new_dimensions));
  }
  CATCH_ALL;
}
