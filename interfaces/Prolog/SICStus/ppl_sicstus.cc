
#include "ppl_install.hh"
#include <sicstus/sicstus.h>
#include <exception>
#include <stdexcept>
#include <sstream>

#include <iostream>

//int SP_put_address(SP_term_ref t, void *pointer) 
//void SP_raise_exception(SP_term_ref t)

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
  const char* ws;

public:
  explicit non_linear(const char* w, SP_term_ref t)
    : internal_exception(t), ws(w) {
  }

  const char* who() const {
    return ws;
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
    string s;
    ostringstream domain(s);
    domain << "[" << LONG_MIN << ", " << LONG_MAX << "]";
    SP_put_string(expected_domain, domain.str().c_str());
  }
  SP_cons_functor(et, SP_atom_from_string("get_integer"), 1, culprit);
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
handle_exception() {
}

static void
handle_exception(const std::exception& /* e */) {
}

#define CATCH_INTERNAL \
  catch (const integer_out_of_range& e) { \
    handle_exception(e); \
  } \
  catch (const non_linear& e) { \
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

// For variables.
static SP_atom a_dollar_VAR;

// For linear expressions.
static SP_atom a_plus;
static SP_atom a_minus;
static SP_atom a_asterisk;

// For constraints.
static SP_atom a_equal_equal;
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
  { &a_dollar_VAR,         "$VAR" },

  { &a_plus,               "+" },
  { &a_minus,              "-" },
  { &a_asterisk,           "*" },

  { &a_equal_equal,        "==" },
  { &a_greater_than_equal, ">=" },
  { &a_equal_less_than,    "=<" },

  { &a_line,               "line" },
  { &a_ray,                "ray" },
  { &a_vertex,             "vertex" },
};

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

extern "C" void*
ppl_new_polyhedron() {
  try {
    return new PPL::Polyhedron();
  }
  CATCH_PPL;
  abort();  // This is only to avoid a gcc warning.
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

static long
get_integer(SP_term_ref t) {
  assert(SP_is_integer(t));
  long v;
  if (SP_get_integer(t, &v))
    return v;
  else
    throw integer_out_of_range(t);
}

static PPL::LinExpression
build_lin_expression(SP_term_ref t) {
  if (SP_is_integer(t))
    return PPL::LinExpression(get_integer(t));
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
	  return PPL::Variable(get_integer(arg));
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
	    return get_integer(arg1) + build_lin_expression(arg2);
	  else if (SP_is_integer(arg2))
	    return build_lin_expression(arg1) + get_integer(arg2);
	  else
	    return build_lin_expression(arg1) + build_lin_expression(arg2);
	else if (functor == a_minus)
	  // Minus.
	  if (SP_is_integer(arg1))
	    return get_integer(arg1) - build_lin_expression(arg2);
	  else if (SP_is_integer(arg2))
	    return build_lin_expression(arg1) - get_integer(arg2);
	  else
	    return build_lin_expression(arg1) - build_lin_expression(arg2);
	else if (functor == a_asterisk)
	  // Times.
	  if (SP_is_integer(arg1))
	    return get_integer(arg1) * build_lin_expression(arg2);
	  else if (SP_is_integer(arg2))
	    return build_lin_expression(arg1) * get_integer(arg2);
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
      if (functor == a_equal_equal)
	// ==
	if (SP_is_integer(arg1))
	  return get_integer(arg1) == build_lin_expression(arg2);
	else if (SP_is_integer(arg2))
	  return build_lin_expression(arg1) == get_integer(arg2);
	else
	  return build_lin_expression(arg1) == build_lin_expression(arg2);
      else if (functor == a_equal_less_than)
	// =<
	if (SP_is_integer(arg1))
	  return get_integer(arg1) <= build_lin_expression(arg2);
	else if (SP_is_integer(arg2))
	  return build_lin_expression(arg1) <= get_integer(arg2);
	else
	  return build_lin_expression(arg1) <= build_lin_expression(arg2);
      else if (functor == a_greater_than_equal)
	// >=
	if (SP_is_integer(arg1))
	  return get_integer(arg1) >= build_lin_expression(arg2);
	else if (SP_is_integer(arg2))
	  return build_lin_expression(arg1) >= get_integer(arg2);
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
    }
    else if (arity == 2) {
      SP_term_ref arg1 = SP_new_term_ref();
      SP_term_ref arg2 = SP_new_term_ref();
      SP_get_arg(1, t, arg1);
      SP_get_arg(2, t, arg2);
      if (functor == a_vertex)
	if (SP_is_integer(arg2))
	  return vertex(build_lin_expression(arg1), get_integer(arg2));
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
  abort();  // This is only to avoid a gcc warning.
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

