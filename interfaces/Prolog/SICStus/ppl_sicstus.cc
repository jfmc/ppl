
#include "ppl_install.hh"
#include <sicstus/sicstus.h>
#include <exception>
#include <stdexcept>

//int SP_put_address(SP_term_ref t, void *pointer) 
//void SP_raise_exception(SP_term_ref t)

namespace PPL = Parma_Polyhedra_Library;

static void
handle_exception() {
}

static void
handle_exception(const std::exception& /* e */) {
}

#define CATCH_ALL \
  catch (const std::exception& e) { \
    handle_exception(e); \
  } \
  catch (...) { \
    handle_exception(); \
  } \
  abort();  // This is only to silence a warning.

static SP_atom a_dollar_VAR;
static SP_atom a_plus;
static SP_atom a_minus;
static SP_atom a_asterisk;
static SP_atom a_equal_equal;
static SP_atom a_greater_than_equal;
static SP_atom a_equal_less_than;

static
struct {
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
};

extern "C"
void
ppl_init(int /* when */) {
  for (size_t i = 0; i < sizeof(sp_atoms)/sizeof(sp_atoms[0]); ++i) {
    SP_atom a = SP_atom_from_string(sp_atoms[i].name);
    SP_register_atom(a);
    *sp_atoms[i].p_atom = a;
  }
}

extern "C"
void
ppl_deinit(int /* when */) {
  for (size_t i = 0; i < sizeof(sp_atoms)/sizeof(sp_atoms[0]); ++i)
    SP_unregister_atom(*sp_atoms[i].p_atom);
}

extern "C"
void*
ppl_new_polyhedron() {
  try {
    return new PPL::Polyhedron();
  }
  CATCH_ALL;
}

extern "C"
void
ppl_delete_polyhedron(void* pp) {
  // If destructors throw it is a catastrophy.
  // Anyway...
  try {
    delete static_cast<PPL::Polyhedron*>(pp);
  }
  CATCH_ALL;
}

/*
  LinExpression operator +(const LinExpression& e1, const LinExpression& e2);
  LinExpression operator +(const Integer& n, const LinExpression& e);
  LinExpression operator +(const LinExpression& e, const Integer& n);

  LinExpression operator -(const LinExpression& e);

  LinExpression operator -(const LinExpression& e1, const LinExpression& e2);
  LinExpression operator -(const Integer& n, const LinExpression& e);
  LinExpression operator -(const LinExpression& e, const Integer& n);

  LinExpression operator *(const Integer& n, const LinExpression& e);
  LinExpression operator *(const LinExpression& e, const Integer& n);
*/

static long
get_integer(SP_term_ref t) {
  long v;
  if (SP_get_integer(t, &v))
    return v;
  else
    throw std::out_of_range("PPL::LinExpression"
			    " build_lin_expression(SP_term_ref)");
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
  throw std::invalid_argument("PPL::LinExpression"
			      " build_lin_expression(SP_term_ref)");
}


/*
  Constraint operator ==(const LinExpression& e1, const LinExpression& e2);
  Constraint operator ==(const LinExpression& e, const Integer& n);
  Constraint operator ==(const Integer& n, const LinExpression& e);

  Constraint operator <=(const LinExpression& e1, const LinExpression& e2);
  Constraint operator <=(const LinExpression& e, const Integer& n);
  Constraint operator <=(const Integer& n, const LinExpression& e);

  Constraint operator >=(const LinExpression& e1, const LinExpression& e2);
  Constraint operator >=(const LinExpression& e, const Integer& n);
  Constraint operator >=(const Integer& n, const LinExpression& e);
*/

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
  throw std::invalid_argument("PPL::LinExpression"
			      " build_lin_expression(SP_term_ref)");
}

extern "C"
void
ppl_insert_constraint(void* pp, SP_term_ref t) {
  try {
    static_cast<PPL::Polyhedron*>(pp)->insert(build_constraint(t));
  }
  CATCH_ALL;
}
