/* Implementation of the C interface.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */


#include <config.h>

#include "Coefficient.defs.hh"
#include "Linear_Expression.defs.hh"
#include "Constraint.defs.hh"
#include "Constraint_System.defs.hh"
#include "Generator.defs.hh"
#include "Generator_System.defs.hh"
#include "Polyhedron.defs.hh"
#include "C_Polyhedron.defs.hh"
#include "NNC_Polyhedron.defs.hh"
#include "Init.defs.hh"
#include "max_space_dimension.hh"
#include "version.hh"
#include "ppl_c.h"
#include <stdexcept>
#include <sstream>
#include <cstdio>
#include <cerrno>
#include <climits>

using namespace Parma_Polyhedra_Library;

#define DECLARE_CONVERSIONS(Type) \
inline const Type* \
to_const(ppl_const_ ## Type ## _t x) { \
  return reinterpret_cast<const Type*>(x); \
} \
 \
inline Type* \
to_nonconst(ppl_ ## Type ## _t x) { \
  return reinterpret_cast<Type*>(x); \
} \
 \
inline ppl_const_ ## Type ## _t \
to_const(const Type* x) { \
  return reinterpret_cast<ppl_const_ ## Type ## _t>(x); \
} \
 \
inline ppl_ ## Type ## _t \
to_nonconst(Type* x) { \
  return reinterpret_cast<ppl_ ## Type ## _t>(x); \
}

namespace {

void (*user_error_handler)(enum ppl_enum_error_code code,
			   const char* description) = 0;

void
notify_error(enum ppl_enum_error_code code, const char* description) {
  if (user_error_handler != 0)
    user_error_handler(code, description);
}

} // namespace

int
ppl_set_error_handler(void (*h)(enum ppl_enum_error_code code,
				const char* description)) {
  user_error_handler = h;
  return 0;
}

#define CATCH_STD_EXCEPTION(exception, code) \
catch(const std::exception& e) { \
  notify_error(code, e.what()); \
  return code; \
}

#define CATCH_ALL \
CATCH_STD_EXCEPTION(bad_alloc, PPL_ERROR_OUT_OF_MEMORY) \
CATCH_STD_EXCEPTION(invalid_argument, PPL_ERROR_INVALID_ARGUMENT) \
CATCH_STD_EXCEPTION(length_error, PPL_ERROR_LENGTH_ERROR) \
CATCH_STD_EXCEPTION(overflow_error, PPL_ARITHMETIC_OVERFLOW) \
CATCH_STD_EXCEPTION(runtime_error, PPL_ERROR_INTERNAL_ERROR) \
CATCH_STD_EXCEPTION(exception, PPL_ERROR_UNKNOWN_STANDARD_EXCEPTION) \
catch(...) { \
  notify_error(PPL_ERROR_UNEXPECTED_ERROR, \
	       "completely unexpected error: a bug in the PPL"); \
  return PPL_ERROR_UNEXPECTED_ERROR; \
}

unsigned int PPL_POLY_CON_RELATION_IS_DISJOINT;
unsigned int PPL_POLY_CON_RELATION_STRICTLY_INTERSECTS;
unsigned int PPL_POLY_CON_RELATION_IS_INCLUDED;
unsigned int PPL_POLY_CON_RELATION_SATURATES;

unsigned int PPL_POLY_GEN_RELATION_SUBSUMES;

unsigned int PPL_COMPLEXITY_CLASS_POLYNOMIAL;
unsigned int PPL_COMPLEXITY_CLASS_SIMPLEX;
unsigned int PPL_COMPLEXITY_CLASS_ANY;

namespace {

// Holds a pointer to the init object.
Init* init_object_ptr = 0;

} // namespace



namespace {

extern "C" const char*
c_variable_default_output_function(ppl_dimension_type var) {
  // On a 64-bits architecture, `var' will not be more than 2^64-1,
  // (2^64-1)/26 is written with 18 decimal digits, plus one letter,
  // plus one terminator makes 20.
#if defined(ULLONG_MAX) && ULLONG_MAX > 18446744073709551615ULL
# error "Please enlarge the buffer in the following line."
#endif
  static char buffer[20];
  buffer[0] = static_cast<char>('A' + var % 26);
  if (ppl_dimension_type i = var / 26) {
    int r = sprintf(buffer+1, "%u", i);
    if (r < 0)
      return 0;
    else if (r >= 19) {
      errno = ERANGE;
      return 0;
    }
  }
  else
    buffer[1] = '\0';
  return buffer;
}

// Holds a pointer to the C current output function.
ppl_io_variable_output_function_type* c_variable_output_function;

void
cxx_Variable_output_function(std::ostream& s, const Variable& v) {
  const char* b = c_variable_output_function(v.id());
  if (b == 0)
    // FIXME: silently doing nothing is not the right thing to do!
    return;
  s << b;
}

extern "C" typedef const char*
c_variable_output_function_type(ppl_dimension_type var);

// Holds a pointer to the C++ saved output function.
Variable::output_function_type* saved_cxx_Variable_output_function;

} // namespace


int
ppl_initialize(void) try {
  if (init_object_ptr != 0)
    // Already initialized: error.
    return PPL_ERROR_INVALID_ARGUMENT;

  init_object_ptr = new Init();

  PPL_POLY_CON_RELATION_IS_DISJOINT
    = Poly_Con_Relation::is_disjoint().get_flags();
  PPL_POLY_CON_RELATION_STRICTLY_INTERSECTS
    = Poly_Con_Relation::strictly_intersects().get_flags();
  PPL_POLY_CON_RELATION_IS_INCLUDED
    = Poly_Con_Relation::is_included().get_flags();
  PPL_POLY_CON_RELATION_SATURATES
    = Poly_Con_Relation::saturates().get_flags();

  PPL_POLY_GEN_RELATION_SUBSUMES
    = Poly_Gen_Relation::subsumes().get_flags();

  PPL_COMPLEXITY_CLASS_POLYNOMIAL = POLYNOMIAL_COMPLEXITY;
  PPL_COMPLEXITY_CLASS_SIMPLEX = SIMPLEX_COMPLEXITY;
  PPL_COMPLEXITY_CLASS_ANY = ANY_COMPLEXITY;

  c_variable_output_function = c_variable_default_output_function;
  saved_cxx_Variable_output_function = Variable::get_output_function();
  Variable::set_output_function(cxx_Variable_output_function);

  return 0;
}
CATCH_ALL

int
ppl_finalize(void) try {
  if (init_object_ptr == 0)
    // Not initialized or already finalized: error.
    return PPL_ERROR_INVALID_ARGUMENT;

  delete init_object_ptr;
  init_object_ptr = 0;

  Variable::set_output_function(saved_cxx_Variable_output_function);

  return 0;
}
CATCH_ALL

int
ppl_version_major(void) try {
  return version_major();
}
CATCH_ALL

int
ppl_version_minor(void) try {
  return version_minor();
}
CATCH_ALL

int
ppl_version_revision(void) try {
  return version_revision();
}
CATCH_ALL

int
ppl_version_beta(void) try {
  return version_beta();
}
CATCH_ALL

int
ppl_version(const char** p) try {
  *p = version();
  return 0;
}
CATCH_ALL

int
ppl_banner(const char** p) try {
  *p = banner();
  return 0;
}
CATCH_ALL

int
ppl_max_space_dimension(ppl_dimension_type* m) try {
  *m = max_space_dimension();
  return 0;
}
CATCH_ALL

int
ppl_not_a_dimension(ppl_dimension_type* m) try {
  *m = not_a_dimension();
  return 0;
}
CATCH_ALL

DECLARE_CONVERSIONS(Coefficient)

DECLARE_CONVERSIONS(Linear_Expression)

DECLARE_CONVERSIONS(Constraint)

DECLARE_CONVERSIONS(Constraint_System)

typedef Constraint_System::const_iterator Constraint_System_const_iterator;
DECLARE_CONVERSIONS(Constraint_System_const_iterator)

DECLARE_CONVERSIONS(Generator)

DECLARE_CONVERSIONS(Generator_System)

typedef Generator_System::const_iterator Generator_System_const_iterator;
DECLARE_CONVERSIONS(Generator_System_const_iterator)

DECLARE_CONVERSIONS(Polyhedron)


int
ppl_new_Coefficient(ppl_Coefficient_t* pc) try {
  *pc = to_nonconst(new Coefficient(0));
  return 0;
}
CATCH_ALL

int
ppl_new_Coefficient_from_mpz_t(ppl_Coefficient_t* pc, mpz_t z) try {
  // FIXME: this is a kludge.
  *pc = to_nonconst(new Coefficient(mpz_class(z)));
  return 0;
}
CATCH_ALL

int
ppl_new_Coefficient_from_Coefficient(ppl_Coefficient_t* pc,
				     ppl_const_Coefficient_t c) try {
  const Coefficient& cc = *to_const(c);
  *pc = to_nonconst(new Coefficient(cc));
  return 0;
}
CATCH_ALL

int
ppl_Coefficient_to_mpz_t(ppl_const_Coefficient_t c, mpz_t z) try {
  // FIXME: this is a kludge.
  mpz_class v;
  Checked::assign<Check_Overflow_Policy>(v, raw_value(*to_const(c)));
  mpz_set(z, v.get_mpz_t());
  return 0;
}
CATCH_ALL

int
ppl_delete_Coefficient(ppl_const_Coefficient_t c) try {
  delete to_const(c);
  return 0;
}
CATCH_ALL

int
ppl_assign_Coefficient_from_mpz_t(ppl_Coefficient_t dst, mpz_t z) try {
  Coefficient& ddst = *to_nonconst(dst);
  // FIXME: this is a kludge.
  ddst = mpz_class(z);
  return 0;
}
CATCH_ALL

int
ppl_assign_Coefficient_from_Coefficient(ppl_Coefficient_t dst,
					ppl_const_Coefficient_t src) try {
  const Coefficient& ssrc = *to_const(src);
  Coefficient& ddst = *to_nonconst(dst);
  ddst = ssrc;
  return 0;
  }
CATCH_ALL

int
ppl_Coefficient_OK(ppl_const_Coefficient_t /* c */) try {
  return 1;
}
CATCH_ALL


int
ppl_new_Linear_Expression(ppl_Linear_Expression_t* ple) try {
  *ple = to_nonconst(new Linear_Expression());
  return 0;
}
CATCH_ALL

int
ppl_new_Linear_Expression_with_dimension(ppl_Linear_Expression_t* ple,
					 ppl_dimension_type d) try {
  *ple = to_nonconst(d == 0
		     ? new Linear_Expression(0)
		     : new Linear_Expression(0*Variable(d-1)));
  return 0;
}
CATCH_ALL

int
ppl_new_Linear_Expression_from_Linear_Expression
(ppl_Linear_Expression_t* ple, ppl_const_Linear_Expression_t le) try {
  const Linear_Expression& lle = *to_const(le);
  *ple = to_nonconst(new Linear_Expression(lle));
  return 0;
}
CATCH_ALL

int
ppl_delete_Linear_Expression(ppl_const_Linear_Expression_t le) try {
  delete to_const(le);
  return 0;
}
CATCH_ALL

int
ppl_assign_Linear_Expression_from_Linear_Expression
(ppl_Linear_Expression_t dst, ppl_const_Linear_Expression_t src) try {
  const Linear_Expression& ssrc = *to_const(src);
  Linear_Expression& ddst = *to_nonconst(dst);
  ddst = ssrc;
  return 0;
}
CATCH_ALL

int
ppl_Linear_Expression_add_to_coefficient(ppl_Linear_Expression_t le,
					 ppl_dimension_type var,
					 ppl_const_Coefficient_t n) try {
  Linear_Expression& lle = *to_nonconst(le);
  const Coefficient& nn = *to_const(n);
  lle += nn * Variable(var);
  return 0;
}
CATCH_ALL

int
ppl_Linear_Expression_add_to_inhomogeneous(ppl_Linear_Expression_t le,
					   ppl_const_Coefficient_t n) try {
  Linear_Expression& lle = *to_nonconst(le);
  const Coefficient& nn = *to_const(n);
  lle += nn;
  return 0;
}
CATCH_ALL

int
ppl_add_Linear_Expression_to_Linear_Expression
(ppl_Linear_Expression_t dst, ppl_const_Linear_Expression_t src) try {
  Linear_Expression& ddst = *to_nonconst(dst);
  const Linear_Expression& ssrc = *to_const(src);
  ddst += ssrc;
  return 0;
}
CATCH_ALL

int
ppl_subtract_Linear_Expression_from_Linear_Expression
(ppl_Linear_Expression_t dst, ppl_const_Linear_Expression_t src) try {
  Linear_Expression& ddst = *to_nonconst(dst);
  const Linear_Expression& ssrc = *to_const(src);
  ddst -= ssrc;
  return 0;
}
CATCH_ALL

int
ppl_multiply_Linear_Expression_by_Coefficient(ppl_Linear_Expression_t le,
					      ppl_const_Coefficient_t n) try {
  Linear_Expression& lle = *to_nonconst(le);
  const Coefficient& nn = *to_const(n);
  lle *= nn;
  return 0;
}
CATCH_ALL

int
ppl_Linear_Expression_space_dimension(ppl_const_Linear_Expression_t le,
				      ppl_dimension_type* m) try {
  *m = to_const(le)->space_dimension();
  return 0;
}
CATCH_ALL

int
ppl_Linear_Expression_coefficient(ppl_const_Linear_Expression_t le,
				  ppl_dimension_type var,
				  ppl_Coefficient_t n) try {
  const Linear_Expression& lle = *to_const(le);
  Coefficient& nn = *to_nonconst(n);
  nn = lle.coefficient(Variable(var));
  return 0;
}
CATCH_ALL

int
ppl_Linear_Expression_inhomogeneous_term(ppl_const_Linear_Expression_t le,
					 ppl_Coefficient_t n) try {
  const Linear_Expression& lle = *to_const(le);
  Coefficient& nn = *to_nonconst(n);
  nn = lle.inhomogeneous_term();
  return 0;
}
CATCH_ALL

int
ppl_Linear_Expression_OK(ppl_const_Linear_Expression_t /* le */) try {
  return 1;
}
CATCH_ALL


int
ppl_new_Constraint(ppl_Constraint_t* pc,
		   ppl_const_Linear_Expression_t le,
		   enum ppl_enum_Constraint_Type t) try {
  Constraint* ppc;
  const Linear_Expression& lle = *to_const(le);
  switch(t) {
  case PPL_CONSTRAINT_TYPE_EQUAL:
    ppc = new Constraint(lle == 0);
    break;
  case PPL_CONSTRAINT_TYPE_GREATER_THAN_OR_EQUAL:
    ppc = new Constraint(lle >= 0);
    break;
  case PPL_CONSTRAINT_TYPE_GREATER_THAN:
    ppc = new Constraint(lle > 0);
    break;
  case PPL_CONSTRAINT_TYPE_LESS_THAN_OR_EQUAL:
    ppc = new Constraint(lle <= 0);
    break;
  case PPL_CONSTRAINT_TYPE_LESS_THAN:
    ppc = new Constraint(lle < 0);
    break;
  default:
    throw std::invalid_argument("ppl_new_Constraint(pc, le, t): "
				"t invalid");
  }
  *pc = to_nonconst(ppc);
  return 0;
}
CATCH_ALL

int
ppl_new_Constraint_zero_dim_false(ppl_Constraint_t* pc) try {
  *pc = to_nonconst(new Constraint(Constraint::zero_dim_false()));
  return 0;
}
CATCH_ALL

int
ppl_new_Constraint_zero_dim_positivity(ppl_Constraint_t* pc) try {
  *pc = to_nonconst(new Constraint(Constraint::zero_dim_positivity()));
  return 0;
}
CATCH_ALL

int
ppl_new_Constraint_from_Constraint(ppl_Constraint_t* pc,
				   ppl_const_Constraint_t c) try {
  const Constraint& cc = *to_const(c);
  *pc = to_nonconst(new Constraint(cc));
  return 0;
}
CATCH_ALL

int
ppl_delete_Constraint(ppl_const_Constraint_t le) try {
  delete to_const(le);
  return 0;
}
CATCH_ALL

int
ppl_assign_Constraint_from_Constraint(ppl_Constraint_t dst,
				      ppl_const_Constraint_t src) try {
  const Constraint& ssrc = *to_const(src);
  Constraint& ddst = *to_nonconst(dst);
  ddst = ssrc;
  return 0;
}
CATCH_ALL

int
ppl_Constraint_space_dimension(ppl_const_Constraint_t c,
			       ppl_dimension_type* m) try {
  *m = to_const(c)->space_dimension();
  return 0;
}
CATCH_ALL

int
ppl_Constraint_type(ppl_const_Constraint_t c) try {
  switch (to_const(c)->type()) {
  case Constraint::EQUALITY:
    return PPL_CONSTRAINT_TYPE_EQUAL;
  case Constraint::NONSTRICT_INEQUALITY:
    return PPL_CONSTRAINT_TYPE_GREATER_THAN_OR_EQUAL;
  case Constraint::STRICT_INEQUALITY:
    return PPL_CONSTRAINT_TYPE_GREATER_THAN;
  default:
    throw std::runtime_error("ppl_Constraint_type()");
  }
}
CATCH_ALL

int
ppl_Constraint_coefficient(ppl_const_Constraint_t c,
			   ppl_dimension_type var,
			   ppl_Coefficient_t n) try {
  const Constraint& cc = *to_const(c);
  Coefficient& nn = *to_nonconst(n);
  nn = cc.coefficient(Variable(var));
  return 0;
}
CATCH_ALL

int
ppl_Constraint_inhomogeneous_term(ppl_const_Constraint_t c,
				  ppl_Coefficient_t n) try {
  const Constraint& cc = *to_const(c);
  Coefficient& nn = *to_nonconst(n);
  nn = cc.inhomogeneous_term();
  return 0;
}
CATCH_ALL

int
ppl_Constraint_OK(ppl_const_Constraint_t /* c */) try {
  return 1;
}
CATCH_ALL

int
ppl_new_Linear_Expression_from_Constraint(ppl_Linear_Expression_t* ple,
					  ppl_const_Constraint_t c) try {
  const Constraint& cc = *to_const(c);
  *ple = to_nonconst(new Linear_Expression(cc));
  return 0;
}
CATCH_ALL


int
ppl_new_Constraint_System(ppl_Constraint_System_t* pcs) try {
  *pcs = to_nonconst(new Constraint_System());
  return 0;
}
CATCH_ALL

int
ppl_new_Constraint_System_zero_dim_empty(ppl_Constraint_System_t* pcs) try {
  *pcs = to_nonconst(new
		     Constraint_System(Constraint_System::zero_dim_empty()));
  return 0;
}
CATCH_ALL


int
ppl_new_Constraint_System_from_Constraint(ppl_Constraint_System_t* pcs,
					  ppl_const_Constraint_t c) try {
  const Constraint& cc = *to_const(c);
  *pcs = to_nonconst(new Constraint_System(cc));
  return 0;
}
CATCH_ALL

int
ppl_new_Constraint_System_from_Constraint_System
(ppl_Constraint_System_t* pcs, ppl_const_Constraint_System_t cs) try {
  const Constraint_System& ccs = *to_const(cs);
  *pcs = to_nonconst(new Constraint_System(ccs));
  return 0;
}
CATCH_ALL

int
ppl_delete_Constraint_System(ppl_const_Constraint_System_t cs) try {
  delete to_const(cs);
  return 0;
}
CATCH_ALL

int
ppl_assign_Constraint_System_from_Constraint_System
(ppl_Constraint_System_t dst, ppl_const_Constraint_System_t src) try {
  const Constraint_System& ssrc = *to_const(src);
  Constraint_System& ddst = *to_nonconst(dst);
  ddst = ssrc;
  return 0;
}
CATCH_ALL

int
ppl_Constraint_System_space_dimension(ppl_const_Constraint_System_t cs,
				      ppl_dimension_type* m) try {
  *m = to_const(cs)->space_dimension();
  return 0;
}
CATCH_ALL

int
ppl_Constraint_System_clear(ppl_Constraint_System_t cs) try {
  to_nonconst(cs)->clear();
  return 0;
}
CATCH_ALL

int
ppl_Constraint_System_insert_Constraint(ppl_Constraint_System_t cs,
					ppl_const_Constraint_t c) try {
  const Constraint& cc = *to_const(c);
  Constraint_System& ccs = *to_nonconst(cs);
  ccs.insert(cc);
  return 0;
}
CATCH_ALL

int
ppl_Constraint_System_OK(ppl_const_Constraint_System_t cs) try {
  return to_const(cs)->OK() ? 1 : 0;
}
CATCH_ALL


int
ppl_new_Constraint_System_const_iterator
(ppl_Constraint_System_const_iterator_t* pcit) try {
  *pcit = to_nonconst(new Constraint_System::const_iterator());
  return 0;
}
CATCH_ALL

int
ppl_new_Constraint_System_const_iterator_from_Constraint_System_const_iterator
(ppl_Constraint_System_const_iterator_t* pcit,
 ppl_const_Constraint_System_const_iterator_t cit)  try {
  *pcit = to_nonconst(new Constraint_System::const_iterator(*to_const(cit)));
  return 0;
}
CATCH_ALL

int
ppl_delete_Constraint_System_const_iterator
(ppl_const_Constraint_System_const_iterator_t cit)
  try {
  delete to_const(cit);
  return 0;
}
CATCH_ALL

int
ppl_assign_Constraint_System_const_iterator_from_Constraint_System_const_iterator
(ppl_Constraint_System_const_iterator_t dst,
 ppl_const_Constraint_System_const_iterator_t src) try {
  const Constraint_System::const_iterator& ssrc = *to_const(src);
  Constraint_System::const_iterator& ddst = *to_nonconst(dst);
  ddst = ssrc;
  return 0;
}
CATCH_ALL

int
ppl_Constraint_System_begin(ppl_const_Constraint_System_t cs,
			    ppl_Constraint_System_const_iterator_t cit) try {
  const Constraint_System& ccs = *to_const(cs);
  Constraint_System::const_iterator& ccit = *to_nonconst(cit);
  ccit = ccs.begin();
  return 0;
}
CATCH_ALL

int
ppl_Constraint_System_end(ppl_const_Constraint_System_t cs,
			  ppl_Constraint_System_const_iterator_t cit) try {
  const Constraint_System& ccs = *to_const(cs);
  Constraint_System::const_iterator& ccit = *to_nonconst(cit);
  ccit = ccs.end();
  return 0;
}
CATCH_ALL

int
ppl_Constraint_System_const_iterator_dereference
(ppl_const_Constraint_System_const_iterator_t cit,
 ppl_const_Constraint_t* pc) try {
  const Constraint_System::const_iterator& ccit = *to_const(cit);
  const Constraint& c = *ccit;
  *pc = to_const(&c);
  return 0;
}
CATCH_ALL

int
ppl_Constraint_System_const_iterator_increment
(ppl_Constraint_System_const_iterator_t cit) try {
  Constraint_System::const_iterator& ccit = *to_nonconst(cit);
  ++ccit;
  return 0;
}
CATCH_ALL

int
ppl_Constraint_System_const_iterator_equal_test
(ppl_const_Constraint_System_const_iterator_t x,
 ppl_const_Constraint_System_const_iterator_t y) try {
  const Constraint_System::const_iterator& xx = *to_const(x);
  const Constraint_System::const_iterator& yy = *to_const(y);
  return (xx == yy) ? 1 : 0;
}
CATCH_ALL


int
ppl_new_Generator(ppl_Generator_t* pg,
		  ppl_const_Linear_Expression_t le,
		  enum ppl_enum_Generator_Type t,
		  ppl_const_Coefficient_t d) try {
  Generator* ppg;
  const Linear_Expression& lle = *to_const(le);
  const Coefficient& dd = *to_const(d);
  switch(t) {
  case PPL_GENERATOR_TYPE_POINT:
    ppg = new Generator(Generator::point(lle, dd));
    break;
  case PPL_GENERATOR_TYPE_CLOSURE_POINT:
    ppg = new Generator(Generator::closure_point(lle, dd));
    break;
  case PPL_GENERATOR_TYPE_RAY:
    ppg = new Generator(Generator::ray(lle));
    break;
  case PPL_GENERATOR_TYPE_LINE:
    ppg = new Generator(Generator::line(lle));
    break;
  default:
    throw std::invalid_argument("ppl_new_Generator(pg, le, t, d): "
				"t invalid");
  }
  *pg = to_nonconst(ppg);
  return 0;
}
CATCH_ALL

int
ppl_new_Generator_zero_dim_point(ppl_Generator_t* pg) try {
  *pg = to_nonconst(new Generator(Generator::zero_dim_point()));
  return 0;
}
CATCH_ALL

int
ppl_new_Generator_zero_dim_closure_point(ppl_Generator_t* pg) try {
  *pg = to_nonconst(new Generator(Generator::zero_dim_closure_point()));
  return 0;
}
CATCH_ALL

int
ppl_new_Generator_from_Generator(ppl_Generator_t* pg,
				 ppl_const_Generator_t g) try {
  const Generator& gg = *to_const(g);
  *pg = to_nonconst(new Generator(gg));
  return 0;
}
CATCH_ALL

int
ppl_delete_Generator(ppl_const_Generator_t le) try {
  delete to_const(le);
  return 0;
}
CATCH_ALL

int
ppl_assign_Generator_from_Generator(ppl_Generator_t dst,
				      ppl_const_Generator_t src) try {
  const Generator& ssrc = *to_const(src);
  Generator& ddst = *to_nonconst(dst);
  ddst = ssrc;
  return 0;
}
CATCH_ALL

int
ppl_Generator_space_dimension(ppl_const_Generator_t g,
			      ppl_dimension_type* m) try {
  *m = to_const(g)->space_dimension();
  return 0;
}
CATCH_ALL

int
ppl_Generator_type(ppl_const_Generator_t g) try {
  switch (to_const(g)->type()) {
  case Generator::LINE:
    return PPL_GENERATOR_TYPE_LINE;
  case Generator::RAY:
    return PPL_GENERATOR_TYPE_RAY;
  case Generator::POINT:
    return PPL_GENERATOR_TYPE_POINT;
  case Generator::CLOSURE_POINT:
    return PPL_GENERATOR_TYPE_CLOSURE_POINT;
  default:
    throw std::runtime_error("ppl_Generator_type()");
  }
}
CATCH_ALL

int
ppl_Generator_coefficient(ppl_const_Generator_t g,
			  ppl_dimension_type var,
			  ppl_Coefficient_t n) try {
  const Generator& gg = *to_const(g);
  Coefficient& nn = *to_nonconst(n);
  nn = gg.coefficient(Variable(var));
  return 0;
}
CATCH_ALL

int
ppl_Generator_divisor(ppl_const_Generator_t g,
		      ppl_Coefficient_t n) try {
  const Generator& gg = *to_const(g);
  Coefficient& nn = *to_nonconst(n);
  nn = gg.divisor();
  return 0;
}
CATCH_ALL

int
ppl_Generator_OK(ppl_const_Generator_t /* g */) try {
  return 1;
}
CATCH_ALL

int
ppl_new_Linear_Expression_from_Generator(ppl_Linear_Expression_t* ple,
					 ppl_const_Generator_t g) try {
  const Generator& gg = *to_const(g);
  *ple = to_nonconst(new Linear_Expression(gg));
  return 0;
}
CATCH_ALL


int
ppl_new_Generator_System(ppl_Generator_System_t* pgs) try {
  *pgs = to_nonconst(new Generator_System());
  return 0;
}
CATCH_ALL

int
ppl_new_Generator_System_zero_dim_univ(ppl_Generator_System_t* pgs) try {
  *pgs = to_nonconst(new Generator_System(Generator_System::zero_dim_univ()));
  return 0;
}
CATCH_ALL

int
ppl_new_Generator_System_from_Generator(ppl_Generator_System_t* pgs,
			      ppl_const_Generator_t g) try {
  const Generator& gg = *to_const(g);
  *pgs = to_nonconst(new Generator_System(gg));
  return 0;
}
CATCH_ALL

int
ppl_new_Generator_System_from_Generator_System
(ppl_Generator_System_t* pgs, ppl_const_Generator_System_t gs) try {
  const Generator_System& ggs = *to_const(gs);
  *pgs = to_nonconst(new Generator_System(ggs));
  return 0;
}
CATCH_ALL

int
ppl_delete_Generator_System(ppl_const_Generator_System_t gs) try {
  delete to_const(gs);
  return 0;
}
CATCH_ALL

int
ppl_assign_Generator_System_from_Generator_System
(ppl_Generator_System_t dst, ppl_const_Generator_System_t src) try {
  const Generator_System& ssrc = *to_const(src);
  Generator_System& ddst = *to_nonconst(dst);
  ddst = ssrc;
  return 0;
}
CATCH_ALL

int
ppl_Generator_System_space_dimension(ppl_const_Generator_System_t gs,
				     ppl_dimension_type* m) try {
  *m = to_const(gs)->space_dimension();
  return 0;
}
CATCH_ALL

int
ppl_Generator_System_clear(ppl_Generator_System_t gs) try {
  to_nonconst(gs)->clear();
  return 0;
}
CATCH_ALL

int
ppl_Generator_System_insert_Generator(ppl_Generator_System_t gs,
				      ppl_const_Generator_t g) try {
  const Generator& gg = *to_const(g);
  Generator_System& ggs = *to_nonconst(gs);
  ggs.insert(gg);
  return 0;
}
CATCH_ALL

int
ppl_Generator_System_OK(ppl_const_Generator_System_t gs) try {
  return to_const(gs)->OK() ? 1 : 0;
}
CATCH_ALL


int
ppl_new_Generator_System_const_iterator
(ppl_Generator_System_const_iterator_t* pgit) try {
  *pgit = to_nonconst(new Generator_System::const_iterator());
  return 0;
}
CATCH_ALL

int
ppl_new_Generator_System_const_iterator_from_Generator_System_const_iterator
(ppl_Generator_System_const_iterator_t* pgit,
 ppl_const_Generator_System_const_iterator_t git)  try {
  *pgit = to_nonconst(new Generator_System::const_iterator(*to_const(git)));
  return 0;
}
CATCH_ALL

int
ppl_delete_Generator_System_const_iterator
(ppl_const_Generator_System_const_iterator_t git) try {
  delete to_const(git);
  return 0;
}
CATCH_ALL

int
ppl_assign_Generator_System_const_iterator_from_Generator_System_const_iterator
(ppl_Generator_System_const_iterator_t dst,
 ppl_const_Generator_System_const_iterator_t src) try {
  const Generator_System::const_iterator& ssrc = *to_const(src);
  Generator_System::const_iterator& ddst = *to_nonconst(dst);
  ddst = ssrc;
  return 0;
}
CATCH_ALL

int
ppl_Generator_System_begin(ppl_const_Generator_System_t gs,
			   ppl_Generator_System_const_iterator_t git) try {
  const Generator_System& ggs = *to_const(gs);
  Generator_System::const_iterator& ggit = *to_nonconst(git);
  ggit = ggs.begin();
  return 0;
}
CATCH_ALL

int
ppl_Generator_System_end(ppl_const_Generator_System_t gs,
			 ppl_Generator_System_const_iterator_t git) try {
  const Generator_System& ggs = *to_const(gs);
  Generator_System::const_iterator& ggit = *to_nonconst(git);
  ggit = ggs.end();
  return 0;
}
CATCH_ALL

int
ppl_Generator_System_const_iterator_dereference
(ppl_const_Generator_System_const_iterator_t git,
 ppl_const_Generator_t* pg) try {
  const Generator_System::const_iterator& ggit = *to_const(git);
  const Generator& c = *ggit;
  *pg = to_const(&c);
  return 0;
}
CATCH_ALL

int
ppl_Generator_System_const_iterator_increment
(ppl_Generator_System_const_iterator_t git) try {
  Generator_System::const_iterator& ggit = *to_nonconst(git);
  ++ggit;
  return 0;
}
CATCH_ALL

int
ppl_Generator_System_const_iterator_equal_test
(ppl_const_Generator_System_const_iterator_t x,
 ppl_const_Generator_System_const_iterator_t y) try {
  const Generator_System::const_iterator& xx = *to_const(x);
  const Generator_System::const_iterator& yy = *to_const(y);
  return (xx == yy) ? 1 : 0;
}
CATCH_ALL


int
ppl_new_C_Polyhedron_from_dimension(ppl_Polyhedron_t* pph,
				    ppl_dimension_type d) try {
  *pph = to_nonconst(new C_Polyhedron(d, Polyhedron::UNIVERSE));
  return 0;
}
CATCH_ALL

int
ppl_new_NNC_Polyhedron_from_dimension(ppl_Polyhedron_t* pph,
				      ppl_dimension_type d) try {
  *pph = to_nonconst(new NNC_Polyhedron(d, Polyhedron::UNIVERSE));
  return 0;
}
CATCH_ALL

int
ppl_new_C_Polyhedron_empty_from_dimension(ppl_Polyhedron_t* pph,
					  ppl_dimension_type d) try {
  *pph = to_nonconst(new C_Polyhedron(d, Polyhedron::EMPTY));
  return 0;
}
CATCH_ALL

int
ppl_new_NNC_Polyhedron_empty_from_dimension(ppl_Polyhedron_t* pph,
					    ppl_dimension_type d) try {
  *pph = to_nonconst(new NNC_Polyhedron(d, Polyhedron::EMPTY));
  return 0;
}
CATCH_ALL

int
ppl_new_C_Polyhedron_from_C_Polyhedron(ppl_Polyhedron_t* pph,
				       ppl_const_Polyhedron_t ph) try {
  const C_Polyhedron& phh = *static_cast<const C_Polyhedron*>(to_const(ph));
  *pph = to_nonconst(new C_Polyhedron(phh));
  return 0;
}
CATCH_ALL

int
ppl_new_C_Polyhedron_from_NNC_Polyhedron(ppl_Polyhedron_t* pph,
					 ppl_const_Polyhedron_t ph) try {
  const NNC_Polyhedron& phh
    = *static_cast<const NNC_Polyhedron*>(to_const(ph));
  *pph = to_nonconst(new C_Polyhedron(phh));
  return 0;
}
CATCH_ALL

int
ppl_new_NNC_Polyhedron_from_C_Polyhedron(ppl_Polyhedron_t* pph,
					 ppl_const_Polyhedron_t ph) try {
  const C_Polyhedron& phh = *static_cast<const C_Polyhedron*>(to_const(ph));
  *pph = to_nonconst(new NNC_Polyhedron(phh));
  return 0;
}
CATCH_ALL

int
ppl_new_NNC_Polyhedron_from_NNC_Polyhedron(ppl_Polyhedron_t* pph,
					   ppl_const_Polyhedron_t ph) try {
  const NNC_Polyhedron& phh
    = *static_cast<const NNC_Polyhedron*>(to_const(ph));
  *pph = to_nonconst(new NNC_Polyhedron(phh));
  return 0;
}
CATCH_ALL

int
ppl_new_C_Polyhedron_from_Constraint_System
(ppl_Polyhedron_t* pph, ppl_const_Constraint_System_t cs) try {
  const Constraint_System& ccs = *to_const(cs);
  *pph = to_nonconst(new C_Polyhedron(ccs));
  return 0;
}
CATCH_ALL

int
ppl_new_C_Polyhedron_recycle_Constraint_System
(ppl_Polyhedron_t* pph, ppl_Constraint_System_t cs) try {
  Constraint_System& ccs = *to_nonconst(cs);
  *pph = to_nonconst(new C_Polyhedron(ccs));
  return 0;
}
CATCH_ALL

int
ppl_new_NNC_Polyhedron_from_Constraint_System
(ppl_Polyhedron_t* pph, ppl_const_Constraint_System_t cs) try {
  const Constraint_System& ccs = *to_const(cs);
  *pph = to_nonconst(new NNC_Polyhedron(ccs));
  return 0;
}
CATCH_ALL

int
ppl_new_NNC_Polyhedron_recycle_Constraint_System
(ppl_Polyhedron_t* pph, ppl_Constraint_System_t cs) try {
  Constraint_System& ccs = *to_nonconst(cs);
  *pph = to_nonconst(new NNC_Polyhedron(ccs));
  return 0;
}
CATCH_ALL

int
ppl_new_C_Polyhedron_from_Generator_System
(ppl_Polyhedron_t* pph, ppl_const_Generator_System_t gs) try {
  const Generator_System& ggs = *to_const(gs);
  *pph = to_nonconst(new C_Polyhedron(ggs));
  return 0;
}
CATCH_ALL

int
ppl_new_C_Polyhedron_recycle_Generator_System(ppl_Polyhedron_t* pph,
					      ppl_Generator_System_t gs) try {
  Generator_System& ggs = *to_nonconst(gs);
  *pph = to_nonconst(new C_Polyhedron(ggs));
  return 0;
}
CATCH_ALL

int
ppl_new_NNC_Polyhedron_from_Generator_System
(ppl_Polyhedron_t* pph, ppl_const_Generator_System_t gs) try {
  const Generator_System& ggs = *to_const(gs);
  *pph = to_nonconst(new C_Polyhedron(ggs));
  return 0;
}
CATCH_ALL

int
ppl_new_NNC_Polyhedron_recycle_Generator_System
(ppl_Polyhedron_t* pph, ppl_Generator_System_t gs) try {
  Generator_System& ggs = *to_nonconst(gs);
  *pph = to_nonconst(new C_Polyhedron(ggs));
  return 0;
}
CATCH_ALL

namespace {

class CBuildBox {
private:
  ppl_dimension_type (*s_d)(void);
  int (*i_e)(void);
  int (*g_l_b)(ppl_dimension_type k, int closed,
	       ppl_Coefficient_t n,
	       ppl_Coefficient_t d);
  int (*g_u_b)(ppl_dimension_type k, int closed,
	       ppl_Coefficient_t n,
	       ppl_Coefficient_t d);

public:
  CBuildBox(ppl_dimension_type (*sd)(void),
	    int (*ie)(void),
	    int (*glb)(ppl_dimension_type k, int closed,
		       ppl_Coefficient_t n,
		       ppl_Coefficient_t d),
	    int (*gub)(ppl_dimension_type k, int closed,
		       ppl_Coefficient_t n,
		       ppl_Coefficient_t d))
    : s_d(sd), i_e(ie), g_l_b(glb), g_u_b(gub) {
  }

  ppl_dimension_type space_dimension() const {
    return s_d();
  }

  bool is_empty(void) const {
    return i_e() != 0;
  }

  bool get_lower_bound(ppl_dimension_type k, bool closed,
		       Coefficient& n, Coefficient& d) const {
    return g_l_b(k, closed, to_nonconst(&n), to_nonconst(&d)) != 0;
  }

  bool get_upper_bound(ppl_dimension_type k, bool closed,
		       Coefficient& n, Coefficient& d) const {
    return g_u_b(k, closed, to_nonconst(&n), to_nonconst(&d)) != 0;
  }
};

} // namespace

int
ppl_new_C_Polyhedron_from_bounding_box
(ppl_Polyhedron_t* pph,
 ppl_dimension_type (*space_dimension)(void),
 int (*is_empty)(void),
 int (*get_lower_bound)(ppl_dimension_type k, int closed,
			ppl_Coefficient_t n,
			ppl_Coefficient_t d),
 int (*get_upper_bound)(ppl_dimension_type k, int closed,
			ppl_Coefficient_t n,
			ppl_Coefficient_t d)) try {
  CBuildBox cbbox(space_dimension, is_empty,
		  get_lower_bound, get_upper_bound);
  *pph = to_nonconst(new C_Polyhedron(cbbox, From_Bounding_Box()));
  return 0;
}
CATCH_ALL

int
ppl_new_NNC_Polyhedron_from_bounding_box
(ppl_Polyhedron_t* pph,
 ppl_dimension_type (*space_dimension)(void),
 int (*is_empty)(void),
 int (*get_lower_bound)(ppl_dimension_type k, int closed,
			ppl_Coefficient_t n,
			ppl_Coefficient_t d),
 int (*get_upper_bound)(ppl_dimension_type k, int closed,
			ppl_Coefficient_t n,
			ppl_Coefficient_t d)) try {
  CBuildBox cbbox(space_dimension, is_empty,
		  get_lower_bound, get_upper_bound);
  *pph = to_nonconst(new NNC_Polyhedron(cbbox, From_Bounding_Box()));
  return 0;
}
CATCH_ALL

int
ppl_delete_Polyhedron(ppl_const_Polyhedron_t ph) try {
  delete to_const(ph);
  return 0;
}
CATCH_ALL

int
ppl_assign_C_Polyhedron_from_C_Polyhedron(ppl_Polyhedron_t dst,
					  ppl_const_Polyhedron_t src) try {
  const C_Polyhedron& ssrc
    = *static_cast<const C_Polyhedron*>(to_const(src));
  C_Polyhedron& ddst = *static_cast<C_Polyhedron*>(to_nonconst(dst));
  ddst = ssrc;
  return 0;
}
CATCH_ALL

int
ppl_assign_NNC_Polyhedron_from_NNC_Polyhedron(ppl_Polyhedron_t dst,
					      ppl_const_Polyhedron_t src) try {
  const NNC_Polyhedron& ssrc
    = *static_cast<const NNC_Polyhedron*>(to_const(src));
  NNC_Polyhedron& ddst = *static_cast<NNC_Polyhedron*>(to_nonconst(dst));
  ddst = ssrc;
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_space_dimension(ppl_const_Polyhedron_t ph,
			       ppl_dimension_type* m) try {
  *m = to_const(ph)->space_dimension();
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_affine_dimension(ppl_const_Polyhedron_t ph,
				ppl_dimension_type* m) try {
  *m = to_const(ph)->affine_dimension();
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_intersection_assign(ppl_Polyhedron_t x,
				   ppl_const_Polyhedron_t y) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  xx.intersection_assign(yy);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_intersection_assign_and_minimize(ppl_Polyhedron_t x,
						ppl_const_Polyhedron_t y) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  return xx.intersection_assign_and_minimize(yy) ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_concatenate_assign(ppl_Polyhedron_t x,
				  ppl_const_Polyhedron_t y) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  xx.concatenate_assign(yy);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_poly_hull_assign(ppl_Polyhedron_t x,
				ppl_const_Polyhedron_t y) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  xx.poly_hull_assign(yy);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_poly_hull_assign_and_minimize(ppl_Polyhedron_t x,
					     ppl_const_Polyhedron_t y) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  return xx.poly_hull_assign_and_minimize(yy) ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_poly_difference_assign(ppl_Polyhedron_t x,
				      ppl_const_Polyhedron_t y) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  xx.poly_difference_assign(yy);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_BHRZ03_widening_assign_with_tokens(ppl_Polyhedron_t x,
						  ppl_const_Polyhedron_t y,
						  unsigned* tp) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  xx.BHRZ03_widening_assign(yy, tp);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_BHRZ03_widening_assign(ppl_Polyhedron_t x,
				      ppl_const_Polyhedron_t y) try {
  return ppl_Polyhedron_BHRZ03_widening_assign_with_tokens(x, y, 0);
}
CATCH_ALL

int
ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens
(ppl_Polyhedron_t x,
 ppl_const_Polyhedron_t y,
 ppl_const_Constraint_System_t cs,
 unsigned* tp) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  const Constraint_System& ccs = *to_const(cs);
  xx.limited_BHRZ03_extrapolation_assign(yy, ccs, tp);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_limited_BHRZ03_extrapolation_assign
(ppl_Polyhedron_t x,
 ppl_const_Polyhedron_t y,
 ppl_const_Constraint_System_t cs) try {
  return ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens(x, y,
									cs, 0);
}
CATCH_ALL

int
ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens
(ppl_Polyhedron_t x,
 ppl_const_Polyhedron_t y,
 ppl_const_Constraint_System_t cs,
 unsigned* tp) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  const Constraint_System& ccs = *to_const(cs);
  xx.bounded_BHRZ03_extrapolation_assign(yy, ccs, tp);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign
(ppl_Polyhedron_t x,
 ppl_const_Polyhedron_t y,
 ppl_const_Constraint_System_t cs) try {
  return ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens(x, y,
									cs, 0);
}
CATCH_ALL

int
ppl_Polyhedron_H79_widening_assign_with_tokens(ppl_Polyhedron_t x,
					       ppl_const_Polyhedron_t y,
					       unsigned* tp) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  xx.H79_widening_assign(yy, tp);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_H79_widening_assign(ppl_Polyhedron_t x,
				   ppl_const_Polyhedron_t y) try {
  return ppl_Polyhedron_H79_widening_assign_with_tokens(x, y, 0);
}
CATCH_ALL

int
ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens
(ppl_Polyhedron_t x,
 ppl_const_Polyhedron_t y,
 ppl_const_Constraint_System_t cs,
 unsigned* tp) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  const Constraint_System& ccs = *to_const(cs);
  xx.limited_H79_extrapolation_assign(yy, ccs, tp);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_limited_H79_extrapolation_assign
(ppl_Polyhedron_t x,
 ppl_const_Polyhedron_t y,
 ppl_const_Constraint_System_t cs) try {
  return ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens(x, y,
								     cs, 0);
}
CATCH_ALL

int
ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens
(ppl_Polyhedron_t x,
 ppl_const_Polyhedron_t y,
 ppl_const_Constraint_System_t cs,
 unsigned* tp) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  const Constraint_System& ccs = *to_const(cs);
  xx.bounded_H79_extrapolation_assign(yy, ccs, tp);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_bounded_H79_extrapolation_assign
(ppl_Polyhedron_t x,
 ppl_const_Polyhedron_t y,
 ppl_const_Constraint_System_t cs) try {
  return ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens(x, y,
								     cs, 0);
}
CATCH_ALL

int
ppl_Polyhedron_time_elapse_assign(ppl_Polyhedron_t x,
				  ppl_const_Polyhedron_t y) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  xx.time_elapse_assign(yy);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_constraints(ppl_const_Polyhedron_t ph,
			   ppl_const_Constraint_System_t* pcs) try {
  const Polyhedron& pph = *to_const(ph);
  const Constraint_System& cs = pph.constraints();
  *pcs = to_const(&cs);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_minimized_constraints(ppl_const_Polyhedron_t ph,
				     ppl_const_Constraint_System_t* pcs) try {
  const Polyhedron& pph = *to_const(ph);
  const Constraint_System& cs = pph.minimized_constraints();
  *pcs = to_const(&cs);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_generators(ppl_const_Polyhedron_t ph,
			  ppl_const_Generator_System_t* pgs) try {
  const Polyhedron& pph = *to_const(ph);
  const Generator_System& gs = pph.generators();
  *pgs = to_const(&gs);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_minimized_generators(ppl_const_Polyhedron_t ph,
				    ppl_const_Generator_System_t* pgs) try {
  const Polyhedron& pph = *to_const(ph);
  const Generator_System& gs = pph.minimized_generators();
  *pgs = to_const(&gs);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_constraint(ppl_Polyhedron_t ph,
			      ppl_const_Constraint_t c) try {
  Polyhedron& pph = *to_nonconst(ph);
  const Constraint& cc = *to_const(c);
  pph.add_constraint(cc);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_constraint_and_minimize(ppl_Polyhedron_t ph,
					   ppl_const_Constraint_t c) try {
  Polyhedron& pph = *to_nonconst(ph);
  const Constraint& cc = *to_const(c);
  pph.add_constraint_and_minimize(cc);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_generator(ppl_Polyhedron_t ph,
			     ppl_const_Generator_t g) try {
  Polyhedron& pph = *to_nonconst(ph);
  const Generator& gg = *to_const(g);
  pph.add_generator(gg);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_generator_and_minimize(ppl_Polyhedron_t ph,
					  ppl_const_Generator_t g) try {
  Polyhedron& pph = *to_nonconst(ph);
  const Generator& gg = *to_const(g);
  pph.add_generator_and_minimize(gg);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_constraints(ppl_Polyhedron_t ph,
			       ppl_const_Constraint_System_t cs) try {
  Polyhedron& pph = *to_nonconst(ph);
  const Constraint_System& ccs = *to_const(cs);
  pph.add_constraints(ccs);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_constraints_and_minimize
(ppl_Polyhedron_t ph, ppl_const_Constraint_System_t cs) try {
  Polyhedron& pph = *to_nonconst(ph);
  const Constraint_System& ccs = *to_const(cs);
  return pph.add_constraints_and_minimize(ccs) ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_generators(ppl_Polyhedron_t ph,
			      ppl_const_Generator_System_t gs) try {
  Polyhedron& pph = *to_nonconst(ph);
  const Generator_System& ggs = *to_const(gs);
  pph.add_generators(ggs);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_generators_and_minimize
(ppl_Polyhedron_t ph, ppl_const_Generator_System_t gs) try {
  Polyhedron& pph = *to_nonconst(ph);
  const Generator_System& ggs = *to_const(gs);
  return pph.add_generators_and_minimize(ggs) ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_recycled_constraints(ppl_Polyhedron_t ph,
					ppl_Constraint_System_t cs) try {
  Polyhedron& pph = *to_nonconst(ph);
  Constraint_System& ccs = *to_nonconst(cs);
  pph.add_recycled_constraints(ccs);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_recycled_constraints_and_minimize
(ppl_Polyhedron_t ph, ppl_Constraint_System_t cs) try {
  Polyhedron& pph = *to_nonconst(ph);
  Constraint_System& ccs = *to_nonconst(cs);
  return pph.add_recycled_constraints_and_minimize(ccs) ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_recycled_generators(ppl_Polyhedron_t ph,
				       ppl_Generator_System_t gs) try {
  Polyhedron& pph = *to_nonconst(ph);
  Generator_System& ggs = *to_nonconst(gs);
  pph.add_recycled_generators(ggs);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_recycled_generators_and_minimize(ppl_Polyhedron_t ph,
						    ppl_Generator_System_t gs)
try {
  Polyhedron& pph = *to_nonconst(ph);
  Generator_System& ggs = *to_nonconst(gs);
  return pph.add_recycled_generators_and_minimize(ggs) ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_space_dimensions_and_embed(ppl_Polyhedron_t ph,
					      ppl_dimension_type d) try {
  Polyhedron& pph = *to_nonconst(ph);
  pph.add_space_dimensions_and_embed(d);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_space_dimensions_and_project(ppl_Polyhedron_t ph,
						ppl_dimension_type d) try {
  Polyhedron& pph = *to_nonconst(ph);
  pph.add_space_dimensions_and_project(d);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_remove_space_dimensions(ppl_Polyhedron_t ph,
				       ppl_dimension_type ds[],
				       size_t n) try {
  Polyhedron& pph = *to_nonconst(ph);
  Variables_Set to_be_removed;
  for (ppl_dimension_type i = n; i-- > 0; )
    to_be_removed.insert(Variable(ds[i]));
  pph.remove_space_dimensions(to_be_removed);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_remove_higher_space_dimensions(ppl_Polyhedron_t ph,
					      ppl_dimension_type d) try {
  Polyhedron& pph = *to_nonconst(ph);
  pph.remove_higher_space_dimensions(d);
  return 0;
}
CATCH_ALL

namespace {

class PIFunc {
private:
  //! Holds the vector implementing the map.
  dimension_type* vec;

  //! Holds the size of \p vec.
  size_t vec_size;

  //! Cache for computing the maximum dimension in the codomain.
  mutable dimension_type max_in_codomain_;

  //! Cache for computing emptiness:
  //! -1 if we still don't know, 0 if not empty, 1 if empty.
  mutable int empty;

public:
  PIFunc(dimension_type* v, size_t n)
    : vec(v), vec_size(n), max_in_codomain_(not_a_dimension()), empty(-1) {
  }

  bool has_empty_codomain() const {
    if (empty < 0) {
      empty = 1;
      for (size_t i = vec_size; i-- > 0; )
	if (vec[i] != not_a_dimension()) {
	  empty = 0;
	  break;
	}
    }
    return empty;
  }

  dimension_type max_in_codomain() const {
    if (max_in_codomain_ == not_a_dimension()) {
      for (size_t i = vec_size; i-- > 0; ) {
	dimension_type vec_i = vec[i];
	if (vec_i != not_a_dimension()
	    && (max_in_codomain_ == not_a_dimension()
		|| vec_i > max_in_codomain_))
	  max_in_codomain_ = vec_i;
      }
    }
    return max_in_codomain_;
  }

  bool maps(dimension_type i, dimension_type& j) const {
    if (i >= vec_size)
      return false;
    dimension_type vec_i = vec[i];
    if (vec_i == not_a_dimension())
      return false;
    j = vec_i;
    return true;
  }
};

} // namespace

int
ppl_Polyhedron_map_space_dimensions(ppl_Polyhedron_t ph,
				    ppl_dimension_type maps[],
				    size_t n) try {
  Polyhedron& pph = *to_nonconst(ph);
  PIFunc pifunc(maps, n);
  pph.map_space_dimensions(pifunc);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_expand_space_dimension(ppl_Polyhedron_t ph,
				      ppl_dimension_type d,
				      ppl_dimension_type m) try {
  Polyhedron& pph = *to_nonconst(ph);
  pph.expand_space_dimension(Variable(d), m);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_fold_space_dimensions(ppl_Polyhedron_t ph,
				     ppl_dimension_type ds[],
				     size_t n,
				     ppl_dimension_type d) try {
  Polyhedron& pph = *to_nonconst(ph);
  Variables_Set to_be_folded;
  for (ppl_dimension_type i = n; i-- > 0; )
    to_be_folded.insert(Variable(ds[i]));
  pph.fold_space_dimensions(to_be_folded, Variable(d));
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_affine_image(ppl_Polyhedron_t ph,
			    ppl_dimension_type var,
			    ppl_const_Linear_Expression_t le,
			    ppl_const_Coefficient_t d) try {
  Polyhedron& pph = *to_nonconst(ph);
  const Linear_Expression& lle = *to_const(le);
  const Coefficient& dd = *to_const(d);
  pph.affine_image(Variable(var), lle, dd);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_affine_preimage(ppl_Polyhedron_t ph,
			       ppl_dimension_type var,
			       ppl_const_Linear_Expression_t le,
			       ppl_const_Coefficient_t d) try {
  Polyhedron& pph = *to_nonconst(ph);
  const Linear_Expression& lle = *to_const(le);
  const Coefficient& dd = *to_const(d);
  pph.affine_preimage(Variable(var), lle, dd);
  return 0;
}
CATCH_ALL

namespace {

inline Relation_Symbol
relation_symbol(enum ppl_enum_Constraint_Type t) {
  switch (t) {
  case PPL_CONSTRAINT_TYPE_LESS_THAN:
    return LESS_THAN;
  case PPL_CONSTRAINT_TYPE_LESS_THAN_OR_EQUAL:
    return LESS_THAN_OR_EQUAL;
  case PPL_CONSTRAINT_TYPE_EQUAL:
    return EQUAL;
  case PPL_CONSTRAINT_TYPE_GREATER_THAN_OR_EQUAL:
    return GREATER_THAN_OR_EQUAL;
  case PPL_CONSTRAINT_TYPE_GREATER_THAN:
    return GREATER_THAN;
  default:
    return static_cast<Relation_Symbol>(t);
  }
}

} // namespace

int
ppl_Polyhedron_generalized_affine_image(ppl_Polyhedron_t ph,
					ppl_dimension_type var,
					enum ppl_enum_Constraint_Type relsym,
					ppl_const_Linear_Expression_t le,
					ppl_const_Coefficient_t d) try {
  Polyhedron& pph = *to_nonconst(ph);
  const Linear_Expression& lle = *to_const(le);
  const Coefficient& dd = *to_const(d);
  pph.generalized_affine_image(Variable(var), relation_symbol(relsym), lle,
			       dd);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_generalized_affine_image_lhs_rhs
(ppl_Polyhedron_t ph,
 ppl_const_Linear_Expression_t lhs,
 enum ppl_enum_Constraint_Type relsym,
 ppl_const_Linear_Expression_t rhs) try {
  Polyhedron& pph = *to_nonconst(ph);
  const Linear_Expression& llhs = *to_const(lhs);
  const Linear_Expression& rrhs = *to_const(rhs);
  pph.generalized_affine_image(llhs, relation_symbol(relsym), rrhs);
  return 0;
}
CATCH_ALL


namespace {

class CShrinkBox {
private:
  void (*s_e)(void);
  void (*r_l_b)(ppl_dimension_type k, int closed,
		ppl_const_Coefficient_t n,
		ppl_const_Coefficient_t d);
  void (*l_u_b)(ppl_dimension_type k, int closed,
		ppl_const_Coefficient_t n,
		ppl_const_Coefficient_t d);

public:
  CShrinkBox(void (*se)(void),
	     void (*rlb)(ppl_dimension_type k, int closed,
			 ppl_const_Coefficient_t n,
			 ppl_const_Coefficient_t d),
	     void (*lub)(ppl_dimension_type k, int closed,
			 ppl_const_Coefficient_t n,
			 ppl_const_Coefficient_t d))
    : s_e(se), r_l_b(rlb), l_u_b(lub) {
  }

  void set_empty() {
    s_e();
  }

  void raise_lower_bound(ppl_dimension_type k, bool closed,
			 const Coefficient& n, const Coefficient& d) {
    r_l_b(k, closed, to_const(&n), to_const(&d));
  }

  void lower_upper_bound(ppl_dimension_type k, bool closed,
			 const Coefficient& n, const Coefficient& d) {
    l_u_b(k, closed, to_const(&n), to_const(&d));
  }
};

} // namespace

int
ppl_Polyhedron_shrink_bounding_box
(ppl_const_Polyhedron_t ph,
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

  const Polyhedron& pph = *to_const(ph);
  CShrinkBox csbox(set_empty, raise_lower_bound, lower_upper_bound);
  pph.shrink_bounding_box(csbox, Complexity_Class(complexity));
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_relation_with_Constraint(ppl_const_Polyhedron_t ph,
					ppl_const_Constraint_t c) try {
  const Polyhedron& pph = *to_const(ph);
  const Constraint& cc = *to_const(c);
  return pph.relation_with(cc).get_flags();
}
CATCH_ALL

int
ppl_Polyhedron_relation_with_Generator(ppl_const_Polyhedron_t ph,
				       ppl_const_Generator_t g) try {
  const Polyhedron& pph = *to_const(ph);
  const Generator& gg = *to_const(g);
  return pph.relation_with(gg).get_flags();
}
CATCH_ALL

int
ppl_Polyhedron_is_empty(ppl_const_Polyhedron_t ph) try {
  const Polyhedron& pph = *to_const(ph);
  return pph.is_empty() ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_is_universe(ppl_const_Polyhedron_t ph) try {
  const Polyhedron& pph = *to_const(ph);
  return pph.is_universe() ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_is_bounded(ppl_const_Polyhedron_t ph) try {
  const Polyhedron& pph = *to_const(ph);
  return pph.is_bounded() ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_bounds_from_above(ppl_const_Polyhedron_t ph,
				 ppl_const_Linear_Expression_t le) try {
  const Polyhedron& pph = *to_const(ph);
  const Linear_Expression& lle = *to_const(le);
  return pph.bounds_from_above(lle) ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_bounds_from_below(ppl_const_Polyhedron_t ph,
				 ppl_const_Linear_Expression_t le) try {
  const Polyhedron& pph = *to_const(ph);
  const Linear_Expression& lle = *to_const(le);
  return pph.bounds_from_below(lle) ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_maximize(ppl_const_Polyhedron_t ph,
			ppl_const_Linear_Expression_t le,
			ppl_Coefficient_t sup_n,
			ppl_Coefficient_t sup_d,
			int* pmaximum,
			ppl_const_Generator_t* ppoint) try {
  const Polyhedron& pph = *to_const(ph);
  const Linear_Expression& lle = *to_const(le);
  Coefficient& ssup_n = *to_nonconst(sup_n);
  Coefficient& ssup_d = *to_nonconst(sup_d);
  bool maximum;
  bool ok = ppoint != 0
    ? pph.maximize(lle, ssup_n, ssup_d, maximum,
		   reinterpret_cast<const Generator**>(ppoint))
    : pph.maximize(lle, ssup_n, ssup_d, maximum);
  if (ok)
    *pmaximum = maximum ? 1 : 0;
  return ok ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_minimize(ppl_const_Polyhedron_t ph,
			ppl_const_Linear_Expression_t le,
			ppl_Coefficient_t inf_n,
			ppl_Coefficient_t inf_d,
			int* pminimum,
			ppl_const_Generator_t* ppoint) try {
  const Polyhedron& pph = *to_const(ph);
  const Linear_Expression& lle = *to_const(le);
  Coefficient& iinf_n = *to_nonconst(inf_n);
  Coefficient& iinf_d = *to_nonconst(inf_d);
  bool minimum;
  bool ok = ppoint != 0
    ? pph.minimize(lle, iinf_n, iinf_d, minimum,
		   reinterpret_cast<const Generator**>(ppoint))
    : pph.minimize(lle, iinf_n, iinf_d, minimum);
  if (ok)
    *pminimum = minimum ? 1 : 0;
  return ok ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_is_topologically_closed(ppl_const_Polyhedron_t ph) try {
  const Polyhedron& pph = *to_const(ph);
  return pph.is_topologically_closed() ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_topological_closure_assign(ppl_Polyhedron_t ph) try {
  Polyhedron& pph = *to_nonconst(ph);
  pph.topological_closure_assign();
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_contains_Polyhedron(ppl_const_Polyhedron_t x,
				   ppl_const_Polyhedron_t y) try {
  const Polyhedron& xx = *to_const(x);
  const Polyhedron& yy = *to_const(y);
  return xx.contains(yy) ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_strictly_contains_Polyhedron(ppl_const_Polyhedron_t x,
					    ppl_const_Polyhedron_t y) try {
  const Polyhedron& xx = *to_const(x);
  const Polyhedron& yy = *to_const(y);
  return xx.strictly_contains(yy) ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_is_disjoint_from_Polyhedron(ppl_const_Polyhedron_t x,
					   ppl_const_Polyhedron_t y) try {
  const Polyhedron& xx = *to_const(x);
  const Polyhedron& yy = *to_const(y);
  return xx.is_disjoint_from(yy) ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_equals_Polyhedron(ppl_const_Polyhedron_t x,
				 ppl_const_Polyhedron_t y) try {
  const Polyhedron& xx = *to_const(x);
  const Polyhedron& yy = *to_const(y);
  return (xx == yy) ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_OK(ppl_const_Polyhedron_t ph) try {
  return to_const(ph)->OK() ? 1 : 0;
}
CATCH_ALL

int
ppl_io_print_variable(ppl_dimension_type var) try {
  const char* b = c_variable_output_function(var);
  if (b == 0 || puts(b) < 0)
    return PPL_STDIO_ERROR;
  return 0;
}
CATCH_ALL

int
ppl_io_fprint_variable(FILE* stream, ppl_dimension_type var) try {
  const char* b = c_variable_output_function(var);
  if (b == 0 || fputs(b, stream) < 0)
    return PPL_STDIO_ERROR;
  return 0;
}
CATCH_ALL

#define DEFINE_PRINT_FUNCTIONS(Type) \
int \
ppl_io_print_ ## Type(ppl_const_ ## Type ## _t x) try { \
  using namespace IO_Operators; \
  std::ostringstream s; \
  s << *to_const(x); \
  if (puts(s.str().c_str()) < 0) \
    return PPL_STDIO_ERROR; \
  return 0; \
} \
CATCH_ALL \
 \
int \
ppl_io_fprint_ ## Type(FILE* stream, ppl_const_ ## Type ## _t x) try { \
  using namespace IO_Operators; \
  std::ostringstream s; \
  s << *to_const(x); \
  if (fputs(s.str().c_str(), stream) < 0) \
    return PPL_STDIO_ERROR; \
  return 0; \
} \
CATCH_ALL

DEFINE_PRINT_FUNCTIONS(Coefficient)

DEFINE_PRINT_FUNCTIONS(Linear_Expression)

DEFINE_PRINT_FUNCTIONS(Constraint)

DEFINE_PRINT_FUNCTIONS(Constraint_System)

DEFINE_PRINT_FUNCTIONS(Generator)

DEFINE_PRINT_FUNCTIONS(Generator_System)

DEFINE_PRINT_FUNCTIONS(Polyhedron)

int
ppl_io_set_variable_output_function(ppl_io_variable_output_function_type* p)
try {
  c_variable_output_function = p;
  return 0;
}
CATCH_ALL

int
ppl_io_get_variable_output_function(ppl_io_variable_output_function_type** pp)
try {
  *pp = c_variable_output_function;
  return 0;
}
CATCH_ALL
