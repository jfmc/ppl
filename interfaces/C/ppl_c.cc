/* Implementation of the C interface.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "Integer.defs.hh"
#include "LinExpression.defs.hh"
#include "Constraint.defs.hh"
#include "ConSys.defs.hh"
#include "Generator.defs.hh"
#include "GenSys.defs.hh"
#include "Polyhedron.defs.hh"
#include "C_Polyhedron.defs.hh"
#include "NNC_Polyhedron.defs.hh"
#include "Init.defs.hh"
#include "ppl_c.h"
#include <stdexcept>

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

// FIXME: this temporary until we rename Integer to Coefficient.
typedef Parma_Polyhedra_Library::Integer Coefficient;

static void (*user_error_handler)(enum ppl_enum_error_code code,
				  const char* description) = 0;

int
ppl_set_error_handler(void (*h)(enum ppl_enum_error_code code,
				const char* description)) {
  user_error_handler = h;
  return 0;
}

static void
notify_error(enum ppl_enum_error_code code, const char* description) {
  if (user_error_handler != 0)
    user_error_handler(code, description);
}

#define CATCH_STD_EXCEPTION(exception, code) \
catch(const std::exception& e) { \
  notify_error(code, e.what()); \
  return code; \
}

#define CATCH_ALL \
CATCH_STD_EXCEPTION(bad_alloc, PPL_ERROR_OUT_OF_MEMORY) \
CATCH_STD_EXCEPTION(invalid_argument, PPL_ERROR_INVALID_ARGUMENT) \
CATCH_STD_EXCEPTION(runtime_error, PPL_ERROR_INTERNAL_ERROR) \
CATCH_STD_EXCEPTION(exception, PPL_ERROR_INTERNAL_ERROR) \
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

static Init* init_object_ptr = 0;

int
ppl_max_space_dimension(ppl_dimension_type* m) try {
  *m = max_space_dimension();
  return 0;
}
CATCH_ALL

int
ppl_initialize(void) try {
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
  return 0;
}
CATCH_ALL

int
ppl_finalize(void) try {
  delete init_object_ptr;
  return 0;
}
CATCH_ALL

DECLARE_CONVERSIONS(Coefficient)

DECLARE_CONVERSIONS(LinExpression)

DECLARE_CONVERSIONS(Constraint)

DECLARE_CONVERSIONS(ConSys)

typedef ConSys::const_iterator ConSys_const_iterator;
DECLARE_CONVERSIONS(ConSys_const_iterator)

DECLARE_CONVERSIONS(Generator)

DECLARE_CONVERSIONS(GenSys)

typedef GenSys::const_iterator GenSys_const_iterator;
DECLARE_CONVERSIONS(GenSys_const_iterator)

DECLARE_CONVERSIONS(Polyhedron)


int
ppl_new_Coefficient(ppl_Coefficient_t* pc) try {
  *pc = to_nonconst(new Integer(0));
  return 0;
}
CATCH_ALL

int
ppl_new_Coefficient_from_mpz_t(ppl_Coefficient_t* pc, mpz_t z) try {
  *pc = to_nonconst(new Integer(z));
  return 0;
}
CATCH_ALL

int
ppl_new_Coefficient_from_Coefficient(ppl_Coefficient_t* pc,
				     ppl_const_Coefficient_t c) try {
  const Coefficient& cc = *to_const(c);
  *pc = to_nonconst(new Integer(cc));
  return 0;
}
CATCH_ALL

int
ppl_Coefficient_to_mpz_t(ppl_const_Coefficient_t c, mpz_t z) try {
  mpz_set(z, to_const(c)->get_mpz_t());
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
  mpz_set(ddst.get_mpz_t(), z);
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
ppl_new_LinExpression(ppl_LinExpression_t* ple) try {
  *ple = to_nonconst(new LinExpression());
  return 0;
}
CATCH_ALL

int
ppl_new_LinExpression_with_dimension(ppl_LinExpression_t* ple,
				     ppl_dimension_type d) try {
  *ple = to_nonconst(new LinExpression(0*Variable(d)));
  return 0;
}
CATCH_ALL

int
ppl_new_LinExpression_from_LinExpression(ppl_LinExpression_t* ple,
					 ppl_const_LinExpression_t le) try {
  const LinExpression& lle = *to_const(le);
  *ple = to_nonconst(new LinExpression(lle));
  return 0;
}
CATCH_ALL

int
ppl_delete_LinExpression(ppl_const_LinExpression_t le) try {
  delete to_const(le);
  return 0;
}
CATCH_ALL

int
ppl_assign_LinExpression_from_LinExpression(ppl_LinExpression_t dst,
					    ppl_const_LinExpression_t src) try {
  const LinExpression& ssrc = *to_const(src);
  LinExpression& ddst = *to_nonconst(dst);
  ddst = ssrc;
  return 0;
  }
CATCH_ALL

int
ppl_LinExpression_add_to_coefficient(ppl_LinExpression_t le,
				     ppl_dimension_type var,
				     ppl_const_Coefficient_t n) try {
  LinExpression& lle = *to_nonconst(le);
  const Integer& nn = *to_const(n);
  lle += nn * Variable(var);
  return 0;
}
CATCH_ALL

int
ppl_LinExpression_add_to_inhomogeneous(ppl_LinExpression_t le,
				       ppl_const_Coefficient_t n) try {
  LinExpression& lle = *to_nonconst(le);
  const Integer& nn = *to_const(n);
  lle += nn;
  return 0;
}
CATCH_ALL

int
ppl_LinExpression_space_dimension(ppl_const_LinExpression_t le) try {
  return to_const(le)->space_dimension();
}
CATCH_ALL

int
ppl_LinExpression_OK(ppl_const_LinExpression_t /* le */) try {
  return 1;
}
CATCH_ALL


int
ppl_new_Constraint(ppl_Constraint_t* pc,
		   ppl_const_LinExpression_t le,
		   enum ppl_enum_Constraint_Type t) try {
  Constraint* ppc;
  const LinExpression& lle = *to_const(le);
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
ppl_Constraint_space_dimension(ppl_const_Constraint_t c) try {
  return to_const(c)->space_dimension();
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
			   int var,
			   ppl_Coefficient_t n) try {
  const Constraint& cc = *to_const(c);
  Integer& nn = *to_nonconst(n);
  nn = cc.coefficient(Variable(var));
  return 0;
}
CATCH_ALL

int
ppl_Constraint_inhomogeneous_term(ppl_const_Constraint_t c,
				  ppl_Coefficient_t n) try {
  const Constraint& cc = *to_const(c);
  Integer& nn = *to_nonconst(n);
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
ppl_new_LinExpression_from_Constraint(ppl_LinExpression_t* ple,
				      ppl_const_Constraint_t c) try {
  const Constraint& cc = *to_const(c);
  *ple = to_nonconst(new LinExpression(cc));
  return 0;
}
CATCH_ALL


int
ppl_new_ConSys(ppl_ConSys_t* pcs) try {
  *pcs = to_nonconst(new ConSys());
  return 0;
}
CATCH_ALL

int
ppl_new_ConSys_zero_dim_empty(ppl_ConSys_t* pcs) try {
  *pcs = to_nonconst(new ConSys(ConSys::zero_dim_empty()));
  return 0;
}
CATCH_ALL


int
ppl_new_ConSys_from_Constraint(ppl_ConSys_t* pcs,
			       ppl_const_Constraint_t c) try {
  const Constraint& cc = *to_const(c);
  *pcs = to_nonconst(new ConSys(cc));
  return 0;
}
CATCH_ALL

int
ppl_new_ConSys_from_ConSys(ppl_ConSys_t* pcs, ppl_const_ConSys_t cs) try {
  const ConSys& ccs = *to_const(cs);
  *pcs = to_nonconst(new ConSys(ccs));
  return 0;
}
CATCH_ALL

int
ppl_delete_ConSys(ppl_const_ConSys_t cs) try {
  delete to_const(cs);
  return 0;
}
CATCH_ALL

int
ppl_assign_ConSys_from_ConSys(ppl_ConSys_t dst, ppl_const_ConSys_t src) try {
  const ConSys& ssrc = *to_const(src);
  ConSys& ddst = *to_nonconst(dst);
  ddst = ssrc;
  return 0;
}
CATCH_ALL

int
ppl_ConSys_space_dimension(ppl_const_ConSys_t cs) try {
  return to_const(cs)->space_dimension();
}
CATCH_ALL

int
ppl_ConSys_insert_Constraint(ppl_ConSys_t cs, ppl_const_Constraint_t c) try {
  const Constraint& cc = *to_const(c);
  ConSys& ccs = *to_nonconst(cs);
  ccs.insert(cc);
  return 0;
}
CATCH_ALL

int
ppl_ConSys_OK(ppl_const_ConSys_t cs) try {
  return to_const(cs)->OK() ? 1 : 0;
}
CATCH_ALL


int
ppl_new_ConSys_const_iterator(ppl_ConSys_const_iterator_t* pcit) try {
  *pcit = to_nonconst(new ConSys::const_iterator());
  return 0;
}
CATCH_ALL

int
ppl_new_ConSys_const_iterator_from_ConSys_const_iterator
(ppl_ConSys_const_iterator_t* pcit,
 ppl_const_ConSys_const_iterator_t cit)  try {
  *pcit = to_nonconst(new ConSys::const_iterator(*to_const(cit)));
  return 0;
}
CATCH_ALL

int
ppl_delete_ConSys_const_iterator(ppl_const_ConSys_const_iterator_t cit)
  try {
  delete to_const(cit);
  return 0;
}
CATCH_ALL

int
ppl_assign_ConSys_const_iterator_from_ConSys_const_iterator
(ppl_ConSys_const_iterator_t dst,
 ppl_const_ConSys_const_iterator_t src) try {
  const ConSys::const_iterator& ssrc = *to_const(src);
  ConSys::const_iterator& ddst = *to_nonconst(dst);
  ddst = ssrc;
  return 0;
}
CATCH_ALL

int
ppl_ConSys_begin(ppl_const_ConSys_t cs, ppl_ConSys_const_iterator_t cit)
  try {
  const ConSys& ccs = *to_const(cs);
  ConSys::const_iterator& ccit = *to_nonconst(cit);
  ccit = ccs.begin();
  return 0;
}
CATCH_ALL

int
ppl_ConSys_end(ppl_const_ConSys_t cs, ppl_ConSys_const_iterator_t cit) try {
  const ConSys& ccs = *to_const(cs);
  ConSys::const_iterator& ccit = *to_nonconst(cit);
  ccit = ccs.end();
  return 0;
}
CATCH_ALL

int
ppl_ConSys_const_iterator_dereference(ppl_const_ConSys_const_iterator_t cit,
				       ppl_const_Constraint_t* pc) try {
  const ConSys::const_iterator& ccit = *to_const(cit);
  const Constraint& c = *ccit;
  *pc = to_const(&c);
  return 0;
}
CATCH_ALL

int
ppl_ConSys_const_iterator_increment(ppl_ConSys_const_iterator_t cit) try {
  ConSys::const_iterator& ccit = *to_nonconst(cit);
  ++ccit;
  return 0;
}
CATCH_ALL

int
ppl_ConSys_const_iterator_equal_test
(ppl_const_ConSys_const_iterator_t x,
 ppl_const_ConSys_const_iterator_t y) try {
  const ConSys::const_iterator& xx = *to_const(x);
  const ConSys::const_iterator& yy = *to_const(y);
  return (xx == yy) ? 1 : 0;
}
CATCH_ALL


int
ppl_new_Generator(ppl_Generator_t* pg,
		  ppl_const_LinExpression_t le,
		  enum ppl_enum_Generator_Type t,
		  ppl_const_Coefficient_t d) try {
  Generator* ppg;
  const LinExpression& lle = *to_const(le);
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
ppl_Generator_space_dimension(ppl_const_Generator_t g) try {
  return to_const(g)->space_dimension();
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
			  int var,
			  ppl_Coefficient_t n) try {
  const Generator& gg = *to_const(g);
  Integer& nn = *to_nonconst(n);
  nn = gg.coefficient(Variable(var));
  return 0;
}
CATCH_ALL

int
ppl_Generator_divisor(ppl_const_Generator_t g,
		      ppl_Coefficient_t n) try {
  const Generator& gg = *to_const(g);
  Integer& nn = *to_nonconst(n);
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
ppl_new_LinExpression_from_Generator(ppl_LinExpression_t* ple,
				     ppl_const_Generator_t g) try {
  const Generator& gg = *to_const(g);
  *ple = to_nonconst(new LinExpression(gg));
  return 0;
}
CATCH_ALL


int
ppl_new_GenSys(ppl_GenSys_t* pgs) try {
  *pgs = to_nonconst(new GenSys());
  return 0;
}
CATCH_ALL

int
ppl_new_GenSys_zero_dim_univ(ppl_GenSys_t* pgs) try {
  *pgs = to_nonconst(new GenSys(GenSys::zero_dim_univ()));
  return 0;
}
CATCH_ALL

int
ppl_new_GenSys_from_Generator(ppl_GenSys_t* pgs,
			      ppl_const_Generator_t g) try {
  const Generator& gg = *to_const(g);
  *pgs = to_nonconst(new GenSys(gg));
  return 0;
}
CATCH_ALL

int
ppl_new_GenSys_from_GenSys(ppl_GenSys_t* pgs, ppl_const_GenSys_t gs) try {
  const GenSys& ggs = *to_const(gs);
  *pgs = to_nonconst(new GenSys(ggs));
  return 0;
}
CATCH_ALL

int
ppl_delete_GenSys(ppl_const_GenSys_t gs) try {
  delete to_const(gs);
  return 0;
}
CATCH_ALL

int
ppl_assign_GenSys_from_GenSys(ppl_GenSys_t dst, ppl_const_GenSys_t src) try {
  const GenSys& ssrc = *to_const(src);
  GenSys& ddst = *to_nonconst(dst);
  ddst = ssrc;
  return 0;
}
CATCH_ALL

int
ppl_GenSys_space_dimension(ppl_const_GenSys_t gs) try {
  return to_const(gs)->space_dimension();
}
CATCH_ALL

int
ppl_GenSys_insert_Generator(ppl_GenSys_t gs, ppl_const_Generator_t g) try {
  const Generator& gg = *to_const(g);
  GenSys& ggs = *to_nonconst(gs);
  ggs.insert(gg);
  return 0;
}
CATCH_ALL

int
ppl_GenSys_OK(ppl_const_GenSys_t gs) try {
  return to_const(gs)->OK() ? 1 : 0;
}
CATCH_ALL


int
ppl_new_GenSys_const_iterator(ppl_GenSys_const_iterator_t* pgit) try {
  *pgit = to_nonconst(new GenSys::const_iterator());
  return 0;
}
CATCH_ALL

int
ppl_new_GenSys_const_iterator_from_GenSys_const_iterator
(ppl_GenSys_const_iterator_t* pgit,
 ppl_const_GenSys_const_iterator_t git)  try {
  *pgit = to_nonconst(new GenSys::const_iterator(*to_const(git)));
  return 0;
}
CATCH_ALL

int
ppl_delete_GenSys_const_iterator(ppl_const_GenSys_const_iterator_t git)
  try {
  delete to_const(git);
  return 0;
}
CATCH_ALL

int
ppl_assign_GenSys_const_iterator_from_GenSys_const_iterator
(ppl_GenSys_const_iterator_t dst,
 ppl_const_GenSys_const_iterator_t src) try {
  const GenSys::const_iterator& ssrc = *to_const(src);
  GenSys::const_iterator& ddst = *to_nonconst(dst);
  ddst = ssrc;
  return 0;
}
CATCH_ALL

int
ppl_GenSys_begin(ppl_const_GenSys_t gs, ppl_GenSys_const_iterator_t git)
  try {
  const GenSys& ggs = *to_const(gs);
  GenSys::const_iterator& ggit = *to_nonconst(git);
  ggit = ggs.begin();
  return 0;
}
CATCH_ALL

int
ppl_GenSys_end(ppl_const_GenSys_t gs, ppl_GenSys_const_iterator_t git) try {
  const GenSys& ggs = *to_const(gs);
  GenSys::const_iterator& ggit = *to_nonconst(git);
  ggit = ggs.end();
  return 0;
}
CATCH_ALL

int
ppl_GenSys_const_iterator_dereference(ppl_const_GenSys_const_iterator_t git,
				       ppl_const_Generator_t* pg) try {
  const GenSys::const_iterator& ggit = *to_const(git);
  const Generator& c = *ggit;
  *pg = to_const(&c);
  return 0;
}
CATCH_ALL

int
ppl_GenSys_const_iterator_increment(ppl_GenSys_const_iterator_t git) try {
  GenSys::const_iterator& ggit = *to_nonconst(git);
  ++ggit;
  return 0;
}
CATCH_ALL

int
ppl_GenSys_const_iterator_equal_test
(ppl_const_GenSys_const_iterator_t x,
 ppl_const_GenSys_const_iterator_t y) try {
  const GenSys::const_iterator& xx = *to_const(x);
  const GenSys::const_iterator& yy = *to_const(y);
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
ppl_new_C_Polyhedron_from_ConSys(ppl_Polyhedron_t* pph,
				 ppl_const_ConSys_t cs) try {
  const ConSys& ccs = *to_const(cs);
  *pph = to_nonconst(new C_Polyhedron(ccs));
  return 0;
}
CATCH_ALL

int
ppl_new_C_Polyhedron_recycle_ConSys(ppl_Polyhedron_t* pph,
				    ppl_ConSys_t cs) try {
  ConSys& ccs = *to_nonconst(cs);
  *pph = to_nonconst(new C_Polyhedron(ccs));
  return 0;
}
CATCH_ALL

int
ppl_new_NNC_Polyhedron_from_ConSys(ppl_Polyhedron_t* pph,
				   ppl_const_ConSys_t cs) try {
  const ConSys& ccs = *to_const(cs);
  *pph = to_nonconst(new NNC_Polyhedron(ccs));
  return 0;
}
CATCH_ALL

int
ppl_new_NNC_Polyhedron_recycle_ConSys(ppl_Polyhedron_t* pph,
				      ppl_ConSys_t cs) try {
  ConSys& ccs = *to_nonconst(cs);
  *pph = to_nonconst(new NNC_Polyhedron(ccs));
  return 0;
}
CATCH_ALL

int
ppl_new_C_Polyhedron_from_GenSys(ppl_Polyhedron_t* pph,
				 ppl_const_GenSys_t gs) try {
  const GenSys& ggs = *to_const(gs);
  *pph = to_nonconst(new C_Polyhedron(ggs));
  return 0;
}
CATCH_ALL

int
ppl_new_C_Polyhedron_recycle_GenSys(ppl_Polyhedron_t* pph,
				    ppl_GenSys_t gs) try {
  GenSys& ggs = *to_nonconst(gs);
  *pph = to_nonconst(new C_Polyhedron(ggs));
  return 0;
}
CATCH_ALL

int
ppl_new_NNC_Polyhedron_from_GenSys(ppl_Polyhedron_t* pph,
				   ppl_const_GenSys_t gs) try {
  const GenSys& ggs = *to_const(gs);
  *pph = to_nonconst(new C_Polyhedron(ggs));
  return 0;
}
CATCH_ALL

int
ppl_new_NNC_Polyhedron_recycle_GenSys(ppl_Polyhedron_t* pph,
				      ppl_GenSys_t gs) try {
  GenSys& ggs = *to_nonconst(gs);
  *pph = to_nonconst(new C_Polyhedron(ggs));
  return 0;
}
CATCH_ALL

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
		       Integer& n, Integer& d) const {
    return g_l_b(k, closed, to_nonconst(&n), to_nonconst(&d)) != 0;
  }

  bool get_upper_bound(ppl_dimension_type k, bool closed,
		       Integer& n, Integer& d) const {
    return g_u_b(k, closed, to_nonconst(&n), to_nonconst(&d)) != 0;
  }
};

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
ppl_Polyhedron_space_dimension(ppl_const_Polyhedron_t ph) try {
  return to_const(ph)->space_dimension();
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
ppl_Polyhedron_poly_difference_assign_and_minimize
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  return xx.poly_difference_assign_and_minimize(yy) ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_BBRZ02_widening_assign(ppl_Polyhedron_t x,
				      ppl_const_Polyhedron_t y) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  xx.BBRZ02_widening_assign(yy);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_limited_BBRZ02_widening_assign(ppl_Polyhedron_t x,
					      ppl_const_Polyhedron_t y,
					      ppl_ConSys_t cs) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  ConSys& ccs = *to_nonconst(cs);
  xx.limited_BBRZ02_widening_assign(yy, ccs);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_H79_widening_assign(ppl_Polyhedron_t x,
				   ppl_const_Polyhedron_t y) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  xx.H79_widening_assign(yy);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_limited_H79_widening_assign(ppl_Polyhedron_t x,
					   ppl_const_Polyhedron_t y,
					   ppl_ConSys_t cs) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  ConSys& ccs = *to_nonconst(cs);
  xx.limited_H79_widening_assign(yy, ccs);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_constraints(ppl_const_Polyhedron_t ph,
			   ppl_const_ConSys_t* pcs) try {
  const Polyhedron& pph = *to_const(ph);
  const ConSys& cs = pph.constraints();
  *pcs = to_const(&cs);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_minimized_constraints(ppl_const_Polyhedron_t ph,
				     ppl_const_ConSys_t* pcs) try {
  const Polyhedron& pph = *to_const(ph);
  const ConSys& cs = pph.minimized_constraints();
  *pcs = to_const(&cs);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_generators(ppl_const_Polyhedron_t ph,
			  ppl_const_GenSys_t* pgs) try {
  const Polyhedron& pph = *to_const(ph);
  const GenSys& gs = pph.generators();
  *pgs = to_const(&gs);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_minimized_generators(ppl_const_Polyhedron_t ph,
				    ppl_const_GenSys_t* pgs) try {
  const Polyhedron& pph = *to_const(ph);
  const GenSys& gs = pph.minimized_generators();
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
ppl_Polyhedron_add_generator(ppl_Polyhedron_t ph,
			     ppl_const_Generator_t g) try {
  Polyhedron& pph = *to_nonconst(ph);
  const Generator& gg = *to_const(g);
  pph.add_generator(gg);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_constraints(ppl_Polyhedron_t ph, ppl_ConSys_t cs) try {
  Polyhedron& pph = *to_nonconst(ph);
  ConSys& ccs = *to_nonconst(cs);
  pph.add_constraints(ccs);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_constraints_and_minimize(ppl_Polyhedron_t ph,
					    ppl_ConSys_t cs) try {
  Polyhedron& pph = *to_nonconst(ph);
  ConSys& ccs = *to_nonconst(cs);
  return pph.add_constraints_and_minimize(ccs) ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_generators(ppl_Polyhedron_t ph, ppl_GenSys_t gs) try {
  Polyhedron& pph = *to_nonconst(ph);
  GenSys& ggs = *to_nonconst(gs);
  pph.add_generators(ggs);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_generators_and_minimize(ppl_Polyhedron_t ph,
					    ppl_GenSys_t gs) try {
  Polyhedron& pph = *to_nonconst(ph);
  GenSys& ggs = *to_nonconst(gs);
  return pph.add_generators_and_minimize(ggs) ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_dimensions_and_embed(ppl_Polyhedron_t ph,
					ppl_dimension_type d) try {
  Polyhedron& pph = *to_nonconst(ph);
  pph.add_dimensions_and_embed(d);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_dimensions_and_project(ppl_Polyhedron_t ph,
					  ppl_dimension_type d) try {
  Polyhedron& pph = *to_nonconst(ph);
  pph.add_dimensions_and_project(d);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_remove_dimensions(ppl_Polyhedron_t ph,
				 size_t ds[],
				 unsigned int n) try {
  Polyhedron& pph = *to_nonconst(ph);
  std::set<Variable> to_be_removed;
  for (ppl_dimension_type i = 0; i < n; ++i)
    to_be_removed.insert(Variable(ds[i]));
  pph.remove_dimensions(to_be_removed);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_remove_higher_dimensions(ppl_Polyhedron_t ph,
					ppl_dimension_type d) try {
  Polyhedron& pph = *to_nonconst(ph);
  pph.remove_higher_dimensions(d);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_affine_image(ppl_Polyhedron_t ph,
			    ppl_dimension_type var,
			    ppl_const_LinExpression_t le,
			    ppl_const_Coefficient_t d) try {
  Polyhedron& pph = *to_nonconst(ph);
  const LinExpression& lle = *to_const(le);
  const Integer& dd = *to_const(d);
  pph.affine_image(Variable(var), lle, dd);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_affine_preimage(ppl_Polyhedron_t ph,
			       ppl_dimension_type var,
			       ppl_const_LinExpression_t le,
			       ppl_const_Coefficient_t d) try {
  Polyhedron& pph = *to_nonconst(ph);
  const LinExpression& lle = *to_const(le);
  const Integer& dd = *to_const(d);
  pph.affine_preimage(Variable(var), lle, dd);
  return 0;
}
CATCH_ALL

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
			 const Integer& n, const Integer& d) {
    r_l_b(k, closed, to_const(&n), to_const(&d));
  }

  void lower_upper_bound(ppl_dimension_type k, bool closed,
			 const Integer& n, const Integer& d) {
    l_u_b(k, closed, to_const(&n), to_const(&d));
  }
};

int
ppl_Polyhedron_shrink_bounding_box
(ppl_const_Polyhedron_t ph,
 void (*set_empty)(void),
 void (*raise_lower_bound)(ppl_dimension_type k, int closed,
			   ppl_const_Coefficient_t n,
			   ppl_const_Coefficient_t d),
 void (*lower_upper_bound)(ppl_dimension_type k, int closed,
			   ppl_const_Coefficient_t n,
			   ppl_const_Coefficient_t d)) try {
  const Polyhedron& pph = *to_const(ph);
  CShrinkBox csbox(set_empty, raise_lower_bound, lower_upper_bound);
  pph.shrink_bounding_box(csbox);

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
ppl_Polyhedron_check_empty(ppl_const_Polyhedron_t ph) try {
  const Polyhedron& pph = *to_const(ph);
  return pph.check_empty() ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_check_universe(ppl_const_Polyhedron_t ph) try {
  const Polyhedron& pph = *to_const(ph);
  return pph.check_universe() ? 1 : 0;
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
				 ppl_const_LinExpression_t le) try {
  const Polyhedron& pph = *to_const(ph);
  const LinExpression& lle = *to_const(le);
  return pph.bounds_from_above(lle) ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_bounds_from_below(ppl_const_Polyhedron_t ph,
				 ppl_const_LinExpression_t le) try {
  const Polyhedron& pph = *to_const(ph);
  const LinExpression& lle = *to_const(le);
  return pph.bounds_from_below(lle) ? 1 : 0;
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
  return (xx >= yy) ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_strictly_contains_Polyhedron(ppl_const_Polyhedron_t x,
					    ppl_const_Polyhedron_t y) try {
  const Polyhedron& xx = *to_const(x);
  const Polyhedron& yy = *to_const(y);
  return (xx > yy) ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_is_disjoint_from_Polyhedron(ppl_const_Polyhedron_t x,
					   ppl_const_Polyhedron_t y) try {
  const Polyhedron& xx = *to_const(x);
  const Polyhedron& yy = *to_const(y);
  return are_disjoint(xx, yy) ? 1 : 0;
}
CATCH_ALL

int
ppl_Polyhedron_OK(ppl_const_Polyhedron_t ph) try {
  return to_const(ph)->OK() ? 1 : 0;
}
CATCH_ALL
