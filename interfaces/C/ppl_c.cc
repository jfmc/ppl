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
#include "Init.defs.hh"
#include "ppl_c.h"

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

int
ppl_new_Coefficient_from_mpz_t(ppl_Coefficient_t* pc, mpz_t z) try {
  *pc = to_nonconst(new Integer(z));
  return 0;
}
CATCH_ALL

int
ppl_Coefficient_to_mpz_t(ppl_Coefficient_t c, mpz_t z) try {
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
ppl_Coefficient_OK(ppl_const_Coefficient_t /* c */) try {
  return 1;
}
CATCH_ALL


DECLARE_CONVERSIONS(LinExpression)

int
ppl_new_LinExpression(ppl_LinExpression_t* ple) try {
  *ple = to_nonconst(new LinExpression());
  return 0;
}
CATCH_ALL

int
ppl_new_LinExpression_with_dimension(ppl_LinExpression_t* ple,
				     unsigned int d) try {
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
ppl_swap_LinExpression(ppl_LinExpression_t x, ppl_LinExpression_t y) try {
  std::swap(*to_nonconst(x), *to_nonconst(y));
  return 0;
}
CATCH_ALL

int
ppl_LinExpression_add_to_coefficient(ppl_LinExpression_t le,
				     unsigned int var,
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


DECLARE_CONVERSIONS(Constraint)

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
#if 0
  case PPL_CONSTRAINT_TYPE_GREATER_THAN:
    ppc = new Constraint(lle > 0);
    break;
#endif
  case PPL_CONSTRAINT_TYPE_LESS_THAN_OR_EQUAL:
    ppc = new Constraint(lle <= 0);
    break;
#if 0
  case PPL_CONSTRAINT_TYPE_LESS_THAN:
    ppc = new Constraint(lle < 0);
    break;
#endif
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
ppl_swap_Constraint(ppl_Constraint_t x, ppl_Constraint_t y) try {
  std::swap(*to_nonconst(x), *to_nonconst(y));
  return 0;
}
CATCH_ALL

int
ppl_Constraint_space_dimension(ppl_const_Constraint_t c) try {
  return to_const(c)->space_dimension();
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


DECLARE_CONVERSIONS(ConSys)

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
ppl_swap_ConSys(ppl_ConSys_t x, ppl_ConSys_t y) try {
  std::swap(*to_nonconst(x), *to_nonconst(y));
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


typedef ConSys::const_iterator ConSys__const_iterator;
DECLARE_CONVERSIONS(ConSys__const_iterator)

int
ppl_new_ConSys__const_iterator(ppl_ConSys__const_iterator_t* pcit) try {
  *pcit = to_nonconst(new ConSys::const_iterator());
  return 0;
}
CATCH_ALL

int
ppl_new_ConSys__const_iterator_from_ConSys__const_iterator
(ppl_ConSys__const_iterator_t* pcit,
 ppl_const_ConSys__const_iterator_t cit)  try {
  *pcit = to_nonconst(new ConSys::const_iterator(*to_const(cit)));
  return 0;
}
CATCH_ALL

int
ppl_delete_ConSys__const_iterator(ppl_const_ConSys__const_iterator_t cit) try {
  delete to_const(cit);
  return 0;
}
CATCH_ALL

int
ppl_assign_ConSys__const_iterator_from_ConSys__const_iterator
(ppl_ConSys__const_iterator_t dst,
 ppl_const_ConSys__const_iterator_t src) try {
  const ConSys::const_iterator& ssrc = *to_const(src);
  ConSys::const_iterator& ddst = *to_nonconst(dst);
  ddst = ssrc;
  return 0;
}
CATCH_ALL

int
ppl_ConSys_begin(ppl_const_ConSys_t cs, ppl_ConSys__const_iterator_t cit) try {
  const ConSys& ccs = *to_const(cs);
  ConSys::const_iterator& ccit = *to_nonconst(cit);
  ccit = ccs.begin();
  return 0;
}
CATCH_ALL

int
ppl_ConSys_end(ppl_const_ConSys_t cs, ppl_ConSys__const_iterator_t cit) try {
  const ConSys& ccs = *to_const(cs);
  ConSys::const_iterator& ccit = *to_nonconst(cit);
  ccit = ccs.end();
  return 0;
}
CATCH_ALL

int
ppl_ConSys__const_iterator_dereference(ppl_const_ConSys__const_iterator_t cit,
				       ppl_const_Constraint_t* pc) try {
  const ConSys::const_iterator& ccit = *to_const(cit);
  const Constraint& c = *ccit;
  *pc = to_const(&c);
  return 0;
}
CATCH_ALL

int
ppl_ConSys__const_iterator_increment(ppl_ConSys__const_iterator_t cit) try {
  ConSys::const_iterator& ccit = *to_nonconst(cit);
  ++ccit;
  return 0;
}
CATCH_ALL

int
ppl_ConSys__const_iterator_equal_test
(ppl_const_ConSys__const_iterator_t x,
 ppl_const_ConSys__const_iterator_t y) try {
  const ConSys::const_iterator& xx = *to_const(x);
  const ConSys::const_iterator& yy = *to_const(y);
  return (xx == yy) ? 1 : 0;
}
CATCH_ALL


DECLARE_CONVERSIONS(Generator)

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
    ppg = new Generator(point(lle, dd));
    break;
#if 0
  case PPL_GENERATOR_TYPE_CLOSURE_POINT:
    ppg = new Generator(closure_point(lle, dd));
    break;
#endif
  case PPL_GENERATOR_TYPE_RAY:
    ppg = new Generator(ray(lle));
    break;
  case PPL_GENERATOR_TYPE_LINE:
    ppg = new Generator(line(lle));
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

#if 0
int
ppl_new_Generator_zero_dim_closure_point(ppl_Generator_t* pg) try {
  *pg = to_nonconst(new Generator(Generator::zero_dim_closure_point()));
  return 0;
}
CATCH_ALL
#endif

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
ppl_swap_Generator(ppl_Generator_t x, ppl_Generator_t y) try {
  std::swap(*to_nonconst(x), *to_nonconst(y));
  return 0;
}
CATCH_ALL

int
ppl_Generator_space_dimension(ppl_const_Generator_t g) try {
  return to_const(g)->space_dimension();
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


DECLARE_CONVERSIONS(GenSys)

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
ppl_swap_GenSys(ppl_GenSys_t x, ppl_GenSys_t y) try {
  std::swap(*to_nonconst(x), *to_nonconst(y));
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


typedef GenSys::const_iterator GenSys__const_iterator;
DECLARE_CONVERSIONS(GenSys__const_iterator)

int
ppl_new_GenSys__const_iterator(ppl_GenSys__const_iterator_t* pgit) try {
  *pgit = to_nonconst(new GenSys::const_iterator());
  return 0;
}
CATCH_ALL

int
ppl_new_GenSys__const_iterator_from_GenSys__const_iterator
(ppl_GenSys__const_iterator_t* pgit,
 ppl_const_GenSys__const_iterator_t git)  try {
  *pgit = to_nonconst(new GenSys::const_iterator(*to_const(git)));
  return 0;
}
CATCH_ALL

int
ppl_delete_GenSys__const_iterator(ppl_const_GenSys__const_iterator_t git) try {
  delete to_const(git);
  return 0;
}
CATCH_ALL

int
ppl_assign_GenSys__const_iterator_from_GenSys__const_iterator
(ppl_GenSys__const_iterator_t dst,
 ppl_const_GenSys__const_iterator_t src) try {
  const GenSys::const_iterator& ssrc = *to_const(src);
  GenSys::const_iterator& ddst = *to_nonconst(dst);
  ddst = ssrc;
  return 0;
}
CATCH_ALL

int
ppl_GenSys_begin(ppl_const_GenSys_t gs, ppl_GenSys__const_iterator_t git) try {
  const GenSys& ggs = *to_const(gs);
  GenSys::const_iterator& ggit = *to_nonconst(git);
  ggit = ggs.begin();
  return 0;
}
CATCH_ALL

int
ppl_GenSys_end(ppl_const_GenSys_t gs, ppl_GenSys__const_iterator_t git) try {
  const GenSys& ggs = *to_const(gs);
  GenSys::const_iterator& ggit = *to_nonconst(git);
  ggit = ggs.end();
  return 0;
}
CATCH_ALL

int
ppl_GenSys__const_iterator_dereference(ppl_const_GenSys__const_iterator_t git,
				       ppl_const_Generator_t* pg) try {
  const GenSys::const_iterator& ggit = *to_const(git);
  const Generator& c = *ggit;
  *pg = to_const(&c);
  return 0;
}
CATCH_ALL

int
ppl_GenSys__const_iterator_increment(ppl_GenSys__const_iterator_t git) try {
  GenSys::const_iterator& ggit = *to_nonconst(git);
  ++ggit;
  return 0;
}
CATCH_ALL

int
ppl_GenSys__const_iterator_equal_test
(ppl_const_GenSys__const_iterator_t x,
 ppl_const_GenSys__const_iterator_t y) try {
  const GenSys::const_iterator& xx = *to_const(x);
  const GenSys::const_iterator& yy = *to_const(y);
  return (xx == yy) ? 1 : 0;
}
CATCH_ALL


DECLARE_CONVERSIONS(Polyhedron)

int
ppl_new_Polyhedron_from_dimension(ppl_Polyhedron_t* pph, unsigned int d) try {
  *pph = to_nonconst(new Polyhedron(d, Polyhedron::UNIVERSE));
  return 0;
}
CATCH_ALL

int
ppl_new_Polyhedron_empty_from_dimension(ppl_Polyhedron_t* pph,
					unsigned int d) try {
  *pph = to_nonconst(new Polyhedron(d, Polyhedron::EMPTY));
  return 0;
}
CATCH_ALL

int
ppl_new_Polyhedron_from_Polyhedron(ppl_Polyhedron_t* pph,
				   ppl_const_Polyhedron_t ph) try {
  const Polyhedron& phh = *to_const(ph);
  *pph = to_nonconst(new Polyhedron(phh));
  return 0;
}
CATCH_ALL

int
ppl_new_Polyhedron_from_ConSys(ppl_Polyhedron_t* pph, ppl_ConSys_t cs) try {
  ConSys& ccs = *to_nonconst(cs);
  *pph = to_nonconst(new Polyhedron(ccs));
  return 0;
}
CATCH_ALL

int
ppl_new_Polyhedron_from_GenSys(ppl_Polyhedron_t* pph, ppl_GenSys_t gs) try {
  GenSys& ggs = *to_nonconst(gs);
  *pph = to_nonconst(new Polyhedron(ggs));
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
ppl_assign_Polyhedron_from_Polyhedron(ppl_Polyhedron_t dst,
				      ppl_const_Polyhedron_t src) try {
  const Polyhedron& ssrc = *to_const(src);
  Polyhedron& ddst = *to_nonconst(dst);
  ddst = ssrc;
  return 0;
}
CATCH_ALL

int
ppl_swap_Polyhedron(ppl_Polyhedron_t x, ppl_Polyhedron_t y) try {
  std::swap(*to_nonconst(x), *to_nonconst(y));
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
  xx.intersection_assign_and_minimize(yy);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_convex_hull_assign(ppl_Polyhedron_t x,
				  ppl_const_Polyhedron_t y) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  xx.convex_hull_assign(yy);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_convex_hull_assign_and_minimize(ppl_Polyhedron_t x,
					       ppl_const_Polyhedron_t y) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  xx.convex_hull_assign_and_minimize(yy);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_convex_difference_assign(ppl_Polyhedron_t x,
					ppl_const_Polyhedron_t y) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  xx.convex_difference_assign(yy);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_convex_difference_assign_and_minimize
(ppl_Polyhedron_t x, ppl_const_Polyhedron_t y) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  xx.convex_difference_assign_and_minimize(yy);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_widening_assign(ppl_Polyhedron_t x,
			       ppl_const_Polyhedron_t y) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  xx.widening_assign(yy);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_limited_widening_assign(ppl_Polyhedron_t x,
				       ppl_const_Polyhedron_t y,
				       ppl_ConSys_t cs) try {
  Polyhedron& xx = *to_nonconst(x);
  const Polyhedron& yy = *to_const(y);
  ConSys& ccs = *to_nonconst(cs);
  xx.limited_widening_assign(yy, ccs);
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
ppl_Polyhedron_generators(ppl_const_Polyhedron_t ph,
			  ppl_const_GenSys_t* pgs) try {
  const Polyhedron& pph = *to_const(ph);
  const GenSys& gs = pph.generators();
  *pgs = to_const(&gs);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_constraint(ppl_Polyhedron_t ph,
			      ppl_const_Constraint_t c) try {
  Polyhedron& pph = *to_nonconst(ph);
  const Constraint& cc = *to_const(c);
  pph.insert(cc);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_generator(ppl_Polyhedron_t ph,
			     ppl_const_Generator_t g) try {
  Polyhedron& pph = *to_nonconst(ph);
  const Generator& gg = *to_const(g);
  pph.insert(gg);
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
  pph.add_generators_and_minimize(ggs);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_dimensions_and_embed(ppl_Polyhedron_t ph,
					unsigned int d) try {
  Polyhedron& pph = *to_nonconst(ph);
  pph.add_dimensions_and_embed(d);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_dimensions_and_project(ppl_Polyhedron_t ph,
					  unsigned int d) try {
  Polyhedron& pph = *to_nonconst(ph);
  pph.add_dimensions_and_project(d);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_remove_dimensions(ppl_Polyhedron_t ph,
				 unsigned int ds[],
				 unsigned int n) try {
  Polyhedron& pph = *to_nonconst(ph);
  std::set<Variable> to_be_removed;
  for (unsigned int i = 0; i < n; ++i)
    to_be_removed.insert(Variable(ds[i]));
  pph.remove_dimensions(to_be_removed);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_remove_higher_dimensions(ppl_Polyhedron_t ph,
					unsigned int d) try {
  Polyhedron& pph = *to_nonconst(ph);
  pph.remove_higher_dimensions(d);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_add_dimensions_and_constraints(ppl_Polyhedron_t ph,
					      ppl_ConSys_t cs) try {
  Polyhedron& pph = *to_nonconst(ph);
  ConSys& ccs = *to_nonconst(cs);
  pph.add_dimensions_and_constraints(ccs);
  return 0;
}
CATCH_ALL

int
ppl_Polyhedron_affine_image(ppl_Polyhedron_t ph,
			    unsigned int var,
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
			       unsigned int var,
			       ppl_const_LinExpression_t le,
			       ppl_const_Coefficient_t d) try {
  Polyhedron& pph = *to_nonconst(ph);
  const LinExpression& lle = *to_const(le);
  const Integer& dd = *to_const(d);
  pph.affine_preimage(Variable(var), lle, dd);
  return 0;
}
CATCH_ALL

class CBox {
private:
  void (*r_l_b)(unsigned int k, int closed,
		ppl_const_Coefficient_t n,
		ppl_const_Coefficient_t d);
  void (*l_u_b)(unsigned int k, int closed,
		ppl_const_Coefficient_t n,
		ppl_const_Coefficient_t d);
  void (*s_e)(unsigned int k);

public:
  CBox(void (*rlb)(unsigned int k, int closed,
		  ppl_const_Coefficient_t n,
		  ppl_const_Coefficient_t d),
      void (*lub)(unsigned int k, int closed,
		  ppl_const_Coefficient_t n,
		  ppl_const_Coefficient_t d),
      void (*se)(unsigned int k))
    : r_l_b(rlb), l_u_b(lub), s_e(se) {
  }

  void raise_lower_bound(unsigned int k, bool closed,
			 const Integer& n, const Integer& d) {
    r_l_b(k, closed, to_const(&n), to_const(&d));
  }

  void lower_upper_bound(unsigned int k, bool closed,
			 const Integer& n, const Integer& d) {
    l_u_b(k, closed, to_const(&n), to_const(&d));
  }

  void set_empty(unsigned int k) {
    s_e(k);
  }
};

int
ppl_Polyhedron_shrink_bounding_box
(ppl_const_Polyhedron_t ph,
 void (*raise_lower_bound)(unsigned int k, int closed,
			   ppl_const_Coefficient_t n,
			   ppl_const_Coefficient_t d),
 void (*lower_upper_bound)(unsigned int k, int closed,
			   ppl_const_Coefficient_t n,
			   ppl_const_Coefficient_t d),
 void (*set_empty)(unsigned int k)) try {

  const Polyhedron& pph = *to_const(ph);
  CBox cbox(raise_lower_bound, lower_upper_bound, set_empty);
  pph.shrink_bounding_box(cbox);

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
ppl_Polyhedron_OK(ppl_const_Polyhedron_t ph) try {
  return to_const(ph)->OK() ? 1 : 0;
}
CATCH_ALL
