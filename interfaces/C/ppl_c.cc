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

#include "ppl_install.hh"
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

static void (*user_error_handler)(int , const char*) = 0;

int
ppl_set_error_handler(void (*h)(int , const char*)) {
  user_error_handler = h;
  return 0;
}

#define CATCH_ALL \
catch(...) { \
  return -1; \
}

DECLARE_CONVERSIONS(Coefficient)

int
ppl_Coefficient_from_mpz_t(ppl_Coefficient_t* pc, mpz_t z) try {
  *pc = to_nonconst(new Integer(z));
  return 0;
}
CATCH_ALL

int
ppl_delete_Coefficient(ppl_const_Coefficient_t c) try {
  delete to_const(c);
  return 0;
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
ppl_swap_LinExpression(ppl_LinExpression_t a, ppl_LinExpression_t b) try {
  std::swap(*to_nonconst(a), *to_nonconst(b));
  return 0;
}
CATCH_ALL

int
ppl_LinExpression_add_to_coefficient(ppl_LinExpression_t le,
				     unsigned int var,
				     ppl_const_Coefficient_t value) try {
  LinExpression& lle = *to_nonconst(le);
  const Integer& vvalue = *to_const(value);
  lle += vvalue * Variable(var);
  return 0;
}
CATCH_ALL

int
ppl_LinExpression_add_to_inhomogeneous(ppl_LinExpression_t le,
				       ppl_const_Coefficient_t value) try {
  LinExpression& lle = *to_nonconst(le);
  const Integer& vvalue = *to_const(value);
  lle += vvalue;
  return 0;
}
CATCH_ALL

int
ppl_LinExpression_space_dimension(ppl_const_LinExpression_t le) try {
  return to_const(le)->space_dimension();
}
CATCH_ALL

DECLARE_CONVERSIONS(Constraint)

int
ppl_new_Constraint(ppl_Constraint_t* pc,
		   ppl_const_LinExpression_t le,
		   enum ppl_enum_Constraint_Type t) try {
  Constraint* pd;
  const LinExpression& e = *to_const(le);
  switch(t) {
  case EQUAL:
    pd = new Constraint(e == 0);
    break;
  case GREATER_THAN_OR_EQUAL:
    pd = new Constraint(e >= 0);
    break;
#if 0
  case GREATER_THAN:
    pd = new Constraint(e > 0);
    break;
#endif
  case LESS_THAN_OR_EQUAL:
    pd = new Constraint(e <= 0);
    break;
#if 0
  case LESS_THAN:
    pd = new Constraint(e < 0);
    break;
#endif
  default:
    // FIXME!
    throw 3;
  }
  *pc = to_nonconst(pd);
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
ppl_swap_Constraint(ppl_Constraint_t a, ppl_Constraint_t b) try {
  std::swap(*to_nonconst(a), *to_nonconst(b));
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
			   ppl_Coefficient_t value) try {
  const Constraint& cc = *to_const(c);
  Integer& vvalue = *to_nonconst(value);
  vvalue = cc.coefficient(Variable(var));
  return 0;
}
CATCH_ALL

int
ppl_Constraint_inhomogeneous_term(ppl_const_Constraint_t c,
				  ppl_Coefficient_t value) try {
  const Constraint& cc = *to_const(c);
  Integer& vvalue = *to_nonconst(value);
  vvalue = cc.coefficient();
  return 0;
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
