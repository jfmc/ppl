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

int
ppl_Coefficient_from_mpz_t(ppl_Coefficient_t* pc, mpz_t z) try {
  *pc = reinterpret_cast<ppl_Coefficient_t>(new Integer(z));
  return 0;
}
CATCH_ALL

int
ppl_delete_Coefficient(ppl_Coefficient_t c) try {
  delete reinterpret_cast<Integer*>(c);
  return 0;
}
CATCH_ALL

int
ppl_new_LinExpression(ppl_LinExpression_t* ple) try {
  *ple = reinterpret_cast<ppl_LinExpression_t>(new LinExpression());
  return 0;
}
CATCH_ALL

int
ppl_new_LinExpression_with_dimension(ppl_LinExpression_t* ple,
				     unsigned int d) try {
  *ple = reinterpret_cast<ppl_LinExpression_t>
    (new LinExpression(0*Variable(d)));
  return 0;
}
CATCH_ALL

int
ppl_delete_LinExpression(ppl_LinExpression_t le) try {
  delete reinterpret_cast<LinExpression*>(le);
  return 0;
}
CATCH_ALL

int
ppl_assign_LinExpresson_from_LinExpression(ppl_LinExpression_t dst,
					   ppl_LinExpression_t src) try {
  const LinExpression& ssrc = *reinterpret_cast<const LinExpression*>(src);
  LinExpression& ddst = *reinterpret_cast<LinExpression*>(dst);
  ddst = ssrc;
  return 0;
}
CATCH_ALL

int
ppl_swap_LinExpresson(ppl_LinExpression_t a, ppl_LinExpression_t b) try {
  std::swap(*reinterpret_cast<LinExpression*>(a),
	    *reinterpret_cast<LinExpression*>(b));
  return 0;
}
CATCH_ALL

int
ppl_LinExpression_add_to_coefficient(ppl_LinExpression_t le,
				     unsigned int var,
				     ppl_Coefficient_t value) try {
  LinExpression& lle = *reinterpret_cast<LinExpression*>(le);
  const Integer& vvalue = *reinterpret_cast<const Integer*>(value);
  lle += vvalue * Variable(var);
  return 0;
}
CATCH_ALL

int
ppl_LinExpression_add_to_inhomogeneous(ppl_LinExpression_t le,
				       ppl_Coefficient_t value) try {
  LinExpression& lle = *reinterpret_cast<LinExpression*>(le);
  const Integer& vvalue = *reinterpret_cast<const Integer*>(value);
  lle += vvalue;
  return 0;
}
CATCH_ALL

int
ppl_LinExpression_space_dimension(ppl_LinExpression_t le) try {
  return reinterpret_cast<LinExpression*>(le)->space_dimension();
}
CATCH_ALL

int
ppl_new_Constraint(ppl_Constraint_t* pc,
		   ppl_LinExpression_t le,
		   enum ppl_enum_Constraint_Type t) try {
  Constraint* pd;
  const LinExpression& e = *reinterpret_cast<LinExpression*>(le);
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
  *pc = reinterpret_cast<ppl_Constraint_t>(pd);
  return 0;
}
CATCH_ALL

int
ppl_delete_Constraint(ppl_Constraint_t le) try {
  delete reinterpret_cast<Constraint*>(le);
  return 0;
}
CATCH_ALL

int
ppl_assign_Constraint_from_Constraint(ppl_Constraint_t dst,
				      ppl_Constraint_t src) try {
  const Constraint& ssrc = *reinterpret_cast<const Constraint*>(src);
  Constraint& ddst = *reinterpret_cast<Constraint*>(dst);
  ddst = ssrc;
  return 0;
}
CATCH_ALL

int
ppl_swap_Constraint(ppl_Constraint_t a, ppl_Constraint_t b) try {
  std::swap(*reinterpret_cast<Constraint*>(a),
	    *reinterpret_cast<Constraint*>(b));
  return 0;
}
CATCH_ALL

int
ppl_Constraint_space_dimension(ppl_Constraint_t c) try {
  return reinterpret_cast<Constraint*>(c)->space_dimension();
}
CATCH_ALL

int
ppl_Constraint_coefficient(ppl_Constraint_t c,
			   int var,
			   ppl_Coefficient_t value) try {
  const Constraint& cc = *reinterpret_cast<const Constraint*>(c);
  Integer& vvalue = *reinterpret_cast<Integer*>(value);
  vvalue = cc.coefficient(Variable(var));
  return 0;
}
CATCH_ALL

int
ppl_Constraint_inhomogeneous_term(ppl_Constraint_t c,
				  ppl_Coefficient_t value) try {
  const Constraint& cc = *reinterpret_cast<const Constraint*>(c);
  Integer& vvalue = *reinterpret_cast<Integer*>(value);
  vvalue = cc.coefficient();
  return 0;
}
CATCH_ALL

int
ppl_new_ConSys(ppl_ConSys_t* pcs) try {
  *pcs = reinterpret_cast<ppl_ConSys_t>(new ConSys());
  return 0;
}
CATCH_ALL

int
ppl_new_ConSys_from_Constraint(ppl_ConSys_t* pcs, ppl_Constraint_t c) try {
  const Constraint& cc = *reinterpret_cast<const Constraint*>(c);
  *pcs = reinterpret_cast<ppl_ConSys_t>(new ConSys(cc));
  return 0;
}
CATCH_ALL

int
ppl_new_ConSys_from_ConSys(ppl_ConSys_t* pcs, ppl_ConSys_t cs) try {
  const Constraint& ccs = *reinterpret_cast<const Constraint*>(cs);
  *pcs = reinterpret_cast<ppl_ConSys_t>(new ConSys(ccs));
  return 0;
}
CATCH_ALL

int
ppl_delete_ConSys(ppl_ConSys_t cs) try {
  delete reinterpret_cast<ConSys*>(cs);
  return 0;
}
CATCH_ALL

int
ppl_assign_ConSys_from_ConSys(ppl_ConSys_t dst, ppl_ConSys_t src) try {
  const ConSys& ssrc = *reinterpret_cast<const ConSys*>(src);
  ConSys& ddst = *reinterpret_cast<ConSys*>(dst);
  ddst = ssrc;
  return 0;
}
CATCH_ALL

int
ppl_ConSys_space_dimension(ppl_ConSys_t cs) try {
  return reinterpret_cast<const ConSys*>(cs)->space_dimension();
}
CATCH_ALL

int
ppl_ConSys_insert_Constraint(ppl_ConSys_t cs, ppl_Constraint_t c) try {
  const Constraint& cc = *reinterpret_cast<const Constraint*>(c);
  ConSys& ccs = *reinterpret_cast<ConSys*>(cs);
  ccs.insert(cc);
  return 0;
}
CATCH_ALL
