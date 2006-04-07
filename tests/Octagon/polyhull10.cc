/* Test Octagon::poly_hull_assign().
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

TOctagon
init(dimension_type n_vars) {
  TOctagon oc(n_vars);
  if (n_vars > 0) {
    Constraint_System cs;
    for (dimension_type i = n_vars; i-- > 0; ) {
      cs.insert(Variable(i) >= 0);
      cs.insert(Variable(i) <= 1);
    }
    for (dimension_type i = 0; i < n_vars; ++i) {
      Variable var_i = Variable(i);
      for (dimension_type j = 0; j <= i; ++j) {
	Variable var_j = Variable(j);
	cs.insert(var_i - var_j >= 0);
	cs.insert(var_i - var_j <= 1);
	cs.insert(var_i + var_j >= 0);
	cs.insert(var_i + var_j <= 1);
      }
    }
    oc = TOctagon(cs);
#if NOISY
    print_constraints(oc, "*** oc ***");
#endif
  }
  return oc;
}

TOctagon
init1(dimension_type n_vars) {
  TOctagon oc(n_vars);
  if (n_vars > 0) {
    Constraint_System cs;
    for (dimension_type i = n_vars; i-- > 0; ) {
      cs.insert(Variable(i) >= 0);
      cs.insert(Variable(i) <= 2);
    }
    for (dimension_type i = 0; i < n_vars; ++i) {
      Variable var_i = Variable(i);
      for (dimension_type j = 0; j <= i; ++j) {
	Variable var_j = Variable(j);
	cs.insert(var_i - var_j >= 0);
	cs.insert(var_i - var_j <= 2);
	cs.insert(var_i + var_j >= 0);
	cs.insert(var_i + var_j <= 2);
      }
    }
    oc = TOctagon(cs);
#if NOISY
    print_constraints(oc, "*** oc ***");
#endif
  }
  return oc;
}


int
main() TRY {
  for (dimension_type i = 10; i < 20; ++i) {
    TOctagon oc1 = init(i);
    TOctagon oc2 = init1(i);
    Octagon<mpq_class> known_result = init1(i);
    oc1.poly_hull_assign(oc2);
    if (oc1 != known_result) {
#if NOISY
      print_constraints(oc1, "*** oc1 ***");
      print_constraints(known_result, "*** known_result ***");
#endif
      return 1;
    }
  }

  return 0;
}
CATCH
