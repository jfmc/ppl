/* Test the LP_Problem class.
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

using namespace Parma_Polyhedra_Library;

bool
test01() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  // Feasible region.
  Constraint_System cs;
  cs.insert(A + B <= 1);
  cs.insert(A + C <= 1);
  cs.insert(B + C <= 1);

  // All integer variables.
  Variables_Set ivs(A, C);

  // Cost function.
  Linear_Expression cost(-2*A - 3*B - 4*C);

  MIP_Problem ilp(cs.space_dimension(), cs.begin(), cs.end(), ivs, cost,
		  MINIMIZATION);

  ilp.solve();

  Generator pg = ilp.optimizing_point();
  nout << "Optimizing point = ";
  using namespace Parma_Polyhedra_Library::IO_Operators;
  nout << pg << endl;
  return true;
}

bool
test02() {
  Variable A(0);
  Variable B(1);

  // Feasible region.
  Constraint_System cs;
  cs.insert(-2*A - B >= -5);
  cs.insert(4*A -4*B >= -5);
  cs.insert(A >= 0);
  cs.insert(B >= 0);

  // All integer variables.
  Variables_Set ivs(A, B);

  // Objective function.
  Linear_Expression cost(A - 2*B);

  MIP_Problem ilp(cs.space_dimension(), cs.begin(), cs.end(), ivs, cost,
		  MAXIMIZATION);
  ilp.solve();

  Generator pg = ilp.optimizing_point();
  nout << "Optimizing point = ";
  using namespace Parma_Polyhedra_Library::IO_Operators;
  nout << pg << endl;

  ilp.set_optimization_mode(MINIMIZATION);
  ilp.solve();

  pg = ilp.optimizing_point();
  nout << "Optimizing point = ";
  using namespace Parma_Polyhedra_Library::IO_Operators;
  nout << pg << endl;
  return true;
}

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
END_MAIN
