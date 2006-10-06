/* Test the MIP_Problem class.
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

namespace {

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

bool
test03() {
  Variable A(0);
  Variable B(1);
  Constraint_System cs;
  cs.insert(Coefficient("-3152519739159347")*A
	    - Coefficient("4503599627370496")*B
	    >= Coefficient("-2837267765243412480"));
  cs.insert(Coefficient("-14411518807585588")*A
	    - Coefficient("3602879701896397")*B
	    >= Coefficient("-19455550390240542720"));
  cs.insert(Coefficient("6325070415853456823515479584966165845298645305129441198653167438357198111499854590373761990669910140474596183259900372230931523043306046152094168748148078435047419508642698792639590866940413010663742739952273283392562733857021646831815729864036236135650314266011211548510419206725953204130822734645187695728365866909171712")*A
	    >= Coefficient("134217729"));
  cs.insert(B >= 20);
  cs.insert(-B >= -500);

  // Integer variables.
  Variables_Set ivs(A);

  // Cost function
  Linear_Expression cost(-4*A - B);

  MIP_Problem lp = MIP_Problem(cs.space_dimension(),
			       cs.begin(), cs.end(),
			       ivs,
			       cost,
			       MINIMIZATION);

  Generator pg = lp.optimizing_point();
  nout << "Optimizing point = ";
  print_generator(pg);

  Coefficient num;
  Coefficient den;
  lp.evaluate_objective_function(pg, num, den);
  nout << "Optimum value = " << num << "/" << den << endl;

  return true;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST_F64(test03);
END_MAIN
