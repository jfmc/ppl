/* Test LP_Problem class.
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

using namespace Parma_Polyhedra_Library::IO_Operators;

int
main() TRY {
  // Variable definition.
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);
  Variable F(5);
  Variable G(6);
  Variable H(7);

  // Cost function
  Linear_Expression cost(-26*A + 343*B + 1233*D - C + F);

  // Feasible region.
  Constraint_System cs;
  cs.insert(A - B + C >= 24);
  cs.insert(B <= 320);
  cs.insert(A + B + 2*D == 23);
  cs.insert(A + 2*B + E == 4112);
  cs.insert(7*A + 5*B + F <= 200);
  cs.insert(138*A + 2*G == 25);
  cs.insert(23*A + 342*B - 34*H == 99);
  for (dimension_type i = A.id(); i <= H.id(); ++i)
    cs.insert(Variable(i) >= 0);

  LP_Problem lp = LP_Problem(cs, cost, MAXIMIZATION);

  Generator pg = lp.optimizing_point();
  nout << "Optimizing point = ";
  print_generator(pg);
  Generator pg_kr = point();
  pg_kr = point(22*B + 1846*C + 863*D + 312468*E + 15090*F + 950*G + 0*H, 76);
  if (pg != pg_kr)
    return -1;

  Coefficient num;
  Coefficient den;
  lp.evaluate_objective_function(pg, num, den);
  nout << "Optimum value = " << num << "/" << den << endl;
  Coefficient num_kr = 1084869;
  Coefficient den_kr = 76;
  if (num != num_kr || den != den_kr)
    return -1;

  // Reoptimizing using another objective function.
  Linear_Expression new_cost = -51*A + 632*B;
  lp.set_objective_function(new_cost);
  pg = lp.optimizing_point();
  nout << "Optimizing point = ";
  print_generator(pg);
  pg_kr = point(782*B + 1598*C + 138244*E + 2890*F + 425*G + 7767*H, 34);
  if (pg != pg_kr)
    return -1;

  lp.evaluate_objective_function(pg, num, den);
  nout << "Optimum value = " << num << "/" << den << endl;
  num_kr = 14536;
  den_kr = 1;
  if (num != num_kr || den != den_kr)
    return -1;

  // Reoptimizing after changing optimization mode.
  lp.set_optimization_mode(MINIMIZATION);
  pg = lp.optimizing_point();
  nout << "Optimizing point = ";
  print_generator(pg);
  pg_kr = point(17100*A + 26174*B + 2274482*C
		+ 1063871*D + 388070456*E + 18627830*F + 0*H,
		94392);
  if (pg != pg_kr)
    return -1;

  lp.evaluate_objective_function(pg, num, den);
  nout << "Optimum value = " << num << "/" << den << endl;
  num_kr = 3917467;
  den_kr = 23598;
  if (num != num_kr || den != den_kr)
    return -1;

  return 0;
}
CATCH
