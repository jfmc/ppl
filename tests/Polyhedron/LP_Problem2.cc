/* Test LP_Problem class.
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "ppl_test.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;
using namespace Parma_Polyhedra_Library::IO_Operators;
#ifndef NOISY
#define NOISY 0
#endif

int
main() {
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
  Constraint_System cs;
  cs.insert(1*A-1*B+C >= 24);
  cs.insert(B <= 320);
  cs.insert(A + 1*B + 2*D == 23);
  cs.insert(1*A + 2*B + 1*E == 4112);
  cs.insert(7*A + 5*B + 1*F <= 200);
  cs.insert(138*A + 2*G == 25);
  cs.insert(23*A + 342*B - 34*H == 99);
  for (dimension_type i = A.id(); i <= H.id(); ++i)
    cs.insert(Variable(i) >= 0);
  Coefficient a,b;
  Generator pg(point());
  LP_Problem lpp = LP_Problem(cs, cost, MAXIMIZATION);
  lpp.solve();
  pg = lpp.optimizing_point();
  lpp.evaluate_objective_function(pg, a, b);
#if NOISY
  cout << "Optimum computed by LP_Problem::solve  "<< a <<
    "/" << b << endl << endl;
  std::cout << "Setting a new cost function " << std::endl;
#endif
  Linear_Expression new_cost = -51*A + 632*B;
  lpp.set_objective_function(new_cost);
  lpp.solve();
  pg = lpp.optimizing_point();
  lpp.evaluate_objective_function(pg, a, b);
#if NOISY
  cout << "Optimum computed by LP_Problem::solve "<< a <<
    "/" << b << endl << endl;
  print_generator(pg);
#endif
  cs.primal_simplex(new_cost, MAXIMIZATION, a ,b, pg);
#if NOISY
  cout << "Optimum computed by Constraint_System::primal_simplex "<< a <<
    "/" << b << endl << endl;
#endif
  //   lpp.evaluate_objective_function(a, b, pg);
  //   print_generator(pg);
#if NOISY
  std::cout << "Setting a new Optimization_Mode" << std::endl;
#endif
  lpp.set_optimization_mode(MINIMIZATION);
  lpp.solve();
  pg = lpp.optimizing_point();
  lpp.evaluate_objective_function(pg, a, b);
#if NOISY
  cout << "Optimum computed by LP_Problem::solve is "<< a <<
    "/" << b << endl << "Computed Generator ";
  print_generator(pg);
#endif
  cs.primal_simplex(new_cost, MINIMIZATION, a ,b, pg);
#if NOISY
  cout << std::endl << "Optimum computed by Constraint_System::primal_simplex"
    " is "<< a << "/" << b << endl << "Computed Generator ";
  print_generator(pg);
#endif
  return 0;
}
