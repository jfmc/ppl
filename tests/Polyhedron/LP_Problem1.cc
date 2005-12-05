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
#include <iostream>
#include <vector>
using namespace std;
using namespace Parma_Polyhedra_Library;
using namespace Parma_Polyhedra_Library::IO_Operators;

#ifndef NOISY
#define NOISY 0
#endif

int
main() {
  // Variable declaration.
  Variable X01(0);
  Variable X02(1);
  Variable X03(2);
  Variable X04(3);
  Variable X05(4);
  Variable X06(5);
  Variable X07(6);
  Variable X08(7);
  Variable X09(8);
  Variable X10(9);
  Variable X11(10);
  Variable X12(11);
  Variable X13(12);
  Variable X14(13);
  Variable X15(14);
  Variable X16(15);
  Variable X17(16);
  Variable X18(17);
  Variable X19(18);
  Variable X20(19);
  Variable X21(20);
  Variable X22(21);
  Variable X23(22);
  Variable X24(23);
  Variable X25(24);
  Variable X26(25);
  Variable X27(26);
  Variable X28(27);
  Variable X29(28);
  Variable X30(29);
  Variable X31(30);
  Variable X32(31);
  Variable X33(32);
  Variable X34(33);
  Variable X35(34);
  Variable X36(35);
  Variable X37(36);
  Variable X38(37);
  Variable X39(38);
  Constraint_System cs;
  cs.insert(X01-X02-X03 +0*X39 == 0);
  cs.insert(Coefficient("2386907802506363")*X01-X04 == 0);
  cs.insert(-X01 >= -80);
  cs.insert(X02-Coefficient("3152519739159347")*X14 >= 0);
  cs.insert(X06+X07+X08+X09-X14-X15 == 0);
  cs.insert(Coefficient("2386907802506363")*X06+Coefficient("2386907802506363")*X07+Coefficient("1080863910568919")*X08+Coefficient("7746191359077253")*X09-X16 == 0);
  cs.insert(-X06+X10 >= -80);
  cs.insert(-X07+X11 >= 0);
  cs.insert(-X08+X12 >= 0);
  cs.insert(-X09+X13 >= 0);
  cs.insert(X22-X23-X24-X25 == 0);
  cs.insert(Coefficient("7746191359077253")*X22-X26 == 0);
  cs.insert(-X22 >= -500);
  cs.insert(X23-Coefficient("3152519739159347")*X36 >= 0);
  cs.insert(Coefficient("7746191359077253")*X28+Coefficient("7746191359077253")*X29+Coefficient("3512807709348987")*X30+Coefficient("3332663724254167")*X31-X38 == 0);
  cs.insert(X28+X29+X30+X31-X36+X37+X39 == 44);
  cs.insert(-X28+X32 >= -500);
  cs.insert(-X29+X33 >= 0);
  cs.insert(-X30+X34 >= 0);
  cs.insert(-X31+X35 >= 0);
  cs.insert(Coefficient("-2661627379775963")*X10-Coefficient("2686397177726501")*X11-Coefficient("5422333951354077")*X12-Coefficient("5469621747441467")*X13+X25-Coefficient("2466846695892189")*X32-Coefficient("4996743786567565")*X33-Coefficient("5064297780978123")*X34-Coefficient("641481471923585")*X35 >= 0);
  cs.insert(X03-Coefficient("7854277750134145")*X22 >= 0);
  cs.insert(X15-Coefficient("7854277750134145")*X28-Coefficient("7782220156096217")*X29-Coefficient("7782220156096217")*X30-Coefficient("7710162562058289")*X31 >= 0);
  cs.insert(Coefficient("-5422333951354077")*X01+X24 >= 0);
  cs.insert(X21 >= 2);
  cs.insert(-X16-X38 >= -300);
  for (dimension_type i = X01.id(); i <= X39.id(); ++i)
    cs.insert(Variable(i) >= 0);

  // Cost function.
  Linear_Expression cost(-10*X02-8*X14-15*X23-12*X36+250*X39);

  // Computed numerator and denominator.
  Coefficient a,b;
  // Here will be stored a feasible / optimized point.
  Generator pg(point());
  Simplex_Status ss;
  LP_Problem lpp = LP_Problem(cs, cost, MAXIMIZATION);
  lpp.solve(pg);
  lpp.get_optimum_value(a, b, pg);
#if NOISY
  cout << "Optimum computed by LP_Problem::solve is "<< a <<
    "/" << b << " Computed Generator";
  print_generator(pg);
#endif
  ss = cs.primal_simplex(cost, MAXIMIZATION, a ,b, pg);
#if NOISY
  cout << "Optimum computed by primal_simplex is "<< a <<
    "/" << b << " Computed Generator";
  print_generator(pg);
#endif
  bool lpp_satisfiable = lpp.is_satisfiable(pg);
#if NOISY
  if (lpp_satisfiable)
    cout << "lpp is feasable" << std::endl;
  else
    cout << "lpp is not feasable" << std::endl;
#endif
  Constraint_System lp_other_constraints;
  lp_other_constraints.insert(X05 >= 5);
  lp_other_constraints.insert(X05 <= 3);
#if NOISY
  std::cout << "Adding new constraints to lpp" << std::endl;
#endif
  lpp.add_constraints(lp_other_constraints);
  lpp_satisfiable = lpp.is_satisfiable(pg);
#if NOISY
  if (lpp_satisfiable)
    cout << "lpp is feasable" << std::endl;
  else
    cout << "lpp is not feasable" << std::endl;
#endif
  lpp.solve(pg);
}
