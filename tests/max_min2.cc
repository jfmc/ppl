/* Test Polyhedron::maximize(const LinExpression&, ...)
   and Polyhedron::minimize(const LinExpression&, ...).
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef NOISY
#define NOISY 1
#endif

int
main() TRY {
  set_handlers();

  Variable x1(0);
  Variable x2(1);
  Variable x3(2);

  C_Polyhedron ph(3);
  ph.add_constraint(-x1-x2-x3 >= -100);
  ph.add_constraint(-10*x1-4*x2-5*x3 >= -600);
  ph.add_constraint(-x1-x2-3*x3 >= -150);
  ph.add_constraint(x1 >= 0);
  ph.add_constraint(x2 >= 0);
  ph.add_constraint(x3 >= 0);

#if NOISY
  print_constraints(ph, "*** ph ***");
#endif

  Integer num;
  Integer den;
  bool included;
  Generator g = point();
  bool ok = ph.maximize(-10*x1-6*x2-4*x3+4, num, den, included, g)
    && num == 4 && den == 1 && included
    && g.is_point()
    && g.coefficient(x1) == 0
    && g.coefficient(x2) == 0
    && g.coefficient(x3) == 0
    && g.divisor() == 1;

#if NOISY
  cout << (included ? "maximum" : "supremum") << " = " << num;
  if (den != 1)
    cout << "/" << den;
  cout << " @ ";
  print_generator(g);
  cout << endl;
#endif

  if (!ok)
    return 1;

  ok = ph.minimize(-10*x1-6*x2-4*x3+4, num, den, included, g)
    && num == -2188 && den == 3 && included
    && g.is_point()
    && g.coefficient(x1) == 100
    && g.coefficient(x2) == 200
    && g.coefficient(x3) == 0
    && g.divisor() == 3;

#if NOISY
  cout << (included ? "minimum" : "infimum") << " = " << num;
  if (den != 1)
    cout << "/" << den;
  cout << " @ ";
  print_generator(g);
  cout << endl;
#endif

  return ok ? 0 : 1;
}
CATCH
