/* Test Polyhedron::maximize(const Linear_Expression&, ...)
   and Polyhedron::minimize(const Linear_Expression&, ...).
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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
#define NOISY 0
#endif

int
main() TRY {
  set_handlers();

  Variable x1(0);
  Variable x2(1);

  C_Polyhedron ph(2);
  ph.add_constraint(-2*x1-x2 >= -5);
  ph.add_constraint(4*x1-4*x2 >= -5);
  ph.add_constraint(x1 >= 0);
  ph.add_constraint(x2 >= 0);

#if NOISY
  print_constraints(ph, "*** ph ***");
#endif

  Coefficient num;
  Coefficient den;
  bool included;
  const Generator* pg;
  bool ok = ph.maximize(x1-2*x2, num, den, included, &pg)
    && num == 5 && den == 2 && included
    && pg->is_point()
    && pg->coefficient(x1) == 5 && pg->coefficient(x2) == 0
    && pg->divisor() == 2;

#if NOISY
  cout << (included ? "maximum" : "supremum") << " = " << num;
  if (den != 1)
    cout << "/" << den;
  cout << " @ ";
  print_generator(*pg);
  cout << endl;
#endif

  if (!ok)
    return 1;

  ok = ph.minimize(x1-2*x2, num, den, included, &pg)
    && num == -15 && den == 4 && included
    && pg->is_point()
    && pg->coefficient(x1) == 5 && pg->coefficient(x2) == 10
    && pg->divisor() == 4;

#if NOISY
  cout << (included ? "minimum" : "infimum") << " = " << num;
  if (den != 1)
    cout << "/" << den;
  cout << " @ ";
  print_generator(*pg);
  cout << endl;
#endif

  return ok ? 0 : 1;
}
CATCH
