/* Test Polyhedron::maximize(const Linear_Expression&, ...)
   and Polyhedron::minimize(const Linear_Expression&, ...).
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

  print_constraints(ph, "*** ph ***");

  Coefficient num;
  Coefficient den;
  bool included;
  Generator g(point());
  bool ok = ph.maximize(x1-2*x2, num, den, included, g)
    && num == 5 && den == 2 && included
    && g.is_point()
    && g.coefficient(x1) == 5 && g.coefficient(x2) == 0
    && g.divisor() == 2;

  nout << (included ? "maximum" : "supremum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << " @ ";
  print_generator(g);
  nout << endl;

  if (!ok)
    return 1;

  ok = ph.minimize(x1-2*x2, num, den, included, g)
    && num == -15 && den == 4 && included
    && g.is_point()
    && g.coefficient(x1) == 5 && g.coefficient(x2) == 10
    && g.divisor() == 4;

  nout << (included ? "minimum" : "infimum") << " = " << num;
  if (den != 1)
    nout << "/" << den;
  nout << " @ ";
  print_generator(g);
  nout << endl;

  return ok ? 0 : 1;
}
CATCH
