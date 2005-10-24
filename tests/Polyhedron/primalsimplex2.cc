/* Test Constraint_System::primal_simplex().
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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

int
main() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Linear_Expression cost(-A);

  // Define the nonnegative orthant as the feasible region.
  Constraint_System cs;
  for (dimension_type i = 0; i < 3; ++i)
    cs.insert(Variable(i) >= 0);

  Coefficient n;
  Coefficient d;
  Generator g(point());
  Simplex_Status status = cs.primal_simplex(cost, MAXIMIZATION, n, d, g);

  Generator opt(point(0*C));
  return (status == SOLVED_PROBLEM && g == opt) ? 0 : 1;
}


