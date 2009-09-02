/* Test the PIP_Problem class
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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

//#define NOISY 1

#include "ppl_test.hh"

namespace {

bool test01() {
  Variable X1(0);
  Variable X2(1);
  Variable I0(2);
  Variable J0(3);
  Variable N(4);
  Variables_Set params(I0, N);

  Constraint_System cs;
  cs.insert(-X1 + N - 1 >= 0);
  cs.insert(X1 - X2 >= 0);
  cs.insert(X1 + I0 - N >= 0);
  cs.insert(-X1 - I0 + N >= 0);
  cs.insert(X2 + J0 - N - 1 >= 0);

  PIP_Problem pip = PIP_Problem(cs.space_dimension(), cs.begin(), cs.end(),
      params);

  if (pip.solve() != OPTIMIZED_PIP_PROBLEM)
    return false;

  return true;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
END_MAIN
