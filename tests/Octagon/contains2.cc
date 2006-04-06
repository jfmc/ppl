/* Test Octagon::contains().
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

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

#ifndef NOISY
#define NOIY 0
#endif

int
main() TRY {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  TOctagon oc1(3);
  TOctagon oc2(3);

  oc1.add_constraint(C >= 0);
  oc1.add_constraint(B + C >= 1);

  oc2.add_constraint(A == 0);
  oc2.add_constraint(C >= 0);
  oc2.add_constraint(B + C >= 1);

#if NOISY
  print_constraints(oc1, "*** oc1 ***");
  print_constraints(oc2, "*** oc2 ***");
#endif

  bool result = oc1.contains(oc2);

#if NOISY
  cout << "*** oc1.contains(oc2) ***"
       << endl
       << (result ? "true" : "false")
       << endl;
#endif

  int retval = result ? 0 : 1;

  return retval;
}
CATCH
