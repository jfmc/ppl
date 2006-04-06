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
  TOctagon oc1(0);

  TOctagon oc2(0, EMPTY);

  TOctagon oc3(0);

#if NOISY
  print_constraints(oc1, "*** oc1 ***");
  print_constraints(oc2, "*** oc2 ***");
  print_constraints(oc3, "*** oc3 ***");
#endif

  bool result1 = oc1.contains(oc2);
  bool result2 = oc1.contains(oc3);

#if NOISY
  cout << "*** oc1.contains(oc2) ***"
       << endl
       << (result1 ? "true" : "false")
       << endl;
  cout << "*** oc1.contains(oc3) ***"
       << endl
       << (result2 ? "true" : "false")
       << endl;
#endif

  int retval = (result1 == result2) ? 0 : 1;

  return retval;
}
CATCH
