/* Test Polyhedron::is_universe(): a generator systems containing
   a point and three rays in a 2-dim vector space may still define
   the universe polyhedron.
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
  Variable x(0);
  Variable y(1);

  GenSys gs;
  gs.insert(point());
  gs.insert(ray(-x));
  gs.insert(ray(x + y));
  gs.insert(ray(x - y));

  C_Polyhedron ph(gs);

#if NOISY
  print_generators(ph, "--- ph ---");
#endif

  bool universe = ph.is_universe();

#if NOISY
  cout << "*** ph.is_universe() ***"
       << endl
       << (universe ? "true" : "false")
       << endl;
#endif

  return universe ? 0 : 1;
}
CATCH
