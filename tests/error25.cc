/* An incorrect use of the function limited_widening_assign: the
   system of constraints is not dimension-compatible with the
   two polyhedra.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "ppl_install.hh"
#include "ehandlers.hh"
#include <stdexcept>

using namespace std;
using namespace Parma_Polyhedra_Library;

#define NOISY 0

int
main() {
  set_handlers();

  Variable x(0);
  Variable y(1);
  Variable z(2);

  Polyhedron ph1(2);
  ph1.insert(x - y >= 0);
  ph1.insert(x >= 0);
  ph1.insert(x <= 2);

  Polyhedron ph2(2);
  ph2.insert(x - y >= 0);
  ph2.insert(x >= 0);
  ph2.insert(x <= 5);

  ConSys cs;
  cs.insert(z <= 5);

  try {
    // This is invalid use of widening_assign.
    ph2.limited_widening_assign(ph1, cs);
  }
  catch (invalid_argument& e) {
#if NOISY
    cout << "invalid_system_of_constraints: " << e.what() << endl;
#endif
    exit(0);
  }
  catch (...) {
    exit(1);
  }

  // Should not get here.
  return 1;
}
