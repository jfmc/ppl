/* Full minimization of a NNC-redundant constraint system.
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
#include "print.hh"
#include "ehandlers.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#define NOISY 0

int
main() {
  set_handlers();

  Variable x(0);

  ConSys cs;
  cs.insert(x > 0);
  cs.insert(x < 2);

  NNC_Polyhedron ph1(cs);

  cs.clear();
  cs.insert(x > 2);
  cs.insert(x < 3);

  NNC_Polyhedron ph2(cs);

  ph1.poly_hull_assign_and_minimize(ph2);

#if NOISY
  std::cout << "(Weakly) minimized poly hull" << std::endl;
  print_constraints(ph1.constraints(), "*** ph1 constraints ***");
  print_generators(ph1.generators(), "*** ph1 generators ***");
#endif

#if NOISY
  std::cout << "After strong minimization" << std::endl;
  print_constraints(ph1.minimized_constraints(),
		    "*** ph1 minimized constraints ***");
  cerr << "poly ph1" << endl;
  cerr << ph1 << endl;
  print_generators(ph1.minimized_generators(),
		   "*** ph1 minimized generators ***");
#endif

  return 0;
}
