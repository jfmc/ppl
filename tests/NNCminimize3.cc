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
  Variable y(1);

  // Building a square.
  ConSys cs;
  cs.insert(x >= 0);
  cs.insert(x <= 1);
  cs.insert(y >= 0);
  cs.insert(y <= 1);

  NNC_Polyhedron ph(cs);

#if NOISY
  std::cout << "Topologically closed square" << std::endl;
  print_constraints(ph.constraints(), "*** ph constraints ***");
  print_generators(ph.generators(), "*** ph generators ***");
#endif
  
  // Removing all the vertices using strict inequalities.
  cs.clear();
  cs.insert(x + y > 0);
  cs.insert(x + y < 2);
  cs.insert(x - y < 1);
  cs.insert(x - y > -1);

  ph.add_constraints_and_minimize(cs);

#if NOISY
  std::cout << "After vertices removal" << std::endl;
  print_constraints(ph.constraints(), "*** ph constraints ***");
  print_generators(ph.generators(), "*** ph generators ***");
#endif

  GenSys gs;
  gs.insert(point(x + y, 4));
  gs.insert(point(x + 5*y, 4));
  gs.insert(point(5*x + y, 4));
  gs.insert(point(5*x + 5*y, 4));

  NNC_Polyhedron ph2(gs);

  ph.intersection_assign(ph2);

#if NOISY
  std::cout << "After intersection" << std::endl;
  print_constraints(ph.constraints(), "*** ph constraints ***");
  print_generators(ph.generators(), "*** ph generators ***");
#endif

  ph.NNC_minimize();

  gs.clear();
  gs.insert(closure_point(x + y));
  gs.insert(point(x + y, 4));
  gs.insert(point(x + 4*y, 4));
  gs.insert(point(4*x + y, 4));

  NNC_Polyhedron known_result(gs);

  bool equal = (ph == known_result);

#if NOISY
  std::cout << "After NNC minimization" << std::endl;
  print_constraints(ph.constraints(), "*** ph constraints ***");
  print_generators(ph.generators(), "*** ph generators ***");
#endif

 return (equal && ph.generators().num_rows() == 7) ? 0 : 1;
}
