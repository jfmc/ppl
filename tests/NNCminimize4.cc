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

  // Building an open square.
  GenSys gs;
  gs.insert(closure_point());
  gs.insert(closure_point(15*x));
  gs.insert(closure_point(15*y));
  gs.insert(closure_point(15*x + 15*y));

  // All of these points, but a (any) single one of them, are redundant.
  gs.insert(point(3*x + 3*y));
  gs.insert(point(6*x + y));
  gs.insert(point(9*x + y));
  gs.insert(point(12*x + 3*y));
  gs.insert(point(3*x + 12*y));
  gs.insert(point(6*x + 14*y));
  gs.insert(point(9*x + 14*y));
  gs.insert(point(12*x + 12*y));
  gs.insert(point(x + 6*y));
  gs.insert(point(x + 9*y));
  gs.insert(point(14*x + 6*y));
  gs.insert(point(14*x + 9*y));

  NNC_Polyhedron ph(gs);

#if NOISY
  cout << endl << "Before NNC minimization" << endl;
  print_constraints(ph.constraints(), "*** ph constraints ***");
  print_generators(ph.generators(), "*** ph generators ***");
#endif

  // ph.strongly_minimize();
  ph.strongly_minimize_constraints();

#if NOISY
  cout << endl << "After NNC minimization" << endl;
  print_constraints(ph.constraints(), "*** ph constraints ***");

  cout << endl << "=== ph ===" << endl << ph << endl;

  print_generators(ph.generators(), "*** ph generators ***");

  cout << endl << "=== ph ===" << endl << ph << endl;

#endif

  gs.clear();
  gs.insert(closure_point());
  gs.insert(closure_point(15*x));
  gs.insert(closure_point(15*y));
  gs.insert(closure_point(15*x + 15*y));
  gs.insert(point(x + y));

  NNC_Polyhedron known_result(gs);

#if NOISY
  cout << endl << "=== ph ===" << endl << ph << endl;
  cout << endl << "=== kr ===" << endl << known_result << endl;
#endif

  return (ph == known_result && ph.constraints().num_rows() == 5) ? 0 : 1;
}
