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

#include "ppl_test.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;
using namespace Parma_Polyhedra_Library::IO_Operators;

#ifndef NOISY
#define NOISY 0
#endif

int
main() {
  set_handlers();

  Variable x(0);

  ConSys cs;
  cs.insert(x > 0);

  NNC_Polyhedron ph(cs);

  cs.clear();
  cs.insert(3*x >= 1);
  cs.insert(2*x <= 1);

  ph.add_constraints(cs);

#if NOISY
  cout << endl << "Before NNC minimization" << endl;
  print_constraints(ph.constraints(), "*** ph constraints ***");
  print_generators(ph.generators(), "*** ph generators ***");
#endif

  ph.minimized_constraints();

  NNC_Polyhedron known_result(1);
  known_result.add_constraint(3*x >= 1);
  known_result.add_constraint(2*x <= 1);

  int retval = (ph == known_result) ? 0 : 1;

#if NOISY
  cout << endl << "After NNC minimization" << endl;
  print_constraints(ph.constraints(), "*** ph constraints ***");

  cout << endl << "=== ph ===" << endl << ph << endl;

  print_generators(ph.generators(), "*** ph generators ***");

  cout << endl << "=== ph ===" << endl << ph << endl;

#endif

  return retval;
}
