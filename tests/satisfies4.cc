/* Testing Polyhedron::satisfies().
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

  ConSys cs;
  cs.insert(x + y >= 1);
  cs.insert(y >= 5);

  Polyhedron ph(cs);
#if NOISY
  print_generators(ph, "--- ph ---");
#endif

  // An equality constraint non-intersecting the polyhedron.
  Constraint c(x == -1);
#if NOISY
  print_constraint(c, "--- c ---");
#endif

  GenSys_Con_Rel rel = ph.satisfies(c);
#if NOISY
  cout << "ph.satisfies(c) == " << rel << endl;
#endif

  return rel == NONE_SATISFIES ? 0 : 1;

}
