/* Testing the function add_dimensions_and_constraints: we add a
   two-dimensional system of constraints to an empty, two-dimensional
   polyhedron. The result is a empty, four-dimansional polyhedron
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

  C_Polyhedron ph(2, C_Polyhedron::EMPTY);

#if NOISY
  print_constraints(ph, "--- ph ---");
#endif

  ConSys cs;
  cs.insert(x >= y);
  cs.insert(x >= 2);

#if NOISY
  print_constraints(cs, "--- cs ---");
#endif

  ph.add_dimensions_and_constraints(cs);

  C_Polyhedron known_result(4, C_Polyhedron::EMPTY);

  int retval = (ph == known_result) ? 0 : 1;

#if NOISY
  print_constraints(ph, "--- After add_dimensions_and_constraints ---");
#endif

  return retval;
}
