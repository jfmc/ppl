/* Test Polyhedron::minimized_constraints(): we apply this function
   to a zero dimensional, non necessarily closed polyhedron.
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

int
main() TRY {
  set_handlers();

  NNC_Polyhedron ph1;

  Constraint_System cs = ph1.minimized_constraints();

  NNC_Polyhedron ph2(cs);

  int retval = (ph1 == ph2) ? 0 : 1;

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
  print_constraints(cs, "*** cs ***");
  print_constraints(ph2, "*** ph2 ***");
#endif

  return retval;
}
CATCH
