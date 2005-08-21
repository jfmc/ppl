/* Remove some variables from the space.
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

  Variable x(0);
  Variable y(1);
  Variable z(2);
  Variable w(3);
  Generator_System gs;
  gs.insert(point(0*x + y +0*z + 2*w));
  C_Polyhedron ph(gs);
#if NOISY
  print_generators(ph, "*** ph ***");
#endif

  // This is the set of the variables that we want to remove.
  Variables_Set to_be_removed;
  to_be_removed.insert(y);
  to_be_removed.insert(z);
  ph.remove_space_dimensions(to_be_removed);

  Generator_System known_result_gs;
  known_result_gs.insert(point(0*x +2*y));
  C_Polyhedron known_result(known_result_gs);

  int retval = (known_result == ph) ? 0 : 1;

#if NOISY
  print_generators(ph, "*** ph ***");
  print_generators(known_result, "*** known_result ***");
#endif

  return retval;
}
CATCH
