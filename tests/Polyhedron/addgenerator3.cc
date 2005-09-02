/* Test Polyhedron::add_generator_and_minimize().
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

  Variable A(0);
  Variable B(1);

  Generator_System gs;
  gs.insert(point());
  gs.insert(point(A + 2*B));
  gs.insert(point(A + B));
  gs.insert(point(2*A + 2*B));
  C_Polyhedron ph(gs);

#if NOISY
  print_generators(ph, "*** ph ***");
#endif

  ph.add_generator_and_minimize(ray(A));

  Generator_System known_gs;
  known_gs.insert(point());
  known_gs.insert(point(A + 2*B));
  known_gs.insert(ray(A));
  C_Polyhedron known_result(known_gs);

  int retval = (ph == known_result && ph.OK(true)) ? 0 : 1;

#if NOISY
  print_generators(ph,
		    "*** After ph.add_generator_and_minimize(ray(A)) ***");
#endif

  return retval;
}
CATCH
