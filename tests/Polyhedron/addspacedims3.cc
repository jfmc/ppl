/* Test Polyhedron::add_space_dimensions_and_embed(): we apply this function
   to a polyhedron defined by its system of generators.
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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

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
  Variable u(3);
  Variable v(4);
  Variable w(5);

  Generator_System gs;
  gs.insert(point());
  gs.insert(ray(x + y));

  C_Polyhedron ph(gs);
#if NOISY
  print_generators(ph, "*** ph ***");
#endif
  Constraint_System cs = ph.constraints();

  ph.add_space_dimensions_and_embed(2);
#if NOISY
  print_generators(ph, "*** After add_space_dimensions_and_embed(4) ***");
#endif

  ph.add_space_dimensions_and_embed(2);

  C_Polyhedron known_result(6, C_Polyhedron::EMPTY);
  known_result.add_generator(point());
  known_result.add_generator(ray(x + y));
  known_result.add_generator(line(z));
  known_result.add_generator(line(u));
  known_result.add_generator(line(v));
  known_result.add_generator(line(w));

  int retval = (ph == known_result) ? 0 : 1;

#if NOISY
  print_generators(ph, "*** ph ***");
#endif

  return retval;
}
CATCH
