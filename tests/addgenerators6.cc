/* Test Polyhedron::add_generators(): we add a one-dimensional
   system of generators to a two-dimensional polyhedron.
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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
main() {
  set_handlers();
  Variable x(0);
  Variable y(1);

  GenSys gs1;
  gs1.insert(ray(x + y));
  gs1.insert(point());

  C_Polyhedron ph(gs1);

#if NOISY
  print_generators(ph, "*** ph ***");
#endif

  GenSys gs2;
  gs2.insert(ray(x));
  gs2.insert(point());

#if NOISY
  print_generators(gs2, "--- gs2 ---");
#endif

  ph.add_generators(gs2);

  C_Polyhedron known_result(2, C_Polyhedron::EMPTY);
  known_result.add_generator(point());
  known_result.add_generator(ray(x));
  known_result.add_generator(ray(x + y));

  int retval = (ph == known_result) ? 0 : 1;

#if NOISY
  print_generators(ph, "*** After add_generators ***");
#endif

  return retval;
}
