/* Test Polyhedron::strongly_minimize(): the polyhedron is
   a segment.
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

#ifndef NOISY
#define NOISY 0
#endif

int
main() {
  set_handlers();

  Variable A(0);

  GenSys gs1;
  gs1.insert(closure_point());
  gs1.insert(closure_point(4*A));
  gs1.insert(point(A));
  gs1.insert(point(3*A, 2));

  NNC_Polyhedron ph1(gs1);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
#endif

  ph1.strongly_minimize();

  GenSys known_gs;
  known_gs.insert(closure_point());
  known_gs.insert(closure_point(4*A));
  known_gs.insert(point(2*A));
  NNC_Polyhedron known_result(known_gs);

  int retval = (ph1 == known_result) ? 0 : 1;

#if NOISY
  print_generators(ph1, "*** After ph1.strongly_minimize() ***");
#endif

  return retval;
}
