/* Inclusion test for NNC_Polyhedron.
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

#define NOISY 0

int
main() {
  set_handlers();

  Variable x(0);
  Variable y(1);

  // Building a square without vertices.
  ConSys cs;
  cs.insert(x >= 0);
  cs.insert(x <= 1);
  cs.insert(y >= 0);
  cs.insert(y <= 1);
  cs.insert(x + y > 0);
  cs.insert(x + y < 2);
  cs.insert(x - y < 1);
  cs.insert(x - y > -1);

  NNC_Polyhedron ph1(cs);
  //ph1.NNC_minimize();

  // Building an equivalent square.
  GenSys gs;
  gs.insert(closure_point());
  gs.insert(closure_point(x));
  gs.insert(closure_point(y));
  gs.insert(closure_point(x + y));
  gs.insert(point(x, 10));
  gs.insert(point(y, 10));
  gs.insert(point(x + 10*y, 10));
  gs.insert(point(10*x + y, 10));

  NNC_Polyhedron ph2(gs);

  int retval = (ph1 >= ph2) ? 0 : 1;
  
  return retval;
}
