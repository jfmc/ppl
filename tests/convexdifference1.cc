/* Testing Polyhedron::convex_difference_assign().
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
main() try {
  set_handlers();

  Variable x(0);
  Variable y(1);

  GenSys gs1;
  gs1.insert(vertex(0*x + 0*y));
  gs1.insert(vertex(4*x + 0*y));
  gs1.insert(vertex(2*x + 2*y));

  Polyhedron ph1(gs1);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
#endif

  GenSys gs2;
  gs2.insert(vertex(0*x + 3*y));
  gs2.insert(vertex(4*x + 3*y));
  gs2.insert(vertex(2*x + 1*y));
 
  Polyhedron ph2(gs2);

#if NOISY
  print_generators(ph2, "*** ph2 ***");
#endif

  Polyhedron computed_result = ph1;
  
  computed_result.convex_difference_assign(ph2);

#if NOISY
  print_generators(computed_result, "*** After convex_difference_assign ***");
#endif

  GenSys gs_known_result;
  gs_known_result.insert(vertex());
  gs_known_result.insert(vertex(3*x + 3*y, 2));
  gs_known_result.insert(vertex(4*x));
  gs_known_result.insert(vertex(5*x + 3*y, 2));

  Polyhedron known_result(gs_known_result);

  size_t retval = (computed_result == known_result) ? 0 : 1;
 
  return retval;
}
catch (std::exception& e) {
  cout << e.what() << endl;
  exit(1);
}
