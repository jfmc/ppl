/* Testing add_generators_and_minimize(gs) for NNC_Polyhedron.
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
  
  GenSys gs1;
  gs1.insert(closure_point());
  gs1.insert(closure_point(4*x));
  gs1.insert(closure_point(4*y));
  gs1.insert(closure_point(4*x + 4*y));
  gs1.insert(point(2*x));
  gs1.insert(point(4*x + y));
  gs1.insert(point(x + 4*y));
  gs1.insert(point(3*y));
  
  NNC_Polyhedron ph(gs1);

#if NOISY
  print_generators(ph, "*** ph ***");
#endif
  
  GenSys gs2;
  gs2.insert(point());
  gs2.insert(point(4*x));
  gs2.insert(point(4*y));
  gs2.insert(point(4*x + 4*y));
  
  ph.add_generators_and_minimize(gs2);
  
  GenSys gs3;
  gs3.insert(point());
  gs3.insert(point(4*x));
  gs3.insert(point(4*y));
  gs3.insert(point(4*x + 4*y));
  
  NNC_Polyhedron known_result(gs3);

  C_Polyhedron closed_ph(ph);
  C_Polyhedron closed_known_result(known_result);

  int retval = (ph == known_result
		&& closed_ph == closed_known_result) ? 0 : 1;

#if NOISY
  print_generators(ph, "*** After add_generators_and_minimize(gs) ***");
  print_generators(closed_ph, "*** closed_ph ***");
#endif

  return retval;
}

