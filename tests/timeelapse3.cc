/* Testing time_elapse_assign() with a NECESSARY_CLOSED polyhedron
   and a NOT_NECESSARY_CLOSED polyhedron.
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

  ConSys cs1;
  cs1.insert(x > 3);
  cs1.insert(y > 3);
  NNC_Polyhedron ph(cs1);

  NNC_Polyhedron ph1(ph);

  GenSys gs;
  gs.insert(point(x + y));
  NNC_Polyhedron ph2(gs);

#if NOISY
  print_constraints(ph1, "**** ph1 ****");
  print_generators(ph2, "**** ph2 ****");
#endif
  
  ph1.time_elapse_assign(ph2);
   
  int retval = (ph1 == ph) ? 0 : 1;

#if NOISY
  print_generators(ph1, "**** ph1_time_elapse_assign(ph2) ****");
#endif
 
 return retval;
}
