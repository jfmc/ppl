/* Testing C_Polyhedron::intersection_assign(): we intersect two
   polyhedra defined by their system of generators.
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
  gs1.insert(point());
  gs1.insert(point(3*x));
  gs1.insert(point(3*y));
  gs1.insert(point(3*x+ 3*y));
  C_Polyhedron ph1(gs1);

  GenSys gs2;
  gs2.insert(point(x));
  gs2.insert(point(4*x));
  gs2.insert(point(x + 3*y));
  gs2.insert(point(4*x+ 3*y));
  C_Polyhedron ph2(gs2);

#if NOISY
  print_generators(ph1, "*** ph1 ***");
  print_generators(ph2, "*** ph2 ***");
#endif

  ph1.intersection_assign(ph2);

  C_Polyhedron known_result(2, C_Polyhedron::EMPTY);
  known_result.add_generator(point(x));
  known_result.add_generator(point(3*x));
  known_result.add_generator(point(x + 3*y));
  known_result.add_generator(point(3*x + 3*y));

  int retval =(ph1 == known_result) ? 0 : 1;

#if NOISY
  print_constraints(ph1, "*** After intersection_assign ***");
#endif

  return retval;
}

