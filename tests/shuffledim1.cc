/* Test Polyhedron::shuffle_dimensions().
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
#include "PFunction.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 1
#endif

void
test1() {
  PFunction function;
  
  C_Polyhedron ph1(3);

#if NOISY
  function.print(cout);
  print_constraints(ph1, "*** ph1 ***");
#endif

  ph1.shuffle_dimensions(function);

  C_Polyhedron known_result;

  bool ok = (ph1 == known_result);

#if NOISY
  print_constraints(ph1, "*** After ph1.shuffle_dimensions(function) ***");
#endif
  
  if(!ok)
    exit(1);
}

void
test2() {
  PFunction function;
  
  C_Polyhedron ph1(3, C_Polyhedron::EMPTY);

#if NOISY
  function.print(cout);
  print_constraints(ph1, "*** ph1 ***");
#endif

  ph1.shuffle_dimensions(function);

  C_Polyhedron known_result(0, C_Polyhedron::EMPTY);

  bool ok = (ph1 == known_result);

#if NOISY
  print_constraints(ph1, "*** After ph1.shuffle_dimensions(function) ***");
#endif
  
  if(!ok)
    exit(1);
}

void
test3() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  
  PFunction function;
  function.insert(0, 3);
  function.insert(2, 0);
  function.insert(1, 2);

  GenSys gs;
  gs.insert(point(2*C));
  gs.insert(line(A + B));
  gs.insert(ray(A + C));

  C_Polyhedron ph1(gs);

#if NOISY
  function.print(cout);
  print_generators(ph1, "*** ph1 ***");
#endif

  ph1.shuffle_dimensions(function);
  
  GenSys known_gs;
  known_gs.insert(point(2*A));
  known_gs.insert(line(D + C));
  known_gs.insert(ray(D + A));
  C_Polyhedron known_result(known_gs);

  bool ok = (ph1 == known_result);

#if NOISY
  print_generators(ph1, "*** After ph1.shuffle_dimensions(function) ***");
#endif
  
  if(!ok)
    exit(1);
}

int
main() {
  set_handlers();

  test1();
  test2();
  test3();

  return 0;
}
