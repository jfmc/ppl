/* Some incorrect uses of the functions of PPL.
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
#include "ehandlers.hh"
#include <stdexcept>

using namespace std;
using namespace Parma_Polyhedra_Library;

#define NOISY 0

void
error1() {
  set_handlers();

  Variable x(0);
  Variable y(1);

  ConSys cs;
  cs.insert(x - y > 0);
  cs.insert(x >= 0);
  
  try {
    // This is an invalid use of the constructor of a polyhedron:
    // it is impossible to built a closed polyhedron starting from a system
    // of constraints that contains strict-inequalities.
    Polyhedron ph(cs);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invlaid_system_of_constraints: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

void
error2() {
  set_handlers();

  Variable x(0);
  Variable y(1);
  
  GenSys gs;
  gs.insert(closure_point(x + y));
  gs.insert(point(x + y));
  gs.insert(ray(x));
  gs.insert(ray(y));
  
  try {
    // This is an invalid use of the constructor of a polyhedron:
    // it is impossible to built a closed polyhedron starting from a system
    // of generators that contains closure points.
    Polyhedron ph(gs);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invlaid_system_of_generators: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    exit(1);
  }
}

int
main() {
  
  error1();
  error2();

  return 0;
}
