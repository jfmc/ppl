/* Test that the right exceptions are thrown in case of incorrect uses.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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
#define NOISY 1
#endif

static void
error1() {
  try {
    // This is an invalid use of the constructor of a polyhedron:
    // it is illegal to (try to) build a polyhedron with a dimensions
    // greater than max_space_dimension().
    C_Polyhedron ph(std::numeric_limits<dimension_type>::max());

    // It is an error if the exception is not thrown.
    exit(1);
  }
  catch(invalid_argument& e) {
#if NOISY
    cout << "invalid_argument: " << e.what() << endl << endl;
#endif
  }
  catch (...) {
    // It is an error if the wrong exception is thrown.
    exit(1);
  }
}

int
main() TRY {
  set_handlers();

  error1();

  return 0;
}
CATCH
