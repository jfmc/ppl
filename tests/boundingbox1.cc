/* Test Polyhedron::shrink_bounding_box().
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
#include <iostream>

using namespace std;
using namespace Parma_Polyhedra_Library;

#define NOISY 1

class TestBBox {
private:
  // Nothing for the time being.

public:
  TestBBox() {
  }

  void raise_lower_bound(unsigned int k, bool closed,
			 const Integer& n, const Integer& d) {
    cout << "raise_lower_bound("
	 << k << ", "
	 << (closed ? "true" : "false") << ", "
	 << n << ", "
	 << d << ")" << endl;
  }

  void lower_upper_bound(unsigned int k, bool closed,
			 const Integer& n, const Integer& d) {
    cout << "lower_upper_bound("
	 << k << ", "
	 << (closed ? "true" : "false") << ", "
	 << n << ", "
	 << d << ")" << endl;
  }

  void set_empty(unsigned int k) {
    cout << "lower_upper_bound("
	 << k << ")" << endl;
  }
};

int
main() {
  Variable x(0);
  Variable y(1);

  ConSys cs;
  cs.insert(x >= 0);
  cs.insert(x <= 1);
  cs.insert(y >= 0);
  cs.insert(y <= 1);
 
  C_Polyhedron ph(cs);

#if NOISY
  print_constraints(ph, "*** ph constraints ***");
  print_generators(ph, "*** ph generators ***");
#endif

  TestBBox box;
  ph.shrink_bounding_box(box);

  if (!ph.OK())
    exit(1);

  return 0;
}
