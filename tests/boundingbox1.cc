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

class Interval {
private:
  bool uclosed;
  Integer uc;
  Integer ud;
  bool lclosed;
  Integer lc;
  Integer ld;

public:
  Interval()
    : uclosed(true), uc(0), ud(0), lclosed(true), lc(0), ld(0) {
  }

  void raise_lower_bound(bool raise_closed,
			 const Integer& n, const Integer& d) {
    assert(raise_closed == uclosed);
    if (lc*d < n*ld) {
      lc = n;
      ld = d;
    }
  }

  void lower_upper_bound(bool lower_closed,
			 const Integer& n, const Integer& d) {
    assert(lower_closed == lclosed);
    if (uc*d > n*ud) {
      uc = n;
      ud = d;
    }
  }

  void set_empty() {
    uc = -1;
    lc = 1;
    ud = 1;
    ld = 1;}
};

class BBox {
private:
  std::vector<Interval> box;

public:
  BBox(unsigned int dimension) {
    box.resize(dimension);
  }

  void raise_lower_bound(unsigned int k, bool closed,
			 const Integer& n, const Integer& d) {
    cout << "raise_lower_bound("
	 << k << ", "
	 << (closed ? "true" : "false") << ", "
	 << n << ", "
	 << d << ")" << endl;
    box[k].raise_lower_bound(closed, n, d);
  }

  void lower_upper_bound(unsigned int k, bool closed,
			 const Integer& n, const Integer& d) {
    cout << "lower_upper_bound("
	 << k << ", "
	 << (closed ? "true" : "false") << ", "
	 << n << ", "
	 << d << ")" << endl;
    box[k].lower_upper_bound(closed, n, d);
  }

  void set_empty(unsigned int k) {
    cout << "lower_upper_bound("
	 << k << ")" << endl;
    box[k].set_empty();
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

  BBox box(ph.space_dimension());
  ph.shrink_bounding_box(box);

  if (!ph.OK())
    exit(1);

  return 0;
}
