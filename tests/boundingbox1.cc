/* Test Polyhedron::bounding_box().
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

#ifndef NOISY
#define NOISY 0
#endif

class BInterval {
private:
  bool uclosed;
  Integer uc;
  Integer ud;
  bool lclosed;
  Integer lc;
  Integer ld;

public:
  BInterval()
    : uclosed(true), uc(1), ud(0), lclosed(true), lc(-1), ld(0) {
  }

  void raise_lower_bound(bool closed,
			 const Integer& c, const Integer& d) {
    assert(d > 0 && ld >= 0);
    if ((closed && lc*d <= c*ld) || (!closed && lc*d < c*ld))  {
        lc = c;
        ld = d;
        lclosed = closed;
    }
  }

  void lower_upper_bound(bool closed,
			 const Integer& c, const Integer& d) {
    assert(d > 0 && ud >= 0);
    if ((!closed && uc*d >= c*ud) || (closed && uc*d > c*ud))  {
        uc = c;
        ud = d;
        uclosed = closed;
    }
  }

  void set_empty() {
    uc = -1;
    lc = 1;
    ud = 1;
    ld = 1;}

  void print_interval() {
  cout << "lower bound = ";
  if (ld != 0) {
    if (lclosed == true)
      cout << " true ";
    else
      cout << " false";
    cout << " : " << lc << " / " << ld << "," << endl;
  }
  else
    cout << " none " << endl;

  cout << "         ";
  cout << "upper bound = ";
  if (ud != 0) {
    if (uclosed == true)
      cout << " true ";
    else
      cout << " false";
    cout << " : " << uc << " / " << ud << "." << endl;
  }
  else
    cout << " none,  " << endl;
  }

  friend bool operator==(const BInterval& x, const BInterval& y);
};

inline bool
operator==(const BInterval& x, const BInterval& y) {
  return x.lclosed == y.lclosed
    && x.uclosed == y.uclosed
    && x.lc*y.ld == y.lc*x.ld
    && x.uc*y.ud == y.uc*x.ud;
}

inline bool
operator!=(const BInterval& x, const BInterval& y) {
  return !(x == y);
}

class BBox {
private:
  vector<BInterval> box;

public:
  BBox(dimension_type dimension) {
    box.resize(dimension);
  }

  dimension_type space_dimension() const {
    return box.size();
  }

  const BInterval& operator[](dimension_type k) const {
    return box[k];
  }

  void print_box(const string& intro = "") {
    if (!intro.empty())
      cout << intro << endl;
    dimension_type dim = box.size();
    for (dimension_type j = 0; j != dim ; j++) {
      cout << j << " AXES:  ";
      box[j].print_interval();
    }
  }

  void raise_lower_bound(dimension_type k, bool closed,
			 const Integer& c, const Integer& d) {
    assert(k < box.size());
    box[k].raise_lower_bound(closed, c, d);
  }

  void lower_upper_bound(dimension_type k, bool closed,
			 const Integer& c, const Integer& d) {
    assert(k < box.size());
    box[k].lower_upper_bound(closed, c, d);
  }

  void set_empty() {
    for (dimension_type k = box.size(); k-- > 0; )
      box[k].set_empty();
  }
};

bool
operator==(const BBox& x, const BBox& y) {
  dimension_type dimension = x.space_dimension();
  if (dimension != y.space_dimension())
    return false;

  for (dimension_type i = dimension; i-- > 0; )
    if (x[i] != y[i])
      return false;

  return true;
}

inline bool
operator!=(const BBox& x, const BBox& y) {
  return !(x == y);
}


// This is a non-bounded closed polyhedron consisting of the line x = y.
// The bounding box is the xy plane - the universal polyhedron.
void test1() {
  Variable x(0);
  Variable y(1);
  C_Polyhedron ph(2);
  ph.add_constraint(x - y >= 0);

  BBox pbox(2);
  ph.shrink_bounding_box(pbox, true);

  BBox nbox(2);
  ph.shrink_bounding_box(nbox);
  
#if NOISY
  print_constraints(ph, "*** test1 ph ***");
  nbox.print_box("*** test1 nbox ***");
  pbox.print_box("*** test1 pbox ***");
#endif

  BBox known_box(2);

#if NOISY
  known_box.print_box("*** test1 known_box ***");
#endif

   if (nbox != known_box || pbox != known_box)
     exit(1);
}

// This is a non-bounded closed polyhedron consisting of the +ve quadrant.
void test2() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(2);
  ph.add_constraint(x >= y);
  ph.add_constraint(y >= 0);

  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, true);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

#if NOISY
  print_constraints(ph, "*** test2 ph ***");
  nbox.print_box("*** test2 nbox ***");
  pbox.print_box("*** test2 pbox ***");
#endif
  
  BBox known_nbox(2);
  known_nbox.raise_lower_bound(0, true, 0, 1);
  known_nbox.raise_lower_bound(1, true, 0, 1);
  
  BBox known_pbox(2);
  known_pbox.raise_lower_bound(1, true, 0, 1);
#if NOISY
  known_nbox.print_box("*** test2 known_nbox ***");
  known_pbox.print_box("*** test2 known_pbox ***");
#endif

  if (nbox != known_nbox || pbox != known_pbox)
    exit(1);
}

// This is a bounded closed polyhedron;
void test3() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(2);
  ph.add_constraint(3 * x +y >= 2);
  ph.add_constraint(x <= 4);
  ph.add_constraint(y <= 4);

  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, true);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

#if NOISY
  print_constraints(ph, "*** test3 ph ***");
  nbox.print_box("*** test3 nbox ***");
  pbox.print_box("*** test3 pbox ***");
#endif

  BBox known_nbox(2);
  known_nbox.raise_lower_bound(0, true, -2, 3);
  known_nbox.lower_upper_bound(0, true, 4, 1);
  known_nbox.raise_lower_bound(1, true, -10, 1);
  known_nbox.lower_upper_bound(1, true, 12, 3);

  BBox known_pbox(2);
  known_pbox.lower_upper_bound(0, true, 4, 1);
  known_pbox.lower_upper_bound(1, true, 4, 1);

#if NOISY
  known_nbox.print_box("*** test3 known_nbox ***");
  known_pbox.print_box("*** test3 known_pbox ***");
#endif

  if (nbox != known_nbox || pbox != known_pbox)
    exit(1);
}

// This is a unbounded closed polyhedron in 4D but bounded in 2D;
void test4() {
  //Variable w(0);
  Variable x(1);
  Variable y(2);
  Variable z(3);

  C_Polyhedron ph(4);
  ph.add_constraint(3 * x + y >= 2);
  ph.add_constraint(x <= 4);
  ph.add_constraint(y <= 4);
  ph.add_constraint(z >= 5);
  
  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, true);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

#if NOISY
  print_constraints(ph, "*** test4 ph ***");
  nbox.print_box("*** test4 nbox ***");
  pbox.print_box("*** test4 pbox ***");
#endif

  BBox known_nbox(4);
  known_nbox.raise_lower_bound(1, true, -2, 3);
  known_nbox.lower_upper_bound(1, true, 4, 1);
  known_nbox.raise_lower_bound(2, true, -10, 1);
  known_nbox.lower_upper_bound(2, true, 12, 3);
  known_nbox.raise_lower_bound(3, true, 15, 3);

  BBox known_pbox(4);
  known_pbox.lower_upper_bound(1, true, 4, 1);
  known_pbox.lower_upper_bound(2, true, 4, 1);
  known_pbox.raise_lower_bound(3, true, 5, 1);

#if NOISY
  known_nbox.print_box("*** test4 known_nbox ***");
  known_pbox.print_box("*** test4 known_pbox ***");
#endif

  if (nbox != known_nbox || pbox != known_pbox)
    exit(1);
}

// This is a universal, 2-dimensional closed polyhedron.
void test5() {
  C_Polyhedron ph(2);

  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, true);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

#if NOISY
  print_constraints(ph, "*** test5 ph ***");
  nbox.print_box("*** test5 nbox ***");
  pbox.print_box("*** test5 pbox ***");
#endif

  BBox known_box(2);

#if NOISY
  known_box.print_box("*** test5 known_box ***");
#endif

  if (nbox != known_box || pbox != known_box)
    exit(1);
}

// This is an zero-dimensional closed polyhedron.
void test6() {
  C_Polyhedron ph;

  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, true);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

#if NOISY
  print_constraints(ph, "*** test6 ph ***");
  nbox.print_box("*** test6 nbox ***");
  pbox.print_box("*** test6 pbox ***");
#endif

  BBox known_box(0);

#if NOISY
  known_box.print_box("*** test6 known_box ***");
#endif

  if (nbox != known_box || pbox != known_box)
    exit(1);
}

// This is an empty closed polyhedron.
void test7() {
  C_Polyhedron ph(2, C_Polyhedron::EMPTY);

  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, true);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

#if NOISY
  print_constraints(ph, "*** test7 ph ***");
  nbox.print_box("*** test7 nbox ***");
  pbox.print_box("*** test7 pbox ***");
#endif

  BBox known_box(ph.space_dimension());
  known_box.set_empty();

#if NOISY
  known_box.print_box("*** test7 known_box ***");
#endif

  if (nbox != known_box || pbox != known_box)
    exit(1);
}

// This is a bounded C polyhedron that is a single point;
void test8() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(2);
  ph.add_constraint(x == 2);
  ph.add_constraint(y == 4);

  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, true);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

#if NOISY
  print_constraints(ph, "*** test8 ph ***");
  nbox.print_box("*** test8 nbox ***");
  pbox.print_box("*** test8 pbox ***");
#endif

  BBox known_box(2);
  known_box.raise_lower_bound(0, true, 2, 1);
  known_box.lower_upper_bound(0, true, 2, 1);
  known_box.raise_lower_bound(1, true, 4, 1);
  known_box.lower_upper_bound(1, true, 4, 1);

#if NOISY
  known_box.print_box("*** test8 known_box ***");
#endif

  if (nbox != known_box || pbox != known_box)
    exit(1);
}

// This is a unit square closed polyhedron
void test9() {
  Variable x(0);
  Variable y(1);

  ConSys cs;
  cs.insert(x >= 0);
  cs.insert(x <= 1);
  cs.insert(y >= 0);
  cs.insert(y <= 1);

  C_Polyhedron ph(cs);

  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, true);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

#if NOISY
  print_constraints(ph, "*** test9 ph constraints ***");
  nbox.print_box("*** test9 nbox ***");
  pbox.print_box("*** test9 pbox ***");
#endif

  BBox known_box(2);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 1, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.lower_upper_bound(1, true, 1, 1);
#if NOISY
  known_box.print_box("*** test9 known_box ***");
#endif

  if (nbox != known_box || pbox != known_box)
    exit(1);
}

// This is a unbounded closed polyhedron in 4D but bounded in 2D
void test10() {
  //Variable w(0);
  Variable x(1);
  Variable y(2);
  Variable z(3);

  C_Polyhedron ph(4);
  ph.add_constraint(3 * x +y >= 2);
  ph.add_constraint(x <= 4);
  ph.add_constraint(y <= 4);
  ph.add_constraint(z >= 5);

#if NOISY
  print_constraints(ph, "*** test10 ph ***");
#endif

  BBox pbox(ph.space_dimension());
  ph.shrink_bounding_box(pbox, true);

  BBox nbox(ph.space_dimension());
  ph.shrink_bounding_box(nbox);

#if NOISY
  nbox.print_box("*** test10 nbox ***");
  pbox.print_box("*** test10 pbox ***");
#endif

  BBox known_nbox(4);
  known_nbox.raise_lower_bound(1, true, -2, 3);
  known_nbox.lower_upper_bound(1, true, 4, 1);
  known_nbox.raise_lower_bound(2, true, -10, 1);
  known_nbox.lower_upper_bound(2, true, 4, 1);
  known_nbox.raise_lower_bound(3, true, 5, 1);

  BBox known_pbox(4);
  known_pbox.raise_lower_bound(1, true, 4, 1);

#if NOISY
  known_nbox.print_box("*** test10 known_nbox ***");
#endif

  if (nbox != known_nbox)
    exit(1);
}

int
main() {
  set_handlers();

  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();
  test8();
  test9();
  test10();

  return 0;
}
