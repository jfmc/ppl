/* Testing Polyhedron::bounding_box().
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

#define NOISY 0
#define C_TESTS 1
#define NNC_TESTS 1

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
  BBox(unsigned int dimension) {
    box.resize(dimension);
  }

  unsigned int space_dimension() const {
    return box.size();
  }

  const BInterval& operator[](size_t k) const {
    return box[k];
  }

  void print_box(const string& intro = "") {
    if (!intro.empty())
      cout << intro << endl;
    size_t dim = box.size();
    for (size_t j = 0; j != dim ; j++) {
      cout << j << " AXES:  ";
      box[j].print_interval();
    }
  }

  void raise_lower_bound(size_t k, bool closed,
			 const Integer& c, const Integer& d) {
    assert(k < box.size());
    box[k].raise_lower_bound(closed, c, d);
  }

  void lower_upper_bound(size_t k, bool closed,
			 const Integer& c, const Integer& d) {
    assert(k < box.size());
    box[k].lower_upper_bound(closed, c, d);
  }

  void set_empty(size_t k) {
    assert(k < box.size());
    box[k].set_empty();
  }
};

bool
operator==(const BBox& x, const BBox& y) {
  unsigned int dimension = x.space_dimension();
  if (dimension != y.space_dimension())
    return false;

  for (unsigned int i = dimension; i-- > 0; )
    if (x[i] != y[i])
      return false;

  return true;
}

inline bool
operator!=(const BBox& x, const BBox& y) {
  return !(x == y);
}

// This is a non-bounded C polyhedron consisting of the line x = y.
// The bounding box is the xy plane - the universal polyhedron.
void test0() {
#if C_TESTS
  Variable x(0);
  Variable y(1);
  C_Polyhedron ph(2);
  ph.add_constraint(x - y >= 0);

#if NOISY
  print_generators(ph, "*** test0 ph ***");
#endif
  
  BBox box(2);
  ph.shrink_bounding_box(box);
#if NOISY
  box.print_box("*** test0 box ***");
#endif
  
  BBox known_box(2);
#if NOISY
  known_box.print_box("*** test0 known ***");
#endif

   if ((box != known_box))
     exit(1);

#endif //C_TESTS
}

  // This is a non-bounded NNC polyhedron consisting of the line x = y.
  // The bounding box is the xy plane - the universal polyhedron.
void test0a() {
#if NNC_TESTS
  Variable x(0);
  Variable y(1);
  NNC_Polyhedron ph(2);
  ph.add_constraint(x - y >= 0);

#if NOISY
  print_generators(ph, "*** test0a ph ***");
#endif
  
  BBox box(2);
  ph.shrink_bounding_box(box);
#if NOISY
  box.print_box("*** test0a box ***");
#endif
  
  BBox known_box(2);
#if NOISY
  known_box.print_box("*** test0a known ***");
#endif

  if (box != known_box)
    exit(1);

#endif //NNC_TESTS
}

  // This is a non-bounded C polyhedron consisting of the +ve quadrant.
void test1() {
#if C_TESTS
  Variable x(0);
  Variable y(1);

  NNC_Polyhedron ph(2);
  ph.add_constraint(x >= y);
  ph.add_constraint(y >= 0);

#if NOISY
  print_generators(ph, "*** test1 ph ***");
#endif
  
  BBox box(ph.space_dimension());
  ph.shrink_bounding_box(box);

#if NOISY
    box.print_box("*** test1 box ***");
#endif
   
  
  BBox known_box(2);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
#if NOISY
  known_box.print_box("*** test1 known ***");
#endif

  if (box != known_box)
    exit(1);

#endif //C_TESTS
}

  // This is a non-bounded NNC polyhedron consisting of the +ve quadrant.
void test1a() {
#if NNC_TESTS
  Variable x(0);
  Variable y(1);

  NNC_Polyhedron ph(2);
  ph.add_constraint(x >= y);
  ph.add_constraint(y >= 0);

#if NOISY
  print_generators(ph, "*** test1a ph ***");
#endif
  
  BBox box(ph.space_dimension());
  ph.shrink_bounding_box(box);

#if NOISY
    box.print_box("*** test1a box ***");
#endif
   
  
  BBox known_box(2);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
#if NOISY
  known_box.print_box("*** test1a known ***");
#endif

  if (box != known_box)
    exit(1);

#endif //NNC_TESTS
}
 
  // This is a bounded C polyhedron;
void test2() {
#if C_TESTS
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph(2);
  ph.add_constraint(3 * x +y >= 2);
  ph.add_constraint(x <= 4);
  ph.add_constraint(y <= 4);

#if NOISY
  print_generators(ph, "*** test2 ph ***");
#endif
  BBox box(ph.space_dimension());
  ph.shrink_bounding_box(box);
#if NOISY
  box.print_box("*** test2 box ***");
#endif
  
  BBox known_box(2);
  known_box.raise_lower_bound(0, true, -2, 3);
  known_box.lower_upper_bound(0, true, 4, 1);
  known_box.raise_lower_bound(1, true, -10, 1);
  known_box.lower_upper_bound(1, true, 12, 3);
#if NOISY
  known_box.print_box("*** test2 known ***");
#endif

  if (box != known_box)
    exit(1);

#endif //C_TESTS
}
 
  // This is a bounded NNC polyhedron;
void test2a() {
#if NNC_TESTS
  Variable x(0);
  Variable y(1);

  NNC_Polyhedron ph(2);
  ph.add_constraint(3 * x +y >= 2);
  ph.add_constraint(x <= 4);
  ph.add_constraint(y <= 4);

#if NOISY
  print_generators(ph, "*** test2a ph ***");
#endif
  BBox box(ph.space_dimension());
  ph.shrink_bounding_box(box);
#if NOISY
  box.print_box("*** test2a box ***");
#endif
  
  BBox known_box(2);
  known_box.raise_lower_bound(0, true, -2, 3);
  known_box.lower_upper_bound(0, true, 4, 1);
  known_box.raise_lower_bound(1, true, -10, 1);
  known_box.lower_upper_bound(1, true, 4, 1);
#if NOISY
  known_box.print_box("*** test2a known ***");
#endif

   if (box != known_box)
     exit(1);

#endif //NNC_TESTS
}
 
  // This is a unbounded C polyhedron in 4D but bounded in 2D;
void test3() {
#if C_TESTS
  //Variable w(0);
  Variable x(1);
  Variable y(2);
  Variable z(3);

  C_Polyhedron ph(4);
  ph.add_constraint(3 * x + y >= 2);
  ph.add_constraint(x <= 4);
  ph.add_constraint(y <= 4);
  ph.add_constraint(z >= 5);

#if NOISY
  print_generators(ph, "*** test3 ph ***");
#endif
  BBox box(ph.space_dimension());
  ph.shrink_bounding_box(box);
#if NOISY
  box.print_box("*** test3 box ***");
#endif
  
  BBox known_box(4);
  known_box.raise_lower_bound(1, true, -2, 3);
  known_box.lower_upper_bound(1, true, 4, 1);
  known_box.raise_lower_bound(2, true, -10, 1);
  known_box.lower_upper_bound(2, true, 12, 3);
  known_box.raise_lower_bound(3, true, 15, 3);
#if NOISY
  known_box.print_box("*** test3 known ***");
#endif
  
   if (box != known_box)
     exit(1);

#endif //C_TESTS
}
 
  // This is a unbounded NNC polyhedron in 4D but bounded in 2D;
void test3a() {
#if NNC_TESTS
  //Variable w(0);
  Variable x(1);
  Variable y(2);
  Variable z(3);

  NNC_Polyhedron ph(4);
  ph.add_constraint(3 * x +y >= 2);
  ph.add_constraint(x <= 4);
  ph.add_constraint(y <= 4);
  ph.add_constraint(z >= 5);

#if NOISY
  print_generators(ph, "*** test3a ph ***");
#endif
  BBox box(ph.space_dimension());
  ph.shrink_bounding_box(box);
#if NOISY
  box.print_box("*** test3a box ***");
#endif
  
  BBox known_box(4);
  known_box.raise_lower_bound(1, true, -2, 3);
  known_box.lower_upper_bound(1, true, 4, 1);
  known_box.raise_lower_bound(2, true, -10, 1);
  known_box.lower_upper_bound(2, true, 4, 1);
  known_box.raise_lower_bound(3, true, 5, 1);
#if NOISY
  known_box.print_box("*** test3a known ***");
#endif
  
   if (box != known_box)
     exit(1);

#endif //NNC_TESTS
}

  // This is a universal, 2-dimensional C polyhedron. 
void test4() {
#if C_TESTS
  C_Polyhedron ph(2);

#if NOISY
  print_constraints(ph, "*** test4 ph ***");
#endif  
 
  BBox box(ph.space_dimension());
  ph.shrink_bounding_box(box);
#if NOISY
  box.print_box("*** test4 box ***");
#endif
  
  BBox known_box(2);
#if NOISY
  known_box.print_box("*** test4 known ***");
#endif

  if (box != known_box)
    exit(1);

#endif //C_TESTS
}

  // This is a universal, 2-dimensional NNC polyhedron. 
void test4a() {
#if NNC_TESTS
  NNC_Polyhedron ph(2);

#if NOISY
  print_constraints(ph, "*** test4a ph ***");
#endif  
 
  BBox box(ph.space_dimension());
  ph.shrink_bounding_box(box);
#if NOISY
  box.print_box("*** test4a box ***");
#endif
  
  BBox known_box(2);
#if NOISY
  known_box.print_box("*** test4a known ***");
#endif

  if (box != known_box)
    exit(1);

#endif //NNC_TESTS
}

  // This is an zero-dimensional C polyhedron. 
void test5() {
#if C_TESTS
  C_Polyhedron ph;

#if NOISY
  print_constraints(ph, "*** test5 ph ***");
#endif  
 
  BBox box(ph.space_dimension());
  ph.shrink_bounding_box(box);
#if NOISY
  box.print_box("*** test5 box ***");
#endif
  
  BBox known_box(0);
#if NOISY
  known_box.print_box("*** test5 known ***");
#endif

  if (box != known_box)
    exit(1);

#endif //C_TESTS
}

  // This is an zero-dimensional NNC polyhedron. 
void test5a() {
#if TEST5a
  NNC_Polyhedron ph;

#if NOISY
  print_constraints(ph, "*** test5a ph ***");
#endif  
 
  BBox box(ph.space_dimension());
  ph.shrink_bounding_box(box);
#if NOISY
  box.print_box("*** test5a box ***");
#endif
  
  BBox known_box(0);
#if NOISY
  known_box.print_box("*** test5a known ***");
#endif

  if (box != known_box)
    exit(1);

#endif //NNC_TESTS
}

  // This is an empty C polyhedron. 
void test6() {
#if C_TESTS
  C_Polyhedron ph(2, C_Polyhedron::EMPTY);

#if NOISY
  print_constraints(ph, "*** test6 ph ***");
#endif  
 
  
  BBox box(ph.space_dimension());
  ph.shrink_bounding_box(box);
#if NOISY
  box.print_box("*** test6 box ***");
#endif

  BBox known_box(ph.space_dimension());
  known_box.set_empty(0);
  known_box.set_empty(1);
#if NOISY
  known_box.print_box("*** test6 known ***");
#endif

  if (box != known_box)
    exit(1);

#endif //C_TESTS
}

  // This is an empty NNC polyhedron. 
void test6a() {
#if NNC_TESTS
  NNC_Polyhedron ph(2, C_Polyhedron::EMPTY);

#if NOISY
  print_constraints(ph, "*** test6a ph ***");
#endif  
 
  
  BBox box(ph.space_dimension());
  ph.shrink_bounding_box(box);
#if NOISY
  box.print_box("*** test6a box ***");
#endif

  BBox known_box(ph.space_dimension());
  known_box.set_empty(0);
  known_box.set_empty(1);
#if NOISY
  known_box.print_box("*** test6a known ***");
#endif

  if (box != known_box)
    exit(1);

#endif //NNC_TESTS
}

// This is a unit square C polyhedron
void test10() {
#if C_TESTS
  Variable x(0);
  Variable y(1);

  ConSys cs;
  cs.insert(x >= 0);
  cs.insert(x <= 1);
  cs.insert(y >= 0);
  cs.insert(y <= 1);
 
  C_Polyhedron ph(cs);

#if NOISY
  print_constraints(ph, "*** test10 ph constraints ***");
  print_generators(ph, "*** test10 ph generators ***");
#endif

  BBox box(ph.space_dimension());
  ph.shrink_bounding_box(box);
#if NOISY
  box.print_box("*** test10 box ***");
#endif
  BBox known_box(2);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 1, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.lower_upper_bound(1, true, 1, 1);
#if NOISY
  known_box.print_box("*** test10 known ***");
#endif

  if (box != known_box)
    exit(1);

#endif //C_TESTS
}

// This is a unit square NNC polyhedron
void test10a() {
#if NNC_TESTS
  Variable x(0);
  Variable y(1);

  ConSys cs;
  cs.insert(x >= 0);
  cs.insert(x <= 1);
  cs.insert(y >= 0);
  cs.insert(y <= 1);
 
  NNC_Polyhedron ph(cs);

#if NOISY
  print_generators(ph, "*** test10a ph generators ***");
#endif

  BBox box(ph.space_dimension());
  ph.shrink_bounding_box(box);
#if NOISY
  box.print_box("*** test10a box ***");
#endif
  BBox known_box(2);
  known_box.raise_lower_bound(0, true, 0, 1);
  known_box.lower_upper_bound(0, true, 1, 1);
  known_box.raise_lower_bound(1, true, 0, 1);
  known_box.lower_upper_bound(1, true, 1, 1);
#if NOISY
  known_box.print_box("*** test10a known ***");
#endif

  if (box != known_box)
    exit(1);

#endif //NNC_TESTS
}
 
  // This is a unbounded NNC polyhedron in 4D but bounded in 2D
  // with strict inequality and closure points at the lower bound.
void test11() {
#if NNC_TESTS
  //Variable w(0);
  Variable x(1);
  Variable y(2);
  Variable z(3);

  NNC_Polyhedron ph(4);
  ph.add_constraint(3 * x +y > 2);
  ph.add_constraint(x <= 4);
  ph.add_constraint(y <= 4);
  ph.add_constraint(z >= 5);

#if NOISY
  print_generators(ph, "*** test11 ph ***");
#endif
  BBox box(ph.space_dimension());
  ph.shrink_bounding_box(box);
#if NOISY
  box.print_box("*** test11 box ***");
#endif
  
  BBox known_box(4);
  known_box.raise_lower_bound(1, false, -2, 3);
  known_box.lower_upper_bound(1, true, 4, 1);
  known_box.raise_lower_bound(2, false, -10, 1);
  known_box.lower_upper_bound(2, true, 4, 1);
  known_box.raise_lower_bound(3, true, 5, 1);
#if NOISY
  known_box.print_box("*** test11 known ***");
#endif
  
  if (box != known_box)
    exit(1);

#endif //NNC_TESTS
}

 
  // This is a bounded NNC polyhedron with strict inequalities 
  // causing upper and lower bounds of the box to be open.
void test12() {
#if NNC_TESTS
  Variable x(0);
  Variable y(1);

  NNC_Polyhedron ph(2);
  ph.add_constraint(3 * x + y >= 2);
  ph.add_constraint(x < 4);
  ph.add_constraint(y <= 4);

#if NOISY
  print_generators(ph, "*** test12 ph ***");
#endif
  BBox box(ph.space_dimension());
  ph.shrink_bounding_box(box);
#if NOISY
  box.print_box("*** test12 box ***");
#endif
  
  BBox known_box(2);
  known_box.raise_lower_bound(0, true, -2, 3);
  known_box.lower_upper_bound(0, false, 4, 1);
  known_box.raise_lower_bound(1, false, -10, 1);
  known_box.lower_upper_bound(1, true, 4, 1);
#if NOISY
  known_box.print_box("*** test12 known ***");
#endif

  if (box != known_box)
    exit(1);

#endif //NNC_TESTS
}
 

int
main() {

  test0();
  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test10();
  test0a();
  test1a();
  test2a();
  test3a();
  test4a();
  test5a();
  test6a();
  test10a();
  test11();
  test12();

  return 0;
}
