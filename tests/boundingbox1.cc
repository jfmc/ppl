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

#define NOISY 1
#define TEST4 1
#define TEST5 1
#define TEST6 1
#define TEST10 1

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
    : uclosed(true), uc(1), ud(0), lclosed(true), lc(-1), ld(0) {
  }

  void raise_lower_bound(bool raise_closed,
			 const Integer& n, const Integer& d) {
    assert(raise_closed == uclosed);
    if ((d*ld > 0 && lc*d <= n*ld) || 
                (d*ld < 0 && lc*d <= n*ld) ||
                     ld == 0)  {
      lc = n;
      ld = d;
    }
  }

  void lower_upper_bound(bool lower_closed,
			 const Integer& n, const Integer& d) {
    assert(lower_closed == lclosed);
    if ((d*ud > 0 && uc*d >= n*ud) || 
                (d*ud < 0 && uc*d <= n*ud) ||
                     ud == 0)  {
      uc = n;
      ud = d;
    }
  }

  void set_empty() {
    uc = -1;
    lc = 1;
    ud = 1;
    ld = 1;}

  void print_interval() {
  cout << "upper bound = " << uc << " / " << ud  << "  "
       << "lower bound = " << lc << " / " << ld << endl;
  }
};

class BBox {
private:
  std::vector<Interval> box;

public:
  BBox(unsigned int dimension) {
    box.resize(dimension);
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
			 const Integer& n, const Integer& d) {
    //cout << "raise_lower_bound("
    // << k << ", "
    // << (closed ? "true" : "false") << ", "
    // << n << ", "
    // << d << ")" << endl;
    assert(k < box.size());
    box[k].raise_lower_bound(closed, n, d);
  }

  void lower_upper_bound(size_t k, bool closed,
			 const Integer& n, const Integer& d) {
    //cout << "lower_upper_bound("
    // << k << ", "
    // << (closed ? "true" : "false") << ", "
    // << n << ", "
    // << d << ")" << endl;
    assert(k < box.size());
    box[k].lower_upper_bound(closed, n, d);
  }

  void set_empty(size_t k) {
    //cout << "lower_upper_bound("
    // << k << ")" << endl;
    assert(k < box.size());
    box[k].set_empty();
  }
};

  // This is a non-bounded polyhedron consisting of the line x = y.
  // The bounding box is the xy plane - the universal polyhedron.
void test0() {
  Variable x(0);
  Variable y(1);
  C_Polyhedron ph0(2);
  ph0.add_constraint(x - y >= 0);

#if NOISY
  print_constraints(ph0, "*** ph0 ***");
  print_generators(ph0, "*** ph0 ***");
#endif
  
  BBox box0(ph0.space_dimension());
  ph0.shrink_bounding_box(box0);
#if NOISY
  box0.print_box("*** box0 ***");
#endif
  
  BBox known_box0(2);
#if NOISY
  known_box0.print_box("*** known0 ***");
#endif

  //if (box0 != known_box0)
  //  exit(1);
  if (!ph0.OK())
    exit(1);
}

  // This is a non-bounded polyhedron consisting of the +ve quadrant.
void test1() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph1(2);
  ph1.add_constraint(x >= y);
  ph1.add_constraint(y >= 0);

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
  print_generators(ph1, "*** ph1 ***");
#endif
  
  BBox box1(ph1.space_dimension());
  ph1.shrink_bounding_box(box1);

#if NOISY
    box1.print_box("*** box1 ***");
#endif
   
  
  BBox known_box1(2);
  known_box1.raise_lower_bound(0, true, 0, 1);
  known_box1.raise_lower_bound(1, true, 0, 1);
#if NOISY
  known_box1.print_box("*** known1 ***");
#endif

  //if (ph1 != known_result1)
  // exit(1);
  if (!ph1.OK())
    exit(1);
}
 
  // This is a bounded polyhedron;
void test2() {
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph2(2);
  ph2.add_constraint(3 * x +y >= 2);
  ph2.add_constraint(x <= 4);
  ph2.add_constraint(y <= 4);

#if NOISY
  print_generators(ph2, "*** ph2 ***");
#endif
  BBox box2(ph2.space_dimension());
  ph2.shrink_bounding_box(box2);
#if NOISY
  box2.print_box("*** box2 ***");
#endif
  
  BBox known_box2(2);
  known_box2.raise_lower_bound(0, true, -2, 3);
  known_box2.lower_upper_bound(0, true, 4, 1);
  known_box2.raise_lower_bound(1, true, -10, 1);
  known_box2.lower_upper_bound(1, true, 12, 3);
#if NOISY
  known_box2.print_box("*** known2 ***");
#endif

  // if (ph2 != known_result2)
  //   exit(1);
  if (!ph2.OK())
    exit(1);
}
 
  // This is a unbounded polyhedron in 4D but bounded in 2D;
void test3() {
  //Variable w(0);
  Variable x(1);
  Variable y(2);
  Variable z(3);

  C_Polyhedron ph3(4);
  ph3.add_constraint(3 * x +y >= 2);
  ph3.add_constraint(x <= 4);
  ph3.add_constraint(y <= 4);
  ph3.add_constraint(z >= 5);

#if NOISY
  print_generators(ph3, "*** ph3 ***");
#endif
  BBox box3(ph3.space_dimension());
  ph3.shrink_bounding_box(box3);
#if NOISY
  box3.print_box("*** box3 ***");
#endif
  
  BBox known_box3(4);
  known_box3.raise_lower_bound(1, true, -2, 3);
  known_box3.lower_upper_bound(1, true, 4, 1);
  known_box3.raise_lower_bound(2, true, -10, 1);
  known_box3.lower_upper_bound(2, true, 12, 3);
  known_box3.raise_lower_bound(3, true, 15, 3);
#if NOISY
  known_box3.print_box("*** known3 ***");
#endif
  
  // if (ph3 != known_result3)
  //   exit(1);
  if (!ph3.OK())
    exit(1);
}

  // This is a universal, 2-dimensional polyhedron. 
void test4() {
#if TEST4
  C_Polyhedron ph4(2);

#if NOISY
  print_constraints(ph4, "*** ph4 ***");
#endif  
 
  BBox box4(ph4.space_dimension());
  ph4.shrink_bounding_box(box4);
#if NOISY
  box4.print_box("*** box4 ***");
#endif
  
  BBox known_box4(2);
#if NOISY
  known_box4.print_box("*** known4 ***");
#endif

  //if (ph4 != known_result4)
  //  exit(1);
  if (!ph4.OK())
    exit(1);
#endif
}
  // This is an zero-dimensional polyhedron. 
void test5() {
#if TEST5
  C_Polyhedron ph5;

#if NOISY
  print_constraints(ph5, "*** ph5 ***");
#endif  
 
  BBox box5(ph5.space_dimension());
  ph5.shrink_bounding_box(box5);
#if NOISY
  box5.print_box("*** box5 ***");
#endif
  
  BBox known_box5(0);
#if NOISY
  known_box5.print_box("*** known5 ***");
#endif

  //if (ph5 != known_result5)
  // exit(1);
  if (!ph5.OK())
    exit(1);
#endif
}

  // This is an empty polyhedron. 
void test6() {
#if TEST6
  C_Polyhedron ph6(2, C_Polyhedron::EMPTY);

#if NOISY
  print_constraints(ph6, "*** ph6 ***");
#endif  
 
  
  BBox box6(ph6.space_dimension());
  ph6.shrink_bounding_box(box6);
#if NOISY
  box6.print_box("*** box6 ***");
#endif

  BBox known_box6(ph6.space_dimension());
  known_box6.set_empty(0);
  known_box6.set_empty(1);
#if NOISY
  known_box6.print_box("*** known6 ***");
#endif

  //if (ph6 != known_result6)
  //  exit(1);
  if (!ph6.OK())
    exit(1);
#endif
}

void test10() {
#if TEST10
  Variable x(0);
  Variable y(1);

  ConSys cs;
  cs.insert(x >= 0);
  cs.insert(x <= 1);
  cs.insert(y >= 0);
  cs.insert(y <= 1);
 
  C_Polyhedron ph10(cs);

#if NOISY
  print_constraints(ph10, "*** ph10 constraints ***");
  print_generators(ph10, "*** ph10 generators ***");
#endif

  BBox box10(ph10.space_dimension());
  ph10.shrink_bounding_box(box10);
#if NOISY
  box10.print_box("*** box10 ***");
#endif
  BBox known_box10(2);
  known_box10.raise_lower_bound(0, true, 0, 1);
  known_box10.lower_upper_bound(0, true, 1, 1);
  known_box10.raise_lower_bound(1, true, 0, 1);
  known_box10.lower_upper_bound(1, true, 1, 1);
#if NOISY
  known_box10.print_box("*** known10 ***");
#endif

  if (!ph10.OK())
    exit(1);
#endif
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

  return 0;
}
