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

#define TEST4 0
#define TEST5 0
#define TEST6 0
#define TEST10 0
#define TEST4a 1
#define TEST5a 1
#define TEST6a 1
#define TEST10a 1

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

  void raise_lower_bound(bool closed,
			 const Integer& c, const Integer& d) {
    assert(d > 0 && ld >= 0);
    if (closed == false)
      if (lc*d <= c*ld)  {
        lc = c;
        ld = d;
        lclosed = closed;
      }
    else 
      assert(closed == true);
      if (lc*d < c*ld)  {
        lc = c;
        ld = d;
        lclosed = closed;
      }
  }

  void lower_upper_bound(bool closed,
			 const Integer& c, const Integer& d) {
    assert(d > 0 && ud >= 0);
    if (closed == false)
      if (uc*d >= c*ud)  {
        uc = c;
        ud = d;
        uclosed = closed;
      }
    else 
      assert(closed == true);
      if (uc*d > c*ud)  {
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
			 const Integer& c, const Integer& d) {
      //cout << "raise_lower_bound("
      // << k << ", "
      //<< (closed ? "true" : "false") << ", "
      //<< c << ", "
      //<< d << ")" << endl;
    assert(k < box.size());
    box[k].raise_lower_bound(closed, c, d);
  }

  void lower_upper_bound(size_t k, bool closed,
			 const Integer& c, const Integer& d) {
      //cout << "lower_upper_bound("
      //<< k << ", "
      //<< (closed ? "true" : "false") << ", "
      //<< c << ", "
      //<< d << ")" << endl;
    assert(k < box.size());
    box[k].lower_upper_bound(closed, c, d);
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
  
  BBox box0(2);
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

  // This is a non-bounded NNC polyhedron consisting of the line x = y.
  // The bounding box is the xy plane - the universal polyhedron.
void test0a() {
  Variable x(0);
  Variable y(1);
  NNC_Polyhedron ph0a(2);
  ph0a.add_constraint(x - y >= 0);

#if NOISY
  print_constraints(ph0a, "*** ph0a ***");
  print_generators(ph0a, "*** ph0a ***");
#endif
  
  BBox box0a(2);
  ph0a.shrink_bounding_box(box0a);
#if NOISY
  box0a.print_box("*** box0a ***");
#endif
  
  BBox known_box0a(2);
#if NOISY
  known_box0a.print_box("*** known0a ***");
#endif

  //if (box0a != known_box0a)
  //  exit(1);
  if (!ph0a.OK())
    exit(1);
}

  // This is a non-bounded C polyhedron consisting of the +ve quadrant.
void test1() {
  Variable x(0);
  Variable y(1);

  NNC_Polyhedron ph1(2);
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

  // This is a non-bounded NNC polyhedron consisting of the +ve quadrant.
void test1a() {
  Variable x(0);
  Variable y(1);

  NNC_Polyhedron ph1a(2);
  ph1a.add_constraint(x >= y);
  ph1a.add_constraint(y >= 0);

#if NOISY
  print_constraints(ph1a, "*** ph1a ***");
  print_generators(ph1a, "*** ph1a ***");
#endif
  
  BBox box1a(ph1a.space_dimension());
  ph1a.shrink_bounding_box(box1a);

#if NOISY
    box1a.print_box("*** box1a ***");
#endif
   
  
  BBox known_box1a(2);
  known_box1a.raise_lower_bound(0, true, 0, 1);
  known_box1a.raise_lower_bound(1, true, 0, 1);
#if NOISY
  known_box1a.print_box("*** known1a ***");
#endif

  //if (ph1a != known_result1a)
  // exit(1);
  if (!ph1a.OK())
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
 
  // This is a bounded polyhedron;
void test2a() {
  Variable x(0);
  Variable y(1);

  NNC_Polyhedron ph2a(2);
  ph2a.add_constraint(3 * x +y >= 2);
  ph2a.add_constraint(x <= 4);
  ph2a.add_constraint(y <= 4);

#if NOISY
  print_generators(ph2a, "*** ph2a ***");
#endif
  BBox box2a(ph2a.space_dimension());
  ph2a.shrink_bounding_box(box2a);
#if NOISY
  box2a.print_box("*** box2a ***");
#endif
  
  BBox known_box2a(2);
  known_box2a.raise_lower_bound(0, true, -2, 3);
  known_box2a.lower_upper_bound(0, true, 4, 1);
  known_box2a.raise_lower_bound(1, true, -10, 1);
  known_box2a.lower_upper_bound(1, true, 4, 1);
#if NOISY
  known_box2a.print_box("*** known2a ***");
#endif

  // if (ph2a != known_result2a)
  //   exit(1);
  if (!ph2a.OK())
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
 
  // This is a unbounded polyhedron in 4D but bounded in 2D;
void test3a() {
  //Variable w(0);
  Variable x(1);
  Variable y(2);
  Variable z(3);

  NNC_Polyhedron ph3a(4);
  ph3a.add_constraint(3 * x +y >= 2);
  ph3a.add_constraint(x <= 4);
  ph3a.add_constraint(y <= 4);
  ph3a.add_constraint(z >= 5);

#if NOISY
  print_generators(ph3a, "*** ph3a ***");
#endif
  BBox box3a(ph3a.space_dimension());
  ph3a.shrink_bounding_box(box3a);
#if NOISY
  box3a.print_box("*** box3a ***");
#endif
  
  BBox known_box3a(4);
  known_box3a.raise_lower_bound(1, true, -2, 3);
  known_box3a.lower_upper_bound(1, true, 4, 1);
  known_box3a.raise_lower_bound(2, true, -10, 1);
  known_box3a.lower_upper_bound(2, true, 4, 1);
  known_box3a.raise_lower_bound(3, true, 5, 1);
#if NOISY
  known_box3a.print_box("*** known3a ***");
#endif
  
  // if (ph3a != known_result3a)
  //   exit(1);
  if (!ph3a.OK())
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

  // This is a universal, 2-dimensional polyhedron. 
void test4a() {
#if TEST4a
  NNC_Polyhedron ph4a(2);

#if NOISY
  print_constraints(ph4a, "*** ph4a ***");
#endif  
 
  BBox box4a(ph4a.space_dimension());
  ph4a.shrink_bounding_box(box4a);
#if NOISY
  box4a.print_box("*** box4a ***");
#endif
  
  BBox known_box4a(2);
#if NOISY
  known_box4a.print_box("*** known4a ***");
#endif

  //if (ph4a != known_result4a)
  //  exit(1);
  if (!ph4a.OK())
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

  // This is an zero-dimensional polyhedron. 
void test5a() {
#if TEST5a
  NNC_Polyhedron ph5a;

#if NOISY
  print_constraints(ph5a, "*** ph5a ***");
#endif  
 
  BBox box5a(ph5a.space_dimension());
  ph5a.shrink_bounding_box(box5a);
#if NOISY
  box5a.print_box("*** box5a ***");
#endif
  
  BBox known_box5a(0);
#if NOISY
  known_box5a.print_box("*** known5a ***");
#endif

  //if (ph5a != known_result5a)
  // exit(1);
  if (!ph5a.OK())
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

  // This is an empty polyhedron. 
void test6a() {
#if TEST6a
  NNC_Polyhedron ph6a(2, C_Polyhedron::EMPTY);

#if NOISY
  print_constraints(ph6a, "*** ph6a ***");
#endif  
 
  
  BBox box6a(ph6a.space_dimension());
  ph6a.shrink_bounding_box(box6a);
#if NOISY
  box6a.print_box("*** box6a ***");
#endif

  BBox known_box6a(ph6a.space_dimension());
  known_box6a.set_empty(0);
  known_box6a.set_empty(1);
#if NOISY
  known_box6a.print_box("*** known6a ***");
#endif

  //if (ph6a != known_result6a)
  //  exit(1);
  if (!ph6a.OK())
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

void test10a() {
#if TEST10a
  Variable x(0);
  Variable y(1);

  ConSys cs;
  cs.insert(x >= 0);
  cs.insert(x <= 1);
  cs.insert(y >= 0);
  cs.insert(y <= 1);
 
  NNC_Polyhedron ph10a(cs);

#if NOISY
  print_constraints(ph10a, "*** ph10a constraints ***");
  print_generators(ph10a, "*** ph10a generators ***");
#endif

  BBox box10a(ph10a.space_dimension());
  ph10a.shrink_bounding_box(box10a);
#if NOISY
  box10a.print_box("*** box10a ***");
#endif
  BBox known_box10a(2);
  known_box10a.raise_lower_bound(0, true, 0, 1);
  known_box10a.lower_upper_bound(0, true, 1, 1);
  known_box10a.raise_lower_bound(1, true, 0, 1);
  known_box10a.lower_upper_bound(1, true, 1, 1);
#if NOISY
  known_box10a.print_box("*** known10a ***");
#endif

  if (!ph10a.OK())
    exit(1);
#endif
}
 
  // This is a unbounded NNC polyhedron in 4D but bounded in 2D
  // with strict inequality and closure points at the lower bound ;
void test11() {
  //Variable w(0);
  Variable x(1);
  Variable y(2);
  Variable z(3);

  NNC_Polyhedron ph11(4);
  ph11.add_constraint(3 * x +y > 2);
  ph11.add_constraint(x <= 4);
  ph11.add_constraint(y <= 4);
  ph11.add_constraint(z >= 5);

#if NOISY
  print_generators(ph11, "*** ph11 ***");
#endif
  BBox box11(ph11.space_dimension());
  ph11.shrink_bounding_box(box11);
#if NOISY
  box11.print_box("*** box11 ***");
#endif
  
  BBox known_box11(4);
  known_box11.raise_lower_bound(1, false, -2, 3);
  known_box11.lower_upper_bound(1, true, 4, 1);
  known_box11.raise_lower_bound(2, false, -10, 1);
  known_box11.lower_upper_bound(2, true, 4, 1);
  known_box11.raise_lower_bound(3, true, 5, 1);
#if NOISY
  known_box11.print_box("*** known11 ***");
#endif
  
  // if (ph11 != known_result11)
  //   exit(1);
  if (!ph11.OK())
    exit(1);
}

 
  // This is a bounded NNC polyhedron with strict inequalities 
  // causing upper and lower bounds of the box to be open;
void test12() {
  Variable x(0);
  Variable y(1);

  NNC_Polyhedron ph12(2);
  ph12.add_constraint(3 * x +y >= 2);
  ph12.add_constraint(x < 4);
  ph12.add_constraint(y <= 4);

#if NOISY
  print_generators(ph12, "*** ph12 ***");
#endif
  BBox box12(ph12.space_dimension());
  ph12.shrink_bounding_box(box12);
#if NOISY
  box12.print_box("*** box12 ***");
#endif
  
  BBox known_box12(2);
  known_box12.raise_lower_bound(0, true, -2, 3);
  known_box12.lower_upper_bound(0, false, 4, 1);
  known_box12.raise_lower_bound(1, false, -10, 1);
  known_box12.lower_upper_bound(1, true, 4, 1);
#if NOISY
  known_box12.print_box("*** known12 ***");
#endif

  // if (ph12 != known_result12)
  //   exit(1);
  if (!ph12.OK())
    exit(1);
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
