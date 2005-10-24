/* Test BD_Shape::affine_dimension().
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

namespace {

void
test1() {
  Variable x(0);
  Variable y(1);

  TBD_Shape bd1(3);

  bd1.add_constraint(x <= 2);
  bd1.add_constraint(x - y == 3);
  bd1.add_constraint(y <= 2);

  TBD_Shape bd2(2);


#if NOISY
  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
#endif

  dimension_type affine_dim1 = bd1.affine_dimension();
  dimension_type affine_dim2 = bd2.affine_dimension();

#if NOISY
  cout << endl
       << "The affine dimension of a system of `bd1' "
       << endl
       << affine_dim1
       << endl;

  cout << endl
       << "The affine dimension of a system of `bd2' "
       << endl
       << affine_dim2
       << endl;  
#endif

  bool ok = (affine_dim1 == affine_dim2);

  if (!ok)
    exit(1);
}

void
test2() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);

  TBD_Shape bd1(5);

  bd1.add_constraint(A <= 5);
  bd1.add_constraint(A - B == 3);
  bd1.add_constraint(C <= 2);
  bd1.add_constraint(E - D == 2);

  TBD_Shape bd2(4);

  bd2.add_constraint(A <= 1);
  bd2.add_constraint(A - D == 8);
  bd2.add_constraint(B <= 7);

#if NOISY
  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
#endif

  dimension_type affine_dim1 = bd1.affine_dimension();
  dimension_type affine_dim2 = bd2.affine_dimension();

#if NOISY
  cout << endl
       << "The affine dimension of a system of `bd1' "
       << endl
       << affine_dim1
       << endl; 

  cout << endl
       << "The affine dimension of a system of `bd2' "
       << endl
       << affine_dim2
       << endl;
#endif

  bool ok = (affine_dim1 == affine_dim2);

  if (!ok)
    exit(1);
}

void
test3() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);

  TBD_Shape bd1(5);

  bd1.add_constraint(A == 5);
  bd1.add_constraint(A - B == 3);
  bd1.add_constraint(C <= 2);
  bd1.add_constraint(E - D == 2);

  TBD_Shape bd2(5);

  bd2.add_constraint(A == 1);
  bd2.add_constraint(E == 1);
  bd2.add_constraint(A - D == 8);
  bd2.add_constraint(B <= 7);

#if NOISY
  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
#endif

  dimension_type affine_dim1 = bd1.affine_dimension();
  dimension_type affine_dim2 = bd2.affine_dimension();

#if NOISY
  cout << endl
       << "The affine dimension of a system of `bd1' "
       << endl
       << affine_dim1
       << endl; 

  cout << endl
       << "The affine dimension of a system of `bd2' "
       << endl
       << affine_dim2
       << endl;
#endif

  bool ok = (affine_dim1 == affine_dim2);

  if (!ok)
    exit(1);
}

void
test4() {
  Variable A(0);
  Variable B(1);
 
  TBD_Shape bd1(2);

  bd1.add_constraint(A <= 3);
  bd1.add_constraint(B - A <= -5);
  bd1.add_constraint(-B <= 2);

  TBD_Shape bd2(2);

  bd2.add_constraint(A == 0);
  bd2.add_constraint(B == 2);

#if NOISY
  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
#endif

  dimension_type affine_dim1 = bd1.affine_dimension();
  dimension_type affine_dim2 = bd2.affine_dimension();

#if NOISY
  cout << endl
       << "The affine dimension of a system of `bd1' "
       << endl
       << affine_dim1
       << endl; 

  cout << endl
       << "The affine dimension of a system of `bd2' "
       << endl
       << affine_dim2
       << endl;
#endif

  bool ok = (affine_dim1 == affine_dim2);

  if (!ok)
    exit(1);
}


void
test5() {
  Variable A(0);
  Variable B(1);
 
  TBD_Shape bd1(2, EMPTY);

  TBD_Shape bd2(7);

  bd2.add_constraint(A <= 1);
  bd2.add_constraint(B == 2);
  bd2.add_constraint(B - A <= -6);


#if NOISY
  print_constraints(bd1, "*** bd1 ***");
  print_constraints(bd2, "*** bd2 ***");
#endif

  dimension_type affine_dim1 = bd1.affine_dimension();
  dimension_type affine_dim2 = bd2.affine_dimension();

#if NOISY
  cout << endl
       << "The affine dimension of a system of `bd1' "
       << endl
       << affine_dim1
       << endl; 

  cout << endl
       << "The affine dimension of a system of `bd2' "
       << endl
       << affine_dim2
       << endl;
#endif

  bool ok = (affine_dim1 == affine_dim2);

  if (!ok)
    exit(1);
}

} // namespace

int
main() TRY {

  test1();
  test2();
  test3();
  test4();
  test5();

  return 0;
}
CATCH


