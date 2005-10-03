/* Test Polyhedra_Powerset<PH>::BGP99_extrapolation_assign().
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

int
main() TRY {
  set_handlers();

  typedef Polyhedra_Powerset<TBD_Shape> BDS_Set;

  Variable A(0);
  Variable B(1);

  TBD_Shape bds1(2);
  bds1.add_constraint(-A + B >= 2);
  bds1.add_constraint(A - B >= -4);
  bds1.add_constraint(A >= 0);
  TBD_Shape bds2(2);
  bds2.add_constraint(-A + B >= 3);
  bds2.add_constraint(A - B >= -8);
  bds2.add_constraint(A >= 1);
  TBD_Shape bds3(2);
  bds3.add_constraint(-A + B >= 4);
  bds3.add_constraint(A - B >= -12);
  bds3.add_constraint(A >= 2);
  TBD_Shape bds4(2);
  bds4.add_constraint(-A + B >= 6);
  bds4.add_constraint(A - B >= -16);
  bds4.add_constraint(A >= 3);

  BDS_Set bdss1(2, EMPTY);
  bdss1.add_disjunct(bds1);
  bdss1.add_disjunct(bds2);
  bdss1.add_disjunct(bds3);

  BDS_Set bdss2(bdss1);
  bdss1.add_disjunct(bds4);

#if NOISY
  using namespace Parma_Polyhedra_Library::IO_Operators;
  cout << "*** bdss1 ***" << endl
       << bdss1 << endl;
  cout << "*** bdss2 ***" << endl
       << bdss2 << endl;
#endif

  TBD_Shape bds5(2);
  bds5.add_constraint(-A + B >= 4);
  bds5.add_constraint(A >= 2);
  bds5.add_constraint(B >= 6);

  BDS_Set known_result(2, EMPTY);
  known_result.add_disjunct(bds1);
  known_result.add_disjunct(bds2);
  known_result.add_disjunct(bds5);

  bdss1.BGP99_extrapolation_assign
    (bdss2, widen_fun_ref(&TBD_Shape::H79_widening_assign), 3);

  int retval = bdss1.geometrically_equals(known_result) ? 0 : 1;

#if NOISY
  cout
    << "*** bdss1.BGP99_extrapolation_assign"
    << "(bdss2, widen_fun_ref(&H79_widening_assign), 3) ***"
    << endl
    << bdss1 << endl;
#endif

  return retval;
}
CATCH
