/* Test Polyhedron::H79_widening_assign(): we apply this function
   to two zero-dimensional polyhedra.
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
#define NOISY 0
#endif

int
main() TRY {
  set_handlers();

  typedef Polyhedra_PowerSet<C_Polyhedron> PSet;

  Variable A(0);
  Variable B(1);

  C_Polyhedron ps1_1(2);
  ps1_1.add_constraint(-A + B >= 5);
  ps1_1.add_constraint(A - B >= -13);
  ps1_1.add_constraint(A >= 3);
  C_Polyhedron ps1_2(2);
  ps1_2.add_constraint(-A + B >= 6);
  ps1_2.add_constraint(A - B >= -16);
  ps1_2.add_constraint(A >= 3);
  C_Polyhedron ps1_3(2);
  ps1_3.add_constraint(-A + B >= 7);
  ps1_3.add_constraint(A - B >= -20);
  ps1_3.add_constraint(A >= 4);
  C_Polyhedron ps1_4(2);
  ps1_4.add_constraint(-A + B >= 8);
  ps1_4.add_constraint(A - B >= -24);
  ps1_4.add_constraint(A >= 5);
  C_Polyhedron ps1_5(2);
  ps1_5.add_constraint(-A + B >= 10);
  ps1_5.add_constraint(A - B >= -28);
  ps1_5.add_constraint(A >= 6);
  C_Polyhedron ps1_6(2);
  ps1_6.add_constraint(-A + B >= 12);
  ps1_6.add_constraint(A - B >= -32);
  ps1_6.add_constraint(A >= 7);
  C_Polyhedron ps1_7(2);
  ps1_7.add_constraint(-A + B >= 2);
  ps1_7.add_constraint(A - B >= -4);
  ps1_7.add_constraint(A >= 0);
  C_Polyhedron ps1_8(2);
  ps1_8.add_constraint(-A + B >= 3);
  ps1_8.add_constraint(A - B >= -8);
  ps1_8.add_constraint(A >= 1);
  C_Polyhedron ps1_9(2);
  ps1_9.add_constraint(-A + B >= 4);
  ps1_9.add_constraint(A - B >= -12);
  ps1_9.add_constraint(A >= 2);

  PSet ps1(2, Polyhedron::EMPTY);
  ps1.add_disjunct(ps1_1);
  ps1.add_disjunct(ps1_2);
  ps1.add_disjunct(ps1_3);
  ps1.add_disjunct(ps1_4);
  ps1.add_disjunct(ps1_5);
  ps1.add_disjunct(ps1_6);
  ps1.add_disjunct(ps1_7);
  ps1.add_disjunct(ps1_8);
  ps1.add_disjunct(ps1_9);

  C_Polyhedron ps2_1(2);
  ps2_1.add_constraint(-A + B >= 2);
  ps2_1.add_constraint(A - B >= -4);
  ps2_1.add_constraint(A >= 0);
  C_Polyhedron ps2_2(2);
  ps2_2.add_constraint(-A + B >= 3);
  ps2_2.add_constraint(A - B >= -8);
  ps2_2.add_constraint(A >= 1);
  C_Polyhedron ps2_3(2);
  ps2_3.add_constraint(-A + B >= 4);
  ps2_3.add_constraint(A - B >= -12);
  ps2_3.add_constraint(A >= 2);
  C_Polyhedron ps2_4(2);
  ps2_4.add_constraint(-A + B >= 6);
  ps2_4.add_constraint(A - B >= -16);
  ps2_4.add_constraint(A >= 3);

  PSet ps2(2, Polyhedron::EMPTY);
  ps2.add_disjunct(ps2_1);
  ps2.add_disjunct(ps2_2);
  ps2.add_disjunct(ps2_3);
  ps2.add_disjunct(ps2_4);

#if NOISY
  using namespace Parma_Polyhedra_Library::IO_Operators;
  cout << "*** ps1 ***" << endl
       << ps1 << endl;
  cout << "*** ps2 ***" << endl
       << ps2 << endl;
#endif

  ps1.BGP99_extrapolation_assign(ps2, &Polyhedron::H79_widening_assign, 5);

#if 0
  C_Polyhedron known_result;
  known_result = ph1;

  int retval = (ph1 == known_result) ? 0 : 1;
#else
  int retval = 0;
#endif

#if NOISY
  cout << "*** ps1 ***" << endl
       << ps1 << endl;
#endif

  return retval;
}
CATCH
