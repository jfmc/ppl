/* Reproduce Mantis issue [PPL 0000353].
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2012 BUGSENG srl (http://bugseng.com)

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://bugseng.com/products/ppl/ . */

#include "ppl_test.hh"

#include "files.hh"
#include <fstream>
#include <iostream>

using namespace IO_Operators;

using std::fstream;
using std::ios_base;

namespace {

bool
test01() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);
  Variable F(5);
  Variable G(6);
  Variable H(7);
  Variable I(8);
  Variable J(9);
  Variable K(10);
  Variable L(11);
  Variable M(12);
  Variable N(13);
  Variable O(14);
  Variable P(15);
  Variable Q(16);
  Variable R(17);
  Variable S(18);
  Variable T(19);
  Variable U(20);
  Variable V(21);
  Variable W(22);
  Variable X(23);
  Variable Y(24);
  Variable Z(25);
  Variable A1(26);
  Variable B1(27);
  Variable C1(28);
  Variable D1(29);
  Variable E1(30);
  Variable F1(31);
  Variables_Set params;

  Constraint_System cs;
  cs.insert(162*B + D + 9*F + 1620*H - F1 == 0);
  cs.insert(E1 == 0);
  cs.insert(D1 == 1);
  cs.insert(162*B + D + 9*F + 1620*H - C1 == 0);
  cs.insert(B1 == 0);
  cs.insert(A1 == 1);
  cs.insert(162*B + D + 9*F + 1620*H - 162*O - Q - 9*S - 1620*Z == 0);
  cs.insert(O - Y == 0);
  cs.insert(S - X == 0);
  cs.insert(Q - W == 0);
  cs.insert(V == 0);
  cs.insert(162*B + D + 9*F + 1620*H - 162*O - Q - 9*S - 1620*U == 0);
  cs.insert(T == 0);
  cs.insert(R == 0);
  cs.insert(P == 0);
  cs.insert(N == 0);
  cs.insert(H - M == 0);
  cs.insert(B - L == 0);
  cs.insert(F - K == 0);
  cs.insert(D - J == 0);
  cs.insert(I == 0);
  cs.insert(G == 0);
  cs.insert(E == 0);
  cs.insert(C == 0);
  cs.insert(A == 0);
  cs.insert(B - O >= 1);
  cs.insert(D >= 0);
  cs.insert(-162*B - D - 9*F - 1620*H + 162*O + Q + 9*S >= -6480);
  cs.insert(-B >= -9);
  cs.insert(-D >= -8);
  cs.insert(-F >= -17);
  cs.insert(F >= 0);
  cs.insert(-Q >= -8);
  cs.insert(-S >= -17);
  cs.insert(S >= 0);
  cs.insert(Q >= 0);
  cs.insert(O >= 0);
  cs.insert(H >= 0);

#if 0
  MIP_Problem mip(cs.space_dimension(), cs.begin(), cs.end());
  std::cout << mip.is_satisfiable() << endl;
  Variables_Set integer_space_dimensions(A, F1);
  mip.add_to_integer_space_dimensions(integer_space_dimensions);
  std::cout << mip.is_satisfiable() << endl;
#endif

  PIP_Problem pip(cs.space_dimension(), cs.begin(), cs.end(), params);

  bool ok = pip.is_satisfiable();

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
END_MAIN
