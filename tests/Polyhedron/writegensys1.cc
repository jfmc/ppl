/* Test operator<<(std::ostream&, const Generator_System&).
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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
#include "files.hh"
#include <fstream>

using std::fstream;
using std::ios_base;

using namespace IO_Operators;

namespace {

bool
test01() {
  const char* my_file = "writegensys1.dat";
  Variable A(0);
  Variable B(1);
  Variable C(2);

  Generator_System gs;
  gs.insert(point());
  gs.insert(point(A - B));
  gs.insert(point(A + C));
  gs.insert(ray(B + C));
  gs.insert(line(C));

  fstream f;
  open(f, my_file, ios_base::out);
  f << gs << endl;
  close(f);
  // FIXME.
  return true;
}

bool
test02() {
  const char* my_file = "writegensys1.dat";
  C_Polyhedron ph(3, EMPTY);

  Generator_System gs = ph.generators();

  fstream f;
  open(f, my_file, ios_base::out);
  f << gs << endl;
  close(f);
  // FIXME.
  return true;
}

bool
test03() {
  const char* my_file = "writegensys1.dat";
  Variable A(0);
  Variable B(1);

  Linear_Expression e1 = 2*A + 4;
  e1 += B;
  Generator_System gs;
  gs.insert(ray(e1));
  gs.insert(point(3*A + B, 2));

  fstream f;
  open(f, my_file, ios_base::out);
  f << gs << endl;
  close(f);
  // FIXME.
  return true;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
END_MAIN
