/* Test operator<<(std::ostream&, const Polyhedron&).
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
#include "files.hh"
#include <fstream>

using namespace std;
using namespace Parma_Polyhedra_Library;
using namespace Parma_Polyhedra_Library::IO_Operators;

#ifndef NOISY
#define NOISY 0
#endif

static const char* my_file = "writepolyhedron1.dat";

int
main() {
  set_handlers();
  Variable x1(0);
  Variable x2(1);
  Variable x3(2);
  Variable x4(3);

  NNC_Polyhedron ph(4);

  ph.add_constraint(     +x2-x3-x4 <= 0);
  ph.add_constraint(-  x1   +x3-x4 <  0);
  ph.add_constraint(+  x1   -x3-x4 <= 0);
  ph.add_constraint(-2*x1+x2+x3-x4 <  0);
  ph.add_constraint(           +x4 <= 1);
  ph.add_constraint(        +x3    <  1);
  ph.add_constraint(-  x1+x2+x3    <= 1);
  ph.add_constraint(        -x3    <  0);
  ph.add_constraint(-  x1          <= 0);
  ph.add_constraint(     -x2       <  0);
  ph.add_constraint(     +x2       <= 1);
  ph.add_constraint(+  x1          <  1);
  ph.add_constraint(+  x1-x2+x3+x4 <= 2);

  fstream f;
  open(f, my_file, ios_base::out);
  f << ph << endl;
  close(f);

  return 0;
}
