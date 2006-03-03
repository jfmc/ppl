/* Test some functionality of class Linear_System.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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

namespace {

bool
test01() {
  const char* data_file = "linearsystem1.dat";

  Variable A(0);
  Variable B(1);

  Random_Number_Generator rng;

#define ROWS 7
#define COLS 3

  Linear_System ls1(NOT_NECESSARILY_CLOSED);
  TEMP_INTEGER(tem);
  for (dimension_type rowi = 0; rowi < ROWS; ++rowi) {
    Linear_Row row(COLS,
		   Linear_Row::Flags(NOT_NECESSARILY_CLOSED,
				     Linear_Row::RAY_OR_POINT_OR_INEQUALITY));
    for (dimension_type col = 0; col < COLS; ++col)
      rng.get(row[col], 0);
    row.strong_normalize();
    ls1.insert(row);

    using std::fstream;
    using std::ios_base;
    fstream f;
    open(f, data_file, ios_base::out);
    ls1.ascii_dump(f);
    close(f);

    open(f, data_file, ios_base::in);
    Linear_System ls2(NECESSARILY_CLOSED);
    ls2.ascii_load(f);
    close(f);

    if (ls1 == ls2)
      continue;

    nout << "Linear_System::ascii_dump/load test failed." << endl
	 << "m1.ascii_dump() gives" << endl;
    ls1.ascii_dump(nout);
    nout << "m2.ascii_dump() gives" << endl;
    ls2.ascii_dump(nout);

    return false;
  }
  return true;
}

} // namespace

BEGIN_MAIN
  NEW_TEST(test01);
END_MAIN
