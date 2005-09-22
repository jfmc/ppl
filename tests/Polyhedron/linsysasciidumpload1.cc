/* Test Linear_System::ascii_dump() and Linear_System::ascii_load().
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
#include "files.hh"
#include <fstream>
#include <cstdlib>

#include <string> // FIX

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

namespace {

const char* data_file = "linsysasciidumpload1.dat";

} // namespace

int
main() TRY {
  set_handlers();
  Variable A(0);
  Variable B(1);

  // Seed the random number generator.
  srand(time(0));

#define ROWS 7
#define COLS 3

  Linear_System ls1(NOT_NECESSARILY_CLOSED);
  TEMP_INTEGER(tem);
  for (dimension_type rowi = 0; rowi < ROWS; ++rowi){
    Linear_Row row(COLS,
		   Linear_Row::Flags(NOT_NECESSARILY_CLOSED,
				     Linear_Row::RAY_OR_POINT_OR_INEQUALITY));
    for (dimension_type col = 0; col < COLS; ++col) {
#if defined(NATIVE_INTEGERS) || defined(CHECKED_INTEGERS)
      if (std::numeric_limits<COEFFICIENT_TYPE>::max() == 0)
	tem = 0;
      else
	tem = rand() % std::numeric_limits<COEFFICIENT_TYPE>::max();
#else
      tem = rand();
#endif
      row[col] = tem;
    }
    row.strong_normalize();
    ls1.insert(row);
  }

  fstream f;
  open(f, data_file, ios_base::out);
  ls1.ascii_dump(f);
  close(f);

  open(f, data_file, ios_base::in);
  Linear_System ls2(NECESSARILY_CLOSED);
  ls2.ascii_load(f);
  close(f);

  int retval = (ls1 == ls2) ? 0 : 1;

  return retval;
}
CATCH
