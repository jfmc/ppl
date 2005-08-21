/* Test Polyhedron::ascii_dump() and Polyhedron::ascii_load().
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

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

namespace {

const char* my_file = "ascii_dump_load1.dat";

} // namespace

int
main() TRY {
  set_handlers();
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph1(3);
  ph1.add_constraint(A - B >= 2);
  ph1.add_constraint(B >= 0);

  ph1.minimized_generators();

  fstream f;
  open(f, my_file, ios_base::out);
  ph1.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in);
  C_Polyhedron ph2;
  ph2.ascii_load(f);
  close(f);

  int retval = (ph1 == ph2) ? 0 : 1;

  return retval;
}
CATCH
