/* Test BD_Shape::ascii_dump() and BD_Shape::ascii_load().
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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
#include "files.hh"
#include <fstream>

static const char* my_file = "ascii_dump_load1.dat";

int
main() TRY {
  Variable A(0);
  Variable B(1);

  TBD_Shape bd1(3);
  bd1.add_constraint(A - B >= 2);
  bd1.add_constraint(B >= 0);


  fstream f;
  open(f, my_file, ios_base::out);
  bd1.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in);
  TBD_Shape bd2;
  bd2.ascii_load(f);
  close(f);

  int retval = (bd1 == bd2) ? 0 : 1;

  return retval;
}
CATCH
