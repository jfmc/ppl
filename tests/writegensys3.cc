/* Test operator<<(std::ostream&, const Generator_System&).
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
#include "files.hh"
#include <fstream>

using namespace std;
using namespace Parma_Polyhedra_Library;
using namespace Parma_Polyhedra_Library::IO_Operators;

#ifndef NOISY
#define NOISY 0
#endif

static const char* my_file = "writegensys3.dat";

int
main() TRY {
  set_handlers();

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

  return 0;
}
CATCH
