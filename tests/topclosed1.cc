/* Test Polyhedron::is_topologically_closed(): every necessarily
   closed, empty and zero-dimensional polyhedra are topologically closed.
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

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

int
main() {
  set_handlers();

  Variable A(0);

  C_Polyhedron ph1(1);
  ph1.add_constraint(A >= 0);

  NNC_Polyhedron ph2(2, NNC_Polyhedron::EMPTY);
  
  NNC_Polyhedron ph3;

  bool ok
    = ph1.is_topologically_closed()
    && ph2.is_topologically_closed()
    &&  ph3.is_topologically_closed();

#if NOISY
  print_constraints(ph1, "*** ph1 ***");
  print_constraints(ph2, "*** ph2 ***");
  print_constraints(ph3, "*** ph3 ***");
#endif

  return ok ? 0 : 1;
}
