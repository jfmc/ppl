/* Test Polyhedron::concatenate_assign(): we concatenate a
   two-dimensional polyhedron to a zero-dimensional, universal
   polyhedron. The resulting polyhedron is equal to the second
   polyhedron.
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
  Variable x(0);
  Variable y(1);

  C_Polyhedron ph;
#if NOISY
  print_constraints(ph, "--- ph ---");
#endif

  ConSys cs;
  cs.insert(x - 3 >= y);
  cs.insert(y >= 0);
  C_Polyhedron qh(cs);

#if NOISY
  print_constraints(qh, "--- qh ---");
#endif

  ph.concatenate_assign(qh);

  int retval = (ph == qh) ? 0 : 1;

#if NOISY
  print_constraints(ph, "--- After concatenate_assign(qh) ---");
#endif

  return retval;
}
CATCH
