/* Test time_elapse_assign() for particular polyhedra.
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
  TBD_Shape oc1(2, EMPTY);
  TBD_Shape oc2(2);
#if NOISY
  print_constraints(oc1, "**** oc1 ****");
  print_constraints(oc2, "**** oc2 ****");
#endif
  oc1.time_elapse_assign(oc2);

  TBD_Shape oc3(2);
  TBD_Shape oc4(2, EMPTY);
#if NOISY
  print_constraints(oc3, "**** oc3 ****");
  print_constraints(oc4, "**** oc4 ****");
#endif
  oc3.time_elapse_assign(oc4);

  int retval = (oc1.is_empty()
		&& oc3.is_empty()) ? 0 : 1;

#if NOISY
  print_constraints(oc1, "**** oc1_time_elapse_assign(oc2) ****");
  print_constraints(oc3, "**** oc3_time_elapse_assign(oc4) ****");
#endif

  return retval;
}
CATCH
