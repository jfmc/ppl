/* Testing time_elapse_assign() for particular polyhedra.
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

#include "ppl_install.hh"
#include "print.hh"
#include "ehandlers.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#define NOISY 0

int
main() {
  set_handlers();

  C_Polyhedron ph1(0, C_Polyhedron::EMPTY);
  C_Polyhedron ph2;

#if NOISY
  print_constraints(ph1, "**** ph1 ****");
  print_constraints(ph2, "**** ph2 ****");
#endif
  ph1.time_elapse_assign(ph2);

  C_Polyhedron ph3(2, C_Polyhedron::EMPTY);
  C_Polyhedron ph4(2);
#if NOISY
  print_constraints(ph3, "**** ph3 ****");
  print_constraints(ph4, "**** ph4 ****");
#endif
  ph3.time_elapse_assign(ph4);

  C_Polyhedron ph5(2);
  C_Polyhedron ph6(2, C_Polyhedron::EMPTY);
#if NOISY
  print_constraints(ph5, "**** ph5 ****");
  print_constraints(ph6, "**** ph6 ****");
#endif
  ph5.time_elapse_assign(ph6);

  int retval = (ph1.check_empty()
		&& ph3.check_empty()
		&& ph5.check_empty()) ? 0 : 1;

#if NOISY
  print_generators(ph1, "**** ph1_time_elapse_assign(ph2) ****");
  print_generators(ph3, "**** ph3_time_elapse_assign(ph4) ****");
  print_generators(ph5, "**** ph5_time_elapse_assign(ph6) ****");
#endif

  return retval;
}
