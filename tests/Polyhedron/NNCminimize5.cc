/* Full minimization of a NNC-redundant constraint system
   and a NNC-redundant generator system.
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

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

int
main() TRY {
  set_handlers();

  Variable x(0);

  Constraint_System cs;
  cs.insert(x > 0);
  cs.insert(x < 2);

  NNC_Polyhedron ph1(cs);

  cs.clear();
  cs.insert(x > 2);
  cs.insert(x < 3);

  NNC_Polyhedron ph2(cs);

  ph1.poly_hull_assign_and_minimize(ph2);

#if NOISY
  cout << "(Weakly) minimized poly hull" << endl;
  print_constraints(ph1.constraints(), "*** ph1 constraints ***");
  print_generators(ph1.generators(), "*** ph1 generators ***");
#endif

  NNC_Polyhedron copy_ph1(ph1);

  int num_constraints = 0;
  for (Constraint_System::const_iterator i = ph1.constraints().begin(),
	 cs_end = ph1.constraints().end(); i != cs_end; ++i)
    ++num_constraints;

  ph1.minimized_constraints();

  int num_minimized_constraints = 0;
  for (Constraint_System::const_iterator i = ph1.constraints().begin(),
	 cs_end = ph1.constraints().end(); i != cs_end; ++i)
    ++num_minimized_constraints;

#if NOISY
  print_constraints(ph1, "*** After ph1.minimized_constraints() ***");
  cout << "num_constraints = " << num_constraints << endl;
  cout << "num_minimized_constraints = "
       << num_minimized_constraints << endl;
#endif

  int num_points = 0;
  for (Generator_System::const_iterator i = copy_ph1.generators().begin(),
	 gs_end = copy_ph1.generators().end(); i != gs_end; ++i)
    if ((*i).is_point() || (*i).is_closure_point())
      ++num_points;

  copy_ph1.minimized_generators();

  int num_minimized_points = 0;
  for (Generator_System::const_iterator i = copy_ph1.generators().begin(),
	 gs_end = copy_ph1.generators().end(); i != gs_end; ++i)
    if ((*i).is_point() || (*i).is_closure_point())
      ++num_minimized_points;

#if NOISY
  print_generators(copy_ph1,
		   "*** After copy_ph1_minimized_generators() ***");
  cout << "num_points = " << num_points << endl;
  cout << "num_minimized_points = "
       << num_minimized_points << endl;
#endif

  return (num_constraints == num_minimized_constraints + 1 &&
	  num_points == num_minimized_points + 1) ? 0 : 1;
}
CATCH
