/* Test Polyhedron::operator<=().
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

  Variable x(0);
  Variable y(1);

  C_Polyhedron segment(2);
  segment.add_constraint(x >= 0);
  segment.add_constraint(x <= 1);
  segment.add_constraint(y == 0);

#if NOISY
  print_constraints(segment, "*** segment constraints ***");
  print_generators(segment, "*** segment generators ***");
#endif

  C_Polyhedron halfline(2);
  halfline.add_constraint(x >= 0);
  halfline.add_constraint(y == 0);

#if NOISY
  print_constraints(halfline, "*** halfline constraints ***");
  print_generators(halfline, "*** halfline generators ***");
#endif

  bool segment_includes_halfline = (segment >= halfline);

#if NOISY
  cout << "segment ";
  if (segment_includes_halfline)
    cout << "includes ";
  else
    cout << "does not include ";
  cout << "or is equal to halfline" << endl;
#endif

  bool halfline_includes_segment = (halfline >= segment);

#if NOISY
  cout << "halfline ";
  if (halfline_includes_segment)
    cout << "includes ";
  else
    cout << "does not include ";
  cout << "or is equal to segment" << endl;
#endif

  return (halfline_includes_segment && !segment_includes_halfline) ? 0 : 1;
}
