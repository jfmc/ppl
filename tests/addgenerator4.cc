/* Test Polyhedron::add_generator(): we add points and rays of
   a system of generators that is not necessarily closed to a
   necessarily closed polyhedron. The generators are built using
   some operator of the class LinExpression.
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
  Variable B(1);

  GenSys gs;
  LinExpression e1 = 3*A;
  e1 += B;
  e1 -= B;
  gs.insert(closure_point(e1, 2));
  LinExpression e2 = 7*A;
  LinExpression e3 = B;
  e2 += B;
  e2 -= e3;
  gs.insert(point(e2, 4));
  LinExpression e4 = A;
  e4 -= e3;
  gs.insert(ray(e4));

#if NOISY
  print_generators(gs, "*** gs ***");
#endif

  C_Polyhedron ph(2, C_Polyhedron::EMPTY);

  for (GenSys::const_iterator i = gs.begin(),
	 iend = gs.end(); i != iend; ++i)
    if (!(*i).is_closure_point())
      ph.add_generator(*i);
  
  GenSys gs_known;
  gs_known.insert(point(7*A + 0*B, 4));
  gs_known.insert(ray(A - B));
  C_Polyhedron known_result(gs_known);

  int retval = (ph == known_result) ? 0 : 1;

#if NOISY
  print_generators(gs, "*** gs ***");
  print_generators(ph, "*** ph ***");
#endif
 
  return retval;
}
