/* Compute the vertices of NNC hypercubes.
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
#include "ehandlers.hh"
#include "print.hh"
#include "timings.hh"
#include <iostream>

using namespace std;
using namespace Parma_Polyhedra_Library;

#define NOISY 0

int
main() {
  // set_handlers();

  const size_t maxdim = 15;
  const Integer half_side = 5;

  for (size_t dim = 1; dim <= maxdim; dim++) {

    start_clock();

    ConSys cs;
    // Inserting lower bounds.
    LinExpression tmpl = 0*Variable(dim-1);
    tmpl += half_side;
    for (size_t axis = 0; axis < dim; axis++) {
      LinExpression lb = tmpl;
      lb -= Variable(axis);
      cs.insert(lb >= 0);
    }
    // Inserting upper bounds.
    for (size_t axis = dim; axis-- > 0; ) {
      LinExpression ub = tmpl;
      ub += Variable(axis);
      cs.insert(ub >= 0);
    }
    NNC_Polyhedron ph(cs);
    ph.generators();

#if NOISY
    print_generators(ph, "--- ph ---");
#endif

    cout << "Hypercube of dimension "
	 << dim
	 << " generated after ";
    print_clock(cout);
    cout << " secs." << endl;
  }

  return 0;
}
