/* Exploit smf when computing the poly-hull of NNC hypercubes.
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
#include "timings.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

#ifndef OUTPUT_TIMINGS
#define OUTPUT_TIMINGS 1
#endif

const Integer lb = 0;
const Integer ub = 5;
const Integer shift_coeff = 3;


void remove_vertices(size_t &to_be_removed,
		     LinExpression expr,
		     size_t axis,
		     ConSys &cs) {
  // Return if we have already removed all vertices.
  if (to_be_removed == 0)
    return;

  if (axis == 0) {
    // Adding first a constraint where variable with index `axis'
    // has coefficient 1.
    expr += Variable(axis);
    expr -= ub;
    cs.insert(expr < 0);
    --to_be_removed;
    if (to_be_removed == 0)
      return;
    // Restoring previous value of `expr'.
    expr -= Variable(axis);
    expr += ub;
    // Then add a constraint where variable with index `axis'
    // has coefficient -1.
    expr -= Variable(axis);
    cs.insert(expr < 0);
    --to_be_removed;
  }
  else {
    // axis > 0.
    // First recursive call with variable with index `axis'
    // having coefficient 1.
    expr += Variable(axis);
    expr -= ub;
    remove_vertices(to_be_removed, expr, axis-1, cs);
    if (to_be_removed == 0)
      return;
    // Restoring previous value of `expr'.
    expr -= Variable(axis);
    expr += ub;
    // Second recursive call with variable with index `axis'
    // having coefficient -1.
    expr -= Variable(axis);
    remove_vertices(to_be_removed, expr, axis-1, cs);
  }
}

void
print_number_cons(NNC_Polyhedron& ph) {

  const ConSys& cs = ph.constraints();
#ifndef NDEBUG
  cout << " (low-level cons: "
       << ph.num_constraints()
       << ") --";
#endif
  ConSys::const_iterator i = cs.begin();
  ConSys::const_iterator cs_end = cs.end();
  dimension_type num_constraints = 0;
  while (i != cs_end) {
    num_constraints++;
    i++;
  }
  cout << " (high-level cons: "
       << num_constraints
       << ")";
}
  
void
compute_smf_cons(NNC_Polyhedron& ph) {
  cout << " smf ";
  start_clock();
  ph.minimized_constraints();
  print_clock(cout);
}

int
smf_poly_hull_test() {
  size_t mindim = 4;
  size_t maxdim = 6;

  for (size_t dim = mindim; dim <= maxdim; dim++) {

    // Number of vertices in the closed hypercube.
    size_t num_vertices = 1;
    for (size_t axis = dim; axis-- > 0; )
      num_vertices *= 2;

#if OUTPUT_TIMINGS
    cout << "============================" << endl;
    cout << dim << "-dimensional hypercubes (max ";
    cout << num_vertices << " vertices):" << endl;
#endif

    // Constraint system for the 1st hypercube.
    ConSys cs1;
    for (size_t axis = dim; axis-- > 0; ) {
      cs1.insert(Variable(axis) >= lb);
      cs1.insert(Variable(axis) <= ub);
    }

    // Constraint system for the 2nd hypercube.
    ConSys cs2;
    for (size_t axis = dim; axis-- > 0; ) {
      cs2.insert(Variable(axis) >= lb + shift_coeff);
      cs2.insert(Variable(axis) <= ub + shift_coeff);
    }

    for (size_t quarters = 0; quarters <= 4; quarters++) {

      size_t to_be_removed1 = (num_vertices * quarters) / 4;
      size_t to_be_removed2 = to_be_removed1;

#if OUTPUT_TIMINGS
      cout << "  "
	   << (num_vertices - to_be_removed1) * 100 / num_vertices
	   << "\% of the vertices ("
	   << (num_vertices - to_be_removed1)
	   << "):"
	   << endl;
#endif

      // Now add a strict inequality constraint for each
      // vertex that has to be removed.
      ConSys cs1_copy = cs1;
      remove_vertices(to_be_removed1,
		      LinExpression(),
		      dim - 1,
		      cs1_copy);
      // Build two copies of 1st NNC hypercube.
      NNC_Polyhedron ph1(cs1_copy);
      NNC_Polyhedron ph1_copy = ph1;

      ConSys cs2_copy = cs2;
      remove_vertices(to_be_removed2,
		      LinExpression(shift_coeff),
		      dim - 1,
		      cs2_copy);
      // Build two copies of 2nd NNC hypercube.
      NNC_Polyhedron ph2(cs2_copy);
      NNC_Polyhedron ph2_copy = ph2;

#if OUTPUT_TIMINGS
      cout << "    wmf ";
      start_clock();
#endif
      // Compute the poly-hull (with minimization).
      ph1.poly_hull_assign_and_minimize(ph2);
#if OUTPUT_TIMINGS
      print_clock(cout);
      print_number_cons(ph1);
      compute_smf_cons(ph1);
      print_number_cons(ph1);
      cout << endl;
#endif


#if OUTPUT_TIMINGS
      cout << "    smf ";
      start_clock();
#endif
      // Compute smf for generators of 2nd hypercube.
      ph2_copy.minimized_generators();
      // Compute the poly-hull (with minimization).
      ph1_copy.poly_hull_assign_and_minimize(ph2_copy);
#if OUTPUT_TIMINGS
      print_clock(cout);     
      print_number_cons(ph1_copy);
      compute_smf_cons(ph1_copy);
      print_number_cons(ph1);
      cout << endl;
#endif

      if (ph1 != ph1_copy)
	return -1;

#if NOISY
      ph.minimized_constraints();
      cout << "Constraints" << endl;
      ph.constraints().ASCII_dump(cout);
      ph.minimized_generators();
      cout << "Generators" << endl;
      ph.generators().ASCII_dump(cout);
#endif
    }
#if OUTPUT_TIMINGS
    cout << "============================" << endl;
#endif
  }
  return 0;
}


int
main() {
  return smf_poly_hull_test();
}
