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

#include "ppl_test.hh"
#include "timings.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#define NOISY 0

const Integer lb = 0;
const Integer ub = 5;

void gen_closed_hypercubes() {
  const size_t mindim = 8;
  const size_t maxdim = 11;

  cout << "=========================================" << endl
       << "Computing generators of CLOSED hypercubes" << endl
       << endl;

  for (size_t dim = mindim; dim <= maxdim; dim++) {
    ConSys cs;
    for (size_t axis = dim; axis-- > 0; ) {
      cs.insert(Variable(axis) >= lb);
      cs.insert(Variable(axis) <= ub);
    }
    NNC_Polyhedron ph(cs);
    cout << dim << "-dimensional hypercube: ";
    start_clock();
    ph.generators();
    print_clock(cout);
    cout << " secs." << endl;
#if NOISY
    print_constraints(ph, "--- ph ---");
    print_generators(ph, "--- ph ---");
#endif
  }
  cout << "=========================================" << endl
       << endl;
}


void gen_open_hypercubes() {
  const size_t mindim = 8;
  const size_t maxdim = 11;

  cout << "=========================================" << endl
       << "Computing generators of OPEN hypercubes" << endl
       << endl;

  for (size_t dim = mindim; dim <= maxdim; dim++) {
    ConSys cs;
    for (size_t axis = dim; axis-- > 0; ) {
      cs.insert(Variable(axis) > lb);
      cs.insert(Variable(axis) < ub);
    }
    NNC_Polyhedron ph(cs);

    cout << dim << "-dimensional hypercube: ";
    start_clock();
    ph.generators();
    print_clock(cout);
    cout << " secs." << endl;
#if NOISY
    print_generators(ph, "--- ph ---");
#endif
  }
  cout << "=========================================" << endl
       << endl;
}


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


void gen_nnc_hypercubes() {
  const size_t mindim = 6;
  const size_t maxdim = 9;

  cout << "=========================================" << endl
       << "Computing generators of NNC hypercubes" << endl
       << "missing some of the vertices" << endl
       << endl;

  for (size_t dim = mindim; dim <= maxdim; dim++) {
    // First build the constraint system defining
    // the closed hypercube.
    ConSys cs;
    for (size_t axis = dim; axis-- > 0; ) {
      cs.insert(Variable(axis) >= lb);
      cs.insert(Variable(axis) <= ub);
    }

    // Number of vertices in the closed hypercube.
    size_t num_vertices = 1;
    for (size_t axis = dim; axis-- > 0; )
      num_vertices *= 2;

    cout << dim << "-dimensional hypercubes (max ";
    cout << num_vertices << " vertices):" << endl;

    for (size_t quarters = 0; quarters <= 4; quarters++) {
      size_t to_be_removed = (num_vertices * quarters) / 4;

      cout << "  having "
	   << (num_vertices - to_be_removed) * 100 / num_vertices
	   << "\% of the vertices ("
	   << (num_vertices - to_be_removed)
	   << "): ";

      // Now add strict inequality constraints
      // each one removing one vertex.
      ConSys cs_copy = cs;
      remove_vertices(to_be_removed, LinExpression(), dim-1, cs_copy);
      NNC_Polyhedron ph(cs_copy);

      start_clock();
      ph.generators();
      print_clock(cout);
      cout << " secs." << endl;

#if NOISY
      ph.minimized_generators();
      print_generators(ph, "--- ph ---");
#endif
    }
  }
  cout << "=========================================" << endl
       << endl;
}


void con_dual_closed_hypercubes() {
  const size_t mindim = 8;
  const size_t maxdim = 11;

  cout << "=========================================" << endl
       << "Computing constraints for the DUAL of a CLOSED hypercube" << endl
       << endl;

  for (size_t dim = mindim; dim <= maxdim; dim++) {
    GenSys gs;
    for (size_t axis = dim; axis-- > 0; ) {
      gs.insert(point(-ub * Variable(axis)));
      gs.insert(point(ub * Variable(axis)));
    }
    NNC_Polyhedron ph(gs);
    cout << dim << "-dimensional DUAL hypercube: ";
    start_clock();
    ph.constraints();
    print_clock(cout);
    cout << " secs." << endl;
#if NOISY
    print_constraints(ph, "--- ph ---");
    print_generators(ph, "--- ph ---");
#endif
  }
  cout << "=========================================" << endl
       << endl;
}


void con_dual_open_hypercubes() {
  const size_t mindim = 8;
  const size_t maxdim = 11;

  cout << "=========================================" << endl
       << "Computing constraints for the DUAL of an OPEN hypercube" << endl
       << endl;

  for (size_t dim = mindim; dim <= maxdim; dim++) {
    GenSys gs;
    for (size_t axis = dim; axis-- > 0; ) {
      gs.insert(closure_point(-ub * Variable(axis)));
      gs.insert(closure_point(ub * Variable(axis)));
    }
    gs.insert(point());
    NNC_Polyhedron ph(gs);
    cout << dim << "-dimensional DUAL hypercube: ";
    start_clock();
    ph.constraints();
    print_clock(cout);
    cout << " secs." << endl;
#if NOISY
    print_constraints(ph, "--- ph ---");
    print_generators(ph, "--- ph ---");
#endif
  }
  cout << "=========================================" << endl
       << endl;
}


void add_facets(size_t &to_be_added,
		LinExpression expr,
		size_t dims,
		size_t axis,
		GenSys &gs) {
  // Return if we have already added all facets.
  if (to_be_added == 0)
    return;

  if (axis == 0) {
    // Adding first a point where the 0-axis has coordinate 1/dims.
    expr += Variable(axis);
    gs.insert(point(ub*expr, dims));
    --to_be_added;
    if (to_be_added == 0)
      return;
    // Restoring previous value of `expr'.
    expr -= Variable(axis);
    // Then, add a point where the 0-axis has coordinate -1/dims.
    expr -= Variable(axis);
    gs.insert(point(ub*expr, dims));
    --to_be_added;
  }
  else {
    // axis > 0.
    // First recursive call with variable with index `axis'
    // having coordinate 1/dims.
    expr += Variable(axis);
    add_facets(to_be_added, expr, dims, axis-1, gs);
    if (to_be_added == 0)
      return;
    // Restoring previous value of `expr'.
    expr -= Variable(axis);
    // Second recursive call with variable with index `axis'
    // having coordinate -1/dims.
    expr -= Variable(axis);
    add_facets(to_be_added, expr, dims, axis-1, gs);
  }
}


void con_dual_nnc_hypercubes() {
  const size_t mindim = 6;
  const size_t maxdim = 8;

  cout << "=========================================" << endl
       << "Computing constraints of duals of NNC hypercubes" << endl
       << "missing some of the facets" << endl
       << endl;

  for (size_t dim = mindim; dim <= maxdim; dim++) {
    // First build the generator system defining
    // the open dual hypercube.
    GenSys gs;
    for (size_t axis = dim; axis-- > 0; ) {
      gs.insert(closure_point(-ub * Variable(axis)));
      gs.insert(closure_point(ub * Variable(axis)));
    }

    // Number of facets in the closed dual hypercube.
    size_t num_facets = 1;
    for (size_t axis = dim; axis-- > 0; )
      num_facets *= 2;

    cout << dim << "-dimensional dual hypercubes (max ";
    cout << num_facets << " facets):" << endl;

    for (size_t quarters = 0; quarters <= 4; quarters++) {
      size_t to_be_added = (num_facets * quarters) / 4;

      cout << "  having "
	   << (to_be_added * 100) / num_facets
	   << "\% of the facets ("
	   << to_be_added
	   << "): ";

      // Now add strict inequality constraints
      // each one removing one vertex.
      GenSys gs_copy = gs;

      if (to_be_added == 0)
	// There has to be a point, at least.
	gs_copy.insert(point());
      else
	add_facets(to_be_added, LinExpression(), dim, dim-1, gs_copy);

      NNC_Polyhedron ph(gs_copy);

#if 0
      cout << "generators" << endl;
      ph.generators().ASCII_dump(cout);
#endif

      start_clock();
      ph.constraints();
      print_clock(cout);
      cout << " secs." << endl;

#if NOISY
      ph.minimized_constraints();
      cout << "Constraints" << endl;
      ph.constraints().ASCII_dump(cout);
      ph.minimized_generators();
      cout << "Generators" << endl;
      ph.generators().ASCII_dump(cout);
#endif
    }
  }
  cout << "=========================================" << endl
       << endl;
}


int
main() {
  //  gen_closed_hypercubes();
  //  gen_open_hypercubes();
  gen_nnc_hypercubes();
  //  con_dual_closed_hypercubes();
  //  con_dual_open_hypercubes();
  con_dual_nnc_hypercubes();
  return 0;
}
