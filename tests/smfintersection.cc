/* Exploit smf when computing the intersection of NNC dual hypercubes.
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

const Integer half_diagonal = 5;
const Integer shift_coeff = 3;

void
closure_points_dual_hypercube(size_t dims,
			      const LinExpression& weight_center,
			      const Integer& half_diagonal,
			      GenSys& gs) {
  // Generator system for the 2nd dual hypercube.
  for (size_t axis = dims; axis-- > 0; ) {
    gs.insert(closure_point(weight_center + half_diagonal * Variable(axis)));
    gs.insert(closure_point(weight_center - half_diagonal * Variable(axis)));
  }
}

void
add_facets(size_t &to_be_added,
	   LinExpression expr,
	   size_t dims,
	   const Integer& half_diagonal,
	   size_t axis,
	   GenSys &gs) {
  // Return if we have already added all facets.
  if (to_be_added == 0)
    return;

  if (axis == 0) {
    // Adding first a point where the 0-axis has coordinate 1/dims.
    expr += half_diagonal * Variable(axis);
    gs.insert(point(expr, dims));
    --to_be_added;
    if (to_be_added == 0)
      return;
    // Restoring previous value of `expr'.
    expr -= half_diagonal * Variable(axis);
    // Then, add a point where the 0-axis has coordinate -1/dims.
    expr -= half_diagonal * Variable(axis);
    gs.insert(point(expr, dims));
    --to_be_added;
  }
  else {
    // axis > 0.
    // First recursive call with variable with index `axis'
    // having coordinate 1/dims.
    expr += half_diagonal * Variable(axis);
    add_facets(to_be_added, expr, dims, half_diagonal, axis-1, gs);
    if (to_be_added == 0)
      return;
    // Restoring previous value of `expr'.
    expr -= half_diagonal * Variable(axis);
    // Second recursive call with variable with index `axis'
    // having coordinate -1/dims.
    expr -= half_diagonal * Variable(axis);
    add_facets(to_be_added, expr, dims, half_diagonal, axis-1, gs);
  }
}

void
NNC_dual_hypercube(size_t dims,
		   const LinExpression& weight_center,
		   const Integer& half_diagonal,
		   size_t facet_percentage,
		   NNC_Polyhedron& ph) {
  GenSys gs;
  closure_points_dual_hypercube(dims, weight_center, half_diagonal, gs);
  // Number of facets in the closed dual hypercube.
  size_t num_facets = 1;
  for (size_t axis = dims; axis-- > 0; )
    num_facets *= 2;
  size_t facets_to_be_added = (num_facets * facet_percentage) / 100;
  if (facets_to_be_added == 0)
    // There has to be a point, at least.
    gs.insert(point(weight_center));
  else
    add_facets(facets_to_be_added, weight_center,
	       dims, half_diagonal, dims-1, gs);
  // Build the polyhedron.
  ph = NNC_Polyhedron(gs);
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
  cout << " (cons: "
       << num_constraints
       << ")";
}

void
print_number_gens(NNC_Polyhedron& ph) {

  const GenSys& gs = ph.generators();
#ifndef NDEBUG
  cout << " (low-level gens: "
       << ph.num_generators()
       << ") --";
#endif
  GenSys::const_iterator i = gs.begin();
  GenSys::const_iterator gs_end = gs.end();
  dimension_type num_generators = 0;
  while (i != gs_end) {
    num_generators++;
    i++;
  }
  cout << " (gens: "
       << num_generators
       << ")";
}

void
compute_smf_gens(NNC_Polyhedron& ph) {
  cout << "    Final smf computation: ";
  start_clock();
  ph.minimized_generators();
  print_clock(cout);
}


int
smf_intersection_polyhull_test(size_t dims) {

  // Build 4 polyhedra and 4 copies of them.

  // 1st-polyhedron.
  LinExpression weight_center;
  NNC_Polyhedron ph1;
  NNC_dual_hypercube(dims, weight_center, 5, 25, ph1);
  NNC_Polyhedron ph1_copy = ph1;

  // 2nd-polyhedron.
  for (size_t axis = dims; axis-- > 0; )
    weight_center += Variable(axis);
  NNC_Polyhedron ph2;
  NNC_dual_hypercube(dims, weight_center, 4, 25, ph2);
  NNC_Polyhedron ph2_copy = ph2;

  // 3rd-polyhedron.
  for (size_t axis = dims; axis-- > 0; )
    weight_center -= 11*Variable(axis);
  NNC_Polyhedron ph3;
  NNC_dual_hypercube(dims, weight_center, 5, 25, ph3);
  NNC_Polyhedron ph3_copy = ph3;

  // 4th-polyhedron.
  for (size_t axis = dims; axis-- > 0; )
    if (axis % 2 == 0)
      weight_center += Variable(axis);
    else
      weight_center -= Variable(axis);
  NNC_Polyhedron ph4;
  NNC_dual_hypercube(dims, weight_center, 4, 25, ph4);
  NNC_Polyhedron ph4_copy = ph4;

  //-----------------------------------------------------
  // Enhanced computation.
  //-----------------------------------------------------
  cout << "  Enhanced computation: " << endl;

  start_clock();
  // Compute smf of ph2_copy constraints.
  ph2_copy.minimized_constraints();
  // Compute the intersection of ph1_copy and ph2_copy.
  ph1_copy.intersection_assign_and_minimize(ph2_copy);

  cout << " after 1st intersection ";
  print_clock(cout);
  cout << endl;

  // Compute smf of ph4_copy constraints.
  ph4_copy.minimized_constraints();
  // Compute the intersection of ph3_copy and ph4_copy.
  ph3_copy.intersection_assign_and_minimize(ph4_copy);

  cout << " after 2nd intersection (cumulative) ";
  print_clock(cout);
  cout << endl;

  // Compute smf of ph3_copy generators.
  ph3_copy.minimized_generators();
  // Compute the poly-hull of ph1_copy and ph3_copy.
  ph1_copy.poly_hull_assign_and_minimize(ph3_copy);
  print_clock(cout);

  print_number_cons(ph1_copy);
  print_number_gens(ph1_copy);
  cout << endl;

  compute_smf_gens(ph1_copy);
  print_number_gens(ph1_copy);
  cout << endl;

  //-----------------------------------------------------
  // Standard computation.
  //-----------------------------------------------------
  cout << "  Standard computation: " << endl;

  start_clock();
  // Compute the intersection of ph1 and ph2.
  ph1.intersection_assign_and_minimize(ph2);

  cout << " after 1st intersection ";
  print_clock(cout);
  cout << endl;

  // Compute the intersection of ph3 and ph4.
  ph3.intersection_assign_and_minimize(ph4);

  cout << " after 2nd intersection (cumulative) ";
  print_clock(cout);
  cout << endl;

  // Compute the poly-hull of ph1 and ph3.
  ph1.poly_hull_assign_and_minimize(ph3);
  print_clock(cout);

  print_number_cons(ph1);
  print_number_gens(ph1);
  cout << endl;

  compute_smf_gens(ph1);
  print_number_gens(ph1);
  cout << endl;

  return 0;
}

int
main() {
  return smf_intersection_polyhull_test(5);
}
