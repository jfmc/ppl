/* Exploit smf when computing the intersection of NNC dual hypercubes.
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
#include "timings.hh"
#include <vector>

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

#ifndef VERY_NOISY
#define VERY_NOISY 0
#endif

#ifndef EXP_EVAL
#define EXP_EVAL 0
#endif

namespace {

void
closure_points_dual_hypercube(const dimension_type dims,
			      const Linear_Expression& weight_center,
			      const Integer& half_diagonal,
			      Generator_System& gs) {
  // An ill-formed (it has no points at all) generator system
  // for a dual hypercube.
  for (dimension_type axis = dims; axis-- > 0; ) {
    gs.insert(closure_point(weight_center + half_diagonal * Variable(axis)));
    gs.insert(closure_point(weight_center - half_diagonal * Variable(axis)));
  }
}

void
add_facets(dimension_type& to_be_added,
	   Generator_System& gs,
	   const Linear_Expression& expr,
	   const dimension_type axis,
	   const dimension_type dims,
	   const Linear_Expression& weight_center,
	   const Integer& half_diagonal) {
  // Return if we have already added all facets.
  if (to_be_added == 0)
    return;

  Linear_Expression expr1 = expr;
  expr1 += half_diagonal * Variable(axis);
  Linear_Expression expr2 = expr;
  expr2 -= half_diagonal * Variable(axis);

  if (axis == 0) {
    gs.insert(point(dims * weight_center + expr1, dims));
    --to_be_added;
    if (to_be_added == 0)
      return;
    gs.insert(point(dims * weight_center + expr2, dims));
    --to_be_added;
    return;
  }

  // Here axis > 0.
  // First recursive call with variable with index `axis'
  // having coordinate 1/dims.
  add_facets(to_be_added, gs, expr1,
	     axis-1, dims, weight_center, half_diagonal);
  if (to_be_added == 0)
    return;
  // Second recursive call with variable with index `axis'
  // having coordinate -1/dims.
  add_facets(to_be_added, gs, expr2,
	     axis-1, dims, weight_center, half_diagonal);
}

NNC_Polyhedron
NNC_dual_hypercube(const dimension_type dims,
		   const Linear_Expression& weight_center,
		   const Integer& half_diagonal,
		   const int facet_percentage) {
  Generator_System gs;
  closure_points_dual_hypercube(dims, weight_center, half_diagonal, gs);
  // Number of facets in the closed dual hypercube.
  dimension_type num_facets = 1;
  for (dimension_type axis = dims; axis-- > 0; )
    num_facets *= 2;
  dimension_type facets_to_be_added = (num_facets * facet_percentage) / 100;
  if (facets_to_be_added == 0)
    // There has to be a point, at least.
    gs.insert(point(weight_center));
  else
    add_facets(facets_to_be_added, gs, Linear_Expression(0),
	       dims-1, dims, weight_center, half_diagonal);
  // Actually build the polyhedron.
  return NNC_Polyhedron(gs);
}

void
build_polyhedra(const dimension_type dims,
		const int perc,
		vector<NNC_Polyhedron>& ph) {

  Linear_Expression weight_center;

  // 1st-polyhedron.
  weight_center = Linear_Expression(0);
  for (dimension_type axis = dims; axis-- > 0; )
    weight_center += Variable(axis);
  ph.push_back(NNC_dual_hypercube(dims, weight_center, 5, perc));

  // 2nd-polyhedron.
  weight_center = Linear_Expression(0);
  for (dimension_type axis = dims; axis-- > 0; )
    weight_center += 2*Variable(axis);
  ph.push_back(NNC_dual_hypercube(dims, weight_center, 4, perc));

  // 3rd-polyhedron.
  weight_center = Linear_Expression(0);
  for (dimension_type axis = dims; axis-- > 0; )
    if (axis % 2 == 0)
      weight_center += 10*Variable(axis);
    else
      weight_center += 2*Variable(axis);      
  ph.push_back(NNC_dual_hypercube(dims, weight_center, 5, perc));

  // 4th-polyhedron.
  weight_center = Linear_Expression(0);
  for (dimension_type axis = dims; axis-- > 0; )
    if (axis % 2 == 0)
      weight_center += 10*Variable(axis);
    else
      weight_center += Variable(axis);
  ph.push_back(NNC_dual_hypercube(dims, weight_center, 4, perc));
}

void
computation(vector<NNC_Polyhedron>& ph, bool enhanced) {

#if NOISY
  cout << endl;
  if (enhanced)
    cout << "Enhanced computation: ";
  else
    cout << "Standard computation: ";
  cout << "working with 4 NNC dual hypercubes of dimension "
       << ph[0].space_dimension() << endl;
  start_clock();
#endif

  // Compute the intersection of ph[0] and ph[1].
#if VERY_NOISY
  // Print dimensions of arguments
  // (being careful to override library laziness).
  cout << "Computing intersection of ph[0] and ph[1]:" << endl; 
  cout << "===  ph[0] generators ===" << endl;
  ph[0].generators().ascii_dump(cout);
  cout << "===  ph[1] generators ===" << endl;
  ph[1].generators().ascii_dump(cout);
#endif
  if (enhanced) {
    ph[0].minimized_constraints();
    ph[1].minimized_constraints();
#if VERY_NOISY
    // Print dimensions of arguments.
    cout << "After the computation of smf for constraints" << endl;
#endif
  }
#if VERY_NOISY
  cout << "===  ph[0] constraints ===" << endl;
  ph[0].constraints().ascii_dump(cout);
  cout << "===  ph[1] constraints ===" << endl;
  ph[1].constraints().ascii_dump(cout);
  cout << endl;
#endif
  ph[0].intersection_assign(ph[1]);

  // Compute the intersection of ph[2] and ph[3].
#if VERY_NOISY
  // Print dimensions of arguments
  // (being careful to override library laziness).
  cout << "Computing intersection of ph[2] and ph[3]:" << endl; 
  cout << "===  ph[2] generators ===" << endl;
  ph[2].generators().ascii_dump(cout);
  cout << "===  ph[3] generators ===" << endl;
  ph[3].generators().ascii_dump(cout);
#endif
  if (enhanced) {
    ph[2].minimized_constraints();
    ph[3].minimized_constraints();
#if VERY_NOISY
    // Print dimensions of arguments.
    cout << "After the computation of smf for constraints" << endl;
#endif
  }
#if VERY_NOISY
  cout << "===  ph[2] constraints ===" << endl;
  ph[2].constraints().ascii_dump(cout);
  cout << "===  ph[3] constraints ===" << endl;
  ph[3].constraints().ascii_dump(cout);
  cout << endl;
#endif
  ph[2].intersection_assign(ph[3]);

  // Compute the poly-hull of ph[0] and ph[2].
#if VERY_NOISY
  cout << "Computing poly-hull of ph[0] and ph[2]:" << endl; 
#endif
  if (enhanced) {
    ph[0].minimized_generators();
    ph[2].minimized_generators();
#if VERY_NOISY
    // Print dimensions of arguments.
    cout << "After the computation of smf for generators" << endl;
#endif
  }
#if VERY_NOISY
  // Print dimensions of arguments
  // (being careful to override library laziness).
  cout << "===  ph[0] generators ===" << endl;
  ph[0].generators().ascii_dump(cout);
  cout << "===  ph[2] generators ===" << endl;
  ph[2].generators().ascii_dump(cout);
  cout << endl;
#endif
  ph[0].poly_hull_assign(ph[2]);
  ph[0].constraints();
#if NOISY
  cout << "Wmf final result timing: ";
  print_clock(cout);
  cout << endl;
#endif

#if VERY_NOISY
  // How many constraints and generators obtained?
  cout << "Final result (wmf)" << endl;
  cout << "===  ph[0] constraints ===" << endl;
  ph[0].constraints().ascii_dump(cout);
  cout << endl;
#endif

#if NOISY
  cout << "Smf (cons) final result timing: ";
  start_clock();
  ph[0].minimized_constraints();
  print_clock(cout);
  cout << endl;
#endif
#if VERY_NOISY
  // How many constraints and generators obtained?
  cout << "Final result (smf cons)" << endl;
  cout << "===  ph[0] constraints ===" << endl;
  ph[0].constraints().ascii_dump(cout);
#endif
}

} // namespace

int
main() TRY {
  vector<NNC_Polyhedron> ph;

#if EXP_EVAL
  dimension_type first_dim = 4;
  dimension_type last_dim = 5;
#else
  dimension_type first_dim = 2;
  dimension_type last_dim = 4;
#endif

  for (dimension_type dims = first_dim; dims <= last_dim; dims++)
    for (int perc = 25; perc <= 50; perc += 25) {
#if NOISY
      cout << endl
	   << "++++++++ DIM = " << dims << "  ++++++++"
	   << endl
	   << "++++++++ PERC = " << perc << " ++++++++"
	   << endl;
#endif
      // Standard evaluation strategy.
      ph.clear();
      build_polyhedra(dims, perc, ph);
      computation(ph, false);

      // Enhanced evaluation strategy.
      ph.clear();
      build_polyhedra(dims, perc, ph);
      computation(ph, true);
    }

  return 0;
}
CATCH
