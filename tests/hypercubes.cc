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
	   size_t axis,
	   const LinExpression& expr,
	   size_t dims,
	   const LinExpression& weight_center,
	   const Integer& half_diagonal,
	   GenSys &gs) {
  // Return if we have already added all facets.
  if (to_be_added == 0)
    return;

  LinExpression expr1 = expr;
  expr1 += half_diagonal * Variable(axis);
  LinExpression expr2 = expr;
  expr2 -= half_diagonal * Variable(axis);

  if (axis == 0) {
    gs.insert(point(dims * weight_center + expr1, dims));
    --to_be_added;
    if (to_be_added == 0)
      return;
    gs.insert(point(dims * weight_center + expr2, dims));
    --to_be_added;
  }
  else {
    // axis > 0.
    // First recursive call with variable with index `axis'
    // having coordinate 1/dims.
    add_facets(to_be_added, axis-1, expr1,
	       dims, weight_center, half_diagonal, gs);
    if (to_be_added == 0)
      return;
    // Second recursive call with variable with index `axis'
    // having coordinate -1/dims.
    add_facets(to_be_added, axis-1, expr2,
	       dims, weight_center, half_diagonal, gs);
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
    add_facets(facets_to_be_added, dims-1, LinExpression(0),
	       dims, weight_center, half_diagonal, gs);
  // Build the polyhedron.
  ph = NNC_Polyhedron(gs);
}

size_t
get_number_cons(NNC_Polyhedron& ph) {
  const ConSys& cs = ph.constraints();
  /*
  ConSys::const_iterator i = cs.begin();
  ConSys::const_iterator cs_end = cs.end();
  dimension_type num_constraints = 0;
  while (i != cs_end) {
    num_constraints++;
    i++;
  }
  return num_constraints;
  */
  return ph.num_cons();
}

size_t
get_number_gens(NNC_Polyhedron& ph) {
  const GenSys& gs = ph.generators();
  /*
  GenSys::const_iterator i = gs.begin();
  GenSys::const_iterator gs_end = gs.end();
  dimension_type num_generators = 0;
  while (i != gs_end) {
    num_generators++;
    i++;
  }
  return num_generators;
  */
  return ph.num_gens();
}

void
compute_smf_gens(NNC_Polyhedron& ph) {
  cout << "    Final smf computation: ";
  start_clock();
  ph.minimized_generators();
  print_clock(cout);
}

int
smf_intersection_polyhull_test(size_t dims, size_t perc) {

  // Build 4 polyhedra and 8 copies of them.

  // 1st-polyhedron.
  LinExpression weight_center;
  for (size_t axis = dims; axis-- > 0; )
    weight_center += Variable(axis);
  NNC_Polyhedron ph1;
  NNC_dual_hypercube(dims, weight_center, 5, perc, ph1);
  NNC_Polyhedron ph1_enh = ph1;
  NNC_Polyhedron ph1_semi = ph1;

  // 2nd-polyhedron.
  weight_center = LinExpression(0);
  for (size_t axis = dims; axis-- > 0; )
    weight_center += 2*Variable(axis);
  NNC_Polyhedron ph2;
  NNC_dual_hypercube(dims, weight_center, 4, perc, ph2);
  NNC_Polyhedron ph2_enh = ph2;
  NNC_Polyhedron ph2_semi = ph2;

  // 3rd-polyhedron.
  weight_center = LinExpression(0);
  for (size_t axis = dims; axis-- > 0; )
    if (axis % 2 == 0)
      weight_center += 10*Variable(axis);
    else
      weight_center += 2*Variable(axis);      
  NNC_Polyhedron ph3;
  NNC_dual_hypercube(dims, weight_center, 5, perc, ph3);
  NNC_Polyhedron ph3_enh = ph3;
  NNC_Polyhedron ph3_semi = ph3;

  // 4th-polyhedron.
  weight_center = LinExpression(0);
  for (size_t axis = dims; axis-- > 0; )
    if (axis % 2 == 0)
      weight_center += 10*Variable(axis);
    else
      weight_center += Variable(axis);
  NNC_Polyhedron ph4;
  NNC_dual_hypercube(dims, weight_center, 4, perc, ph4);
  NNC_Polyhedron ph4_enh = ph4;
  NNC_Polyhedron ph4_semi = ph4;

  cout << "Working with 4 NNC dual-hypercubes," << endl
       << "each one having " << get_number_gens(ph1) << " gens" << endl
       << endl;

  //-----------------------------------------------------
  // Standard computation.
  //-----------------------------------------------------
  cout << "=================================" << endl;
  cout << "  Standard computation: " << endl;

  // Compute the intersection of ph1 and ph2.
  start_clock();
#if NOISY
  // Print dimensions of arguments.
  cout << "Computing intersection of ph1 and ph2:" << endl;

  /*****
  cout << "ph1 wmf cons" << endl;
  ph1.constraints().ascii_dump(cout);
  cout << endl;

  cout << "ph2 wmf cons" << endl;
  ph2.constraints().ascii_dump(cout);
  cout << endl;
  ******/

  cout << "  ph1 wmf cons: " << get_number_cons(ph1) << endl;
  cout << "  ph2 wmf cons: " << get_number_cons(ph2) << endl;
#endif
  ph1.intersection_assign_and_minimize(ph2);

  // Compute the intersection of ph3 and ph4.
#if NOISY
  // Print dimensions of arguments.
  cout << "Computing intersection of ph3 and ph4:" << endl; 
  cout << "  ph3 wmf cons: " << get_number_cons(ph3) << endl;
  cout << "  ph4 wmf cons: " << get_number_cons(ph4) << endl;
#endif
  ph3.intersection_assign_and_minimize(ph4);
  //  cout << "After the two intersections: ";
  //  print_clock(cout);
  //  cout << endl;

  // Compute the poly-hull of ph1 and ph3.
  //  start_clock();
#if NOISY
  // Print dimensions of arguments.
  cout << "Computing poly-hull of ph1 and ph3:" << endl; 
  cout << "  ph1 wmf gens: " << get_number_gens(ph1) << endl;
  cout << "  ph3 wmf gens: " << get_number_gens(ph3) << endl;
#endif
  ph1.poly_hull_assign_and_minimize(ph3);
  cout << "After poly-hull: ";
  print_clock(cout);
  cout << endl;

  // How many constraints obtained?
  cout << "  Final result wmf cons: " << get_number_cons(ph1) << endl;

  // How many constraints obtained?
  cout << "  Final result smf cons (time ";
  start_clock();
  ph1.minimized_constraints();
  print_clock(cout);
  cout << "): " << get_number_cons(ph1) << endl;
  cout << endl;

  //-----------------------------------------------------
  // Enhanced computation.
  //-----------------------------------------------------
  cout << "=================================" << endl;
  cout << endl << "  Enhanced computation: " << endl;

  // Compute the intersection of ph1_enh and ph2_enh.
  start_clock();
  ph1_enh.minimized_constraints();
  ph2_enh.minimized_constraints();
#if NOISY
  // Print dimensions of arguments.
  cout << "Computing intersection of ph1 and ph2:" << endl; 
  cout << "  ph1 smf cons: " << get_number_cons(ph1_enh) << endl;
  cout << "  ph2 smf cons: " << get_number_cons(ph2_enh) << endl;
#endif
  ph1_enh.intersection_assign_and_minimize(ph2_enh);

  // Compute the intersection of ph3_enh and ph4_enh.
  ph3_enh.minimized_constraints();
  ph4_enh.minimized_constraints();
#if NOISY
  // Print dimensions of arguments.
  cout << "Computing intersection of ph3 and ph4:" << endl; 
  cout << "  ph3 smf cons: " << get_number_cons(ph3_enh) << endl;
  cout << "  ph4 smf cons: " << get_number_cons(ph4_enh) << endl;
#endif
  ph3_enh.intersection_assign_and_minimize(ph4_enh);
  //  cout << "After the two intersections: ";
  //  print_clock(cout);
  //  cout << endl;

  // Compute the poly-hull of ph1_enh and ph3_enh.
  //  start_clock();
  ph1_enh.minimized_generators();
  ph3_enh.minimized_generators();
#if NOISY
  // Print dimensions of arguments.
  cout << "Computing poly-hull of ph1 and ph3:" << endl; 
  cout << "  ph1 smf gens: " << get_number_gens(ph1_enh) << endl;
  cout << "  ph3 smf gens: " << get_number_gens(ph3_enh) << endl;
#endif
  ph1_enh.poly_hull_assign_and_minimize(ph3_enh);
  cout << "After poly-hull: ";
  print_clock(cout);
  cout << endl;

  // How many constraints obtained?
  cout << "  Final result wmf cons: " << get_number_cons(ph1_enh) << endl;

  // How many constraints obtained?
  cout << "  Final result smf cons (time ";
  start_clock();
  ph1_enh.minimized_constraints();
  print_clock(cout);
  cout << "): " << get_number_cons(ph1_enh) << endl;
  cout << endl;

  //-----------------------------------------------------
  // Semi-enhanced computation.
  //-----------------------------------------------------
  cout << "=================================" << endl;
  cout << endl << "  Semi-enhanced computation: " << endl;

  // Compute the intersection of ph1_semi and ph2_semi.
  start_clock();
  ph2_semi.minimized_constraints();
#if NOISY
  // Print dimensions of arguments.
  cout << "Computing intersection of ph1 and ph2:" << endl; 
  cout << "  ph1 wmf cons: " << get_number_cons(ph1_semi) << endl;
  cout << "  ph2 smf cons: " << get_number_cons(ph2_semi) << endl;
#endif
  ph1_semi.intersection_assign_and_minimize(ph2_semi);

  // Compute the intersection of ph3_semi and ph4_semi.
  ph4_semi.minimized_constraints();
#if NOISY
  // Print dimensions of arguments.
  cout << "Computing intersection of ph3 and ph4:" << endl; 
  cout << "  ph3 wmf cons: " << get_number_cons(ph3_semi) << endl;
  cout << "  ph4 smf cons: " << get_number_cons(ph4_semi) << endl;
#endif
  ph3_semi.intersection_assign_and_minimize(ph4_semi);
  //  cout << "After the two intersections: ";
  //  print_clock(cout);
  //  cout << endl;

  // Compute the poly-hull of ph1_semi and ph3_semi.
  //  start_clock();
  ph3_semi.minimized_generators();
#if NOISY
  // Print dimensions of arguments.
  cout << "Computing poly-hull of ph1 and ph3:" << endl; 
  cout << "  ph1 wmf gens: " << get_number_gens(ph1_semi) << endl;
  cout << "  ph3 smf gens: " << get_number_gens(ph3_semi) << endl;
#endif
  ph1_semi.poly_hull_assign_and_minimize(ph3_semi);
  cout << "After poly-hull: ";
  print_clock(cout);
  cout << endl;

  // How many constraints obtained?
  cout << "  Final result wmf cons: " << get_number_cons(ph1_semi) << endl;

  // How many constraints obtained?
  cout << "  Final result smf cons (time ";
  start_clock();
  ph1_semi.minimized_constraints();
  print_clock(cout);
  cout << "): " << get_number_cons(ph1_semi) << endl;
  cout << endl;

  return 0;
}

int
main() {
  for (size_t dims = 4; dims <= 5; dims++)
    for (size_t perc = 25; perc <= 50; perc += 25) {
      cout << endl
	   << "++++++++ DIM = " << dims << "  ++++++++"
	   << endl
	   << "++++++++ PERC = " << perc << " ++++++++"
	   << endl;
      smf_intersection_polyhull_test(dims, perc);
    }
  return 0;
}
