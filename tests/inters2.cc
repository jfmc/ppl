/* Intersection of a pyramid with an half-space of variable height.
   Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include <config.h>
#include "ppl.hh"
#include "print.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#define NOISY 1

int
count_vertices(const Polyhedron& ph) {
  int count = 0;
  const GenSys& gs = ph.generators();
  for (GenSys::const_iterator i = gs.begin(), gs_end = gs.end();
       i != gs_end;
       ++i)
    if (i->type() == Generator::VERTEX)
      ++count;
  return count;
}

int
main() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  // This is the height of the pyramid.
  const Integer pyramid_height = 16;

  // We will intersect it with the half-spaces `z <= k'
  // with k = i*(height/4) for i = -1, 0, 1, ..., 5.
  struct {
    Integer plane_height;
    int num_vertices;
  } ph_nv[]
      = { {-1*(pyramid_height/4), 0},
	  { 0*(pyramid_height/4), 4},
	  { 1*(pyramid_height/4), 8},
	  { 2*(pyramid_height/4), 8},
	  { 3*(pyramid_height/4), 8},
	  { 4*(pyramid_height/4), 1},
	  { 5*(pyramid_height/4), 0}
      };

  GenSys gs;
  gs.insert(0*x + 0*y + 0*z /= 1); 
  gs.insert(2*x + 0*y + 0*z /= 1); 
  gs.insert(0*x + 2*y + 0*z /= 1); 
  gs.insert(2*x + 2*y + 0*z /= 1); 
  gs.insert(x + y + pyramid_height*z /= 1); 
  Polyhedron pyramid(gs);

#if NOISY
    print_constraints(pyramid, "*** pyramid constraints ***");
    print_generators(pyramid, "*** pyramid generators ***");
#endif

  bool ok = false;

  for (size_t i = 0; i <= 6; ++i) {
    Polyhedron hyper_space(3);
    hyper_space.insert(z <= ph_nv[i].plane_height);

    Polyhedron computed_result = pyramid;
    computed_result.intersection_assign(hyper_space);

    if (ok
	&& count_vertices(computed_result) != ph_nv[i].num_vertices)
      ok = false;

#if NOISY
    print_constraints(hyper_space, "*** hyper_space ***");
    print_generators(computed_result, "*** computed_result ***");
#endif
  }
  return ok ? 0 : 1;
}
