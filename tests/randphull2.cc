/* Compute random polytopes by generating points on the surface
   of an n-dimensional sphere.
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
#include <vector>
#include <cmath>

using namespace std;
using namespace Parma_Polyhedra_Library;
using namespace Parma_Polyhedra_Library::IO_Operators;

#ifndef NOISY
#define NOISY 0
#endif

#ifndef M_PI
# define M_PI           3.14159265358979323846  /* pi */
#endif

// Please note: the following function picks up random point on the
// n-dimensional sphere, but they are not uniformly distributed.
// See the following paper on how to obtain a uniform distribution:

// J. S. Hicks, R. F. Wheeling.
// An efficient method for generating uniformly distributed points
// on the surface of an n-dimensional sphere,
// Communications of the ACM, Volume 2, Issue 4, pp. 17-19, April 1959.
//
// M. E. Muller.
// A note on a method for generating points uniformly on n-dimensional spheres,
// Communications of the ACM, Volume 2 Issue 4, pp. 19-20, April 1959.
//
// G. F. Schrack.
// Remark on algorithm 381 [G5],
// Communications of the ACM, Volume 15 Issue 6, p. 468, June 1972.

static void
point_on_the_unit_n_sphere(unsigned n,
			   const vector<float>& theta,
			   vector<float>& coordinate) {
  assert(n >= 2);
  if (n == 2) {
    coordinate[0] *= sin(theta[0]);
    coordinate[1] *= cos(theta[0]);
  }
  else {
    point_on_the_unit_n_sphere(n-1, theta, coordinate);
    float sin_theta_n_2 = sin(theta[n-2]);
    for (unsigned i = n-1; i-- > 0; )
      coordinate[i] *= sin_theta_n_2;
    coordinate[n-1] *= cos(theta[n-2]);
  }
}

static void
random_polytope(C_Polyhedron& ph, unsigned dimension, unsigned num_points,
		float radius = 1.0) {
  if (dimension < 2)
    abort();

  vector<float> theta(dimension-1);
  vector<float> coordinate(dimension);

  for (unsigned n = num_points; n > 0; --n) {
    // Compute n-1 random angles.
    for (unsigned i = dimension-1; i-- > 0; )
      theta[i] = 2.0*M_PI*static_cast<double>(rand())/RAND_MAX;
    // Compute float coordinates.
    for (unsigned i = dimension; i-- > 0; )
      coordinate[i] = radius;
    point_on_the_unit_n_sphere(dimension, theta, coordinate);

    LinExpression le;
    for (unsigned i = dimension; i-- > 0; )
      le += Variable(i)*Integer(coordinate[i]*1000000.0);
    ph.add_generator(point(le));
  }
}


int
main() {
  set_handlers();

  for (int dimension = 2; dimension <= 6; ++dimension) {
    C_Polyhedron ph(dimension, C_Polyhedron::EMPTY);
    random_polytope(ph, dimension, dimension*dimension);
    const ConSys& cs = ph.constraints();
    unsigned num_constraints = 0;
    for (ConSys::const_iterator i = cs.begin(), cs_end = cs.end();
	 i != cs_end;
	 ++i)
      ++num_constraints;
    const GenSys& gs = ph.generators();
    unsigned num_points = 0;
    for (GenSys::const_iterator i = gs.begin(), gs_end = gs.end();
	 i != gs_end;
	 ++i) {
      if (i->type() != Generator::POINT)
	exit(1);
      ++num_points;
    }

#if NOISY
    cout << "dimension = " << dimension
	 << ", points = " << num_points << " (" << dimension*dimension << ")"
	 << ", constraints = " << num_constraints << endl;
#endif
  }
  return 0;
}
