/* Compute convex-hulls of random polytopes.
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

#include "ppl.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#define NOISY 0

int
count_vertices(const Polyhedron& ph) {
  if (ph.check_empty() || ph.is_zero_dim())
    return 0;

  int count = 0;
  const GenSys& gs = ph.generators();
  for (GenSys::const_iterator i = gs.begin(), gs_end = gs.end();
       i != gs_end;
       ++i)
    if (i->type() == Generator::VERTEX)
      ++count;
  return count;
}

#if NOISY
#define COUNT(ph) cout << count_vertices(ph) << endl
#else
#define COUNT(ph) (void) count_vertices(ph)
#endif

int
main() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  const Integer maxc = 10000;

  // Polyhedra born full.
  Polyhedron ph(3);
  // We need an empty one.
  ph.insert(x <= 0);
  ph.insert(x >= 1);

  COUNT(ph);
  for (int n = 1; n <= 200; ++n) {
    ph.insert(random(maxc)*x + random(maxc)*y + random(maxc)*z /= 1);
    COUNT(ph);
  }

  return 0;
}
