/* FIXME: to be written.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

bool
test01() {
  Variable x(0);
  Variable y(1);

  Grid p(2);
  p.add_congruence(x %= 0);

  using namespace IO_Operators;
  nout << "p = " << p << endl;

  Grid q(2);
  q.add_congruence((x %= 4) / 9);

  nout << "q = " << q << endl;

  bool finite_partition;

  std::pair<Grid, Pointset_Powerset<Grid> >
    result = approximate_partition(p, q, finite_partition);

  nout << "*** q partition ***" << endl;
  nout << "  === p inters q === " << endl << "  " << result.first << endl;
  nout << "  ===    rest    === " << endl << "  " << result.second << endl;

#if 0
  if (!aux_test03(p, q, result))
    return false;
#endif

  if (!finite_partition)
    return false;

  result = approximate_partition(q, p, finite_partition);

  nout << "*** p partition ***" << endl;
  nout << "  === q inters p === " << endl << "  " << result.first << endl;
  nout << "  ===    rest    === " << endl << "  " << result.second << endl;

#if 0
  return aux_test03(q, p, result);
#endif

  return finite_partition;
}

BEGIN_MAIN
  DO_TEST(test01);
END_MAIN
