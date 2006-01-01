/* Test linear_partition().
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

using namespace Parma_Polyhedra_Library::IO_Operators;

template <typename PH>
Polyhedra_Powerset<NNC_Polyhedron>
complement(const PH& ph) {
  std::pair<PH, Polyhedra_Powerset<NNC_Polyhedron> > partition
    = linear_partition(ph, PH(ph.space_dimension(), UNIVERSE));
  return partition.second;
}

int main() TRY {
  set_handlers();

  Variable x(0);
  Variable y(1);

  C_Polyhedron p(2, EMPTY);
  p.add_generator(point(x));
  p.add_generator(point(y));
  p.add_generator(point(-x));
  p.add_generator(point(-y));

  nout << "p = " << p << endl;

  Polyhedra_Powerset<NNC_Polyhedron> p_c = complement(p);

  nout << "complement(p) = " << p_c << endl;

  C_Polyhedron q(2);
  q.add_constraint(x >= -1);
  q.add_constraint(x <=  1);
  q.add_constraint(y >=  1);
  q.add_constraint(y <=  3);

  nout << "q = " << q << endl;

  Polyhedra_Powerset<NNC_Polyhedron> q_c = complement(q);

  nout << "complement(q) = " << q_c << endl;

  // FIXME

  return 0;
}
CATCH
