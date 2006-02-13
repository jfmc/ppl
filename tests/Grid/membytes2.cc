/* Test Grid::total_memory_in_bytes() and
   Grid::external_memory_in_bytes().
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

int
main() TRY {
  set_handlers();

  Variable x(0);
  Variable y(1);
  Variable z(2);

  Grid gr1(3);
  gr1.add_constraint(4*x - 2*y - z + 14 >= 0);
  gr1.add_constraint(4*x + 2*y - z + 2 >= 0);
  gr1.add_constraint(x + y - 1 >= 0);
  gr1.add_constraint(x + y + 2*z - 5 >= 0);

  const memory_size_type gr1_total_size = gr1.total_memory_in_bytes();
  const memory_size_type gr1_external_size = gr1.external_memory_in_bytes();

  Determinate<Grid> dgr(gr1);

  const memory_size_type dgr_total_size = dgr.total_memory_in_bytes();
  const memory_size_type dgr_external_size = dgr.external_memory_in_bytes();

  nout << "gr1.total_memory_in_bytes() = " << gr1_total_size
       << endl
       << "gr1.external_memory_in_bytes() = " << gr1_external_size
       << endl
       << "dgr.total_memory_in_bytes() = " << dgr_total_size
       << endl
       << "dgr.external_memory_in_bytes() = " << dgr_external_size
       << endl;

  Polyhedra_Powerset<Grid> pgr1(gr1);

  Grid gr2(3);
  gr2.add_constraint(x >= 0);
  gr2.add_constraint(y >= 0);
  gr2.add_constraint(z >= 0);
  gr2.add_constraint(x <= 1);
  gr2.add_constraint(y <= 1);
  gr2.add_constraint(z <= 1);
  Polyhedra_Powerset<Grid> pgr2(gr2);

  // TODO: Include these sections when poly_difference_assign is
  //       defined for Grid.
#if 0
  Polyhedra_Powerset<Grid> p2gr2 = pgr2;
  p2gr2.poly_difference_assign(pgr1);
#endif

  const memory_size_type pgr1_total_size = pgr1.total_memory_in_bytes();
  const memory_size_type pgr1_external_size = pgr1.external_memory_in_bytes();
  const memory_size_type pgr2_total_size = pgr2.total_memory_in_bytes();
  const memory_size_type pgr2_external_size = pgr2.external_memory_in_bytes();
#if 0
  const memory_size_type p2gr2_total_size = p2gr2.total_memory_in_bytes();
  const memory_size_type p2gr2_external_size = p2gr2.external_memory_in_bytes();
#endif

  nout << "pgr1.total_memory_in_bytes() = " << pgr1_total_size
       << endl
       << "pgr1.external_memory_in_bytes() = " << pgr1_external_size
       << endl
       << "pgr2.total_memory_in_bytes() = " << pgr2_total_size
       << endl
       << "pgr2.external_memory_in_bytes() = " << pgr2_external_size
#if 0
       << endl
       << "p2gr2.total_memory_in_bytes() = " << p2gr2_total_size
       << endl
       << "p2gr2.external_memory_in_bytes() = " << p2gr2_external_size
#endif
       << endl;

  return 0;
}
CATCH
