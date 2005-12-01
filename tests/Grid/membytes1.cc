/* Test Grid::total_memory_in_bytes() and
   Grid::external_memory_in_bytes().
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

namespace {

void
add_congruence(Grid& gr, const Congruence& cg) {
  const memory_size_type gr_memory_before = gr.total_memory_in_bytes();
  const memory_size_type cg_memory = cg.total_memory_in_bytes();
  gr.add_congruence(cg);
  const memory_size_type gr_memory_after = gr.total_memory_in_bytes();
  nout << gr_memory_before
       << " + " << cg_memory
       << " -> " << gr_memory_after
       << endl;
}

void
add_generator(Grid& gr, const Grid_Generator& g) {
  const memory_size_type gr_memory_before = gr.total_memory_in_bytes();
  const memory_size_type g_memory = g.total_memory_in_bytes();
  gr.add_generator(g);
  const memory_size_type gr_memory_after = gr.total_memory_in_bytes();
  nout << gr_memory_before
       << " + " << g_memory
       << " -> " << gr_memory_after
       << endl;
}

void
minimize(Grid& gr) {
  const memory_size_type gr_memory_before = gr.total_memory_in_bytes();
  (void) gr.minimized_generators();
  const memory_size_type gr_memory_after = gr.total_memory_in_bytes();
  nout << gr_memory_before
       << " -m-> " << gr_memory_after
       << endl;
}

} // namespace

int
main() TRY {
  set_handlers();

  Variable x(0);
  Variable y(1);
  Variable z(2);

  const memory_size_type x_total_size = x.total_memory_in_bytes();
  const memory_size_type x_external_size = x.external_memory_in_bytes();

  nout << "*** Size of variables ***"
       << endl
       << "x.total_memory_in_bytes() = " << x_total_size
       << endl
       << "x.external_memory_in_bytes() = " << x_external_size
       << endl << endl;

  nout << "*** Size of linear expressions ***"
       << endl;

  Linear_Expression le(0);
  memory_size_type le_total_size = le.total_memory_in_bytes();
  memory_size_type le_external_size = le.external_memory_in_bytes();
  using namespace IO_Operators;
  nout << "(" << le << ").total_memory_in_bytes() = " << le_total_size
       << endl
       << "(" << le << ").external_memory_in_bytes() = " << le_external_size
       << endl;
  le += x;
  le_total_size = le.total_memory_in_bytes();
  le_external_size = le.external_memory_in_bytes();
  nout << "(" << le << ").total_memory_in_bytes() = " << le_total_size
       << endl
       << "(" << le << ").external_memory_in_bytes() = " << le_external_size
       << endl;
  le += 2*y;
  le_total_size = le.total_memory_in_bytes();
  le_external_size = le.external_memory_in_bytes();
  nout << "(" << le << ").total_memory_in_bytes() = " << le_total_size
       << endl
       << "(" << le << ").external_memory_in_bytes() = " << le_external_size
       << endl;
  le += 4*z;
  le_total_size = le.total_memory_in_bytes();
  le_external_size = le.external_memory_in_bytes();
  nout << "(" << le << ").total_memory_in_bytes() = " << le_total_size
       << endl
       << "(" << le << ").external_memory_in_bytes() = " << le_external_size
       << endl << endl;

  nout << "*** Adding congruences to a grid ***" << endl;

  Grid gr1(3);
  add_congruence(gr1, 4*x - 2*y - z + 14 %= 0);
  add_congruence(gr1, 4*x + 2*y - z + 2 %= 0);
  add_congruence(gr1, x + y - 1 %= 0);
  add_congruence(gr1, x + y + 2*z - 5 %= 0);
  minimize(gr1);
  add_congruence(gr1, x + 1 %= 0);
  add_congruence(gr1, x + z - 1 %= 0);
  add_congruence(gr1, 2*x + y -2*z + 7 %= 0);
  add_congruence(gr1, x - y + 2*z + 1 %= 0);
  minimize(gr1);
  add_congruence(gr1, x - y + 5 %= 0);
  add_congruence(gr1, 2*x - y - 2*z + 13 %= 0);
  add_congruence(gr1, -2*x - y + 2*z + 1 %= 0);
  add_congruence(gr1, -x + y - 1 %= 0);
  minimize(gr1);
  add_congruence(gr1, -x + y -2*z + 7 %= 0);
  add_congruence(gr1, -4*x + 2*y + z - 4 %= 0);
  add_congruence(gr1, -2*x + y + 2*z - 5 %= 0);
  add_congruence(gr1, -x + 1 %= 0);
  minimize(gr1);
  add_congruence(gr1, -x - z + 5 %= 0);
  add_congruence(gr1, -4*x - 2*y + z + 8 %= 0);
  add_congruence(gr1, -x - y + 5 %= 0);
  add_congruence(gr1, -x - y -2*z +13 %= 0);
  minimize(gr1);

  const memory_size_type gr1_total_size = gr1.total_memory_in_bytes();
  const memory_size_type gr1_external_size = gr1.external_memory_in_bytes();
  const Congruence_System& cgs = gr1.congruences();
  const memory_size_type cgs_total_size = cgs.total_memory_in_bytes();
  const memory_size_type cgs_external_size = cgs.external_memory_in_bytes();
  const Grid_Generator_System& gs = gr1.generators();
  const memory_size_type gs_total_size = gs.total_memory_in_bytes();
  const memory_size_type gs_external_size = gs.external_memory_in_bytes();

  nout << endl
       << "*** Size of the user-visible grid components ***"
       << endl
       << "gr1.total_memory_in_bytes() = " << gr1_total_size
       << endl
       << "cgs.total_memory_in_bytes() = " << cgs_total_size
       << endl
       << "gs.total_memory_in_bytes() = " << gs_total_size
       << endl
       << "gr1.external_memory_in_bytes() = " << gr1_external_size
       << endl
       << "cgs.external_memory_in_bytes() = " << cgs_external_size
       << endl
       << "gs.external_memory_in_bytes() = " << gs_external_size
       << endl << endl;

  nout << "*** Adding generators to a grid ***" << endl;

  Grid gr2(3, EMPTY);
  unsigned n = 0;
  for (Grid_Generator_System::const_iterator i = gs.begin(),
	 gs_end = gs.end(); i != gs_end; ++i) {
    add_generator(gr2, *i);
    if (++n % 4 == 0)
      minimize(gr2);
  }

  nout << endl
       << "*** Size of a congruence system vs size of contained congruences"
       << endl
       << "cgs.total_memory_in_bytes() = " << cgs_total_size
       << endl;

  memory_size_type cgs_elements_size = 0;
  for (Congruence_System::const_iterator i = cgs.begin(),
	 cgs_end = cgs.end(); i != cgs_end; ++i)
    cgs_elements_size += i->total_memory_in_bytes();

  nout << "Sum of sizes of contained congruences = " << cgs_elements_size
       << endl << endl
       << "*** Size of a generator system vs size of contained generators"
       << endl
       << "gs.total_memory_in_bytes() = " << gs_total_size
       << endl;

  memory_size_type gs_elements_size = 0;
  for (Grid_Generator_System::const_iterator i = gs.begin(),
	 gs_end = gs.end(); i != gs_end; ++i)
    gs_elements_size += i->total_memory_in_bytes();

  nout << "Sum of sizes of contained generators = " << gs_elements_size
       << endl << endl;

  return 0;
}
CATCH
