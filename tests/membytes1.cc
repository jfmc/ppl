/* Test the total_memory_in_bytes() and external_memory_in_bytes() methods.
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

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 1
#endif

void
add_constraint(C_Polyhedron& ph, const Constraint& c) {
  const memory_size_type ph_memory_before = ph.total_memory_in_bytes();
  const memory_size_type c_memory = c.total_memory_in_bytes();
  ph.add_constraint(c);
  const memory_size_type ph_memory_after = ph.total_memory_in_bytes();
#if NOISY
  cout << ph_memory_before
       << " + " << c_memory
       << " -> " << ph_memory_after
       << endl;
#endif
}

void
minimize(C_Polyhedron& ph) {
  const memory_size_type ph_memory_before = ph.total_memory_in_bytes();
  (void) ph.minimized_generators();
  const memory_size_type ph_memory_after = ph.total_memory_in_bytes();
#if NOISY
  cout << ph_memory_before
       << " -m-> " << ph_memory_after
       << endl;
#endif
}

int
main() TRY {
  set_handlers();

  Variable x(0);
  Variable y(1);
  Variable z(2);

  C_Polyhedron ph(3);
  add_constraint(ph, 4*x - 2*y - z + 14 >= 0);
  add_constraint(ph, 4*x + 2*y - z + 2 >= 0);
  add_constraint(ph, x + y - 1 >= 0);
  add_constraint(ph, x + y + 2*z - 5 >= 0);
  minimize(ph);
  add_constraint(ph, x + 1 >= 0);
  add_constraint(ph, x + z - 1 >= 0);
  add_constraint(ph, 2*x + y -2*z + 7 >= 0);
  add_constraint(ph, x - y + 2*z + 1 >= 0);
  minimize(ph);
  add_constraint(ph, x - y + 5 >= 0);
  add_constraint(ph, 2*x - y - 2*z + 13 >= 0);
  add_constraint(ph, -2*x - y + 2*z + 1 >= 0);
  add_constraint(ph, -x + y - 1 >= 0);
  minimize(ph);
  add_constraint(ph, -x + y -2*z + 7 >= 0);
  add_constraint(ph, -4*x + 2*y + z - 4 >= 0);
  add_constraint(ph, -2*x + y + 2*z - 5 >= 0);
  add_constraint(ph, -x + 1 >= 0);
  minimize(ph);
  add_constraint(ph, -x - z + 5 >= 0);
  add_constraint(ph, -4*x - 2*y + z + 8 >= 0);
  add_constraint(ph, -x - y + 5 >= 0);
  add_constraint(ph, -x - y -2*z +13 >= 0);
  minimize(ph);

  const memory_size_type ph_total_size = ph.total_memory_in_bytes();
  const memory_size_type ph_external_size = ph.external_memory_in_bytes();
  const ConSys& cs = ph.constraints();
  const memory_size_type cs_total_size = cs.total_memory_in_bytes();
  const memory_size_type cs_external_size = cs.external_memory_in_bytes();
  const GenSys& gs = ph.generators();
  const memory_size_type gs_total_size = gs.total_memory_in_bytes();
  const memory_size_type gs_external_size = gs.external_memory_in_bytes();

#if NOISY
  cout << "ph.total_memory_in_bytes() = " << ph_total_size << endl
       << "cs.total_memory_in_bytes() = " << cs_total_size << endl
       << "gs.total_memory_in_bytes() = " << gs_total_size << endl
       << "ph.external_memory_in_bytes() = " << ph_external_size << endl
       << "cs.external_memory_in_bytes() = " << cs_external_size << endl
       << "gs.external_memory_in_bytes() = " << gs_external_size << endl;
#endif

  return 0;
}
CATCH
