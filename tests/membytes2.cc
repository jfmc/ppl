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
#define NOISY 0
#endif

static void loo(...) {
}

int
main() TRY {
  set_handlers();

  // Avoid warnings.
  loo();

  Variable x(0);
  Variable y(1);
  Variable z(2);

  C_Polyhedron ph(3);
  ph.add_constraint(4*x - 2*y - z + 14 >= 0);
  ph.add_constraint(4*x + 2*y - z + 2 >= 0);
  ph.add_constraint(x + y - 1 >= 0);
  ph.add_constraint(x + y + 2*z - 5 >= 0);

  const memory_size_type ph_total_size = ph.total_memory_in_bytes();
  const memory_size_type ph_external_size = ph.external_memory_in_bytes();

  Determinate<C_Polyhedron> dph(ph);

  const memory_size_type dph_total_size = dph.total_memory_in_bytes();
  const memory_size_type dph_external_size = dph.external_memory_in_bytes();

#if NOISY
  cout << "ph.total_memory_in_bytes() = " << ph_total_size
       << endl
       << "ph.external_memory_in_bytes() = " << ph_external_size
       << endl
       << "dph.total_memory_in_bytes() = " << dph_total_size
       << endl
       << "dph.external_memory_in_bytes() = " << dph_external_size
       << endl;
#else
  loo(ph_total_size, ph_external_size, dph_total_size, dph_external_size);
#endif

  Polyhedra_Powerset<C_Polyhedron> pph(ph);

  C_Polyhedron qh(3);
  qh.add_constraint(x >= 0);
  qh.add_constraint(y >= 0);
  qh.add_constraint(z >= 0);
  qh.add_constraint(x <= 1);
  qh.add_constraint(y <= 1);
  qh.add_constraint(z <= 1);
  Polyhedra_Powerset<C_Polyhedron> pqh(qh);

  Polyhedra_Powerset<C_Polyhedron> prh = pqh;
  prh.poly_difference_assign(pph);

  const memory_size_type pph_total_size = pph.total_memory_in_bytes();
  const memory_size_type pph_external_size = pph.external_memory_in_bytes();
  const memory_size_type pqh_total_size = pqh.total_memory_in_bytes();
  const memory_size_type pqh_external_size = pqh.external_memory_in_bytes();
  const memory_size_type prh_total_size = prh.total_memory_in_bytes();
  const memory_size_type prh_external_size = prh.external_memory_in_bytes();

#if NOISY
  cout << "pph.total_memory_in_bytes() = " << pph_total_size
       << endl
       << "pph.external_memory_in_bytes() = " << pph_external_size
       << endl
       << "pqh.total_memory_in_bytes() = " << pqh_total_size
       << endl
       << "pqh.external_memory_in_bytes() = " << pqh_external_size
       << endl
       << "prh.total_memory_in_bytes() = " << prh_total_size
       << endl
       << "prh.external_memory_in_bytes() = " << prh_external_size
       << endl;
#else
  loo(pph_total_size, pph_external_size,
      pqh_total_size, pqh_external_size,
      prh_total_size, prh_external_size);
#endif

  return 0;
}
CATCH
