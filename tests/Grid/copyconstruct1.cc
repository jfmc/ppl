/* Test copy construction of grids.
   Copyright (C) 2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

namespace {

// Universe and empty grids of the first ten dimensions.

void
test1() {
  nout << "test1:" << endl;

  for (unsigned int dim = 0; dim < 10; ++dim) {

    // Universe.

    Grid gr(dim);

    Grid gr_copy = gr;

    if (find_variation(gr_copy))
      exit(1);

    Grid known_gr(dim);

    if (gr_copy == known_gr) {

      // Empty.

      gr = Grid(dim, EMPTY);

      Grid gr_copy = gr;

      if (find_variation(gr_copy))
	exit(1);

      Grid known_gr(dim, EMPTY);

      if (gr_copy == known_gr)
	continue;

      nout << "Copied empty grid should equal known grid." << endl;
    }
    else
      nout << "Copied universe grid should equal known grid." << endl;

    nout << "dimension: " << dim << endl
	 << " grid:" << endl << gr_copy << endl
	 << "known:" << endl << known_gr << endl;

    dump_grids(gr, known_gr);

    exit(1);
  }
}

} // namespace

int
main() TRY {
  set_handlers();

  nout << "copyconstruct1:" << endl;

  test1();

  return 0;
}
CATCH
