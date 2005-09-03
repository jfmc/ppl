/* Test operator<<(std::ostream&, const Congruence_System&).
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

using namespace Parma_Polyhedra_Library::IO_Operators;

#define find_variation find_variation_template<Congruence_System>

int
main() TRY {
  set_handlers();

  Variable A(0);
  Variable B(1);

  Congruence_System cgs;

  if (!cgs.OK()) {
    nout << "cgs.OK() failed" << endl << "ASCII dump: ";
    cgs.ascii_dump(nout);
    return 1;
  }

  stringstream ss;
  ss << cgs;
  if (ss.str().compare("true")) {
    nout << "  output: " << ss.str() << endl
	 << "expected: true" << endl;
    return 1;
  }

  cgs.insert(A - 2*B %= 2);
  cgs.insert(2*A %= 4);
  if (find_variation(cgs))
    exit(1);

#define OUTPUT "A - 2*B = 0 (mod 1), 2*A = 0 (mod 1)"

  ss.str("");
  ss << cgs;
  if (ss.str().compare(OUTPUT)) {
    nout << "  output: " << ss.str() << endl
	 << "expected: " OUTPUT << endl;
    return 1;
  }

  return 0;
}
CATCH
