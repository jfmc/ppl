/* Implementation of simple print functions used in test programs.
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

namespace PPL = Parma_Polyhedra_Library;

using namespace Parma_Polyhedra_Library;
using namespace Parma_Polyhedra_Library::IO_Operators;

bool
PPL::check_distance(const Checked_Number<mpq_class, Extended_Number_Policy>& d,
		    const char* max_d_s, const char* d_name) {
  Checked_Number<mpq_class, Extended_Number_Policy>
    max_d((max_d_s ? max_d_s : "0"), ROUND_NOT_NEEDED);
  assert(max_d >= 0);
  if (d > max_d) {
    Checked_Number<float, Extended_Number_Policy> dd(d, ROUND_UP);
    nout << "Excessive " << d_name << " distance " << dd
	 << ": should be at most " << max_d << "."
	 << endl;
    return false;
  }
  else
    return true;
}
