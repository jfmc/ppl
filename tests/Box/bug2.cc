/* Test Box::refine(const Constraint_System&) with instances that may
   require a watchdog timer.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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
#include <sstream>

namespace {

bool
test01() {
  Variable A(0);
  Variable B(1);

  std::string input_string("\
empty 0 empty_up_to_date 1 space_dim 2\n\
(-1.40129846432481707092372958328991613128026194187651577175706828388979108268586060148663818836212158203125e-45, 2.8025969286496341418474591665798322625605238837530315435141365677795821653717212029732763767242431640625e-45)\n\
(-1.40129846432481707092372958328991613128026194187651577175706828388979108268586060148663818836212158203125e-45, 2.8025969286496341418474591665798322625605238837530315435141365677795821653717212029732763767242431640625e-45)\n");
  std::stringstream input_stream(input_string);
  Box<fl_r_oc> box(2);
  box.ascii_load(input_stream);

  using namespace IO_Operators;

  std::cout << "Box before refine(A == 2*B)" << std::endl
	    << box << std::endl;

  box.refine(A == 2*B);

  std::cout << "Box after refine(A == 2*B)" << std::endl
	    << box << std::endl;

  return true;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
END_MAIN
