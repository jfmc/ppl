/* Test PIP_Problem::ascii_dump() and PIP_Problem::ascii_load().
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "files.hh"
#include <string>
#include <fstream>

using std::string;
using std::fstream;
using std::ios_base;

namespace {

bool
test01() {
  const char* my_file = "ascii_dump_load1.dat";

  Variable A(0);
  Constraint_System cs;
  cs.insert(A >= 1);

  PIP_Problem pip1(3, cs.begin(), cs.end(), Variables_Set());
  pip1.clear();

  fstream f;
  open(f, my_file, ios_base::out);
  pip1.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in);
  PIP_Problem pip2;
  pip2.ascii_load(f);
  close(f);

  bool ok = pip1.space_dimension() == 0
    && pip2.space_dimension() == 0
    && pip1.constraints_begin() == pip1.constraints_end()
    && pip2.constraints_begin() == pip2.constraints_end();

  return ok;
}

bool
test02() {
  const char* my_file = "ascii_dump_load1.dat";

  PIP_Problem pip1(3);
  pip1.solve();

  fstream f;
  open(f, my_file, ios_base::out);
  pip1.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in);
  PIP_Problem pip2;
  pip2.ascii_load(f);
  close(f);

  bool ok = pip1.space_dimension() == 3
    && pip2.space_dimension() == 3
    && pip1.constraints_begin() == pip1.constraints_end()
    && pip2.constraints_begin() == pip2.constraints_end();

  return ok;
}

bool
test03() {
  const char* my_file = "ascii_dump_load1.dat";

  Variable A(0);
  Constraint_System cs;
  cs.insert(A >= 1);
  cs.insert(A <= 0);

  PIP_Problem pip1(1, cs.begin(), cs.end(), Variables_Set());
  pip1.set_control_parameter(PIP_Problem::CUTTING_STRATEGY_DEEPEST);
  pip1.solve();

  fstream f;
  open(f, my_file, ios_base::out);
  pip1.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in);
  PIP_Problem pip2;
  pip2.ascii_load(f);
  close(f);

  bool ok = pip1.space_dimension() == pip2.space_dimension()
    && 2 == std::distance(pip1.constraints_begin(), pip1.constraints_end())
    && 2 == std::distance(pip2.constraints_begin(), pip2.constraints_end());

  return ok;
}

bool
test04() {
  const char* my_file = "ascii_dump_load1.dat";

  Variable A(0);
  Constraint_System cs;
  cs.insert(A >= 5);

  PIP_Problem pip1(3, cs.begin(), cs.end(), Variables_Set());

  fstream f;
  open(f, my_file, ios_base::out);
  pip1.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in);
  PIP_Problem pip2;
  pip2.ascii_load(f);
  close(f);

  bool ok = pip1.space_dimension() == pip2.space_dimension()
    && 1 == std::distance(pip1.constraints_begin(), pip1.constraints_end())
    && 1 == std::distance(pip2.constraints_begin(), pip2.constraints_end());

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
END_MAIN
