/* Test the total_memory_in_bytes() and external_memory_in_bytes() methods.
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

namespace {

namespace test01_namespace {

void
add_constraint(TBox& box, const Constraint& c) {
  const memory_size_type box_memory_before = box.total_memory_in_bytes();
  const memory_size_type c_memory = c.total_memory_in_bytes();
  box.add_constraint(c);
  const memory_size_type box_memory_after = box.total_memory_in_bytes();

  nout << box_memory_before
       << " + " << c_memory
       << " -> " << box_memory_after
       << endl;
}

void
minimize(TBox& box) {
  const memory_size_type box_memory_before = box.total_memory_in_bytes();
  (void) box.minimized_constraints();
  const memory_size_type box_memory_after = box.total_memory_in_bytes();

  nout << box_memory_before
       << " -m-> " << box_memory_after
       << endl;
}

} // namespace test01_namespace

bool
test01() {
  using namespace test01_namespace;

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

  le -= 4*y;
  le_total_size = le.total_memory_in_bytes();
  le_external_size = le.external_memory_in_bytes();

  nout << "(" << le << ").total_memory_in_bytes() = " << le_total_size
       << endl
       << "(" << le << ").external_memory_in_bytes() = " << le_external_size
       << endl;

  le += 4;
  le_total_size = le.total_memory_in_bytes();
  le_external_size = le.external_memory_in_bytes();

  nout << "(" << le << ").total_memory_in_bytes() = " << le_total_size
       << endl
       << "(" << le << ").external_memory_in_bytes() = " << le_external_size
       << endl << endl;

  nout << "*** Adding constraints to a bounded difference shape ***" << endl;

  TBox box(3);
  add_constraint(box, 2*x - 2*y >= 0);
  add_constraint(box, 4*x - 2*y - z + 2 >= 0);
  add_constraint(box, x - y - 1 <= 0);
  add_constraint(box, x >= 0);
  minimize(box);
  add_constraint(box, x + 1 >= 0);
  add_constraint(box, x - z - 1 >= 0);
  add_constraint(box, 2*x - 2*z + 7 >= 0);
  add_constraint(box, y - 2*z + 1 >= 0);
  minimize(box);
  add_constraint(box, x - y + 5 >= 0);
  add_constraint(box, 2*x - 2*z + 13 >= 0);
  add_constraint(box, -2*x + 2*z + 1 >= 0);
  add_constraint(box, -x + y - 1 >= 0);
  minimize(box);
  add_constraint(box, -x + y + 7 >= 0);
  add_constraint(box, -4*x + 4*y - 4 >= 0);
  add_constraint(box, -2*x + 2*z - 5 >= 0);
  add_constraint(box, -x + 1 >= 0);
  minimize(box);
  add_constraint(box, -x - z + 5 >= 0);
  add_constraint(box, -4*x - 2*y + z + 8 >= 0);
  add_constraint(box, -x + y + 5 >= 0);
  add_constraint(box, -x - y -2*z +13 >= 0);
  minimize(box);

  const memory_size_type box_total_size = box.total_memory_in_bytes();
  const memory_size_type box_external_size = box.external_memory_in_bytes();
  const Constraint_System& cs = box.constraints();
  const memory_size_type cs_total_size = cs.total_memory_in_bytes();
  const memory_size_type cs_external_size = cs.external_memory_in_bytes();

  nout << endl;

  nout << "*** Size of the user-visible polyhedra components ***"
       << endl
       << "box.total_memory_in_bytes() = " << box_total_size
       << endl
       << "cs.total_memory_in_bytes() = " << cs_total_size
       << endl
       << "box.external_memory_in_bytes() = " << box_external_size
       << endl
       << "cs.external_memory_in_bytes() = " << cs_external_size
       << endl << endl;

  nout << "*** Size of a constraint system vs size of contained constraints"
       << endl
       << "cs.total_memory_in_bytes() = " << cs_total_size
       << endl;

  memory_size_type cs_elements_size = 0;
  for (Constraint_System::const_iterator i = cs.begin(),
	 cs_end = cs.end(); i != cs_end; ++i)
    cs_elements_size += i->total_memory_in_bytes();

  nout << "Sum of sizes of contained constraints = " << cs_elements_size
       << endl << endl;

  return true;
}

bool test02() {
  Variable x(0);
  Variable y(1);
  Variable z(2);

  TBox box(3);
  box.add_constraint(4*x - 4*y + 14 >= 0);
  box.add_constraint(x - z + 2 >= 0);
  box.add_constraint(x + y - 1 >= 0);
  box.add_constraint(y - z - 5 >= 0);

  const memory_size_type box_total_size = box.total_memory_in_bytes();
  const memory_size_type box_external_size = box.external_memory_in_bytes();

  Determinate<TBox> dbox(box);

  const memory_size_type dbox_total_size = dbox.total_memory_in_bytes();
  const memory_size_type dbox_external_size = dbox.external_memory_in_bytes();

  nout << "box.total_memory_in_bytes() = " << box_total_size
       << endl
       << "box.external_memory_in_bytes() = " << box_external_size
       << endl
       << "dbox.total_memory_in_bytes() = " << dbox_total_size
       << endl
       << "dbox.external_memory_in_bytes() = " << dbox_external_size
       << endl;

  Pointset_Powerset<TBox> pbox(box);

  TBox qbox(3);
  qbox.add_constraint(x >= 0);
  qbox.add_constraint(y >= 0);
  qbox.add_constraint(z >= 0);
  qbox.add_constraint(x <= 1);
  qbox.add_constraint(y <= 1);
  qbox.add_constraint(z <= 1);
  Pointset_Powerset<TBox> pqbox(qbox);

  Pointset_Powerset<TBox> prbox = pqbox;
  prbox.poly_difference_assign(pbox);

  const memory_size_type pbox_total_size = pbox.total_memory_in_bytes();
  const memory_size_type pbox_external_size = pbox.external_memory_in_bytes();
  const memory_size_type pqbox_total_size = pqbox.total_memory_in_bytes();
  const memory_size_type pqbox_external_size = pqbox.external_memory_in_bytes();
  const memory_size_type prbox_total_size = prbox.total_memory_in_bytes();
  const memory_size_type prbox_external_size = prbox.external_memory_in_bytes();

  nout << "pbox.total_memory_in_bytes() = " << pbox_total_size
       << endl
       << "pbox.external_memory_in_bytes() = " << pbox_external_size
       << endl
       << "pqbox.total_memory_in_bytes() = " << pqbox_total_size
       << endl
       << "pqbox.external_memory_in_bytes() = " << pqbox_external_size
       << endl
       << "prbox.total_memory_in_bytes() = " << prbox_total_size
       << endl
       << "prbox.external_memory_in_bytes() = " << prbox_external_size
       << endl;

  return true;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
END_MAIN
