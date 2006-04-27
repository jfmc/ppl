/* Test Octagon::ascii_dump() and Octagon::ascii_load().
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

#include "files.hh"
#include <string>
#include <fstream>

namespace {

const char* my_file = "ascii_dump_load1.dat";

bool
test01() {
  Variable A(0);
  Variable B(1);

  TOctagon oc1(3);
  oc1.add_constraint(A - B >= 2);
  oc1.add_constraint(B >= 0);


  fstream f;
  open(f, my_file, ios_base::out);
  oc1.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in);
  TOctagon oc2;
  oc2.ascii_load(f);
  close(f);

  print_constraints(oc1, "*** oc1 ***");
  print_constraints(oc2, "*** oc2 ***");

  bool ok = (oc1 == oc2);

  return ok;
}

bool
test02() {
  Variable A(0);
  Variable B(1);
  
  TOctagon oc1(3);
  oc1.add_constraint(A - B >= 0);


  fstream f;
  open(f, my_file, ios_base::out);
  oc1.ascii_dump(f);
  f.seekp(0);
  f << "A";
  close(f);

  open(f, my_file, ios_base::in);
  TOctagon oc2;
  bool ok = !oc2.ascii_load(f);
  close(f);

  return ok;
}

bool
test03() {

  nout << "test03()" << endl;

  Variable A(0);
  Variable B(1);

  TOctagon oc(2);
  oc.add_constraint(A >= 0);
  oc.add_constraint(B >= 0);

  fstream f;
  open(f, my_file, ios_base::out);
  oc.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in | ios_base::out);
  string str;
  do
    f >> str;
  while (str != "-EM");
  f.seekp(0, ios_base::cur);
  f << " A";
  close(f);

  open(f, my_file, ios_base::in);
  TOctagon oc2;
  bool ok = !oc2.ascii_load(f);
  close(f);

  return ok;
}

bool
test04() {

  cout << "test04()" << endl;

  Variable A(0);
  Variable B(1);

  TOctagon oc(2);
  oc.add_constraint(A >= 0);
  oc.add_constraint(B >= 1);

  fstream f;
  open(f, my_file, ios_base::out);
  oc.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in | ios_base::out);
  string str;
  do
    f >> str;
  while(str != "+ZE");
  f.seekp(0, ios_base::cur);
  f << "A";
  close(f);

  open(f, my_file, ios_base::in);
  TOctagon oc2;
  bool ok = !oc2.ascii_load(f);
  close(f);

  return ok;
}

bool
test05() {

  nout << "test05()" << endl;

  Variable A(0);
  Variable B(1);

  TOctagon oc(2);
  oc.add_constraint(A >= 0);
  oc.add_constraint(B >= 2);

  fstream f;
  open(f, my_file, ios_base::out);
  oc.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in | ios_base::out);
  string str;
  do
    f >> str;
  while(str != "-SC");
  f.seekp(0, ios_base::cur);
  f << "A";
  close(f);

  open(f, my_file, ios_base::in);
  TOctagon oc2;
  bool ok = !oc2.ascii_load(f);
  close(f);

  return ok;
}

bool
test06() {

  nout << "test06()" << endl;

  Variable A(0);
  Variable B(1);

  TOctagon oc(2);
  oc.add_constraint(A >= 0);
  oc.add_constraint(B >= 3);

  fstream f;
  open(f, my_file, ios_base::out);
  oc.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in | ios_base::out);
  string str;
  do
    f >> str;
  while(str != "+inf");
  f.seekp(0, ios_base::cur);
  f << "A";
  close(f);

  open(f, my_file, ios_base::in);
  TOctagon oc2;
  bool ok = !oc2.ascii_load(f);
  close(f);

  return ok;
}

bool
test07() {

  cout << "test07()" << endl;

  Variable A(0);
  Variable B(1);

  TOctagon oc(2);
  oc.add_constraint(A >= 0);
  oc.add_constraint(B >= 3);

  fstream f;
  open(f, my_file, ios_base::out);
  oc.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in | ios_base::out);
  string str;
  do
    f >> str;
  while(str != "+inf");
  do
    f >> str;
  while(str != "+inf");
  f.seekp(0, ios_base::cur);
  f << " 3 ";
  close(f);

  open(f, my_file, ios_base::in);
  TOctagon oc2;
  bool ok = !oc2.ascii_load(f);
  close(f);

  return ok;
}

bool
test08() {
  TOctagon oc1(0, EMPTY);

  fstream f;
  open(f, my_file, ios_base::out);
  oc1.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in);
  TOctagon oc2;
  oc2.ascii_load(f);
  close(f);

  bool ok = (oc1 == oc2);

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST(test05);
  DO_TEST(test06);
  DO_TEST(test07);
  DO_TEST(test08);
END_MAIN
