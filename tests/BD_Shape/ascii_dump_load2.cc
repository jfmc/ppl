/* Test BD_Shape::ascii_dump() and BD_Shape::ascii_load().
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

using std::string;
using std::fstream;
using std::ios_base;

namespace {

const char* my_file = "ascii_dump_load2.dat";

void
test1() {

  nout << "test1()" << endl;

  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(A >= 0);
  bd.add_constraint(B >= 0);

  fstream f;
  open(f, my_file, ios_base::out);
  bd.ascii_dump(f);
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
  TBD_Shape bd2;
  bool ok = !bd2.ascii_load(f);
  close(f);

  if (!ok)
    exit(1);
}

void
test2() {

  nout << "test2()" << endl;

  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(A >= 0);
  bd.add_constraint(B >= 1);

  fstream f;
  open(f, my_file, ios_base::out);
  bd.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in | ios_base::out);
  string str;
  do
    f >> str;
  while (str != "+ZE");
  f.seekp(0, ios_base::cur);
  f << "A";
  close(f);

  open(f, my_file, ios_base::in);
  TBD_Shape bd2;
  bool ok = !bd2.ascii_load(f);
  close(f);

  if (!ok)
    exit(1);
}

void
test3() {

  nout << "test3()" << endl;

  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(A >= 0);
  bd.add_constraint(B >= 2);

  fstream f;
  open(f, my_file, ios_base::out);
  bd.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in | ios_base::out);
  string str;
  do
    f >> str;
  while (str != "-SPC");
  f.seekp(0, ios_base::cur);
  f << "A";
  close(f);

  open(f, my_file, ios_base::in);
  TBD_Shape bd2;
  bool ok = !bd2.ascii_load(f);
  close(f);

  if (!ok)
    exit(1);
}

void
test4() {

  nout << "test4()" << endl;

  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(A >= 0);
  bd.add_constraint(B >= 3);

  fstream f;
  open(f, my_file, ios_base::out);
  bd.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in | ios_base::out);
  string str;
  do
    f >> str;
  while (str != "+inf");
  f.seekp(0, ios_base::cur);
  f << "A";
  close(f);

  open(f, my_file, ios_base::in);
  TBD_Shape bd2;
  bool ok = !bd2.ascii_load(f);
  close(f);

  if (!ok)
    exit(1);
}

void
test5() {

  nout << "test5()" << endl;

  Variable A(0);
  Variable B(1);

  TBD_Shape bd(2);
  bd.add_constraint(A >= 0);
  bd.add_constraint(B >= 3);

  fstream f;
  open(f, my_file, ios_base::out);
  bd.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in | ios_base::out);
  string str;
  do
    f >> str;
  while (str != "+inf");
  do
    f >> str;
  while (str != "+inf");
  f.seekp(0, ios_base::cur);
  f << " 3 ";
  close(f);

  open(f, my_file, ios_base::in);
  TBD_Shape bd2;
  bool ok = !bd2.ascii_load(f);
  close(f);

  if (!ok)
    exit(1);
}

} // namespace

int
main() TRY {
  test1();
  test2();
  test3();
  test4();
  test5();

  return 0;
}
CATCH
