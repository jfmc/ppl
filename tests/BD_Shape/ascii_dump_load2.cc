/* Test BD_Shape::ascii_dump() and BD_Shape::ascii_load().
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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
#include "files.hh"
#include <fstream>

static const char* my_file = "ascii_dump_load2.dat";


static void
test1() {
#if NOISY
  cout << "test1()" << endl;
#endif

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

static void
test2() {
#if NOISY
  cout << "test2()" << endl;
#endif

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
  while(str != "+ZE");
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

static void
test3() {
#if NOISY
  cout << "test3()" << endl;
#endif

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
  while(str != "-TC");
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

static void
test4() {
#if NOISY
  cout << "test4()" << endl;
#endif

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
  while(str != "+inf");
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

static void
test5() {
#if NOISY
  cout << "test5()" << endl;
#endif

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
  while(str != "+inf");
  do
    f >> str;
  while(str != "+inf");
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
