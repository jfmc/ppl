/* Test ascii_dump() 5 ascii_load(): we read a non completed file.
   we test these functions in the case that the file does not contain
   the right thing.
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
#include "files.hh"
#include <string>
#include <fstream>

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

static const char* my_file = "ascii_dump_load5.dat";

static void
test1() {
#if NOISY
  cout << "test1()" << endl;
#endif

  fstream f;
  open(f, my_file, ios_base::out);
  f << "space_dim 2\n"
    << "-ZE -EM  -CM -GM  +CS -GS  -SC -SG\n"
    << "con_sys (up-to-date)\n"
    << "topology NECESSARILY_CLOSED\n"
    << "3 x 3 (not_sorted)\n"
    << "1 0 0";
  close(f);

  open(f, my_file, ios_base::in);
  C_Polyhedron ph2;
  bool ok =! ph2.ascii_load(f);
  close(f);

  if (!ok)
    exit(1);
}

static void
test2() {
#if NOISY
  cout << "test2()" << endl;
#endif

  fstream f;
  open(f, my_file, ios_base::out);
  f << "space_dim 2\n"
    << "-ZE -EM  -CM -GM  +CS -GS  -SC -SG\n"
    << "con_sys (up-to-date)\n"
    << "topology NECESSARILY_CLOSED\n"
    << "3 x 3 (not_sorted)\n"
    << "1 0 0   >=\n"
    << "0 1 0   >=\n"
    << "0 0 1   >=\n\n"
    << "gen_sys (not_up-to-date)\n"
    << "topology NECESSARILY_CLOSED\n"
    <<" 3 x 3 (not_sorted)\n"
    << "1 0 0";
  close(f);

  open(f, my_file, ios_base::in);
  C_Polyhedron ph2;
  bool ok =! ph2.ascii_load(f);
  close(f);

  if (!ok)
    exit(1);
}

static void
test3() {
#if NOISY
  cout << "test3()" << endl;
#endif

  fstream f;
  open(f, my_file, ios_base::out);
  f << "space_dim 2\n"
    << "-ZE -EM  -CM -GM  +CS -GS  -SC -SG\n"
    << "con_sys (up-to-date)\n"
    << "topology NECESSARILY_CLOSED\n"
    << "3";
  close(f);

  open(f, my_file, ios_base::in);
  C_Polyhedron ph2;
  bool ok =! ph2.ascii_load(f);
  close(f);

  if (!ok)
    exit(1);
}


static void
test4() {
#if NOISY
  cout << "test4()" << endl;
#endif

  fstream f;
  open(f, my_file, ios_base::out);
  f << "space_dim 2\n"
    << "-ZE -EM  -CM -GM  +CS -GS  -SC -SG\n"
    << "con_sys (up-to-date)\n"
    << "topology";
  close(f);

  open(f, my_file, ios_base::in);
  C_Polyhedron ph2;
  bool ok =! ph2.ascii_load(f);
  close(f);

  if (!ok)
    exit(1);
}


static void
test5() {
#if NOISY
  cout << "test5()" << endl;
#endif

  fstream f;
  open(f, my_file, ios_base::out);
  f << "space_dim 3\n"
    << "-ZE -EM  +CM +GM  +CS +GS  +SC -SG\n"
    << "con_sys (up-to-date)\n"
    << "topology NOT_NECESSARILY_CLOSED\n"
    << "4 x 5 (sorted)\n"
    << "1 0 0 0 -1   >\n"
    << "0 0 0 0 1   >=\n"
    << "0 0 1 0 0   >=\n"
    << "-2 1 -1 0 0   >=\n"
    << "\n"
    << "gen_sys (up-to-date)\n"
    << "topology NOT_NECESSARILY_CLOSED\n"
    << "5 x 5 (sorted)\n"
    << "0 0 0 1 0   L\n"
    << "0 1 0 0 0   R\n"
    << "0 1 1 0 0   R\n"
    << "1 2 0 0 0   C\n"
    << "1 2 0 0 1   P\n"
    << "\n"
    << "sat_c\n"
    << "5";
    close(f);

  open(f, my_file, ios_base::in);
  C_Polyhedron ph2;
  bool ok =! ph2.ascii_load(f);
  close(f);

  if (!ok)
    exit(1);
}


int
main() TRY {
  set_handlers();

  test1();
  test2();
  test3();
  test4();
  test5();

  return 0;
}
CATCH
