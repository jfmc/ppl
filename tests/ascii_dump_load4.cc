/* Test Polyhedron::ascii_dump() e Polyhedron::ascii_load():
   we test these functions in the case that the file does not contain
   the right thing.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include <string>
#include <fstream>
#include "ppl_test.hh"
#include "files.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

const char* my_file = "ascii_dump_load_4.dat";

void
test1() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2);
  ph.add_constraint(A == 2);   
  ph.add_constraint(B >= 0);   
  
  fstream f;
  open(f, my_file, ios_base::out);
  ph.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in);
  C_Polyhedron ph2;
  bool ok = ph2.ascii_load(f);
  close(f);

  if (!ok)
    exit(1);
}

void
test2() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2);
  ph.add_constraint(A == 2);   
  ph.add_constraint(B >= 0);   
  
  fstream f;
  open(f, my_file, ios_base::out);
  ph.ascii_dump(f);
  close(f);
  
  open(f, my_file, ios_base::in | ios_base::out);
  std::string str;
  do
    f >> str;
  while(str != "=");
  f << "A";
  close(f);
  
  open(f, my_file, ios_base::in);
  C_Polyhedron ph2;
  bool ok = !ph2.ascii_load(f);
  close(f);

  if (!ok)
    exit(1);
}

void
test3() {
  Variable A(0);
  Variable B(1);

  NNC_Polyhedron ph1(2);
  ph1.add_constraint(A == 2);   
  ph1.add_constraint(B >= 0);

  ConSys cs = ph1.constraints();

  C_Polyhedron ph(6);
  ph.add_constraints(cs);
  
  fstream f;
  open(f, my_file, ios_base::out);
  ph.ascii_dump(f);
  close(f);
  
  open(f, my_file, ios_base::in | ios_base::out);
  std::string str;
  do
    f >> str;
  while(str != "=");
  f.seekp(-6, ios_base::cur);
  f << "A";
  close(f);
  
  open(f, my_file, ios_base::in);
  C_Polyhedron ph2;
  bool ok = !ph2.ascii_load(f);
  close(f);

  if (!ok)
    exit(1);
}

void
test4() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2);
  ph.add_constraint(A >= 0);   
  ph.add_constraint(B >= 0);   
  
  fstream f;
  open(f, my_file, ios_base::out);
  ph.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in | ios_base::out);
  std::string str;
  do
    f >> str;
  while(str != "NECESSARILY_CLOSED");
  f.seekp(-2, ios_base::cur);
  f << "A";
  close(f);

  open(f, my_file, ios_base::in);
  C_Polyhedron ph2;
  bool ok = !ph2.ascii_load(f);
  close(f);

  if (!ok)
    exit(1);
}

void
test5() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2);
  ph.add_constraint(A >= 0);   
  ph.add_constraint(B >= 0);   
  
  fstream f;
  open(f, my_file, ios_base::out);
  ph.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in | ios_base::out);
  std::string str;
  do
    f >> str;
  while(str != "x");
  f.seekp(1, ios_base::cur);
  f << "A";
  close(f);

  open(f, my_file, ios_base::in);
  C_Polyhedron ph2;
  bool ok = !ph2.ascii_load(f);
  close(f);

  if (!ok)
    exit(1);
}


void
test6() {
  Variable A(0);
  Variable B(1);

  C_Polyhedron ph(2);
  ph.add_constraint(A >= 0);   
  ph.add_constraint(B >= 0);   
  
  fstream f;
  open(f, my_file, ios_base::out);
  ph.ascii_dump(f);
  close(f);

  open(f, my_file, ios_base::in | ios_base::out);
  std::string str;
  do
    f >> str;
  while(str != "(not_sorted)");
  f.seekp(-2, ios_base::cur);
  f << "A";
  close(f);

  open(f, my_file, ios_base::in);
  C_Polyhedron ph2;
  bool ok = !ph2.ascii_load(f);
  close(f);

  if (!ok)
    exit(1);
}

int
main() {
  set_handlers();
  
  test1();
  test2();
  test3();
  test4();
  test5();
  test6();

  return 0;
}
