/* Test operator<<(std::ostream& s, const Poly_Con_Relation& r)
   and operator<<(std::ostream& s, const Poly_Gen_Relation& r).
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
using namespace Parma_Polyhedra_Library::IO_Operators;

#ifndef NOISY
#define NOISY 0
#endif

namespace {

const char* my_file = "writerelation1.dat";

void
test1() {
  Poly_Con_Relation rel(Poly_Con_Relation::nothing());
  rel.OK();

  fstream f;
  open(f, my_file, ios_base::out);
  f << rel << endl;
  close(f);
}

void
test2() {
  Poly_Gen_Relation rel(Poly_Gen_Relation::nothing());
  rel.OK();

  fstream f;
  open(f, my_file, ios_base::out);
  f << rel << endl;
  close(f);
}

void
test3() {
  Poly_Con_Relation rel(Poly_Con_Relation::is_disjoint());
  rel.OK();

  fstream f;
  open(f, my_file, ios_base::out);
  f << rel << endl;
  close(f);
}

void
test4() {
  Poly_Gen_Relation rel(Poly_Gen_Relation::subsumes());
  rel.OK();

  fstream f;
  open(f, my_file, ios_base::out);
  f << rel << endl;
  close(f);
}

} // namespace

int
main() TRY {
  set_handlers();

  test1();
  test2();
  test3();
  test4();

  return 0;
}
CATCH
