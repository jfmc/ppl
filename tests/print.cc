/* Implementation of simple print functions used in test programs.
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

#include "print.hh"
#include "ppl_install.hh"
#include <iostream>
#include <string>

using namespace Parma_Polyhedra_Library;
using namespace Parma_Polyhedra_Library::IO_Operators;
using namespace std;

void
print_constraint(const Constraint& c, const string& intro, ostream& s) {
  if (!intro.empty())
    s << intro << endl;
  s << c << endl;
}

void
print_constraints(const ConSys& cs, const string& intro, ostream& s) {
  if (!intro.empty())
    s << intro << endl;
  ConSys::const_iterator i = cs.begin();
  ConSys::const_iterator cs_end = cs.end();
  bool printed_something = i != cs_end;
  while (i != cs_end) {
    s << *i++;
    if (i != cs_end)
      s << "," << endl;
  }
  s << (printed_something ? "." : "true.") << endl;
}

void
print_constraints(const Polyhedron& ph, const string& intro, ostream& s) {
  print_constraints(ph.constraints(), intro, s);
}

void
print_generator(const Generator& g, const string& intro, ostream& s) {
  if (!intro.empty())
    s << intro << endl;
  s << g << endl;
}

void
print_generators(const GenSys& gs, const string& intro, ostream& s) {
  if (!intro.empty())
    s << intro << endl;
  GenSys::const_iterator i = gs.begin();
  GenSys::const_iterator gs_end = gs.end();
  bool printed_something = i != gs_end;
  while (i != gs_end) {
    s << *i++;
    if (i != gs_end)
      s << "," << endl;
  }
  s << (printed_something ? "." : "false.") << endl;
}

void
print_generators(const Polyhedron& ph, const string& intro, ostream& s) {
  print_generators(ph.generators(), intro, s);
}
