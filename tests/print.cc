/* Implementation of simple print functions used in test programs.
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

#include "print.hh"
#include "ppl.hh"
#include "Partial_Function.defs.hh"
#include <iostream>
#include <string>

using namespace Parma_Polyhedra_Library;
using namespace Parma_Polyhedra_Library::IO_Operators;

void
print_constraint(const Constraint& c,
		 const std::string& intro, std::ostream& s) {
  if (!intro.empty())
    s << intro << "\n";
  s << c << std::endl;
}

void
print_constraints(const Polyhedron& ph,
		  const std::string& intro, std::ostream& s) {
  print_constraints(ph.constraints(), intro, s);
}

void
print_constraints(const Constraint_System& cs,
		  const std::string& intro, std::ostream& s) {
  if (!intro.empty())
    s << intro << "\n";
  Constraint_System::const_iterator i = cs.begin();
  Constraint_System::const_iterator cs_end = cs.end();
  bool printed_something = i != cs_end;
  while (i != cs_end) {
    s << *i++;
    if (i != cs_end)
      s << ",\n";
  }
  s << (printed_something ? "." : "true.") << std::endl;
}

void
print_congruences(const Congruence_System& cs,
		  const std::string& intro, std::ostream& s) {
  if (!intro.empty())
    s << intro << std::endl;
  Congruence_System::const_iterator i = cs.begin();
  Congruence_System::const_iterator cs_end = cs.end();
  bool printed_something = i != cs_end;
  while (i != cs_end) {
    s << *i++;
    if (i != cs_end)
      s << "," << std::endl;
  }
  s << (printed_something ? "." : "true.") << std::endl;
}

// FIXME: why this?
void
print_constraints(const Congruence_System& cs,
		  const std::string& intro, std::ostream& s) {
  print_congruences(cs, intro, s);
}

void
print_congruences(const Grid& gr,
		  const std::string& intro, std::ostream& s) {
  print_constraints(gr.congruences(), intro, s);
}

void
print_generator(const Generator& g,
		const std::string& intro, std::ostream& s) {
  if (!intro.empty())
    s << intro << "\n";
  s << g << std::endl;
}

void
print_generators(const Polyhedron& ph,
		 const std::string& intro, std::ostream& s) {
  print_generators(ph.generators(), intro, s);
}

void
print_generators(const Generator_System& gs,
		 const std::string& intro, std::ostream& s) {
  if (!intro.empty())
    s << intro << "\n";
  Generator_System::const_iterator i = gs.begin();
  Generator_System::const_iterator gs_end = gs.end();
  bool printed_something = i != gs_end;
  while (i != gs_end) {
    s << *i++;
    if (i != gs_end)
      s << ",\n";
  }
  s << (printed_something ? "." : "false.") << std::endl;
}

void
print_function(const Partial_Function& function,
	       const std::string& intro, std::ostream& s) {
  if (!intro.empty())
    s << intro << std::endl;
  function.print(s);
}
