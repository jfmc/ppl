/* Declaration of simple print functions used in test programs.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_print_hh
#define PPL_print_hh 1

#include "ppl_install.hh"
#include <iostream>
#include <string>

void
print_constraint(const Parma_Polyhedra_Library::Constraint& c,
		 const std::string& intro = "",
		 std::ostream& s = std::cout);

void
print_constraints(const Parma_Polyhedra_Library::Constraint_System& cs,
		  const std::string& intro = "",
		  std::ostream& s = std::cout);

void
print_constraints(const Parma_Polyhedra_Library::Polyhedron& ph,
		  const std::string& intro = "",
		  std::ostream& s = std::cout);

void
print_generator(const Parma_Polyhedra_Library::Generator& g,
		const std::string& intro = "",
		std::ostream& s = std::cout);

void
print_generators(const Parma_Polyhedra_Library::Generator_System& gs,
		 const std::string& intro = "",
		 std::ostream& s = std::cout);

void
print_generators(const Parma_Polyhedra_Library::Polyhedron& ph,
		 const std::string& intro = "",
		 std::ostream& s = std::cout);

#endif // !defined(PPL_print_hh)
