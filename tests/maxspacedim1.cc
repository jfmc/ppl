/* Test the *::max_space_dimension() methods.
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

#include "ppl_test.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

template <typename T>
dimension_type foo() {
  return T::max_space_dimension();
}

template dimension_type foo<Variable>();
template dimension_type foo<LinExpression>();
template dimension_type foo<ConSys>();
template dimension_type foo<GenSys>();
template dimension_type foo<C_Polyhedron>();
template dimension_type foo<NNC_Polyhedron>();
template dimension_type foo<Polyhedra_Powerset<C_Polyhedron> >();
template dimension_type foo<Polyhedra_Powerset<NNC_Polyhedron> >();

#define PRINT(T) \
cout << #T "::max_space_dimension() = " << T::max_space_dimension() << endl

int
main() TRY {
  set_handlers();

#if NOISY
  PRINT(Variable);
  PRINT(LinExpression);
  PRINT(ConSys);
  PRINT(GenSys);
  PRINT(C_Polyhedron);
  PRINT(NNC_Polyhedron);
  PRINT(Polyhedra_Powerset<C_Polyhedron>);
  PRINT(Polyhedra_Powerset<NNC_Polyhedron>);
#endif

  return 0;
}
CATCH
