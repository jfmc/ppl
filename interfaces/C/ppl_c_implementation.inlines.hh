/* Implementation of the C interface: inline functions.
   Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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

#ifndef PPL_ppl_c_implementation_inlines_hh
#define PPL_ppl_c_implementation_inlines_hh 1

namespace Parma_Polyhedra_Library {

namespace C_Interface {

//! Reinterpret an mpz_t as mpz_class.
inline mpz_class&
reinterpret_mpz_class(mpz_t n) {
  return reinterpret_cast<mpz_class&>(*n);
}

DECLARE_CONVERSIONS(Coefficient, Coefficient)

DECLARE_CONVERSIONS(Linear_Expression, Linear_Expression)

DECLARE_CONVERSIONS(Constraint, Constraint)

DECLARE_CONVERSIONS(Constraint_System, Constraint_System)

typedef Constraint_System::const_iterator Constraint_System_const_iterator;
DECLARE_CONVERSIONS(Constraint_System_const_iterator,
                    Constraint_System_const_iterator)

DECLARE_CONVERSIONS(Generator, Generator)

DECLARE_CONVERSIONS(Generator_System, Generator_System)

typedef Generator_System::const_iterator Generator_System_const_iterator;
DECLARE_CONVERSIONS(Generator_System_const_iterator,
                    Generator_System_const_iterator)

DECLARE_CONVERSIONS(Congruence, Congruence)

DECLARE_CONVERSIONS(Congruence_System, Congruence_System)

typedef Congruence_System::const_iterator Congruence_System_const_iterator;
DECLARE_CONVERSIONS(Congruence_System_const_iterator,
                    Congruence_System_const_iterator)

DECLARE_CONVERSIONS(Grid_Generator, Grid_Generator)

DECLARE_CONVERSIONS(Grid_Generator_System, Grid_Generator_System)

typedef Grid_Generator_System::const_iterator
  Grid_Generator_System_const_iterator;
DECLARE_CONVERSIONS(Grid_Generator_System_const_iterator,
                    Grid_Generator_System_const_iterator)

DECLARE_CONVERSIONS(MIP_Problem, MIP_Problem)

inline Relation_Symbol
relation_symbol(enum ppl_enum_Constraint_Type t) {
  switch (t) {
  case PPL_CONSTRAINT_TYPE_LESS_THAN:
    return LESS_THAN;
  case PPL_CONSTRAINT_TYPE_LESS_OR_EQUAL:
    return LESS_OR_EQUAL;
  case PPL_CONSTRAINT_TYPE_EQUAL:
    return EQUAL;
  case PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL:
    return GREATER_OR_EQUAL;
  case PPL_CONSTRAINT_TYPE_GREATER_THAN:
    return GREATER_THAN;
  default:
    return static_cast<Relation_Symbol>(t);
  }
}

} // namespace C_Interface

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_ppl_c_implementation_inlines_hh)
