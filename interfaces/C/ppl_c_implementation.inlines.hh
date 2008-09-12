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

#define DECLARE_CONVERSIONS(Type, CPP_Type) \
inline const CPP_Type* \
to_const(ppl_const_ ## Type ## _t x) { \
  return reinterpret_cast<const CPP_Type*>(x); \
} \
 \
inline CPP_Type* \
to_nonconst(ppl_ ## Type ## _t x) { \
  return reinterpret_cast<CPP_Type*>(x); \
} \
 \
inline ppl_const_ ## Type ## _t \
to_const(const CPP_Type* x) { \
  return reinterpret_cast<ppl_const_ ## Type ## _t>(x); \
} \
 \
inline ppl_ ## Type ## _t \
to_nonconst(CPP_Type* x) { \
  return reinterpret_cast<ppl_ ## Type ## _t>(x); \
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

} // namespace C_Interface

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_ppl_c_implementation_inlines_hh)
