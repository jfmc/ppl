/* Concrete_Expression class declaration.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Concrete_Expression_defs_hh
#define PPL_Concrete_Expression_defs_hh 1

#include "Concrete_Expression.types.hh"

namespace Parma_Polyhedra_Library {


template <typename Target>
class Concrete_Expression_Base {
  //! Returns the type of \* this.
  Concrete_Expression_Type type() const;

  //! Returns the kind of \* this.
  Concrete_Expression_Kind kind() const;
};

template <typename Target>
class Binary_Operator_Base : public Concrete_Expression<Target> {
};

template <typename Target>
class Unary_Operator_Base : public Concrete_Expression<Target> {
};

template <typename Target>
class Cast_Operator_Base : public Concrete_Expression<Target> {
};

template <typename Target>
class Integer_Constant_Base : public Concrete_Expression<Target> {
};

template <typename Target>
class Floating_Point_Constant_Base : public Concrete_Expression<Target> {
};

template <typename Target>
class Approximable_Reference_Base : public Concrete_Expression<Target> {
};

} // namespace Parma_Polyhedra_Library

#include "Concrete_Expression.inlines.hh"

#endif // !defined(PPL_Concrete_Expression_defs_hh)
