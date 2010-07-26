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
#include "globals.types.hh"

namespace Parma_Polyhedra_Library {

class Concrete_Expression_Type {
public:
  /*! \brief
    Returns the bounded integer type corresponding to \p width,
    \p representation and \p overflow.
  */
  static Concrete_Expression_Type
  bounded_integer(Bounded_Integer_Type_Width width,
                  Bounded_Integer_Type_Representation representation,
                  Bounded_Integer_Type_Overflow overflow);

  /*! \brief
    Returns the floating point type corresponding to \p format.
  */
  static Concrete_Expression_Type
  floating_point(Floating_Point_Format format);

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this is a bounded
    integer type.
  */
  bool is_bounded_integer() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this is a floating
    point type.
  */
  bool is_floating_point() const;

  /*! \brief
    Returns the width in bits of the bounded integer type encoded by
    \p *this.

    The behavior is undefined if \p *this does not encode a bounded
    integer type.
  */
  Bounded_Integer_Type_Width
  bounded_integer_type_width() const;

  /*! \brief
    Returns the representation of the bounded integer type encoded by
    \p *this.

    The behavior is undefined if \p *this does not encode a bounded
    integer type.
  */
  Bounded_Integer_Type_Representation
  bounded_integer_type_representation() const;

  /*! \brief
    Returns the overflow behavior of the bounded integer type encoded by
    \p *this.

    The behavior is undefined if \p *this does not encode a bounded
    integer type.
  */
  Bounded_Integer_Type_Overflow
  bounded_integer_type_overflow() const;

  /*! \brief
    Returns the format of the floating point type encoded by \p *this.

    The behavior is undefined if \p *this does not encode a floating
    point type.
  */
  Floating_Point_Format floating_point_format() const;

  //! Checks if all the invariants are satisfied.
  bool OK() const;

private:
  //! A 32-bit word encoding the type.
  struct Implementation {
    unsigned int bounded_integer:1;
    Bounded_Integer_Type_Width bounded_integer_type_width:23;
    Bounded_Integer_Type_Representation bounded_integer_type_representation:2;
    Bounded_Integer_Type_Overflow bounded_integer_type_overflow:2;
    Floating_Point_Format floating_point_format:4;
  };

  //! Constructor from \p implementation.
  Concrete_Expression_Type(Implementation implementation);

  //! The encoding of \p *this.
  Implementation impl;
};

template <typename Target>
class Concrete_Expression_Base {
public:
  //! Returns the type of \* this.
  Concrete_Expression_Type type() const;

  //! Returns the kind of \* this.
  Concrete_Expression_Kind kind() const;
};

template <typename Target>
class Binary_Operator_Base : public Concrete_Expression<Target> {
public:
  //! Returns a constant identifying the operator of \p *this.
  Concrete_Expression_BOP binary_operator() const;

  //! Returns the left-hand side of \p *this.
  const Concrete_Expression<Target>* left_hand_side() const;

  //! Returns the right-hand side of \p *this.
  const Concrete_Expression<Target>* right_hand_side() const;
};

template <typename Target>
class Unary_Operator_Base : public Concrete_Expression<Target> {
public:
  //! Returns a constant identifying the operator of \p *this.
  Concrete_Expression_UOP unary_operator() const;

  //! Returns the argument \p *this.
  const Concrete_Expression<Target>* argument() const;
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
public:
  /*! \brief
    If \p *this is a variable reference, returns the variable's
    index. Returns <CODE>not_a_dimension()</CODE> otherwise.
  */
  dimension_type associated_dimension() const;
};

} // namespace Parma_Polyhedra_Library

#include "Concrete_Expression.inlines.hh"

#endif // !defined(PPL_Concrete_Expression_defs_hh)
