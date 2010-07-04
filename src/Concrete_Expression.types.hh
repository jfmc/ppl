/* Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

This file is free software; as a special exception the author gives
unlimited permission to copy and/or distribute it, with or without
modifications, as long as this notice is preserved.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. */

#ifndef PPL_Concrete_Expression_types_hh
#define PPL_Concrete_Expression_types_hh 1

namespace Parma_Polyhedra_Library {

//! The base class of all concrete expressions.
template <typename Target>
class Concrete_Expression;

//! A binary operator applied to two concrete expressions.
template <typename Target>
class Binary_Operator;

//! A unary operator applied to one concrete expression.
template <typename Target>
class Unary_Operator;

//! A cast operator applied converting one concrete expression to some type.
template <typename Target>
class Cast_Operator;

//! An integer constant concrete expression.
template <typename Target>
class Integer_Constant;

//! A floating-point constant concrete expression.
template <typename Target>
class Floating_Point_Constant;

//! A concrete expression representing a reference to some approximable.
template <typename Target>
class Approximable_Reference;

enum Concrete_Expression_Type  {
  // To be defined with all the floating point formats and using
  // Bounded_Integer_Type_Width, Bounded_Integer_Type_Representation
  // and Bounded_Integer_Type_Overflow.
  A,
  B,
  C
};

/*! \brief
  Encodes the kind of concrete expression.

  The values should be defined by the particular instance
  and uniquely identify one of: Binary_Operator, Unary_Operator,
  Cast_Operator, Integer_Constant, Floating_Point_Constant, or
  Approximable_Reference.  For example, the Binary_Operator kind
  integer constant should be defined by an instance as the member
  <CODE>Binary_Operator<T>::KIND</CODE>
*/
typedef int Concrete_Expression_Kind;

/*! \brief
  Encodes a binary operator of concrete expressions.

  The values should be uniquely defined by the particular instance and
  named: PLUS, MINUS, TIMES, DIV, REM, BAND, BOR, BXOR, LSHIFT,
  RSHIFT.
*/
typedef int Concrete_Expression_BOP;

/*! \brief
  Encodes a unary operator of concrete expressions.

  The values should be uniquely defined by the particular instance and
  named: UPLUS, UMINUS, BNOT.
*/
typedef int Concrete_Expression_UOP;

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Concrete_Expression_types_hh)
