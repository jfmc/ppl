/* Definitions for the C_Expr class and its subclasses: inline functions.
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

#ifndef PPL_C_Expr_inlines_hh
#define PPL_C_Expr_inlines_hh 1

namespace Parma_Polyhedra_Library {

inline
Binary_Operator<C_Expr>
::Binary_Operator(int binary_operator,
                  const Concrete_Expression<C_Expr>* left_hand_side,
                  const Concrete_Expression<C_Expr>* right_hand_side)
  : bop(binary_operator),
    lhs(left_hand_side),
    rhs(right_hand_side) {
}

inline
Binary_Operator<C_Expr>::~Binary_Operator<C_Expr>() {
}

inline Concrete_Expression_BOP
Binary_Operator<C_Expr>::binary_operator() const {
  return bop;
}

inline const Concrete_Expression<C_Expr>*
Binary_Operator<C_Expr>::left_hand_side() const {
  return lhs;
}

inline const Concrete_Expression<C_Expr>*
Binary_Operator<C_Expr>::right_hand_side() const {
  return rhs;
}

inline
Unary_Operator<C_Expr>
::Unary_Operator(int unary_operator,
                 const Concrete_Expression<C_Expr>* argument)
  : uop(unary_operator),
    arg(argument) {
}

inline
Unary_Operator<C_Expr>::~Unary_Operator<C_Expr>() {
}

inline Concrete_Expression_BOP
Unary_Operator<C_Expr>::unary_operator() const {
  return uop;
}

inline const Concrete_Expression<C_Expr>*
Unary_Operator<C_Expr>::argument() const {
  return arg;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_C_Expr_inlines_hh)
