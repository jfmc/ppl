/* Declarations for the C_Expr class and its subclasses.
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

#ifndef PPL_C_Expr_defs_hh
#define PPL_C_Expr_defs_hh 1

#include "Concrete_Expression.defs.hh"

namespace Parma_Polyhedra_Library {

struct C_Expr;

template <>
class Concrete_Expression<C_Expr> : public Concrete_Expression_Base<C_Expr> {
public:
  //! Returns the type of \* this.
  Concrete_Expression_Type type() const;

  //! Returns the kind of \* this.
  virtual Concrete_Expression_Kind kind() const = 0;
};

template <>
class Binary_Operator<C_Expr> : public Binary_Operator_Base<C_Expr> {
public:
  //! Constructor from operator, lhs and rhs.
  Binary_Operator<C_Expr>(int binary_operator,
                          const Concrete_Expression<C_Expr>* left_hand_side,
                          const Concrete_Expression<C_Expr>* right_hand_side);

  //! Do-nothing destructor.
  ~Binary_Operator<C_Expr>();

  //! Returns the type of \p *this.
  Concrete_Expression_Type type() const;

  //! Returns the kind of \p *this.
  Concrete_Expression_Kind kind() const;

  //! Returns the binary operator of \p *this.
  Concrete_Expression_BOP binary_operator() const;

  //! Returns the left-hand side of \p *this.
  const Concrete_Expression<C_Expr>* left_hand_side() const;

  //! Returns the right-hand side of \p *this.
  const Concrete_Expression<C_Expr>* right_hand_side() const;

  //! Constant identifying binary operator nodes.
  enum {
    KIND = 1
  };

  //! Constants encoding the different binary operators.
  enum {
    ADD,
    SUB,
    MUL,
    DIV,
    REM,
    BAND,
    BOR,
    BXOR,
    LSHIFT,
    RSHIFT
  };

private:
  //! The operator of \p *this.
  const int bop;

  //! The left-hand side of \p *this.
  const Concrete_Expression<C_Expr>* lhs;

  //! The right-hand side of \p *this.
  const Concrete_Expression<C_Expr>* rhs;
};

template <>
class Unary_Operator<C_Expr> : public Unary_Operator_Base<C_Expr> {
public:
  //! Constructor from operator and argument.
  Unary_Operator<C_Expr>(int unary_operator,
                         const Concrete_Expression<C_Expr>* argument);

  //! Do-nothing destructor.
  ~Unary_Operator<C_Expr>();

  //! Returns the type of \p *this.
  Concrete_Expression_Type type() const;

  //! Returns the kind of \p *this.
  Concrete_Expression_Kind kind() const;

  //! Returns the unary operator of \p *this.
  Concrete_Expression_UOP unary_operator() const;

  //! Returns the argument of \p *this.
  const Concrete_Expression<C_Expr>* argument() const;

  //! Constant identifying unary operator nodes.
  enum {
    KIND = 2
  };

  //! Constants encoding the different unary operators.
  enum {
    UPLUS,
    UMINUS,
    BNOT
  };

private:
  //! The operator of \p *this.
  const int uop;

  //! The argument of \p *this.
  const Concrete_Expression<C_Expr>* arg;
};

template <>
class Cast_Operator<C_Expr>
  : public Cast_Operator_Base<C_Expr> {
public:
  //! Constant identifying cast nodes.
  enum { KIND = 3 };
};

template <>
class Integer_Constant<C_Expr>
  : public Integer_Constant_Base<C_Expr> {
public:
  //! Constant identifying integer constant nodes.
  enum { KIND = 4 };
};

template <>
class Floating_Point_Constant<C_Expr>
  : public Floating_Point_Constant_Base<C_Expr> {
public:
  //! Constant identifying floating constant nodes.
  enum { KIND = 5 };
};

// We currently only consider variable references.
template <>
class Approximable_Reference<C_Expr>
  : public Approximable_Reference_Base<C_Expr> {
public:
  //! Builds a reference to the variable having the given index.
  Approximable_Reference<C_Expr>(dimension_type var_index);

  //! Do-nothing destructor.
  ~Approximable_Reference<C_Expr>();

  //! Returns the type of \p *this.
  Concrete_Expression_Type type() const;

  //! Returns the kind of \p *this.
  Concrete_Expression_Kind kind() const;

  /*! \brief
    If \p *this is a variable reference, returns the variable's
    index. Returns <CODE>not_a_dimension()</CODE> otherwise.
  */
  dimension_type associated_dimension() const;

  //! Constant identifying approximable reference nodes.
  enum { KIND = 6 };

private:
  //! The index of the referenced variable.
  dimension_type var_dimension;
};

} // namespace Parma_Polyhedra_Library

#include "C_Expr.inlines.hh"
//#include "C_Expr.templates.hh"

#endif // !defined(PPL_C_Expr_defs_hh)
