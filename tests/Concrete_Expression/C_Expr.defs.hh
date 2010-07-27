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

enum C_Expr_Kind {
  BOP,
  UOP,
  CAST,
  INT_CON,
  FP_CON,
  APPROX_REF
};

template <>
class Concrete_Expression<C_Expr> : public Concrete_Expression_Common<C_Expr> {
public:
  //! Builds a concrete expression of the given kind.
  Concrete_Expression<C_Expr>(C_Expr_Kind KIND);

  //! Returns the type of \* this.
  Concrete_Expression_Type type() const;

  //! Returns the kind of \* this.
  Concrete_Expression_Kind kind() const;

private:
  //! The expression's kind.
  C_Expr_Kind expr_kind;
};

template <>
class Binary_Operator<C_Expr> : public Concrete_Expression<C_Expr>,
                                public Binary_Operator_Common<C_Expr> {
public:
  //! Constructor from operator, lhs and rhs.
  Binary_Operator<C_Expr>(Concrete_Expression_BOP binary_operator,
                          const Concrete_Expression<C_Expr>* left_hand_side,
                          const Concrete_Expression<C_Expr>* right_hand_side);

  //! Do-nothing destructor.
  ~Binary_Operator<C_Expr>();

  //! Returns the type of \p *this.
  Concrete_Expression_Type type() const;

  //! Returns the binary operator of \p *this.
  Concrete_Expression_BOP binary_operator() const;

  //! Returns the left-hand side of \p *this.
  const Concrete_Expression<C_Expr>* left_hand_side() const;

  //! Returns the right-hand side of \p *this.
  const Concrete_Expression<C_Expr>* right_hand_side() const;

  //! Constant identifying binary operator nodes.
  enum {
    KIND = BOP
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
  const Concrete_Expression_BOP bop;

  //! The left-hand side of \p *this.
  const Concrete_Expression<C_Expr>* lhs;

  //! The right-hand side of \p *this.
  const Concrete_Expression<C_Expr>* rhs;
};

template <>
class Unary_Operator<C_Expr> : public Concrete_Expression<C_Expr>,
                               public Unary_Operator_Common<C_Expr> {
public:
  //! Constructor from operator and argument.
  Unary_Operator<C_Expr>(Concrete_Expression_UOP unary_operator,
                         const Concrete_Expression<C_Expr>* argument);

  //! Do-nothing destructor.
  ~Unary_Operator<C_Expr>();

  //! Returns the type of \p *this.
  Concrete_Expression_Type type() const;

  //! Returns the unary operator of \p *this.
  Concrete_Expression_UOP unary_operator() const;

  //! Returns the argument of \p *this.
  const Concrete_Expression<C_Expr>* argument() const;

  //! Constant identifying unary operator nodes.
  enum {
    KIND = UOP
  };

  //! Constants encoding the different unary operators.
  enum {
    UPLUS,
    UMINUS,
    BNOT
  };

private:
  //! The operator of \p *this.
  const Concrete_Expression_UOP uop;

  //! The argument of \p *this.
  const Concrete_Expression<C_Expr>* arg;
};

template <>
class Cast_Operator<C_Expr>
  : public Concrete_Expression<C_Expr>,
    public Cast_Operator_Common<C_Expr> {
public:
  //! Constructor from cast type and argument.
  Cast_Operator<C_Expr>(Concrete_Expression_Type c_type,
                        const Concrete_Expression<C_Expr>* ar);

  //! Do-nothing destructor.
  ~Cast_Operator<C_Expr>();

  //! Returns the type of \p *this.
  Concrete_Expression_Type type() const;

  //! Returns the casted expression.
  const Concrete_Expression<C_Expr>* argument() const;

  //! Constant identifying cast nodes.
  enum { KIND = CAST };

private:
  //! The type of the cast expression.
  Concrete_Expression_Type cast_type;

  //! The casted expression.
  const Concrete_Expression<C_Expr>* arg;
};

template <>
class Integer_Constant<C_Expr>
  : public Concrete_Expression<C_Expr>,
    public Integer_Constant_Common<C_Expr> {
public:
  //! Do-nothing destructor.
  ~Integer_Constant<C_Expr>();

  //! Returns the type of \p *this.
  Concrete_Expression_Type type() const;

  //! Constant identifying integer constant nodes.
  enum { KIND = INT_CON };
};

template <>
class Floating_Point_Constant<C_Expr>
  : public Concrete_Expression<C_Expr>,
    public Floating_Point_Constant_Common<C_Expr> {
public:
  //! Constructor from value.
  Floating_Point_Constant<C_Expr>(const char* value_string,
                                  unsigned int string_size);

  //! Do-nothing destructor.
  ~Floating_Point_Constant<C_Expr>();

  //! Returns the type of \p *this.
  Concrete_Expression_Type type() const;

  /*! \brief
    Returns a string for the floating point constant as written
    in the analyzed program.
  */
  const char* get_value_as_string() const;

  //! Constant identifying floating constant nodes.
  enum { KIND = FP_CON };

private:
  //! The floating point constant as written.
  char* value;
};

// We currently only consider references to floating point variables.
template <>
class Approximable_Reference<C_Expr>
  : public Concrete_Expression<C_Expr>,
    public Approximable_Reference_Common<C_Expr> {
public:
  //! Builds a reference to the variable having the given index.
  Approximable_Reference<C_Expr>(dimension_type var_index);

  //! Do-nothing destructor.
  ~Approximable_Reference<C_Expr>();

  //! Returns the type of \p *this.
  Concrete_Expression_Type type() const;

  /*! \brief
    If \p *this is a variable reference, returns the variable's
    index. Returns <CODE>not_a_dimension()</CODE> otherwise.
  */
  dimension_type associated_dimension() const;

  //! Constant identifying approximable reference nodes.
  enum { KIND = APPROX_REF };

private:
  //! The index of the referenced variable.
  dimension_type var_dimension;
};

} // namespace Parma_Polyhedra_Library

#include "C_Expr.inlines.hh"
//#include "C_Expr.templates.hh"

#endif // !defined(PPL_C_Expr_defs_hh)
