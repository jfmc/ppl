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

#include "C_Expr.types.hh"
#include "Concrete_Expression.types.hh"
#include "Proxy.defs.hh"

namespace Parma_Polyhedra_Library {

class C_Expr {
  //! Returns the type of \p *this.
  virtual Concrete_Expression_Type type() const = 0;

  //! Returns the kind of \p *this.
  virtual Concrete_Expression_Kind kind() const = 0;
};

class Bin_Op : public C_Expr {
public:
  //! Returns the type of \p *this.
  Concrete_Expression_Type type() const;

  //! Returns the kind of \p *this.
  Concrete_Expression_Kind kind() const;

  //! Returns the left-hand side of \p *this.
  const C_Expr* left_hand_side() const;

  //! Returns the right-hand side of \p *this.
  const C_Expr* right_hand_side() const;

private:
  //! The left-hand side of \p *this.
  const C_Expr* lhs;

  //! The right-hand side of \p *this.
  const C_Expr* rhs;
};

class Un_Op : public C_Expr {
};

class Cast_Op : public C_Expr {
};

class Int_Const : public C_Expr {
};

class Float_Const : public C_Expr {
};

class Appr_Ref : public C_Expr {
};

struct PPL_C_Expr;

template <>
struct Underlying_To_Exposed<PPL_C_Expr, C_Expr> {
  typedef Concrete_Expression<PPL_C_Expr> Type;
};

template <>
struct Underlying_To_Exposed<PPL_C_Expr, Bin_Op> {
  typedef Binary_Operator<PPL_C_Expr> Type;
};

template<>
struct Exposed_To_Underlying<PPL_C_Expr, Concrete_Expression<PPL_C_Expr> > {
  typedef C_Expr Type;
};

template<>
struct Exposed_To_Underlying<PPL_C_Expr, Binary_Operator<PPL_C_Expr> > {
  typedef Bin_Op Type;
};

template <>
class Concrete_Expression<PPL_C_Expr>
  : public Concrete_Expression_Base<PPL_C_Expr>,
    public Proxy<PPL_C_Expr> {
private:
  typedef Exposed_To_Underlying<PPL_C_Expr,
                                Concrete_Expression<PPL_C_Expr> >::Type
  Underlying;

public:
#if 0
  template <typename T>
  static bool classof(const T* expr) {
    return Underlying::classof(underlying(expr));
  }
#endif
};

template <>
class Binary_Operator<PPL_C_Expr>
  : public Binary_Operator_Base<PPL_C_Expr> {
public:
  const Concrete_Expression<PPL_C_Expr>* get_lhs() {
    return exposed(underlying(this)->left_hand_side());
  }
};

} // namespace Parma_Polyhedra_Library

#include "C_Expr.inlines.hh"
//#include "C_Expr.templates.hh"

#endif // !defined(PPL_C_Expr_defs_hh)
