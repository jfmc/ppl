1/* Linearization function implementation.
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

#ifndef PPL_linearize_hh
#define PPL_linearize_hh 1

#include "Concrete_Expression.defs.hh"
#include "Float.defs.hh"
#include "Linear_Form.defs.hh"
#include "Box.defs.hh"
#include <map>

namespace Parma_Polyhedra_Library {

template <typename Target, typename FP_Interval_Type>
static bool
bnot_linearize(const Unary_Operator<Target>& uop_expr,
               const Box<FP_Interval_Type>& int_store,
               const std::map<dimension_type, Linear_Form<FP_Interval_Type>>& lf_store,
               Linear_Form<FP_Interval_Type>& result) {
  typedef typename FP_Interval_Type::boundary_type analyzer_format;
  typedef Linear_Form<FP_Interval_Type> FP_Linear_Form;
  typedef Box<FP_Interval_Type> FP_Interval_Abstract_Store;
  typedef std::map<dimension_type, FP_Linear_Form> FP_Linear_Form_Abstract_Store;

  if (!linearize(uop_expr.get_arg(), int_store, lf_store, result))
    return false;

  FP_Interval_Type int_r;
  result.intervalize(int_store, int_r);
  bool lb_is_positive = (!int_r.is_lower_boundary_infinity() &&
                         int_r.lower() > 0);
  bool ub_is_negative = (!int_r.is_upper_boundary_infinity() &&
                         int_r.upper() < 0);
  if (lb_is_positive || ub_is_negative) {
    result = FP_Linear_Form(FP_Interval(0));
    return true;
  }
  else if (int_r.is_singleton()) {
    // Here int_r is the singleton of 0.
    // FIXME: Check if the negation of 0 MUST be 1.
    result = FP_Linear_Form(FP_Interval(1));
    return true;
  }
  else
    // Here int_r strictly contains 0.
    return false;
}

template <typename Target, typename FP_Interval_Type>
bool
linearize(const Concrete_Expression<Target>& expr,
          const Box<FP_Interval_Type>& int_store,
          const std::map<dimension_type, Linear_Form<FP_Interval_Type>>& lf_store,
          Linear_Form<FP_Interval_Type>& result) {
  typedef typename FP_Interval_Type::boundary_type analyzer_format;
  typedef Linear_Form<FP_Interval_Type> FP_Linear_Form;
  typedef Box<FP_Interval_Type> FP_Interval_Abstract_Store;
  typedef std::map<dimension_type, FP_Linear_Form> FP_Linear_Form_Abstract_Store;

  // Check that analyzer_format is a floating point type.
  PPL_COMPILE_TIME_CHECK(!std::numeric_limits<analyzer_format>::is_exact,
      "linearize<Target, FP_Interval_Type>:"
      " FP_Interval_Type is not the type of an interval with floating point boundaries.");

  // Check that we are dealing with an expression of floating point type.
  PPL_ASSERT(expr.is_floating_point());

  /*
  Floating_Point_Format analyzed_format = expr.floating_point_format();
  FP_Interval_Type absolute_error =
                   compute_absolute_error<FP_Interval_Type>(analyzed_format);
  */
  switch(expr.kind()) {
  case INT_CON:
    // TODO.
    break;
  case FP_CON:
    Floating_Point_Constant<Target> fpc_expr =
      static_cast<Floating_Point_Constant<Target>>(expr);
    result = FP_Linear_Form(FP_Interval(fpc_expr.get_value_as_string()));
    return true;
    break;
  case UOP:
    Unary_Operator<Target> uop_expr =
      static_cast<Unary_Operator<Target>>(expr);
    switch (uop_expr.get_uop()) {
    case PLUS:
      return linearize(uop_expr.get_arg(), int_store, lf_store, result);
      break;
    case MINUS:
      if (!linearize(uop_expr.get_arg(), int_store, lf_store, result))
        return false;

      result.negate();
      return true;
      break;
    case BNOT:
      return bnot_linearize(uop_expr, int_store, lf_store, result);
      break;
    default:
      throw std::runtime_error("PPL internal error");
    }
    break;
  case BOP:
    Binary_Operator<Target> bop_expr =
      static_cast<Binary_Operator<Target>>(expr);
    switch (bop_expr.get_bop()) {
    case ADD:
      return add_linearize(bop_expr, int_store, lf_store, result);
      break;
    case SUB:
      return sub_linearize(bop_expr, int_store, lf_store, result);
      break;
    case MUL:
      return mul_linearize(bop_expr, int_store, lf_store, result);
      break;
    case DIV:
      return div_linearize(bop_expr, int_store, lf_store, result);
      break;
    case REM:
    case BAND:
    case BOR:
    case BXOR:
    case LSHIFT:
    case RSHIFT:
    default:
      throw std::runtime_error("PPL internal error");
    }
    break;
  case CAST:
    // TODO.
    break;
  default:
    throw std::runtime_error("PPL internal error");
  }
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_linearize_hh)
