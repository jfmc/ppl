/* Linearization function implementation.
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
  PPL_ASSERT(uop_expr.get_uop() == BNOT);

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
static bool
add_linearize(const Binary_Operator<Target>& bop_expr,
              const Box<FP_Interval_Type>& int_store,
              const std::map<dimension_type, Linear_Form<FP_Interval_Type>>& lf_store,
              Linear_Form<FP_Interval_Type>& result) {
  PPL_ASSERT(bop_expr.get_bop() == ADD);

  typedef typename FP_Interval_Type::boundary_type analyzer_format;
  typedef Linear_Form<FP_Interval_Type> FP_Linear_Form;
  typedef Box<FP_Interval_Type> FP_Interval_Abstract_Store;
  typedef std::map<dimension_type, FP_Linear_Form> FP_Linear_Form_Abstract_Store;

  if (!linearize(*(bop_expr.get_lhs()), int_store, lf_store, result))
    return false;

  Floating_Point_Format analyzed_format = bop_expr.floating_point_format();
  FP_Linear_Form rel_error;
  result.relative_error(analyzed_format, rel_error);
  result += rel_error;
  FP_Linear_Form linearized_second_operand;
  if (!linearize(*(bop_expr.get_rhs()), int_store, lf_store,
                 linearized_second_operand))
    return false;

  result += linearized_second_operand;
  linearized_second_operand.relative_error(analyzed_format, rel_error);
  result += rel_error;
  FP_Interval_Type absolute_error =
                   compute_absolute_error<FP_Interval_Type>(analyzed_format);
  result += absolute_error;
  return !result.overflows();
}

template <typename Target, typename FP_Interval_Type>
static bool
sub_linearize(const Binary_Operator<Target>& bop_expr,
              const Box<FP_Interval_Type>& int_store,
              const std::map<dimension_type, Linear_Form<FP_Interval_Type>>& lf_store,
              Linear_Form<FP_Interval_Type>& result) {
  PPL_ASSERT(bop_expr.get_bop() == SUB);

  typedef typename FP_Interval_Type::boundary_type analyzer_format;
  typedef Linear_Form<FP_Interval_Type> FP_Linear_Form;
  typedef Box<FP_Interval_Type> FP_Interval_Abstract_Store;
  typedef std::map<dimension_type, FP_Linear_Form> FP_Linear_Form_Abstract_Store;

  if (!linearize(*(bop_expr.get_lhs()), int_store, lf_store, result))
    return false;

  Floating_Point_Format analyzed_format = bop_expr.floating_point_format();
  FP_Linear_Form rel_error;
  result.relative_error(analyzed_format, rel_error);
  result += rel_error;
  FP_Linear_Form linearized_second_operand;
  if (!linearize(*(bop_expr.get_rhs()), int_store, lf_store,
                 linearized_second_operand))
    return false;

  result -= linearized_second_operand;
  linearized_second_operand.relative_error(analyzed_format, rel_error);
  result += rel_error;
  FP_Interval_Type absolute_error =
                   compute_absolute_error<FP_Interval_Type>(analyzed_format);
  result += absolute_error;
  return !result.overflows();
}

template <typename Target, typename FP_Interval_Type>
static bool
mul_linearize(const Binary_Operator<Target>& bop_expr,
              const Box<FP_Interval_Type>& int_store,
              const std::map<dimension_type, Linear_Form<FP_Interval_Type>>& lf_store,
              Linear_Form<FP_Interval_Type>& result) {
  PPL_ASSERT(bop_expr.get_bop() == MUL);

  typedef typename FP_Interval_Type::boundary_type analyzer_format;
  typedef Linear_Form<FP_Interval_Type> FP_Linear_Form;
  typedef Box<FP_Interval_Type> FP_Interval_Abstract_Store;
  typedef std::map<dimension_type, FP_Linear_Form> FP_Linear_Form_Abstract_Store;

  /*
    FIXME: We currently adopt the "Interval-Size Local" strategy in order to
    decide which of the two linear forms must be intervalized, as described
    in Section 6.2.4 ("Multiplication Strategies") of Antoine Mine's Ph.D.
    thesis "Weakly Relational Numerical Abstract Domains".
    In this Section are also described other multiplication strategies, such
    as All-Cases, Relative-Size Local, Simplification-Driven Global and
    Homogeneity Global.
  */

  // Here we choose which of the two linear forms must be intervalized.

  // true if we intervalize the first form, false if we intervalize the second.
  bool intervalize_first;
  FP_Linear_Form linearized_first_operand;
  if (!linearize(*(bop_expr.get_lhs()), int_store, lf_store,
                 linearized_first_operand))
    return false;
  FP_Interval_Type intervalized_first_operand;
  linearized_first_operand.intervalize(int_store, intervalized_first_operand);
  FP_Linear_Form linearized_second_operand;
  if (!linearize(*(bop_expr.get_rhs()), int_store, lf_store,
                 linearized_second_operand))
    return false;
  FP_Interval_Type intervalized_second_operand;
  linearized_second_operand.intervalize(int_store, intervalized_second_operand);
  boundary_type first_interval_size, second_interval_size;

  // FIXME: we are not sure that what we do here is policy-proof.
  if (intervalized_first_operand.is_bounded()) {
    if (intervalized_second_operand.is_bounded()) {
      first_interval_size = intervalized_first_operand.upper() -
                            intervalized_first_operand.lower();
      second_interval_size = intervalized_second_operand.upper() -
                             intervalized_second_operand.lower();
      if (first_interval_size <= second_interval_size)
        intervalize_first = true;
      else
        intervalize_first = false;
    }
    else
      intervalize_first = true;
  }
  else {
    if (intervalized_second_operand.is_bounded())
      intervalize_first = false;
    else
      return false;
  }

  // Here we do the actual computation.
  // For optimizing, we store the relative error directly into result.
  Floating_Point_Format analyzed_format = bop_expr.floating_point_format();
  if (intervalize_first) {
    linearized_second_operand.relative_error(analyzed_format, result);
    linearized_second_operand *= intervalized_first_operand;
    result *= intervalized_first_operand;
    result += linearized_second_operand;
  }
  else {
    linearized_first_operand.relative_error(analyzed_format, result);
    linearized_first_operand *= intervalized_second_operand;
    result *= intervalized_second_operand;
    result += linearized_first_operand;
  }

  FP_Interval_Type absolute_error =
                   compute_absolute_error<FP_Interval_Type>(analyzed_format);
  result += absolute_error;
  return !result.overflows();
}

template <typename Target, typename FP_Interval_Type>
static bool
div_linearize(const Binary_Operator<Target>& bop_expr,
              const Box<FP_Interval_Type>& int_store,
              const std::map<dimension_type, Linear_Form<FP_Interval_Type>>& lf_store,
              Linear_Form<FP_Interval_Type>& result) {
  PPL_ASSERT(bop_expr.get_bop() == DIV);

  typedef typename FP_Interval_Type::boundary_type analyzer_format;
  typedef Linear_Form<FP_Interval_Type> FP_Linear_Form;
  typedef Box<FP_Interval_Type> FP_Interval_Abstract_Store;
  typedef std::map<dimension_type, FP_Linear_Form> FP_Linear_Form_Abstract_Store;

  FP_Linear_Form linearized_second_operand;
  if (!linearize(*(bop_expr.get_rhs()), int_store, lf_store,
                 linearized_second_operand))
    return false;
  FP_Interval_Type intervalized_second_operand;
  linearized_second_operand.intervalize(int_store, intervalized_second_operand);

  // Check if we may divide by zero.
  if ((intervalized_second_operand.is_lower_boundary_infinity() ||
       intervalized_second_operand.lower() <= 0) &&
      (intervalized_second_operand.is_upper_boundary_infinity() ||
       intervalized_second_operand.upper() >= 0))
    return false;

  if (!linearize(*(bop_expr.get_lhs()), int_store, lf_store, result))
    return false;

  Floating_Point_Format analyzed_format = bop_expr.floating_point_format();
  FP_Linear_Form rel_error;
  result.relative_error(analyzed_format, rel_error);
  result /= intervalized_second_operand;
  rel_error /= intervalized_second_operand;
  result += rel_error;
  FP_Interval_Type absolute_error =
                   compute_absolute_error<FP_Interval_Type>(analyzed_format);
  result += absolute_error;
  return !result.overflows();
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

  switch(expr.kind()) {
  case Integer_Constant<Target>::KIND:
    // TODO.
    break;
  case Floating_Point_Constant<Target>::KIND:
    Floating_Point_Constant<Target> fpc_expr =
      static_cast<Floating_Point_Constant<Target>>(expr);
    result = FP_Linear_Form(FP_Interval(fpc_expr.get_value_as_string()));
    return true;
    break;
  case Unary_Operator<Target>::KIND:
    Unary_Operator<Target> uop_expr =
      static_cast<Unary_Operator<Target>>(expr);
    switch (uop_expr.get_uop()) {
    case Unary_Operator<Target>::UPLUS:
      return linearize(uop_expr.get_arg(), int_store, lf_store, result);
      break;
    case Unary_Operator<Target>::UMINUS:
      if (!linearize(uop_expr.get_arg(), int_store, lf_store, result))
        return false;

      result.negate();
      return true;
      break;
    case Unary_Operator<Target>::BNOT:
      return bnot_linearize(uop_expr, int_store, lf_store, result);
      break;
    default:
      throw std::runtime_error("PPL internal error");
    }
    break;
  case Binary_Operator<Target>::KIND:
    Binary_Operator<Target> bop_expr =
      static_cast<Binary_Operator<Target>>(expr);
    switch (bop_expr.get_bop()) {
    case Binary_Operator<Target>::ADD:
      return add_linearize(bop_expr, int_store, lf_store, result);
      break;
    case Binary_Operator<Target>::SUB:
      return sub_linearize(bop_expr, int_store, lf_store, result);
      break;
    case Binary_Operator<Target>::MUL:
      return mul_linearize(bop_expr, int_store, lf_store, result);
      break;
    case Binary_Operator<Target>::DIV:
      return div_linearize(bop_expr, int_store, lf_store, result);
      break;
    case Binary_Operator<Target>::REM:
    case Binary_Operator<Target>::BAND:
    case Binary_Operator<Target>::BOR:
    case Binary_Operator<Target>::BXOR:
    case Binary_Operator<Target>::LSHIFT:
    case Binary_Operator<Target>::RSHIFT:
      // FIXME: can we do better?
      return false;
      break;
    default:
      throw std::runtime_error("PPL internal error");
    }
    break;
  case Approximable_Reference<Target>::KIND:
    // TODO.
    break;
  case Cast_Operator<Target>::KIND:
    // TODO.
    break;
  default:
    throw std::runtime_error("PPL internal error");
  }
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_linearize_hh)
