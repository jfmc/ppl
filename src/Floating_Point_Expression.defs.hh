/* Declarations for the Floating_Point_Expression class and its constituents.
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Floating_Point_Expression_defs_hh
#define PPL_Floating_Point_Expression_defs_hh 1

#include "globals.defs.hh"
#include "Floating_Point_Expression.types.hh"
#include "Linear_Form.defs.hh"
#include <cmath>
#include <map>

namespace Parma_Polyhedra_Library {

//! Exception class indicating the failure of a linearization attempt.
class Linearization_Failed {};

//! Policy class defining the IEEE754 single precision format.
struct IEEE754_Single {
  static const unsigned short fraction_bits = 23;
  static const unsigned short exponent_bits = 8;
  static const unsigned short exponent_bias = 127;
};

//! Policy class defining the IEEE754 double precision format.
struct IEEE754_Double {
  static const unsigned short fraction_bits = 52;
  static const unsigned short exponent_bits = 11;
  static const unsigned short exponent_bias = 1023;
};

/*! \ingroup PPL_CXX_Interface
  A floating point expression on a given format.

  This class represents a concrete
  <EM>floating point expression</EM> of format \f$$\mathbf{f}$\f$, this 
  includes constants, variables of format \f$$\mathbf{f}$\f$, binary and unary
  arithmetic operators.

  \par Template type parameters

  - The class template type parameter \p FP_Interval_Type represents the type
  of the intervals used in the abstract domain. The interval bounds
  should have a floating point type and open intervals should not be
  allowed, otherwise the analysis will not be sound.
  - The class template type parameter \p FP_Format represents the floating
  point format used in the concrete domain.
  This parameter must be a struct which contains \f$3\f$ fields:
    -# <CODE>static const unsigned short fraction_bits</CODE> that represents
    the number of bits of the fraction.
    -# <CODE>static const unsigned short exponent_bits</CODE> that represents
    the number of bits of the exponent.
    -# <CODE>static const unsigned short exponent_bias</CODE> that represents
    the value of exponent bias.
*/
template <typename FP_Interval_Type, typename FP_Format>
class Floating_Point_Expression {

public:

  //! Alias for a linear form with template argument \p FP_Iterval_Type.
  typedef Linear_Form<FP_Interval_Type> FP_Linear_Form;

  //! Alias for a map that associates a variable index to an interval.
  /*!
    The type a linear form abstract store associating each variable with an
    interval that correctly approximates its value.
  */
  typedef std::map<dimension_type, FP_Interval_Type> FP_Interval_Abstract_Store;

  //! Alias for a map that associates a variable index to a linear form.
  /*!
    The type a linear form abstract store associating each variable with a
    linear form that correctly approximates its value.
  */
  typedef std::map<dimension_type, FP_Linear_Form>
          FP_Linear_Form_Abstract_Store;

  //! The floating point format used by the analyzer.
  typedef typename FP_Interval_Type::boundary_type boundary_type;

  //! The interval policy used by \p FP_Interval_Type.
  typedef typename FP_Interval_Type::info_type info_type;

  //! Destructor.
  virtual ~Floating_Point_Expression();

  //! Linearizes a floating point expression.
  /*! \brief
    Linearizes a floating point expression.

    Makes \p result become a Linear Form that correctly approximates the
    value of the floating point expression in the given composite
    abstract store.
    \param int_store The interval abstract store.
    \param lf_store The linear form abstract store.
    \param result The modified linear form.
 
    All variables occuring in the floating point expression MUST have
    an associated interval in \p int_store.
    If this precondition is not met, calling the method causes an
    undefined behavior.
  */
  virtual void linearize(const FP_Interval_Abstract_Store& int_store,
                         const FP_Linear_Form_Abstract_Store& lf_store,
                         FP_Linear_Form& result) const = 0;

   /*! \brief
    Absolute error.

    Initialized by computing the smallest non-zero positive
    number in the less precise floating point format between the
    analyzer format and the analyzed format.
  */
  static boundary_type absolute_error;


  // FIXME: this may not be the best place for them.
  /*! \brief
    Verifies if a given linear form overflows.
    \param lf The linear form to verify.
    \return
    Returns <CODE>true</CODE> if all coefficients in \p lf are bounded,
    <CODE>false</CODE> otherwise.
  */
  static bool overflows(const FP_Linear_Form& lf);

  /*! \brief
    Computes the relative error of a given linear form.

    Static helper method that is used by <CODE>linearize</CODE>
    to account for the relative errors on \p lf.
    \param lf The linear form used to compute the relative error.
    \param result Becomes the linear form corresponding to a relative
    error committed on \p lf.
  */
  static void relative_error(const FP_Linear_Form& lf,
                             FP_Linear_Form& result);

   /*! \brief
     Makes \p result become an interval that overapproximates all the
     possible values of \p lf in the interval abstract store \p store.
     \param lf The linear form to aproximate.
     \param store The abstract store.
     \param result The linear form that will be modified.
   */
  static void intervalize(const FP_Linear_Form& lf,
                          const FP_Interval_Abstract_Store& store,
                          FP_Interval_Type& result);

}; // class Floating_Point_Expression


template <typename FP_Interval_Type, typename FP_Format>
typename Floating_Point_Expression<FP_Interval_Type, FP_Format>::boundary_type
Floating_Point_Expression<FP_Interval_Type, FP_Format>::absolute_error =
  std::max(static_cast<typename Floating_Point_Expression<FP_Interval_Type,
	   FP_Format>::boundary_type>
           (pow(2, 1 - FP_Format::exponent_bias - FP_Format::fraction_bits)),
  std::numeric_limits<typename Floating_Point_Expression<FP_Interval_Type,
                                                         FP_Format>
	                       ::boundary_type>::denorm_min());

} // namespace Parma_Polyhedra_Library

#include "Floating_Point_Expression.inlines.hh"
#include "Floating_Point_Expression.templates.hh"

#endif // !defined(PPL_Floating_Point_Expression_defs_hh)
