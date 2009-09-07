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

//! The floating point expression on a given format
/*! \ingroup PPL_CXX_Interface
  This class offers a generic implemenation of a
  <EM>floating point expression</EM> of format \f$\mathbf{f}\f$, this includes
  constants, variables of format \f$\mathbf{f}\f$, binary and unary
  arithmetic operators.

  \par Template type parameters
  The class template type parameter \p FP_Interval_Type represents the type
  of the intervals used in the abstract domain. Here we assume that the
  value has floating point type.
  The class template type parameter \p FP_Format represents the format
  of the floating point expression used in the concrete domain.
  This parameter must be a struct which contains \f$3\f$ fields:

  - <CODE>static const unsigned short fraction_bits</CODE> that represents the
    number of bits of the fraction.
  - <CODE>static const unsigned short exponent_bits</CODE> that represents the
    number of bits of the exponent.
  - <CODE>static const unsigned short exponent_bias</CODE> that represents the
    value of exponent bias.
*/
template <typename FP_Interval_Type, typename FP_Format>
class Floating_Point_Expression {

public:

  //! Alias for a linear form with template argument \p FP_Iterval_Type.
  typedef Linear_Form<FP_Interval_Type> FP_Linear_Form;

  //! Alias for a map that associates an index to a type.
  typedef std::map<dimension_type, FP_Interval_Type> FP_Interval_Abstract_Store;

  typedef typename FP_Interval_Type::boundary_type boundary_type;

  typedef typename FP_Interval_Type::info_type info_type;

  //! Destructor.
  virtual ~Floating_Point_Expression();

  /* \brief
     Builds a linear form that correctly approximates the floating point
     expression in the given abstract store.
  */
  virtual void linearize(const FP_Interval_Abstract_Store& store,
                         FP_Linear_Form& result) const = 0;

  //!  Absolute error.
  /* \brief
     Initialized by computing the smallest non-zero positive
     number in the less precise format between the analyzer and the analyzed
     format.
  */
  static const boundary_type absolute_error;

  /* \brief
    Verification if a given linear form overflows.
    Return <CODE>true</CODE> if is bounded, <CODE>false</CODE> otherwise.
  */  // FIXME: this may not be the best place for them.
  static bool overflows(const FP_Linear_Form& lf);

  /* \brief
     Returns a linear form that is used by <CODE>linearize</CODE> to account 
     for the relative errors on \p lf.
  */
  static void relative_error(const FP_Linear_Form& lf,
                             FP_Linear_Form& result);

   /* \brief
     Returns an interval that over approximates the range of \p lf in
     the apstract store \p store.
  */
  static void intervalize(const FP_Linear_Form& lf,
                          const FP_Interval_Abstract_Store& store,
                          FP_Interval_Type& result);

}; // class Floating_Point_Expression


template <typename FP_Interval_Type, typename FP_Format>
const typename Floating_Point_Expression<FP_Interval_Type, FP_Format>::
boundary_type
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
