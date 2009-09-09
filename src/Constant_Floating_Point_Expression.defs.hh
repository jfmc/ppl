/* Declarations for the Constant_Floating_Point_Expression class and
   its constituents.
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

#ifndef PPL_Constant_Floating_Point_Expression_defs_hh
#define PPL_Constant_Floating_Point_Expression_defs_hh 1

#include "Floating_Point_Expression.defs.hh"
#include "globals.defs.hh"
#include "Constant_Floating_Point_Expression.types.hh"
#include <map>

namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Constant_Floating_Point_Expression */
template<typename FP_Interval_Type, typename FP_Format>
void swap(Parma_Polyhedra_Library::Constant_Floating_Point_Expression<
                                   FP_Interval_Type, FP_Format>& x,
          Parma_Polyhedra_Library::Constant_Floating_Point_Expression<
                                   FP_Interval_Type, FP_Format>& y);

} // namespace std

namespace Parma_Polyhedra_Library {

/*! \brief
  A generic Constant Floating Point Expression.

  \ingroup PPL_CXX_interface

  \par Template type parameters

  - The class template type parameter \p FP_Interval_Type represents the type
  of the intervals used in the abstract domain.
  - The class template type parameter \p FP_Format represents the format
  of the floating point variable used in the concrete domain.

  \par Linearizations of floating-point constant expressions

  Given a costant expression \f$\textrm{const}_{\mathbf{f},\mathbf{r}}(c)\f$
  and an interval abstract store \f$\rho^{\#}\f$ where \f$\mathbf{f}\f$ is a
  floating point format and \f$\mathbf{r}\f$ a rounding mode,
  we construct the interval linear form
  \f$\linexpr{\textrm{const}_{\mathbf{f},\mathbf{r}}(c)}\f$ as
  follow:
  \f[
  \linexpr{\textrm{const}_{\mathbf{f},\mathbf{r}}(c)}\rho^{\#} =
  [\textrm{const}_{\mathbf{f},-\infty}(c);
  \textrm{const}_{\mathbf{f},+\infty}(c)].
  \f]
 */
template <typename FP_Interval_Type, typename FP_Format>
class Constant_Floating_Point_Expression
  : public Floating_Point_Expression<FP_Interval_Type, FP_Format> {

public:

  /*! \brief
     Alias for the Linear_Form<FP_Interval_Type> from
     Floating_Point_Expression
  */
  typedef typename
  Floating_Point_Expression<FP_Interval_Type, FP_Format>::
  FP_Linear_Form FP_Linear_Form;

  /*! \brief
     Alias for the std::map<dimension_type, FP_Interval_Type> from
     Floating_Point_Expression.
  */
  typedef typename
  Floating_Point_Expression<FP_Interval_Type, FP_Format>::
  FP_Interval_Abstract_Store FP_Interval_Abstract_Store;

  typedef typename
  Floating_Point_Expression<FP_Interval_Type, FP_Format>::
  FP_Linear_Form_Abstract_Store FP_Linear_Form_Abstract_Store;

  /*! \brief
     Alias for the P_Interval_Type::boundary_type from
     Floating_Point_Expression.
  */
  typedef typename
  Floating_Point_Expression<FP_Interval_Type, FP_Format>::boundary_type
  boundary_type;

  /*! \brief
     Alias for the P_Interval_Type::info_type from Floating_Point_Expression.
  */
  typedef typename
  Floating_Point_Expression<FP_Interval_Type, FP_Format>::info_type info_type;

  //! \name Constructors and Destructor
  //@{
  /*! \brief
    Constructor with two parameters: builds the constant floating point
    expression from \p lower_bound and \p upper_bound.
  */
  Constant_Floating_Point_Expression(const boundary_type lower_bound,
                                     const boundary_type upper_bound);

  //! Destructor.
  ~Constant_Floating_Point_Expression();

  //@} // Constructors and Destructor

    // FIXME: Modify documentation when exceptions are fixed.
  /*! \brief
    Linearizes the expression in a given astract state.

    Modifies a linear form \p result in the abstract store \p store
    corresponding to an inhomogenous term which over-approximates
    *this.value.

    \param int_store Interval floating-point store.
    \param lf_store Linear form store.
    \param result The modified linear form.

     \exception Parma_Polyhedra_Library::Linearization_Failed
    Thrown if the method <CODE>linearize</CODE> fails.
  */
  void linearize(const FP_Interval_Abstract_Store& int_store,
                 const FP_Linear_Form_Abstract_Store& lf_store,
                 FP_Linear_Form& result) const;

  //!  Swaps \p *this with \p y.
  void swap(Constant_Floating_Point_Expression& y);

private:

  // FIXME: this is a temporary solution: we should find a way to convert
  // a floating point with an arbitrary format to an interval.
  FP_Interval_Type value;

  #ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  /*! \brief
    Copy constructor: temporary inhibited.
  */
  #endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  Constant_Floating_Point_Expression(
			  const Constant_Floating_Point_Expression& y);

  #ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  /*! \brief
    Assignment operator: temporary inhibited.
  */
  #endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAIL
  Constant_Floating_Point_Expression& operator=(
		          const Constant_Floating_Point_Expression& y);

}; // class Constant_Floating_Point_Expression

} // namespace Parma_Polyhedra_Library

#include "Constant_Floating_Point_Expression.inlines.hh"

#endif // !defined(PPL_Constant_Floating_Point_Expression_defs_hh)
