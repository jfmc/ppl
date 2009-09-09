/* Declarations for the Multiplication_Floating_Point_Expression class and
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

#ifndef PPL_Multiplication_Floating_Point_Expression_defs_hh
#define PPL_Multiplication_Floating_Point_Expression_defs_hh 1

#include "Floating_Point_Expression.defs.hh"
#include "globals.defs.hh"
#include "Multiplication_Floating_Point_Expression.types.hh"
#include <map>

namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Multiplication_Floating_Point_Expression */
template <typename FP_Interval_Type, typename FP_Format>
void swap(Parma_Polyhedra_Library
	  ::Multiplication_Floating_Point_Expression<FP_Interval_Type,
	                                             FP_Format>& x,
	  Parma_Polyhedra_Library
	  ::Multiplication_Floating_Point_Expression<FP_Interval_Type,
	                                             FP_Format>& y);

} // namespace std

namespace Parma_Polyhedra_Library {

/*! \brief
  A generic Multiplication Floating Point Expression.

  \ingroup PPL_CXX_interface

  \par Template type parameters

  - The class template type parameter \p FP_Interval_Type represents the type
  of the intervals used in the abstract domain.
  - The class template type parameter \p FP_Format represents the format
  of the floating point variable used in the concrete domain.

  \par Linearizations of floating-point sum expressions


  Let \f$i + \sum_{v \in \cV}i_{v}v \f$ and
  \f$i' + \sum_{v \in \cV}i'_{v}v \f$
  be two linear forms, \f$\aslf\f$ and \f$\amlf\f$ two sound abstract
  operator on linear forms such that:
  \f[
  \left(i + \sum_{v \in \cV}i_{v}v\right)
  \aslf
  \left(i' + \sum_{v \in \cV}i'_{v}v\right)
  =
  \left(i \asifp i'\right) +
  \sum_{v \in \cV}\left(i_{v} \asifp i'_{v}\right)v,
  \f]
  \f[
  i
  \amlf
  \left(i' + \sum_{v \in \cV}i'_{v}v\right)
  =
  \left(i \amifp i'\right) +
  \sum_{v \in \cV}\left(i \amifp i'_{v}\right)v.
  \f]
  Given an expression \f$[a;b] \otimes e_{2}\f$ and an interval
  abstract store \f$\rho^{\#}\f$, we construct the interval linear form
  \f$\linexpr{[a;b] \otimes e_{2}}\rho^{\#}\f$ on \f$\cV\f$ as follow:
  \f[
  \linexpr{[a;b] \otimes e_{2}}\rho^{\#}
  =
  \left([a;b]
  \amlf
  \linexpr{e_{2}}\rho^{\#}\right)
  \aslf
  \left([a;b]
  \amlf
  \varepsilon_{\mathbf{f}}\left(\linexpr{e_{2}}\rho^{\#}\right)\right)
  \aslf
  mf_{\mathbf{f}}[-1;1],
  \f]
  given an expression \f$e_{1} \otimes [a;b]\f$ and an interval
  abstract store \f$\rho^{\#}\f$, we construct the interval linear form
  \f$\linexpr{e_{1} \otimes [a;b]}\rho^{\#}\f$ on \f$\cV\f$ as
  follow:
  \f[
  \linexpr{e_{1} \otimes [a;b]}\rho^{\#}
  =
  \linexpr{[a;b] \otimes e_{1}}\rho^{\#},
  \f]
  given an expression \f$e_{1} \otimes e_{2}\f$ and an interval
  abstract store \f$\rho^{\#}\f$, we construct the interval linear form
  \f$\linexpr{e_{1} \otimes e_{2}}\rho^{\#}\f$ on \f$\cV\f$ as
  follow:
  \f[
  \linexpr{e_{1} \otimes e_{2}}\rho^{\#}
  =
  \linexpr{\iota\left(\linexpr{e_{1}\rho^{\#}}\right) \otimes e_{2}}\rho^{\#}.
  \f]
  where \f$\varepsilon_{\mathbf{f}}(l)\f$ is the linear form obtained
  by the method <CODE>Floating_Point_Expression::relative_error()</CODE>,
  \f$\iota(l)\rho^{\#}\f$ is the linear form obtained by the method
  <CODE>Floating_Point_Expression::intervalize()</CODE> and
  \f$mf_{\mathbf{f}}[-1;1]\f$ the value of absolute error defined in
  <CODE>Floating_Point_Expression::absolute_error</CODE>.
*/
template <typename FP_Interval_Type, typename FP_Format>
class Multiplication_Floating_Point_Expression
  : public Floating_Point_Expression<FP_Interval_Type, FP_Format> {

public:

  /*! \brief
     Alias for the Linear_Form<FP_Interval_Type> from
     Floating_Point_Expression
  */
  typedef typename
  Floating_Point_Expression<FP_Interval_Type, FP_Format>
  ::FP_Linear_Form FP_Linear_Form;

  /*! \brief
     Alias for the std::map<dimension_type, FP_Interval_Type> from
     Floating_Point_Expression.
  */
  typedef typename
  Floating_Point_Expression<FP_Interval_Type, FP_Format>
  ::FP_Interval_Abstract_Store FP_Interval_Abstract_Store;

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
    Constructor with two parameters: builds the multiplication floating point
    expression from \p x and \p y corresponding to the floating point
    expression \p x * \p y.
  */
  Multiplication_Floating_Point_Expression(
	   Floating_Point_Expression<FP_Interval_Type, FP_Format>* const x,
           Floating_Point_Expression<FP_Interval_Type, FP_Format>* const y);

  //! Destructor.
  ~Multiplication_Floating_Point_Expression();

  //@} // Constructors and Destructor.

  // FIXME: Modify documentation when exceptions are fixed
  // FIXME: Add complete description of method
  /*! \brief
    Linearizes the expression in a given astract state.

     Modiefies a linear form in the abstract store \p store constructed by
     adding the following linear forms:

     \param int_store Interval floating-point store.
     \param lf_store Linear form store.
     \param result The linear form corresponding to \f$e \amlf e' \f$

     \exception Parma_Polyhedra_Library::Linearization_Failed
    Thrown if the method <CODE>linearize</CODE> fails.
  */
  void linearize(const FP_Interval_Abstract_Store& int_store,
		 const FP_Linear_Form_Abstract_Store& lf_store,
                 FP_Linear_Form& result) const;

  //! Swaps \p *this with \p y.
  void swap(Multiplication_Floating_Point_Expression<FP_Interval_Type,
	                                             FP_Format>& y);

private:

  //! Pointer to the first operand.
  Floating_Point_Expression<FP_Interval_Type, FP_Format>* first_operand;
  //! Pointer to the second operand.
  Floating_Point_Expression<FP_Interval_Type, FP_Format>* second_operand;

  #ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  /*! \brief
    Copy constructor: temporary inhibited.
  */
  #endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  Multiplication_Floating_Point_Expression(
         const Multiplication_Floating_Point_Expression<FP_Interval_Type,
                                                        FP_Format>& e);

  #ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  /*! \brief
    Assignment operator: temporary inhibited.
  */
  #endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  Multiplication_Floating_Point_Expression<FP_Interval_Type, FP_Format>&
  operator=(const Multiplication_Floating_Point_Expression<FP_Interval_Type,
	    FP_Format>& e);


}; // class Multiplication_Floating_Point_Expression

} // namespace Parma_Polyhedra_Library

#include "Multiplication_Floating_Point_Expression.inlines.hh"
#include "Multiplication_Floating_Point_Expression.templates.hh"

#endif // !defined(PPL_Multiplication_Floating_Point_Expression_defs_hh)
