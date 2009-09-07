/* Declarations for the Division_Floating_Point_Expression class and its
   constituents.
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

#ifndef PPL_Division_Floating_Point_Expression_defs_hh
#define PPL_Division_Floating_Point_Expression_defs_hh 1

#include "Floating_Point_Expression.defs.hh"
#include "globals.defs.hh"
#include "Division_Floating_Point_Expression.types.hh"
#include <map>

namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Division_Floating_Point_Expression */
template <typename FP_Interval_Type, typename FP_Format>
void swap(Parma_Polyhedra_Library
	  ::Division_Floating_Point_Expression<FP_Interval_Type, FP_Format>& x,
	  Parma_Polyhedra_Library
	  ::Division_Floating_Point_Expression<FP_Interval_Type, FP_Format>& y);

} // namespace std

namespace Parma_Polyhedra_Library {

//! A generic Division Floating Point Expression
/*! \ingroup PPL_CXX_interface
  \par Template type parameters
  - The class template type parameter \p FP_Interval_Type represents the type
  of the intervals used in the abstract domain.
  - The class template type parameter \p FP_Format represents the format
  of the floating point variable used in the concrete domain.
*/
template <typename FP_Interval_Type, typename FP_Format>
class Division_Floating_Point_Expression
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
    Constructor with two parameters: builds the division floating point
    expression from \p num and \p den corresponding to the floating point
    expression \p num / \p den.
  */
  Division_Floating_Point_Expression(
	   Floating_Point_Expression<FP_Interval_Type, FP_Format>* const num,
           Floating_Point_Expression<FP_Interval_Type, FP_Format>* const den);

  //! Destructor.
  ~Division_Floating_Point_Expression();

  //@} // Constructors and Destructor

  // TODO: comment
  void linearize(const FP_Interval_Abstract_Store& store,
                 FP_Linear_Form& result) const;

  //! Swaps \p *this with \p y.
  void swap(Division_Floating_Point_Expression<FP_Interval_Type,
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
  Division_Floating_Point_Expression(
         const Division_Floating_Point_Expression<FP_Interval_Type,
                                                  FP_Format>& e);

  #ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  /*! \brief
    Assignment operator: temporary inhibited.
  */
  #endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  Division_Floating_Point_Expression<FP_Interval_Type, FP_Format>&
  operator=(const Division_Floating_Point_Expression<FP_Interval_Type,
	    FP_Format>& e);


}; // class Division_Floating_Point_Expression

} // namespace Parma_Polyhedra_Library

#include "Division_Floating_Point_Expression.inlines.hh"
#include "Division_Floating_Point_Expression.templates.hh"

#endif // !defined(PPL_Division_Floating_Point_Expression_defs_hh)
