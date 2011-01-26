/* Oracle class declaration.
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

#ifndef PPL_Oracle_hh
#define PPL_Oracle_hh 1

#include "globals.types.hh"
#include "Concrete_Expression.types.hh"
#include <set>

namespace Parma_Polyhedra_Library {

/*! \brief
  An abstract class to be implemented by an external analyzer such
  as eCLAIR in order to provide to the PPL the necessary information
  for performing the analysis of floating point or integer computations.

  \par Template type parameters

  - The class template parameter \p Target specifies the implementation
  of Concrete_Expression to be used.
  - The class template parameter \p Interval_Type represents the type
  of the intervals used in the abstract domain. The interval bounds
  should have a floating point or integer type.
*/
template <typename Target, typename Interval_Type>
class Oracle {
public:
  /*
    FIXME: the const qualifiers on expressions may raise CLANG
    compatibility issues. It may be necessary to omit them.
  */

  /*! \brief
    Asks the external analyzer for an interval that correctly
    approximates the floating point entity referenced by \p dim.
    Result is stored into \p result.

    \return <CODE>true</CODE> if the analyzer was able to find a correct
    approximation, <CODE>false</CODE> otherwise.
  */
  virtual bool get_interval(dimension_type dim, Interval_Type& result) const
    = 0;

  /*! \brief
    Asks the external analyzer for an interval that correctly
    approximates the value of floating point constant \p expr.
    Result is stored into \p result.

    \return <CODE>true</CODE> if the analyzer was able to find a correct
    approximation, <CODE>false</CODE> otherwise.
  */
  virtual bool get_fp_constant_value(
               const Floating_Point_Constant<Target>& expr,
                     Interval_Type& result) const = 0;

  /*! \brief
    Asks the external analyzer for an interval that correctly approximates
    the value of \p expr, which must be of integer type.
    Result is stored into \p result.

    \return <CODE>true</CODE> if the analyzer was able to find a correct
    approximation, <CODE>false</CODE> otherwise.
  */
  virtual bool get_integer_expr_value(const Concrete_Expression<Target>& expr,
                                      Interval_Type& result) const = 0;

  /*! \brief
    Asks the external analyzer for the possible space dimensions that
    are associated to the approximable reference \p expr.
    Result is stored into \p result.

    \return <CODE>true</CODE> if the analyzer was able to return
    the (possibly empty!) set, <CODE>false</CODE> otherwise.

    The resulting set MUST NOT contain <CODE>not_a_dimension()</CODE>.
  */
  virtual bool get_associated_dimensions(
	  const Approximable_Reference<Target>& expr,
          std::set<dimension_type>& result) const = 0;

};	

} // namespace Parma_Polyhedra_Library
#endif // !defined(PPL_Oracle_hh)
