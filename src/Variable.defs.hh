/* Variable class declaration.
   Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef _Variable_defs_hh
#define _Variable_defs_hh 1

#include "Variable.types.hh"
#include <iosfwd>

/*!
  \addtogroup LinExpression
  @{
*/

//! A dimension of the space.
/*!
  An object of the class Variable represents a dimension of the space,
  that is one of the Cartesian axes.
  Variables are used as base blocks in order to build
  more complex linear expressions.
  Each variable is identified by a non-negative integer,
  representing the index of the corresponding Catesian axis
  (the first axis has index 0).
*/

class Parma_Polyhedra_Library::Variable {
public:
  //! Constructor: \p id is the index of the Cartesian axis.
  explicit Variable(unsigned int id);
  //! Returns the index of the Cartesian axis.
  unsigned int id() const;

private:
  //! The index of the Cartesian axis.
  unsigned int varid;
};

/*!
  @}
*/

namespace Parma_Polyhedra_Library {

  /*!
    \addtogroup LinExpression
    @{
  */

  //! Output operator.
  std::ostream&
  operator <<(std::ostream& s, const Parma_Polyhedra_Library::Variable& var);

  /*!
    @}
  */
}

#if !OUTLINE
#include "Variable.inlines.hh"
#endif

#endif


