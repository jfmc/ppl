/* Variable class declaration.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Variable_defs_hh
#define PPL_Variable_defs_hh 1

#include "Variable.types.hh"
#include "Init.types.hh"
#include "globals.hh"
#include <iosfwd>
#include <set>

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::Variable */
std::ostream&
operator<<(std::ostream& s, const Variable& v);

} // namespace IO_Operators

//! Defines a total ordering on variables.
/*! \relates Variable */
bool less(Variable v, Variable w);

} // namespace Parma_Polyhedra_Library

//! A dimension of the space.
/*!
  An object of the class Variable represents a dimension of the space,
  that is one of the Cartesian axes.
  Variables are used as base blocks in order to build
  more complex linear expressions.
  Each variable is identified by a non-negative integer,
  representing the index of the corresponding Cartesian axis
  (the first axis has index 0).

  Note that the ``meaning'' of an object of the class Variable
  is completely specified by the integer index provided to its
  constructor:
  be careful not to be mislead by C++ language variable names.
  For instance, in the following example the linear expressions
  <CODE>e1</CODE> and <CODE>e2</CODE> are equivalent,
  since the two variables <CODE>x</CODE> and <CODE>z</CODE> denote
  the same Cartesian axis.
  \code
  Variable x(0);
  Variable y(1);
  Variable z(0);
  LinExpression e1 = x + y;
  LinExpression e2 = y + z;
  \endcode

*/
class Parma_Polyhedra_Library::Variable {

public:
  //! Builds the variable corresponding to the Cartesian axis of index \p i.
  explicit Variable(dimension_type i);

  //! Returns the index of the Cartesian axis associated to the variable.
  dimension_type id() const;

  //! Type of output functions.
  typedef void output_function_type(std::ostream& s, const Variable& v);

  //! Sets the output function to be used for printing Variable objects.
  static void set_output_function(output_function_type* p);

  //! Returns the pointer to the current output function.
  static output_function_type* get_output_function();

  //! Binary predicate defining the total ordering on variables.
  struct Compare {
    //! Returns <CODE>true</CODE> if and only if \p x comes before \p y.
    bool operator()(Variable x, Variable y) const;
  };

private:
  //! The index of the Cartesian axis.
  dimension_type varid;

  // The initialization class needs to set the default output function.
  friend class Init;

  friend std::ostream&
  Parma_Polyhedra_Library::IO_Operators::operator<<(std::ostream& s,
						    const Variable& v);

  //! Pointer to the current output function.
  static output_function_type* current_output_function;

  //! The default output function.
  static void default_output_function(std::ostream& s, const Variable& v);
};

#include "Variable.inlines.hh"

namespace Parma_Polyhedra_Library {

//! An std::set containing variables in increasing order of dimension index.
typedef std::set<Variable, Variable::Compare> Variables_Set;

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Variable_defs_hh)
