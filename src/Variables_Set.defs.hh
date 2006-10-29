/* Variables_Set class declaration.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Variables_Set_defs_hh
#define PPL_Variables_Set_defs_hh 1

#include "Variables_Set.types.hh"
#include "Variable.defs.hh"
#include "globals.types.hh"
#include <iosfwd>
#include <set>

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::Variables_Set */
std::ostream&
operator<<(std::ostream& s, const Variables_Set& v);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

//! An std::set containing variables in increasing order of dimension index.
class Parma_Polyhedra_Library::Variables_Set
  : public std::set<Variable, Variable::Compare> {
private:
  typedef std::set<Variable, Variable::Compare> Base;

public:
  //! Builds the empty set of variables.
  Variables_Set();

  //! Builds the singleton set of variables containing \p v;
  explicit Variables_Set(const Variable& v);

  /*! \brief
    Builds the set of variables in the range \p v to \p w.

    If <CODE>v.id() <= w.id()</CODE>, this constructor builds the
    set of variables corresponding to the Cartesian axes of indices
    <CODE>v.id()</CODE>, <CODE>v.id()+1</CODE>, ..., <CODE>w.id()</CODE>.
    The empty set it built otherwise.
  */
  Variables_Set(const Variable& v, const Variable& w);

  //! Returns the maximum space dimension a Variables_Set can handle.
  static dimension_type max_space_dimension();

  /*! \brief
    Returns the dimension of the smallest vector space enclosing all
    the variables.
  */
  dimension_type space_dimension() const;

  //! Returns the total size in bytes of the memory occupied by \p *this.
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  //! Checks if all the invariants are satisfied.
  bool OK() const;
};

#include "Variables_Set.inlines.hh"

#endif // !defined(PPL_Variables_Set_defs_hh)
