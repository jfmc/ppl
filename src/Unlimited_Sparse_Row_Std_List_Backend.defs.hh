/* Unlimited_Sparse_Row_Std_List_Backend class declaration.
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

#ifndef PPL_Unlimited_Sparse_Row_Std_List_Backend_defs_hh
#define PPL_Unlimited_Sparse_Row_Std_List_Backend_defs_hh 1

#include "Unlimited_Sparse_Row_Std_List_Backend.types.hh"
#include "Coefficient.defs.hh"
#include <list>

namespace Parma_Polyhedra_Library {

class Unlimited_Sparse_Row_Std_List_Backend
  : public std::list<std::pair<dimension_type,Coefficient> > {
public:
  typedef std::pair<dimension_type,Coefficient> value_type;

private:
  //! To save typing and broken lines.
  typedef Unlimited_Sparse_Row_Std_List_Backend This;

  //! To save typing and broken lines.
  typedef std::list<value_type> Base;

public:
  //! Needed to satisfy the backend requirements.
  //! This is not a typedef to allow overloading of methods with both types.
  class dangerous_iterator : public iterator {
  public:
    dangerous_iterator(iterator i=iterator());

    static dangerous_iterator next(iterator i);
  };

  //! Needed to satisfy the backend requirements.
  //! The original splice() methods return void.
  iterator splice(iterator& position,This& x);

  //! Needed to satisfy the backend requirements.
  //! The original splice() methods return void.
  iterator splice(iterator& position,This& x,iterator i);

  //! Needed to satisfy the backend requirements.
  //! The original splice() methods return void.
  iterator splice(iterator& position,This& x,iterator first,iterator last);

  //! Returns the total size in bytes of the memory occupied by \p *this.
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  //! Always returns true, provided for compatibility with other backends.
  bool OK() const;
};

}

#include "Unlimited_Sparse_Row_Std_List_Backend.inlines.hh"

#endif // !defined(PPL_Unlimited_Sparse_Row_Std_List_Backend_defs_hh)
