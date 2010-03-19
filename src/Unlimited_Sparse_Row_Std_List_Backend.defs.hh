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
  : public std::list<std::pair<dimension_type, Coefficient> > {
public:
  typedef std::pair<dimension_type, Coefficient> value_type;

private:
  //! To save typing and broken lines.
  typedef Unlimited_Sparse_Row_Std_List_Backend This;

  //! To save typing and broken lines.
  typedef std::list<value_type> Base;

  template <typename Compare>
  class value_key_comparison;

  template <typename Compare>
  class key_value_comparison;

  template <typename Compare>
  static Unlimited_Sparse_Row_Std_List_Backend::value_key_comparison<Compare>
  value_key_compare(const Compare& comp);

  template <typename Compare>
  static Unlimited_Sparse_Row_Std_List_Backend::key_value_comparison<Compare>
  key_value_compare(const Compare& comp);

public:
  //! Needed to satisfy the backend requirements.
  //! This is not a typedef to allow overloading of methods with both types.
  class dangerous_iterator : public iterator {
  public:
    dangerous_iterator();
    dangerous_iterator(iterator i);

    static dangerous_iterator next(iterator i);
  };

  dangerous_iterator begin_dangerous();
  dangerous_iterator end_dangerous();

  dangerous_iterator find_dangerous(const dimension_type c);
  dangerous_iterator lower_bound_dangerous(const dimension_type c);
  iterator find(const dimension_type c);
  iterator lower_bound(const dimension_type c);
  const_iterator find(const dimension_type c) const;
  const_iterator lower_bound(const dimension_type c) const;

  //! Looks for an element with key c, assuming it is in [itr,end()) .
  dangerous_iterator find_dangerous(const dimension_type c,
                                    dangerous_iterator itr);
  //! Lower bound of key c, assuming it is in [itr,end()) .
  dangerous_iterator lower_bound_dangerous(const dimension_type c,
                                           dangerous_iterator itr);

  //! Looks for an element with key c, assuming it is in [itr,end()) .
  iterator find(const dimension_type c, iterator itr);
  //! Lower bound of key c, assuming it is in [itr,end()) .
  iterator lower_bound(const dimension_type c, iterator itr);

  //! Looks for an element with key c, assuming it is in [itr,end()) .
  const_iterator find(const dimension_type c, const_iterator itr) const;
  //! Lower bound of key c, assuming it is in [itr,end()) .
  const_iterator lower_bound(const dimension_type c,
                             const_iterator itr) const;

  //! A faster equivalent of
  //! itr1=find_dangerous(c1); itr2=find_dangerous(c2); .
  void find2_dangerous(const dimension_type c1, const dimension_type c2,
                       dangerous_iterator& itr1, dangerous_iterator& itr2);

  //! A faster equivalent of itr1=find(c1); itr2=find(c2); .
  void find2(const dimension_type c1, const dimension_type c2,
             iterator& itr1, iterator& itr2);

  //! A faster equivalent of itr1=find(c1); itr2=find(c2); .
  void find2(const dimension_type c1, const dimension_type c2,
             const_iterator& itr1, const_iterator& itr2) const;

  // This would be hidden by the next declaration.
  using Base::insert;

  //! Needed to satisfy the backend requirements.
  dangerous_iterator insert(dangerous_iterator pos, dimension_type i,
                            const Coefficient& x);

  //! Needed to satisfy the backend requirements.
  //! The original splice() methods return void.
  iterator splice(iterator& position, This& x);

  //! Needed to satisfy the backend requirements.
  //! The original splice() methods return void.
  iterator splice(iterator& position, This& x, iterator i);

  //! Needed to satisfy the backend requirements.
  //! The original splice() methods return void.
  iterator splice(iterator& position, This& x, iterator first, iterator last);

  //! Returns the total size in bytes of the memory occupied by \p *this.
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  //! Always returns true, provided for compatibility with other backends.
  bool OK() const;
};

template <typename Compare>
class Unlimited_Sparse_Row_Std_List_Backend::value_key_comparison
  : public std::binary_function<Unlimited_Sparse_Row_Std_List_Backend
                                ::value_type&,
                                dimension_type, bool> {
public:
  value_key_comparison(const Compare& comp);

  bool operator()(const Unlimited_Sparse_Row_Std_List_Backend::value_type& x,
                  const dimension_type y) const;

private:
  Compare comp_;
};

template <typename Compare>
class Unlimited_Sparse_Row_Std_List_Backend::key_value_comparison
  : public std::binary_function<dimension_type,
                                Unlimited_Sparse_Row_Std_List_Backend
                                ::value_type&, bool> {
public:
  key_value_comparison(const Compare& comp);

  bool operator()(const dimension_type x,
                  const
                  Unlimited_Sparse_Row_Std_List_Backend::value_type& y) const;

private:
  Compare comp_;
};

}

#include "Unlimited_Sparse_Row_Std_List_Backend.inlines.hh"

#endif // !defined(PPL_Unlimited_Sparse_Row_Std_List_Backend_defs_hh)
