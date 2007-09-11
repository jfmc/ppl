/* Temp_* class declarations.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Temp_defs_hh
#define PPL_Temp_defs_hh 1

#include "meta_programming.hh"
#include "Slow_Copy.hh"

namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! A pool of temporary items of type \p T.
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
template <typename T>
class Temp_Item {
public:
  //! Obtain a refeence to a temporary item.
  static Temp_Item& obtain();

  //! Release the temporary item \p p.
  static void release(Temp_Item& p);

  //! Get a reference to the encapsulated item.
  T& item();

private:
  //! The encapsulated item.
  T item_;

  //! Pointer to the next item in the free list.
  Temp_Item* next;

  //! Head of the free list.
  static Temp_Item* free_list_head;

  //! Default constructor.
  Temp_Item();

  //! Copy constructor: private and intentionally not implemented.
  Temp_Item(const Temp_Item&);

  //! Assignment operator: private and intentionally not implemented.
  Temp_Item& operator=(const Temp_Item&);
};

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! A pool of temporary items of type \p T.
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
template <typename T>
class Temp_Real_Holder {
public:
  //! FIXME!
  Temp_Real_Holder(Temp_Item<T>& obj);

  //! FIXME!
  ~Temp_Real_Holder();

  //! FIXME!
  T& item();

private:
  //! FIXME!
  Temp_Item<T>& hold;
};

template <typename T>
class Temp_Null_Holder {
public:
  //! FIXME!
  Temp_Null_Holder();

  //! FIXME!
  T item();

private:
  //! FIXME!
  T item_;
};

template <typename T>
class Temp_List {
public:
  typedef T& type;
  typedef Temp_Real_Holder<T> holder_type;
  static holder_type obtain_holder();
};

template <typename T>
class Temp_Local {
public:
  typedef T type;
  typedef Temp_Null_Holder<T> holder_type;
  static holder_type obtain_holder();
};

template <typename T, typename Enable = void>
class Dirty_Temp;

template <typename T>
class Dirty_Temp<T, typename Enable_If<Slow_Copy<T>::value>::type>
  : public Temp_List<T> {
};

template <typename T>
class Dirty_Temp<T, typename Enable_If<!Slow_Copy<T>::value>::type>
  : public Temp_Local<T> {
};

} // namespace Parma_Polyhedra_Library

#include "Temp.inlines.hh"
#include "Temp.templates.hh"

#endif // !defined(PPL_Temp_defs_hh)
