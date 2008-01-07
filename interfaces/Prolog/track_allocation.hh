/* Code for keeping track of polyhedra allocations and deallocations.
   Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_track_allocation_hh
#define PPL_track_allocation_hh

#ifndef PROLOG_TRACK_ALLOCATION
#define PROLOG_TRACK_ALLOCATION 0
#endif

#if PROLOG_TRACK_ALLOCATION

#include <set>
#include <iostream>

namespace Parma_Polyhedra_Library {

class Poly_Tracker {
public:
  //! Construct an allocation tracker with no registered objects.
  Poly_Tracker();

  /*! \brief
    Register an object whose deletion is under the Prolog programmer
    responsibility.
  */
  void insert(const void* pp);

  /*! \brief
    Register an object whose deletion is under the PPL library
    responsibility.
  */
  void weak_insert(const void* pp);

  //! Check whether the object was correctly registered.
  void check(const void* pp) const;

  /*! \brief
    Unregister an object whose deletion is under the Prolog programmer
    responsibility.
  */
  void remove(const void* pp);

  /*! \brief
    Destroy the allocation tracker: an error message will be output
    if there still are registered objects whose deletion was under
    the Prolog programmer responsibility.
  */
  ~Poly_Tracker();

private:
  //! The type for recording a set of pointers to PPL library objects.
  typedef std::set<const void*, std::less<const void*> > Set;

  /*! \brief
    A set of pointers to objects whose deallocation is under the
    rensponsibility of the Prolog programmer: they should be deallocated
    before the termination of the program.
  */
  Set s;

  /*! \brief
    A set of pointers to objects whose deallocation is under the
    rensponsibility of the PPL library: they should not be deallocated
    by the Prolog programmer.
  */
  Set weak_s;
};

inline
Poly_Tracker::Poly_Tracker() {
}

inline
Poly_Tracker::~Poly_Tracker() {
  Set::size_type n = s.size();
  if (n > 0)
    std::cerr
      << "Poly_Tracker: " << n << " polyhedra leaked!"
      << std::endl;
}

inline void
Poly_Tracker::insert(const void* pp) {
  std::pair<Set::iterator, bool> stat = s.insert(pp);
  if (!stat.second) {
    std::cerr
      << "Poly_Tracker: two polyhedra at the same address at the same time?!"
      << std::endl;
    abort();
  }
}

inline void
Poly_Tracker::weak_insert(const void* pp) {
  weak_s.insert(pp);
}

inline void
Poly_Tracker::check(const void* pp) const {
  if (s.find(pp) == s.end()
      && weak_s.find(pp) == weak_s.end()) {
    std::cerr
      << "Poly_Tracker: attempt to access a nonexistent polyhedron."
      << std::endl;
    abort();
  }
}

void
Poly_Tracker::remove(const void* pp) {
  if (s.erase(pp) != 1) {
    std::cerr
      << "Poly_Tracker: attempt to deallocate a nonexistent polyhedron."
      << std::endl;
    abort();
  }
}

namespace {

inline Poly_Tracker&
poly_tracker() {
  static Poly_Tracker pt;
  return pt;
}

} // namespace

} // namespace Parma_Polyhedra_Library


#define REGISTER(x) Parma_Polyhedra_Library::poly_tracker().insert(x)
#define PPL_WEAK_REGISTER(x) Parma_Polyhedra_Library::poly_tracker().weak_insert(x)
#define PPL_UNREGISTER(x) Parma_Polyhedra_Library::poly_tracker().remove(x)
#define PPL_CHECK(x) Parma_Polyhedra_Library::poly_tracker().check(x)

#else

#define PPL_REGISTER(x)
#define PPL_WEAK_REGISTER(x)
#define PPL_UNREGISTER(x)
#define PPL_CHECK(x)

#endif

#endif // !defined(PPL_track_allocation_hh)
