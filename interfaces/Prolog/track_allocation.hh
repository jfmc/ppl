/* Code for keeping track of polyhedra allocations and deallocations.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef _track_allocation_hh
#define _track_allocation_hh

#ifndef TRACK_ALLOCATION
#define TRACK_ALLOCATION 0
#endif

#if TRACK_ALLOCATION

#include <set>
#include <iostream>

namespace Parma_Polyhedra_Library {

class PolyTracker {
public:
  void insert(const void* pp);
  void check(const void* pp);
  void remove(const void* pp);

  PolyTracker();
  ~PolyTracker();

private:
  typedef std::set<const void*, std::less<const void*> > Set;
  Set s;
};

PolyTracker::PolyTracker() {
}

PolyTracker::~PolyTracker() {
  size_t n = s.size();
  if (n > 0) 
    std::cerr << n << " polyhedra leaked!" << std::endl;
}

void
PolyTracker::insert(const void* pp) {
  std::pair<Set::iterator, bool> stat = s.insert(pp);
  if (!stat.second)
    abort();
}

void
PolyTracker::check(const void* pp) {
  if (s.find(pp) == s.end())
    abort();
}

void
PolyTracker::remove(const void* pp) {
  if (s.erase(pp) != 1)
    abort();
}

} // namespace Parma_Polyhedra_Library

static Parma_Polyhedra_Library::PolyTracker poly_tracker;

#define REGISTER(x) poly_tracker.insert(x)
#define UNREGISTER(x) poly_tracker.remove(x)
#define CHECK(x) poly_tracker.check(x)

#else

#define REGISTER(x)
#define UNREGISTER(x)
#define CHECK(x)

#endif

#endif
