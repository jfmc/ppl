/* Declarations and inline functions for class BBox, a toy bounding box.
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

#ifndef PPL_BBox_hh
#define PPL_BBox_hh 1

#include "ppl_install.hh"
#include <iosfwd>
#include <vector>

using Parma_Polyhedra_Library::Integer;

class BInterval {
public:
  BInterval();
  void raise_lower_bound(bool closed,
			 const Integer& c, const Integer& d);
  void lower_upper_bound(bool closed,
			 const Integer& c, const Integer& d);
  void set_empty();
  void print(std::ostream& s) const;

private:
  bool uclosed;
  Integer uc;
  Integer ud;
  bool lclosed;
  Integer lc;
  Integer ld;

  friend bool operator==(const BInterval& x, const BInterval& y);
  friend bool operator<=(const BInterval& x, const BInterval& y);
};

inline
BInterval::BInterval()
  : uclosed(true), uc(1), ud(0), lclosed(true), lc(-1), ld(0) {
}

inline bool
operator!=(const BInterval& x, const BInterval& y) {
  return !(x == y);
}

using Parma_Polyhedra_Library::dimension_type;

class BBox {
public:
  BBox(dimension_type dimension);
  dimension_type space_dimension() const;
  const BInterval& operator[](dimension_type k) const;
  void print(std::ostream& s, const std::string& intro = "") const;
  void raise_lower_bound(dimension_type k, bool closed,
			 const Integer& c, const Integer& d);
  void lower_upper_bound(dimension_type k, bool closed,
			 const Integer& c, const Integer& d);
  void set_empty();

private:
  std::vector<BInterval> box;
};

inline
BBox::BBox(dimension_type dimension) {
  box.resize(dimension);
}

inline dimension_type
BBox::space_dimension() const {
  return box.size();
}

inline const BInterval&
BBox::operator[](dimension_type k) const {
  return box[k];
}

inline void
BBox::raise_lower_bound(dimension_type k, bool closed,
			const Integer& c, const Integer& d) {
  assert(k < box.size());
  box[k].raise_lower_bound(closed, c, d);
}

inline void
BBox::lower_upper_bound(dimension_type k, bool closed,
			const Integer& c, const Integer& d) {
  assert(k < box.size());
  box[k].lower_upper_bound(closed, c, d);
}

bool
operator==(const BBox& x, const BBox& y);

bool
operator<=(const BBox& x, const BBox& y);

inline bool
operator!=(const BBox& x, const BBox& y) {
  return !(x == y);
}

#endif // !defined(PPL_BBox_hh)
