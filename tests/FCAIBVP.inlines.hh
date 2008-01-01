/* FCAIBVP class implementation: inline functions.
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

#ifndef PPL_FCAIBVP_inlines_hh
#define PPL_FCAIBVP_inlines_hh 1

namespace Parma_Polyhedra_Library {

inline
FCAIBVP::FCAIBVP()
  : set() {
}

inline
FCAIBVP::FCAIBVP(const Variable& x)
  : set() {
  set.insert(x.id());
}

inline
FCAIBVP::FCAIBVP(const Variables_Set& y)
  : set() {
  for (Variables_Set::const_iterator i = y.begin(),
	 y_end = y.end(); i != y_end; ++i)
    set.insert(*i);
}

inline
FCAIBVP::FCAIBVP(const FCAIBVP& y, unsigned offset)
  : set() {
  for (Set::const_iterator i = y.set.begin(),
	 y_set_end = y.set.end(); i != y_set_end; ++i)
    set.insert(*i + offset);
}

inline memory_size_type
FCAIBVP::total_memory_in_bytes() const {
  return 1;
}

inline bool
FCAIBVP::is_top() const {
  return set.empty();
}

inline bool
FCAIBVP::is_bottom() const {
  return false;
}

inline bool
FCAIBVP::definitely_entails(const FCAIBVP& y) const{
  const FCAIBVP& x = *this;
  return std::includes(x.set.begin(), x.set.end(),
		       y.set.begin(), y.set.end());
}

inline void
FCAIBVP::upper_bound_assign(const FCAIBVP& y) {
  FCAIBVP& x = *this;
  FCAIBVP z;
  std::set_intersection(x.set.begin(), x.set.end(),
			y.set.begin(), y.set.end(),
			std::inserter(z.set, z.set.begin()));
  std::swap(x, z);
}

inline void
FCAIBVP::difference_assign(const FCAIBVP& y) {
  FCAIBVP& x = *this;
  FCAIBVP z;
  std::set_difference(x.set.begin(), x.set.end(),
		      y.set.begin(), y.set.end(),
		      std::inserter(z.set, z.set.begin()));
  std::swap(x, z);
}

inline void
FCAIBVP::meet_assign(const FCAIBVP& y) {
  set.insert(y.set.begin(), y.set.end());
}

inline void
FCAIBVP::weakening_assign(const FCAIBVP& y) {
  difference_assign(y);
}

inline bool
FCAIBVP::has_nontrivial_weakening() {
  return true;
}

inline bool
FCAIBVP::OK() const {
  return true;
}

inline bool
operator==(const FCAIBVP& x, const FCAIBVP& y) {
  return x.definitely_entails(y) && y.definitely_entails(x);
}

inline bool
operator!=(const FCAIBVP& x, const FCAIBVP& y) {
  return !(x == y);
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_FCAIBVP_inlines_hh)
