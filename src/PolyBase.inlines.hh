/* PolyBase class implementation: inline functions.
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


#ifndef _PolyBase_inlines_hh
#define _PolyBase_inlines_hh 1

#include <algorithm>

namespace Parma_Polyhedra_Library {

inline void
PolyBase::swap(PolyBase& y) {
  std::swap(space_dim, y.space_dim);
  std::swap(con_sys, y.con_sys);
  std::swap(gen_sys, y.gen_sys);
  std::swap(sat_c, y.sat_c);
  std::swap(sat_g, y.sat_g);
  std::swap(status, y.status);
}


/*!
  Returns <CODE>true</CODE> if \p *this is definitely known to be
  an empty polyhedron.
  Note that the return value <CODE>false</CODE> does not necessarily
  implies that \p *this is non-empty.
*/
inline bool
PolyBase::is_empty() const {
  return status.test_empty();
}

/*!
  Returns <CODE>true</CODE> if the system of constraints is up-to-date.
*/
inline bool
PolyBase::constraints_are_up_to_date() const {
  return status.test_c_up_to_date();
}


/*!
  Returns <CODE>true</CODE> if the system of generators is up-to-date.
*/
inline bool
PolyBase::generators_are_up_to_date() const {
  return status.test_g_up_to_date();
}

/*!
  Returns <CODE>true</CODE> if the system of constraints is minimized.
*/
inline bool
PolyBase::constraints_are_minimized() const {
  return status.test_c_minimized();
}

/*!
  Returns <CODE>true</CODE> if the system of generators is minimized.
*/
inline bool
PolyBase::generators_are_minimized() const {
  return status.test_g_minimized();
}

/*!
  Returns <CODE>true</CODE> if \p sat_c is up-to-date.
*/
inline bool
PolyBase::sat_c_is_up_to_date() const {
  return status.test_sat_c_up_to_date();
}

/*!
  Returns <CODE>true</CODE> if \p sat_g is up-to-date.
*/
inline bool
PolyBase::sat_g_is_up_to_date() const {
  return status.test_sat_g_up_to_date();
}

/*!
  Sets \p status to express that constraints are up-to-date.
*/
inline void
PolyBase::set_constraints_up_to_date() {
  status.set_c_up_to_date();
}


/*!
  Sets \p status to express that generators are up-to-date.
*/
inline void
PolyBase::set_generators_up_to_date() {
  status.set_g_up_to_date();
}

/*!
  Sets \p status to express that constraints are minimized.
*/
inline void
PolyBase::set_constraints_minimized() {
  set_constraints_up_to_date();
  status.set_c_minimized();
}

/*!
  Sets \p status to express that generators are minimized.
*/
inline void
PolyBase::set_generators_minimized() {
  set_generators_up_to_date();
  status.set_g_minimized();
}

/*!
  Sets \p status to express that \p sat_c is up-to-date,
  i.e., it actually represents the relations between
  generators and constraints.
*/
inline void
PolyBase::set_sat_c_up_to_date() {
  status.set_sat_c_up_to_date();
}


/*!
  Sets \p status to express \p sat_g is up-to-date,
  i.e., it actually represents the relations between
  constraints and generators.
*/
inline void
PolyBase::set_sat_g_up_to_date() {
  status.set_sat_g_up_to_date();
}


/*!
  Clears the status flag indicating that the polyhedron is empty.
*/
inline void
PolyBase::clear_empty() {
  status.reset_empty();
}


/*!
  Sets \p status to express that \p sat_c is no longer up-to-date.
*/
inline void
PolyBase::clear_sat_c_up_to_date() {
  status.reset_sat_c_up_to_date();
  // Can get rid of sat_c here.
}


/*!
  Sets \p status to express that \p sat_g is no longer up-to-date.
*/
inline void
PolyBase::clear_sat_g_up_to_date() {
  status.reset_sat_g_up_to_date();
  // Can get rid of sat_g here.
}

/*!
  Sets \p status to express that constraints are no longer minimized.
*/
inline void
PolyBase::clear_constraints_minimized() {
  status.reset_c_minimized();
}

/*!
  Sets \p status to express that generators are no longer minimized.
*/
inline void
PolyBase::clear_generators_minimized() {
  status.reset_g_minimized();
}

/*!
  Sets \p status to express that constraints are no longer up-to-date
  (thus, they are neither minimized and the saturation matrices are no
  longer meaningful).
*/
inline void
PolyBase::clear_constraints_up_to_date() {
  clear_constraints_minimized();
  clear_sat_c_up_to_date();
  clear_sat_g_up_to_date();
  status.reset_c_up_to_date();
  // Can get rid of con_sys here.
}


/*!
  Sets \p status to express that generators are no longer up-to-date
  (thus, they are neither minimized and the saturation matrices are
  no longer meaningful).
*/
inline void
PolyBase::clear_generators_up_to_date() {
  clear_generators_minimized();
  clear_sat_c_up_to_date();
  clear_sat_g_up_to_date();
  status.reset_g_up_to_date();
  // Can get rid of gen_sys here.
}

inline bool
operator==(const PolyBase& x, const PolyBase& y) {
  return x <= y && y <= x;
}

inline bool
operator!=(const PolyBase& x, const PolyBase& y) {
  return !(x == y);
}

inline bool
operator>=(const PolyBase& x, const PolyBase& y) {
  return y <= x;
}

inline bool
operator<(const PolyBase& x, const PolyBase& y) {
  return x <= y && !(x >= y);
}

inline bool
operator>(const PolyBase& x, const PolyBase& y) {
  return y < x;
}


inline size_t
PolyBase::space_dimension() const {
  return space_dim;
}

inline bool
PolyBase::check_empty() const {
  minimize();
  return is_empty();
}

} // namespace Parma_Polyhedra_Library


/*!
  Specializes std::swap to use the fast swap that is provided
  as a member function instead of using the default algorithm
  (which creates a temporary and uses assignment).
*/
inline void
std::swap(Parma_Polyhedra_Library::PolyBase& x,
	  Parma_Polyhedra_Library::PolyBase& y) {
  x.swap(y);
}


#endif
