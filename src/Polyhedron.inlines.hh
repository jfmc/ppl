/* Polyhedron class implementation: inline functions.
   Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef _Polyhedron_inlines_hh
#define _Polyhedron_inlines_hh 1

#include <algorithm>
#include <stdexcept>

INLINE void
Parma_Polyhedra_Library::Polyhedron::swap(Polyhedron& y) {
  std::swap(con_sys, y.con_sys);
  std::swap(gen_sys, y.gen_sys);
  std::swap(sat_c, y.sat_c);
  std::swap(sat_g, y.sat_g);
  std::swap(status, y.status);
}

 
/*!
  Specializes std::swap to use the fast swap that is provided
  as a member function instead of using the default algorithm
  (which creates a temporary and uses assignment).
*/
INLINE void
std::swap(Parma_Polyhedra_Library::Polyhedron& x,
	  Parma_Polyhedra_Library::Polyhedron& y) {
  x.swap(y);
}


/*!
  Returns <CODE>true</CODE> if and only if \p *this is empty.
*/
INLINE bool
Parma_Polyhedra_Library::Polyhedron::is_empty() const {
  return status.test_empty();
}

/*!
  Returns <CODE>true</CODE> if and only if \p *this is the
  full zero-dimensional polyhedron.
*/
INLINE bool
Parma_Polyhedra_Library::Polyhedron::is_zero_dim() const {
  return status.test_zero_dim();
}

/*!
  Returns <CODE>true</CODE> if the system of constraints is up-to-date.
*/
INLINE bool
Parma_Polyhedra_Library::Polyhedron::constraints_are_up_to_date() const {
  return status.test_c_up_to_date();
}


/*!
  Returns <CODE>true</CODE> if the system of generators is up-to-date.
*/
INLINE bool
Parma_Polyhedra_Library::Polyhedron::generators_are_up_to_date() const {
  return status.test_g_up_to_date();
}

/*!
  Returns <CODE>true</CODE> if the system of constraints is minimized.
*/
INLINE bool
Parma_Polyhedra_Library::Polyhedron::constraints_are_minimized() const {
  return status.test_c_minimized();
}

/*!
  Returns <CODE>true</CODE> if the system of generators is minimized.
*/
INLINE bool
Parma_Polyhedra_Library::Polyhedron::generators_are_minimized() const {
  return status.test_g_minimized();
}

/*!
  Returns <CODE>true</CODE> if \p sat_c is up-to-date.
*/
INLINE bool
Parma_Polyhedra_Library::Polyhedron::sat_c_is_up_to_date() const {
  return status.test_sat_c_up_to_date();
}

/*!
  Returns <CODE>true</CODE> if \p sat_g is up-to-date.
*/
INLINE bool
Parma_Polyhedra_Library::Polyhedron::sat_g_is_up_to_date() const {
  return status.test_sat_g_up_to_date();
}

/*!
  Sets \p status to express constraints are up-to-date.
*/
INLINE void
Parma_Polyhedra_Library::Polyhedron::set_constraints_up_to_date() {
  status.set_c_up_to_date();
}


/*!
  Sets \p status to express generators are up-to-date.
*/
INLINE void
Parma_Polyhedra_Library::Polyhedron::set_generators_up_to_date() {
  status.set_g_up_to_date();
}

/*!
  Sets \p status to express constraints are minimized.
*/
INLINE void
Parma_Polyhedra_Library::Polyhedron::set_constraints_minimized() {
  set_constraints_up_to_date();
  status.set_c_minimized();
}

/*!
  Sets \p status to express generators are minimized.
*/
INLINE void
Parma_Polyhedra_Library::Polyhedron::set_generators_minimized() {
  set_generators_up_to_date();
  status.set_g_minimized();
}

/*!
  Sets \p status to express \p sat_c actually represents
  relations between generators and constraints, i.e. it is up-to-date.
*/
INLINE void
Parma_Polyhedra_Library::Polyhedron::set_sat_c_up_to_date() {
  status.set_sat_c_up_to_date();
}


/*!
  Sets \p status to express \p sat_g actually represents
  relations between constraints and generators, i.e. it is up-to-date.
*/
INLINE void
Parma_Polyhedra_Library::Polyhedron::set_sat_g_up_to_date() {
  status.set_sat_g_up_to_date();
}


/*!
  Clears the flag indicating the emptiness of the polyhedron.
*/
INLINE void
Parma_Polyhedra_Library::Polyhedron::clear_empty() {
  status.reset_empty();
}


/*!
  Sets \p status to express sat_c
  is no more up-to-date.
*/
INLINE void
Parma_Polyhedra_Library::Polyhedron::clear_sat_c_up_to_date() {
  status.reset_sat_c_up_to_date();
  // Can get rid of sat_c here.
}


/*!
  Sets \p status to express \p sat_g
  is no longer up-to-date.
*/
INLINE void
Parma_Polyhedra_Library::Polyhedron::clear_sat_g_up_to_date() {
  status.reset_sat_g_up_to_date();
  // Can get rid of sat_g here.
}

/*!
  Sets \p status to express constraints are
  no longer minimized.
*/
INLINE void
Parma_Polyhedra_Library::Polyhedron::clear_constraints_minimized() {
  status.reset_c_minimized();
}

/*!
  Sets \p status to express generators are
  no longer minimized.
*/
INLINE void
Parma_Polyhedra_Library::Polyhedron::clear_generators_minimized() {
  status.reset_g_minimized();
}

/*!
  Sets \p status to express constraints are no longer up-to-date
  (then no minimized) and then the saturation matrices are no
  more meaningful.
*/
INLINE void
Parma_Polyhedra_Library::Polyhedron::clear_constraints_up_to_date() {
  clear_constraints_minimized();
  clear_sat_c_up_to_date();
  clear_sat_g_up_to_date();
  status.reset_c_up_to_date();
  // Can get rid of con_sys here.
}


/*!
  Sets \p status to express the fact that generators are no longer up-to-date
  (then no minimized) and then the saturation matrices are
  no longer meaningful.
*/
INLINE void
Parma_Polyhedra_Library::Polyhedron::clear_generators_up_to_date() {
  clear_generators_minimized();
  clear_sat_c_up_to_date();
  clear_sat_g_up_to_date();
  status.reset_g_up_to_date();
  // Can get rid of gen_sys here.
}

/*!
  Forces \p status to be that of a zero-dimensional polyhedron.
*/
INLINE void
Parma_Polyhedra_Library::Polyhedron::set_zero_dim() {
  status.set_zero_dim();
}

INLINE bool
Parma_Polyhedra_Library::operator ==(const Polyhedron& x,
				     const Polyhedron& y) {
  return x <= y && y <= x;
}

INLINE bool
Parma_Polyhedra_Library::operator !=(const Polyhedron& x,
				     const Polyhedron& y) {
  return !(x == y);
}

INLINE bool
Parma_Polyhedra_Library::operator >=(const Polyhedron& x,
				     const Polyhedron& y) {
  return y <= x;
}

INLINE bool
Parma_Polyhedra_Library::operator <(const Polyhedron& x,
				    const Polyhedron& y) {
  return x <= y && x != y;
}

INLINE bool
Parma_Polyhedra_Library::operator >(const Polyhedron& x,
				    const Polyhedron& y) {
  return y < x;
}


/*!
  Updates the constraints as necessary, then returns a constant
  reference to the system of constraints.
*/
INLINE const Parma_Polyhedra_Library::ConSys&
Parma_Polyhedra_Library::Polyhedron::constraints() const {
  if (is_empty())
    throw std::invalid_argument("PPL::Polyhedron::constraints() "
				"*this is empty");
   if (is_zero_dim())
    throw std::invalid_argument("PPL::Polyhedron::constraints() "
				"*this is zero-dimensional");
  if (!constraints_are_up_to_date())
    update_constraints();

  // We insist in returning a sorted system of constraints.
  if (!con_sys.is_sorted()) {
    if (sat_c_is_up_to_date()) {
      const_cast<Polyhedron&>(*this).obtain_sorted_constraints_with_sat_c();
      if (sat_g_is_up_to_date())
	const_cast<SatMatrix&>(sat_g).transpose_assign(sat_c);
    }
    else {
      const_cast<ConSys&>(con_sys).sort_rows();
      if (sat_g_is_up_to_date()) {
#ifndef NDEBUG
	const_cast<Polyhedron&>(*this).clear_sat_g_up_to_date();
#endif
	const_cast<Polyhedron&>(*this).update_sat_g();
      }
    }
  }
  return con_sys;
}


/*!
  Updates the generators as necessary, then returns a constant
  reference to the system of generators.
*/
INLINE const Parma_Polyhedra_Library::GenSys&
Parma_Polyhedra_Library::Polyhedron::generators() const {
  if (is_empty())
    throw std::invalid_argument("PPL::Polyhedron::generators() "
				"*this is empty");
  if (is_zero_dim())
    throw std::invalid_argument("PPL::Polyhedron::generators() "
				"*this is zero-dimensional");

  if (!generators_are_up_to_date())
    update_generators();

  // We insist in returning a sorted system of generators.
  if (!gen_sys.is_sorted()) {
    if (sat_g_is_up_to_date()) {
      const_cast<Polyhedron&>(*this).obtain_sorted_generators_with_sat_g();
      if (sat_c_is_up_to_date())
	const_cast<SatMatrix&>(sat_c).transpose_assign(sat_g);
    }
    else {
      const_cast<GenSys&>(gen_sys).sort_rows();
      if (sat_c_is_up_to_date()) {
#ifndef NDEBUG
	const_cast<Polyhedron&>(*this).clear_sat_c_up_to_date();
#endif
	const_cast<Polyhedron&>(*this).update_sat_c();
      }
    }
  }
  return gen_sys;
}

INLINE size_t
Parma_Polyhedra_Library::Polyhedron::num_dimensions() const {
  assert(!is_empty());
  if (is_zero_dim())
    return 0;
  else if (constraints_are_up_to_date())
    return con_sys.num_columns()-1;
  else {
    assert(generators_are_up_to_date());
    return gen_sys.num_columns()-1;
  }
}

INLINE bool
Parma_Polyhedra_Library::Polyhedron::check_empty() const {
  minimize();
  return is_empty();
}

#endif
