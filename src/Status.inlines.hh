/* Status class implementation: inline functions.
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

INLINE
Parma_Polyhedra_Library::Status::Status(status_t mask)
  : flags(mask) {
}

INLINE
Parma_Polyhedra_Library::Status::Status()
  : flags(ZERO_DIM) {
}

INLINE Parma_Polyhedra_Library::Status
Parma_Polyhedra_Library::operator &(const Status& x, const Status& y) {
  return Status(x.flags & y.flags);
}

INLINE Parma_Polyhedra_Library::Status
Parma_Polyhedra_Library::operator |(const Status& x, const Status& y) {
  return Status(x.flags | y.flags);
}

INLINE bool
Parma_Polyhedra_Library::Status::test_all(status_t mask) const {
  return (flags & mask) == mask;
}

INLINE bool
Parma_Polyhedra_Library::Status::test_any(status_t mask) const {
  return flags & mask;
}

INLINE void
Parma_Polyhedra_Library::Status::set(status_t mask) {
  flags |= mask;
}

INLINE void
Parma_Polyhedra_Library::Status::reset(status_t mask) {
  flags &= ~mask;
}

INLINE bool
Parma_Polyhedra_Library::Status::test_zero_dim() const {
  return flags == ZERO_DIM;
}

INLINE void
Parma_Polyhedra_Library::Status::reset_zero_dim() {
  // This is a no-op if the current status is not zero-dim,
  if (flags == ZERO_DIM)
    // No longer zero-dim: what else can we say?
    flags = EMPTY;
}

INLINE void
Parma_Polyhedra_Library::Status::set_zero_dim() {
  // Zero-dim is incompatible with anything else.
  flags = ZERO_DIM;
}

INLINE bool
Parma_Polyhedra_Library::Status::test_empty() const {
  return test_any(EMPTY);
}

INLINE void
Parma_Polyhedra_Library::Status::reset_empty() {
  reset(EMPTY);
}

INLINE void
Parma_Polyhedra_Library::Status::set_empty() {
  flags = EMPTY;
}

INLINE bool
Parma_Polyhedra_Library::Status::test_c_up_to_date() const {
  return test_any(C_UP_TO_DATE);
}

INLINE void
Parma_Polyhedra_Library::Status::reset_c_up_to_date() {
  reset(C_UP_TO_DATE);
}

INLINE void
Parma_Polyhedra_Library::Status::set_c_up_to_date() {
  set(C_UP_TO_DATE);
}

INLINE bool
Parma_Polyhedra_Library::Status::test_g_up_to_date() const {
  return test_any(G_UP_TO_DATE);
}

INLINE void
Parma_Polyhedra_Library::Status::reset_g_up_to_date() {
  reset(G_UP_TO_DATE);
}

INLINE void
Parma_Polyhedra_Library::Status::set_g_up_to_date() {
  set(G_UP_TO_DATE);
}

INLINE bool
Parma_Polyhedra_Library::Status::test_c_minimized() const {
  return test_any(C_MINIMIZED);
}

INLINE void
Parma_Polyhedra_Library::Status::reset_c_minimized() {
  reset(C_MINIMIZED);
}

INLINE void
Parma_Polyhedra_Library::Status::set_c_minimized() {
  set(C_MINIMIZED);
}

INLINE bool
Parma_Polyhedra_Library::Status::test_g_minimized() const {
  return test_any(G_MINIMIZED);
}

INLINE void
Parma_Polyhedra_Library::Status::reset_g_minimized() {
  reset(G_MINIMIZED);
}

INLINE void
Parma_Polyhedra_Library::Status::set_g_minimized() {
  set(G_MINIMIZED);
}

INLINE bool
Parma_Polyhedra_Library::Status::test_sat_c_up_to_date() const {
  return test_any(SAT_C_UP_TO_DATE);
}

INLINE void
Parma_Polyhedra_Library::Status::reset_sat_c_up_to_date() {
  reset(SAT_C_UP_TO_DATE);
}

INLINE void
Parma_Polyhedra_Library::Status::set_sat_c_up_to_date() {
  set(SAT_C_UP_TO_DATE);
}

INLINE bool
Parma_Polyhedra_Library::Status::test_sat_g_up_to_date() const {
  return test_any(SAT_G_UP_TO_DATE);
}

INLINE void
Parma_Polyhedra_Library::Status::reset_sat_g_up_to_date() {
  reset(SAT_G_UP_TO_DATE);
}

INLINE void
Parma_Polyhedra_Library::Status::set_sat_g_up_to_date() {
  set(SAT_G_UP_TO_DATE);
}
