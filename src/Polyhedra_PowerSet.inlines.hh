/* Polyhedra_PowerSet class implementation: inline functions.
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Polyhedra_PowerSet_inlines_hh
#define PPL_Polyhedra_PowerSet_inlines_hh 1

#include "ConSys.defs.hh"
#include "ConSys.inlines.hh"
#include "algorithms.hh"
#include <algorithm>
#include <deque>

namespace Parma_Polyhedra_Library {

template <typename PH>
Polyhedra_PowerSet<PH>::Polyhedra_PowerSet(dimension_type num_dimensions,
					   Polyhedron::Degenerate_Kind kind)
  : space_dim(num_dimensions) {
  if (kind == Polyhedron::UNIVERSE)
    push_back(Determinate<PH>(num_dimensions, true));
}

template <typename PH>
Polyhedra_PowerSet<PH>::Polyhedra_PowerSet(const Polyhedra_PowerSet& y)
  : Base(y), space_dim(y.space_dim) {
}

template <typename PH>
Polyhedra_PowerSet<PH>::Polyhedra_PowerSet(const ConSys& cs)
  : space_dim(cs.space_dimension()) {
  push_back(Determinate<PH>(cs));
}

template <typename PH>
Polyhedra_PowerSet<PH>&
Polyhedra_PowerSet<PH>::operator=(const Polyhedra_PowerSet& y) {
  Base::operator=(y);
  space_dim = y.space_dim;
  return *this;
}

template <typename PH>
dimension_type
Polyhedra_PowerSet<PH>::space_dimension() const {
  return space_dim;
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::concatenate_assign(const Polyhedra_PowerSet& y) {
  Sequence new_sequence;
  const Polyhedra_PowerSet<PH>& x = *this;
  for (const_iterator xi = x.begin(), x_end = x.end(); xi != x_end; ++xi)
    for (const_iterator yi = y.begin(), yend = y.end(); yi != yend; ++yi) {
      CS zi = *xi;
      zi.concatenate_assign(*yi);
      assert(!zi.is_bottom());
      new_sequence.push_back(zi);
    }
  std::swap(sequence, new_sequence);
  space_dim += y.space_dim;
  assert(OK());
  assert(is_omega_reduced());
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::add_constraint(const Constraint& c) {
  for (iterator xi = begin(), xin = xi, x_end = end(); xi != x_end; xi = xin) {
    ++xin;
    CS& xv = *xi;
    xv.add_constraint(c);
    if (xv.is_bottom()) {
      erase(xi);
      x_end = end();
    }
  }
  omega_reduce();
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::add_constraints(ConSys& cs) {
  for (iterator xi = begin(), xin = xi, x_end = end(); xi != x_end; xi = xin) {
    ++xin;
    CS& xv = *xi;
    xv.add_constraints(cs);
    if (xv.is_bottom()) {
      erase(xi);
      x_end = end();
    }
  }
  omega_reduce();
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::add_dimensions_and_embed(dimension_type m) {
  for (iterator i = begin(), send = end(); i != send; ++i)
    i->add_dimensions_and_embed(m);
  space_dim += m;
  assert(OK());
  assert(is_omega_reduced());
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::add_dimensions_and_project(dimension_type m) {
  for (iterator i = begin(), send = end(); i != send; ++i)
    i->add_dimensions_and_project(m);
  space_dim += m;
  assert(OK());
  assert(is_omega_reduced());
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::remove_dimensions(const Variables_Set& to_be_removed) {
  for (iterator i = begin(), send = end(); i != send; ++i)
    i->remove_dimensions(to_be_removed);
  space_dim -= to_be_removed.size();
  omega_reduce();
  assert(OK());
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::remove_higher_dimensions(dimension_type
						 new_dimension) {
  for (iterator i = begin(), send = end(); i != send; ++i)
    i->remove_higher_dimensions(new_dimension);
  space_dim = new_dimension;
  omega_reduce();
  assert(OK());
}

template <typename PH>
template <typename PartialFunction>
void
Polyhedra_PowerSet<PH>::map_dimensions(const PartialFunction& pfunc) {
  if (is_bottom()) {
    dimension_type n = 0;
    for (dimension_type i = space_dim; i-- > 0; ) {
      dimension_type new_i;
      if (pfunc.maps(i, new_i))
	++n;
    }
    space_dim = n;
  }
  else {
    iterator sbegin = begin();
    for (iterator i = sbegin, send = end(); i != send; ++i)
      i->map_dimensions(pfunc);
    space_dim = sbegin->space_dimension();
    omega_reduce();
  }
  assert(OK());
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::pairwise_reduce() {
  size_type n = size();
  size_type deleted;
  do {
    Sequence new_sequence;
    std::deque<bool> marked(n, false);
    deleted = 0;
    iterator sbegin = begin();
    iterator send = end();
    unsigned i_index = 0;
    for (iterator i = sbegin; i != send; ++i, ++i_index) {
      if (marked[i_index])
	continue;
      PH& pi = i->polyhedron();
      int j_index = 0;
      const_iterator j;
      for (j = i, ++j; j != send; ++j, ++j_index) {
	if (marked[j_index])
	  continue;
	const PH& pj = j->polyhedron();
	if (poly_hull_assign_if_exact(pi, pj)) {
	  marked[i_index] = marked[j_index] = true;
	  new_sequence.push_back(pi);
	  ++deleted;
	  goto next;
	}
      }
    next:
      ;
    }
    i_index = 0;
    for (const_iterator i = sbegin; i != send; ++i, ++i_index)
      if (!marked[i_index])
	new_sequence.push_back(*i);
    std::swap(sequence, new_sequence);
    n -= deleted;
  } while (deleted > 0);
  omega_reduce();
  assert(OK());
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::extrapolation_assign(const Polyhedra_PowerSet& y,
					     void (Polyhedron::*wm)
					     (const Polyhedron&, unsigned*)) {
  pairwise_reduce();
  size_type n = size();
  Sequence new_sequence;
  std::deque<bool> marked(n, false);
  iterator sbegin = begin();
  iterator send = end();
  unsigned i_index = 0;
  for (iterator i = sbegin; i != send; ++i, ++i_index)
    for (const_iterator j = y.begin(), y_end = y.end(); j != y_end; ++j) {
      PH& pi = i->polyhedron();
      const PH& pj = j->polyhedron();
      if (pi.contains(pj)) {
	(pi.*wm)(pj, 0);
	new_sequence.push_back(pi);
	marked[i_index] = true;
      }
    }
  i_index = 0;
  for (iterator i = sbegin; i != send; ++i, ++i_index)
    if (!marked[i_index])
      new_sequence.push_back(*i);
  std::swap(sequence, new_sequence);
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::H79_extrapolation_assign(const Polyhedra_PowerSet& y) {
  extrapolation_assign(y, &PH::H79_widening_assign);
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::BHRZ03_extrapolation_assign(const
						    Polyhedra_PowerSet& y) {
  extrapolation_assign(y, &PH::BHRZ03_widening_assign);
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::collapse(unsigned max_disjuncts) {
  assert(max_disjuncts > 0);
  size_type n = size();
  if (n > max_disjuncts) {
    iterator sbegin = begin();
    iterator send = end();

    iterator i = sbegin;
    // Move `i' to the last polyhedron that will survive.
    for (unsigned m = max_disjuncts-1; m-- > 0; )
      ++i;

    // This polyhedron will be assigned the poly-hull of itself
    // and of all the polyhedra that follow.
    PH& ph = i->polyhedron();
    const_iterator j = i;
    for (++j; j != send; ++j)
      ph.poly_hull_assign(j->polyhedron());

    // Ensure omega-reduction.
    for (iterator k = sbegin, kn = k; k != i; k = kn) {
      ++kn;
      if (ph.contains(k->polyhedron()))
	erase(k);
    }

    // Erase the surplus polyhedra.
    erase(++i, send);
  }
  assert(OK());
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::widening_assign(const Polyhedra_PowerSet& y,
					void (Polyhedron::*wm)
					(const Polyhedron&, unsigned*),
					unsigned max_disjuncts) {
  collapse(max_disjuncts);
  extrapolation_assign(y, wm);
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::H79_widening_assign(const Polyhedra_PowerSet& y,
					    unsigned max_disjuncts) {
  widening_assign(y, &PH::H79_widening_assign, max_disjuncts);
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::BHRZ03_widening_assign(const Polyhedra_PowerSet& y,
					       unsigned max_disjuncts) {
  widening_assign(y, &PH::BHRZ03_widening_assign, max_disjuncts);
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::
limited_extrapolation_assign(const Polyhedra_PowerSet& y,
			     const ConSys& cs,
			     void (Polyhedron::*wm)
			     (const Polyhedron&, const ConSys&, unsigned*)) {
  pairwise_reduce();
  size_type n = size();
  Sequence new_sequence;
  std::deque<bool> marked(n, false);
  iterator sbegin = begin();
  iterator send = end();
  unsigned i_index = 0;
  for (iterator i = sbegin; i != send; ++i, ++i_index)
    for (const_iterator j = y.begin(), y_end = y.end(); j != y_end; ++j) {
      PH& pi = i->polyhedron();
      const PH& pj = j->polyhedron();
      if (pi.contains(pj)) {
	(pi.*wm)(pj, cs, 0);
	new_sequence.push_back(pi);
	marked[i_index] = true;
      }
    }
  i_index = 0;
  for (iterator i = sbegin; i != send; ++i, ++i_index)
    if (!marked[i_index])
      new_sequence.push_back(*i);
  std::swap(sequence, new_sequence);
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::
limited_extrapolation_assign(const Polyhedra_PowerSet& y,
			     const ConSys& cs,
			     void (Polyhedron::*wm)
			     (const Polyhedron&, const ConSys&, unsigned*),
			     unsigned max_disjuncts) {
  collapse(max_disjuncts);
  limited_extrapolation_assign(y, cs, wm);
}


template <typename PH>
void
Polyhedra_PowerSet<PH>::
limited_H79_extrapolation_assign(const Polyhedra_PowerSet& y,
				 const ConSys& cs,
				 unsigned max_disjuncts) {
  limited_extrapolation_assign(y,
			       cs,
			       &PH::limited_H79_extrapolation_assign,
			       max_disjuncts);
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::
limited_BHRZ03_extrapolation_assign(const Polyhedra_PowerSet& y,
				    const ConSys& cs,
				    unsigned max_disjuncts) {
  limited_extrapolation_assign(y,
			       cs,
			       &PH::limited_BHRZ03_extrapolation_assign,
			       max_disjuncts);
}

template <typename PH>
bool
Polyhedra_PowerSet<PH>::OK() const {
  for (const_iterator i = begin(), send = end(); i != send; ++i) {
    if (i->space_dimension() != space_dim)
      return false;
    if (!Base::OK())
      return false;
  }
  return true;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Polyhedra_PowerSet_inlines_hh)
