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
#include <string>

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
inline void
Polyhedra_PowerSet<PH>::swap(Polyhedra_PowerSet& y) {
  Base::swap(y);
  std::swap(space_dim, y.space_dim);
}

template <typename PH>
dimension_type
Polyhedra_PowerSet<PH>::space_dimension() const {
  return space_dim;
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::concatenate_assign(const Polyhedra_PowerSet& y) {
  // Ensure omega-reduction here, since what follows has quadratic complexity.
  omega_reduce();
  y.omega_reduce();
  Sequence new_sequence;
  const Polyhedra_PowerSet<PH>& x = *this;
  for (const_iterator xi = x.begin(), x_end = x.end(),
	 y_begin = y.begin(), y_end = y.end(); xi != x_end; ) {
    for (const_iterator yi = y_begin; yi != y_end; ++yi) {
      CS zi = *xi;
      zi.concatenate_assign(*yi);
      assert(!zi.is_bottom());
      new_sequence.push_back(zi);
    }
    ++xi;
    if (abandon_exponential_computations && xi != x_end && y_begin != y_end) {
      // Hurry up!
      PH xph = xi->polyhedron();
      for (++xi; xi != x_end; ++xi)
	xph.poly_hull_assign(xi->polyhedron());
      const_iterator yi = y_begin;
      PH yph = yi->polyhedron();
      for (++yi; yi != y_end; ++yi)
	yph.poly_hull_assign(yi->polyhedron());
      xph.concatenate_assign(yph);
      std::swap(sequence, new_sequence);
      add_disjunct(xph);
      goto done;
    }
  }
  std::swap(sequence, new_sequence);
 done:
  space_dim += y.space_dim;
  assert(OK());
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
    else
      reduced = false;
  }
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::add_constraints(const ConSys& cs) {
  for (iterator xi = begin(), xin = xi, x_end = end(); xi != x_end; xi = xin) {
    ++xin;
    CS& xv = *xi;
    ConSys cs_copy = cs;
    xv.add_constraints(cs_copy);
    if (xv.is_bottom()) {
      erase(xi);
      x_end = end();
    }
    else
      reduced = false;
  }
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::add_dimensions_and_embed(dimension_type m) {
  for (iterator i = begin(), send = end(); i != send; ++i)
    i->add_dimensions_and_embed(m);
  space_dim += m;
  assert(OK());
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::add_dimensions_and_project(dimension_type m) {
  for (iterator i = begin(), send = end(); i != send; ++i)
    i->add_dimensions_and_project(m);
  space_dim += m;
  assert(OK());
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::remove_dimensions(const Variables_Set& to_be_removed) {
  Variables_Set::size_type num_removed = to_be_removed.size();
  if (num_removed > 0) {
    for (iterator i = begin(), send = end(); i != send; ++i) {
      i->remove_dimensions(to_be_removed);
      reduced = false;
    }
    space_dim -= num_removed.size();
    assert(OK());
  }
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::remove_higher_dimensions(dimension_type
						 new_dimension) {
  if (new_dimension < space_dim) {
    for (iterator i = begin(), send = end(); i != send; ++i) {
      i->remove_higher_dimensions(new_dimension);
      reduced = false;
    }
    space_dim = new_dimension;
    assert(OK());
  }
}

template <typename PH>
bool
Polyhedra_PowerSet<PH>::
semantically_contains(const Polyhedra_PowerSet& y) const {
  for (const_iterator yi = y.begin(), y_end = y.end(); yi != y_end; ++yi)
    if (!check_containment(yi->polyhedron(), *this))
      return false;
  return true;
}

template <typename PH>
bool
Polyhedra_PowerSet<PH>::
semantically_equals(const Polyhedra_PowerSet& y) const {
  const Polyhedra_PowerSet& x = * this;
  return x.semantically_contains(y) && y.semantically_contains(x);
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
    reduced = false;
  }
  assert(OK());
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::pairwise_reduce() {
  // It is wise to omega-reduce before pairwise-reducing.
  omega_reduce();

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
	  add_non_bottom_disjunct(new_sequence, pi);
	  ++deleted;
	  goto next;
	}
      }
    next:
      ;
    }
    iterator nsbegin = new_sequence.begin();
    iterator nsend = new_sequence.end();
    i_index = 0;
    for (const_iterator i = sbegin; i != send; ++i, ++i_index)
      if (!marked[i_index])
	add_non_bottom_disjunct(new_sequence, *i, nsbegin, nsend);
    std::swap(sequence, new_sequence);
    n -= deleted;
  } while (deleted > 0);
  assert(OK());
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::
BGP99_heuristics_assign(const Polyhedra_PowerSet& y,
			void (Polyhedron::*wm)
			(const Polyhedron&, unsigned*)) {
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
	add_non_bottom_disjunct(new_sequence, pi);
	marked[i_index] = true;
      }
    }
  iterator nsbegin = new_sequence.begin();
  iterator nsend = new_sequence.end();
  i_index = 0;
  for (iterator i = sbegin; i != send; ++i, ++i_index)
    if (!marked[i_index])
      add_non_bottom_disjunct(new_sequence, *i, nsbegin, nsend);
  std::swap(sequence, new_sequence);
  assert(OK());
  assert(is_omega_reduced());
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::
limited_BGP99_heuristics_assign(const Polyhedra_PowerSet& y,
				const ConSys& cs,
				void (Polyhedron::*lwm)(const Polyhedron&,
							const ConSys&,
							unsigned*)) {
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
	(pi.*lwm)(pj, cs, 0);
	add_non_bottom_disjunct(new_sequence, pi);
	marked[i_index] = true;
      }
    }
  iterator nsbegin = new_sequence.begin();
  iterator nsend = new_sequence.end();
  i_index = 0;
  for (iterator i = sbegin; i != send; ++i, ++i_index)
    if (!marked[i_index])
      add_non_bottom_disjunct(new_sequence, *i, nsbegin, nsend);
  std::swap(sequence, new_sequence);
  assert(OK());
  assert(is_omega_reduced());
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::BGP99_extrapolation_assign(const Polyhedra_PowerSet& y,
						   void (Polyhedron::*wm)
						   (const Polyhedron&,
						    unsigned*),
						   unsigned max_disjuncts) {
  // `x' is the current iteration value.
  Polyhedra_PowerSet<PH>& x = *this;

#ifndef NDEBUG
  {
    // We assume that y is entailed by or equal to *this.
    const Polyhedra_PowerSet<PH> x_copy = x;
    const Polyhedra_PowerSet<PH> y_copy = y;
    assert(y_copy.definitely_entails(x_copy));
  }
#endif

  x.pairwise_reduce();
  if (max_disjuncts != 0)
    x.collapse(max_disjuncts);
  x.BGP99_heuristics_assign(y, wm);
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::
limited_BGP99_extrapolation_assign(const Polyhedra_PowerSet& y,
				   const ConSys& cs,
				   void (Polyhedron::*lwm)
				   (const Polyhedron&,
				    const ConSys&,
				    unsigned*),
				   unsigned max_disjuncts) {
  // `x' is the current iteration value.
  Polyhedra_PowerSet<PH>& x = *this;

#ifndef NDEBUG
  {
    // We assume that y is entailed by or equal to *this.
    const Polyhedra_PowerSet<PH> x_copy = x;
    const Polyhedra_PowerSet<PH> y_copy = y;
    assert(y_copy.definitely_entails(x_copy));
  }
#endif

  x.pairwise_reduce();
  if (max_disjuncts != 0)
    x.collapse(max_disjuncts);
  x.limited_BGP99_heuristics_assign(y, cs, lwm);
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::
collect_multiset_lgo_info(multiset_lgo_info& info) const {
  assert(is_omega_reduced());
  assert(info.size() == 0);
  for (const_iterator i = begin(), iend = end(); i != iend; i++) {
    base_lgo_info ph_info(i->polyhedron());
    info[ph_info]++;
  }
}

template <typename PH>
bool
Polyhedra_PowerSet<PH>::
is_multiset_lgo_stabilizing(const multiset_lgo_info& y_info) const {
  multiset_lgo_info x_info;
  collect_multiset_lgo_info(x_info);
  typename multiset_lgo_info::const_iterator
    xi = x_info.begin(),
    xend = x_info.end(),
    yi = y_info.begin(),
    yend = y_info.end();
  while (xi != xend && yi != yend) {
    const base_lgo_info& xi_info = xi->first;  
    const base_lgo_info& yi_info = yi->first;
    switch (xi_info.compare(yi_info)) {
    case 0:
      // xi_info == yi_info: check the number of multiset occurrences.
      {
	const size_type& xi_count = xi->second;
	const size_type& yi_count = yi->second;
	if (xi_count == yi_count) {
	  // Same number of occurrences: compare the next pair.
	  xi++;
	  yi++;
	}
	else
	  // Different number of occurrences: can decide ordering.
	  return xi_count < yi_count;
	break;
      }
    case 1:
      // xi_info > yi_info: it is not stabilizing.
      return false;

    case -1:
      // xi_info < yi_info: it is stabilizing.
      return true;
    }
  }
  // Here xi == xend or yi == yend.
  // Stabilization is achieved if `y_info' still has other elements.
  return (yi != yend);
}

// TODO: to be generalized so as to use an arbitrary lgo relation.
template <typename PH>
void
Polyhedra_PowerSet<PH>::BHZ03_widening_assign(const Polyhedra_PowerSet& y,
					      void (Polyhedron::*wm)
					      (const Polyhedron&, unsigned*)) {
  // `x' is the current iteration value.
  Polyhedra_PowerSet<PH>& x = *this;

#ifndef NDEBUG
  {
    // We assume that y is entailed by or equal to *this.
    const Polyhedra_PowerSet<PH> x_copy = x;
    const Polyhedra_PowerSet<PH> y_copy = y;
    assert(y_copy.definitely_entails(x_copy));
  }
#endif

  // First widening technique: do nothing.

  // If `y' is the empty collection, do nothing.
  assert(x.size() > 0);
  if (y.size() == 0)
    return;

  // Compute the poly-hull of `x'.
  PH x_hull(x.space_dim, PH::EMPTY);
  for (const_iterator i = x.begin(), iend = x.end(); i != iend; ++i)
    x_hull.poly_hull_assign(i->polyhedron());

  // Compute the poly-hull of `y'.
  PH y_hull(y.space_dim, PH::EMPTY);
  for (const_iterator i = y.begin(), iend = y.end(); i != iend; ++i)
    y_hull.poly_hull_assign(i->polyhedron());
  // Compute the base-level lgo info for `y_hull'.
  const base_lgo_info y_hull_info(y_hull);

  // If the hull info is stabilizing, do nothing.
  int hull_stabilization = y_hull_info.compare(x_hull);
  if (hull_stabilization == 1)
    return;

  // Multiset ordering is only useful when `y' is not a singleton. 
  const bool y_is_not_a_singleton = y.size() > 1;

  // The multiset lgo information for `y':
  // we want to be lazy about its computation.
  multiset_lgo_info y_info;
  bool y_info_computed = false;

  if (hull_stabilization == 0 && y_is_not_a_singleton) {
    // Collect the multiset lgo information for `y'.
    y.collect_multiset_lgo_info(y_info);
    y_info_computed = true;
    // If multiset ordering is stabilizing, do nothing.
    if (x.is_multiset_lgo_stabilizing(y_info))
      return;
  }

  // Second widening technique: try the BGP99 powerset heuristics.
  Polyhedra_PowerSet<PH> extrapolated_x = x;
  extrapolated_x.BGP99_extrapolation_assign(y, wm);

  // Compute the poly-hull of `extrapolated_x'.
  PH extrapolated_x_hull(x.space_dim, PH::EMPTY);
  for (const_iterator i = extrapolated_x.begin(),
	 iend = extrapolated_x.end(); i != iend; ++i)
    extrapolated_x_hull.poly_hull_assign(i->polyhedron());
  
  // Check for stabilization and, if successful,
  // commit to the result of the extrapolation.
  hull_stabilization = y_hull_info.compare(extrapolated_x_hull);
  if (hull_stabilization == 1) {
    // The poly-hull is stabilizing.
    std::swap(x, extrapolated_x);
    return;
  }
  else if (hull_stabilization == 0 && y_is_not_a_singleton) {
    // If not already done, compute multiset lgo info for `y'.
    if (!y_info_computed) {
      y.collect_multiset_lgo_info(y_info);
      y_info_computed = true;
    }
    if (extrapolated_x.is_multiset_lgo_stabilizing(y_info)) {
      std::swap(x, extrapolated_x);
      return;
    }
    // Third widening technique: pairwise-reduction on `extrapolated_x'.
    // Note that pairwise-reduction does not affect the computation
    // of the poly-hulls, so that we only have to check the multiset
    // lgo relation.
    Polyhedra_PowerSet<PH> reduced_extrapolated_x(extrapolated_x);
    reduced_extrapolated_x.pairwise_reduce();
    if (reduced_extrapolated_x.is_multiset_lgo_stabilizing(y_info)) {
      std::swap(x, reduced_extrapolated_x);
      return;
    }
  }

  // Fourth widening technique: this is applicable only when
  // `y_hull' is a proper subset of `extrapolated_x_hull'.
  if (extrapolated_x_hull.strictly_contains(y_hull)) {
    // Compute (y_hull \widen extrapolated_x_hull).
    PH ph = extrapolated_x_hull;
    (ph.*wm)(y_hull, 0);
    // Compute the poly-difference between `ph' and `extrapolated_x_hull'.
    ph.poly_difference_assign(extrapolated_x_hull);
    x.add_disjunct(ph);
    return;
  }

  // Fallback to the computation of the poly-hull.
  Polyhedra_PowerSet<PH> x_hull_singleton(x.space_dim, PH::EMPTY);
  x_hull_singleton.add_disjunct(x_hull);
  std::swap(x, x_hull_singleton);
}

// TODO: to be generalized so as to use an arbitrary lgo relation.
template <typename PH>
void
Polyhedra_PowerSet<PH>
::limited_BHZ03_widening_assign(const Polyhedra_PowerSet& y,
				const ConSys& cs,
				void (Polyhedron::*lwm)
				(const Polyhedron&,
				 const ConSys&,
				 unsigned*)) {
  // `x' is the current iteration value.
  Polyhedra_PowerSet<PH>& x = *this;

#ifndef NDEBUG
  {
    // We assume that y is entailed by or equal to *this.
    const Polyhedra_PowerSet<PH> x_copy = x;
    const Polyhedra_PowerSet<PH> y_copy = y;
    assert(y_copy.definitely_entails(x_copy));
  }
#endif

  // First widening technique: do nothing.

  // If `y' is the empty collection, do nothing.
  assert(x.size() > 0);
  if (y.size() == 0)
    return;

  // Compute the poly-hull of `x'.
  PH x_hull(x.space_dim, PH::EMPTY);
  for (const_iterator i = x.begin(), iend = x.end(); i != iend; ++i)
    x_hull.poly_hull_assign(i->polyhedron());

  // Compute the poly-hull of `y'.
  PH y_hull(y.space_dim, PH::EMPTY);
  for (const_iterator i = y.begin(), iend = y.end(); i != iend; ++i)
    y_hull.poly_hull_assign(i->polyhedron());
  // Compute the base-level lgo info for `y_hull'.
  const base_lgo_info y_hull_info(y_hull);

  // If the hull info is stabilizing, do nothing.
  int hull_stabilization = y_hull_info.compare(x_hull);
  if (hull_stabilization == 1)
    return;

  // Multiset ordering is only useful when `y' is not a singleton. 
  const bool y_is_not_a_singleton = y.size() > 1;

  // The multiset lgo information for `y':
  // we want to be lazy about its computation.
  multiset_lgo_info y_info;
  bool y_info_computed = false;

  if (hull_stabilization == 0 && y_is_not_a_singleton) {
    // Collect the multiset lgo information for `y'.
    y.collect_multiset_lgo_info(y_info);
    y_info_computed = true;
    // If multiset ordering is stabilizing, do nothing.
    if (x.is_multiset_lgo_stabilizing(y_info))
      return;
  }

  // Second widening technique: try the BGP99 powerset heuristics.
  Polyhedra_PowerSet<PH> extrapolated_x = x;
  extrapolated_x.limited_BGP99_heuristics_assign(y, cs, lwm);

  // Compute the poly-hull of `extrapolated_x'.
  PH extrapolated_x_hull(x.space_dim, PH::EMPTY);
  for (const_iterator i = extrapolated_x.begin(),
	 iend = extrapolated_x.end(); i != iend; ++i)
    extrapolated_x_hull.poly_hull_assign(i->polyhedron());
  
  // Check for stabilization and, if successful,
  // commit to the result of the extrapolation.
  hull_stabilization = y_hull_info.compare(extrapolated_x_hull);
  if (hull_stabilization == 1) {
    // The poly-hull is stabilizing.
    std::swap(x, extrapolated_x);
    return;
  }
  else if (hull_stabilization == 0 && y_is_not_a_singleton) {
    // If not already done, compute multiset lgo info for `y'.
    if (!y_info_computed) {
      y.collect_multiset_lgo_info(y_info);
      y_info_computed = true;
    }
    if (extrapolated_x.is_multiset_lgo_stabilizing(y_info)) {
      std::swap(x, extrapolated_x);
      return;
    }
    // Third widening technique: pairwise-reduction on `extrapolated_x'.
    // Note that pairwise-reduction does not affect the computation
    // of the poly-hulls, so that we only have to check the multiset
    // lgo relation.
    Polyhedra_PowerSet<PH> reduced_extrapolated_x(extrapolated_x);
    reduced_extrapolated_x.pairwise_reduce();
    if (reduced_extrapolated_x.is_multiset_lgo_stabilizing(y_info)) {
      std::swap(x, reduced_extrapolated_x);
      return;
    }
  }

  // Fourth widening technique: this is applicable only when
  // `y_hull' is a proper subset of `extrapolated_x_hull'.
  if (extrapolated_x_hull.strictly_contains(y_hull)) {
    // Compute (y_hull \widen extrapolated_x_hull).
    PH ph = extrapolated_x_hull;
    (ph.*lwm)(y_hull, cs, 0);
    // Compute the poly-difference between `ph' and `extrapolated_x_hull'.
    ph.poly_difference_assign(extrapolated_x_hull);
    x.add_disjunct(ph);
    return;
  }

  // Fallback to the computation of the poly-hull.
  Polyhedra_PowerSet<PH> x_hull_singleton(x.space_dim, PH::EMPTY);
  x_hull_singleton.add_disjunct(x_hull);
  std::swap(x, x_hull_singleton);
}

template <typename PH>
void
Polyhedra_PowerSet<PH>::ascii_dump(std::ostream& s) const {
  s << "size " << size()
    << "\nspace_dim " << space_dim
    << endl;
  for (const_iterator i = begin(), send = end(); i != send; ++i)
    i->polyhedron().ascii_dump(s);
}

template <typename PH>
bool
Polyhedra_PowerSet<PH>::ascii_load(std::istream& s) {
  std::string str;

  if (!(s >> str) || str != "size")
    return false;

  size_type sz;

  if (!(s >> sz))
    return false;

  if (!(s >> str) || str != "space_dim")
    return false;

  if (!(s >> space_dim))
    return false;

  Polyhedra_PowerSet new_pps(space_dim, Polyhedron::EMPTY);
  while (sz-- > 0) {
    PH ph;
    if (!ph.ascii_load(s))
      return false;
    new_pps.add_disjunct(ph);
  }
  swap(new_pps);

  // Check for well-formedness.
  assert(OK());
  return true;
}

template <typename PH>
bool
Polyhedra_PowerSet<PH>::OK() const {
  for (const_iterator i = begin(), send = end(); i != send; ++i)
    if (i->space_dimension() != space_dim) {
#ifndef NDEBUG
      std::cerr << "Space dimension mismatch: is " << i->space_dimension()
		<< " in an element of the sequence,\nshould be "
		<< space_dim << " ."
		<< std::endl;
#endif
      return false;
    }
  return Base::OK();
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::Polyhedra_PowerSet */
template <typename PH>
inline void
swap(Parma_Polyhedra_Library::Polyhedra_PowerSet<PH>& x,
     Parma_Polyhedra_Library::Polyhedra_PowerSet<PH>& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Polyhedra_PowerSet_inlines_hh)
