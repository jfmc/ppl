/* Polyhedra_Powerset class implementation: inline functions.
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

#ifndef PPL_Polyhedra_Powerset_inlines_hh
#define PPL_Polyhedra_Powerset_inlines_hh 1

#include "BHRZ03_Certificate.types.hh"
#include "ConSys.defs.hh"
#include "ConSys.inlines.hh"
#include "Widening_Function.defs.hh"
#include <algorithm>
#include <deque>
#include <string>
#include <iostream>

namespace Parma_Polyhedra_Library {

template <typename PH>
dimension_type
Polyhedra_Powerset<PH>::max_space_dimension() {
  return PH::max_space_dimension();
}

template <typename PH>
Polyhedra_Powerset<PH>::Polyhedra_Powerset(dimension_type num_dimensions,
					   Polyhedron::Degenerate_Kind kind)
  : space_dim(num_dimensions) {
  if (kind == Polyhedron::UNIVERSE)
    push_back(Determinate<PH>(num_dimensions, true));
}

template <typename PH>
Polyhedra_Powerset<PH>::Polyhedra_Powerset(const Polyhedra_Powerset& y)
  : Base(y), space_dim(y.space_dim) {
}

template <>
template <>
inline
Polyhedra_Powerset<NNC_Polyhedron>
::Polyhedra_Powerset(const Polyhedra_Powerset<C_Polyhedron>& y)
  : space_dim(y.space_dimension()) {
  for (Polyhedra_Powerset<C_Polyhedron>::const_iterator i = y.begin(),
	 y_end = y.end(); i != y_end; ++i)
    push_back(Determinate<NNC_Polyhedron>(NNC_Polyhedron(i->element())));
}

template <>
template <>
inline
Polyhedra_Powerset<C_Polyhedron>
::Polyhedra_Powerset(const Polyhedra_Powerset<NNC_Polyhedron>& y)
  : space_dim(y.space_dimension()) {
  for (Polyhedra_Powerset<NNC_Polyhedron>::const_iterator i = y.begin(),
	 y_end = y.end(); i != y_end; ++i) {
    NNC_Polyhedron ph = i->element();
    ph.topological_closure_assign();
    push_back(Determinate<C_Polyhedron>(C_Polyhedron(ph)));
  }
}

template <typename PH>
Polyhedra_Powerset<PH>::Polyhedra_Powerset(const ConSys& cs)
  : space_dim(cs.space_dimension()) {
  push_back(Determinate<PH>(cs));
}

template <typename PH>
Polyhedra_Powerset<PH>&
Polyhedra_Powerset<PH>::operator=(const Polyhedra_Powerset& y) {
  Base::operator=(y);
  space_dim = y.space_dim;
  return *this;
}

template <typename PH>
template <typename QH>
Polyhedra_Powerset<PH>&
Polyhedra_Powerset<PH>::operator=(const Polyhedra_Powerset<QH>& y) {
  Polyhedra_Powerset<PH> pps = y;
  swap(pps);
  return *this;
}

template <typename PH>
inline void
Polyhedra_Powerset<PH>::swap(Polyhedra_Powerset& y) {
  Base::swap(y);
  std::swap(space_dim, y.space_dim);
}

template <typename PH>
dimension_type
Polyhedra_Powerset<PH>::space_dimension() const {
  return space_dim;
}

template <typename PH>
void
Polyhedra_Powerset<PH>::concatenate_assign(const Polyhedra_Powerset& y) {
  // Ensure omega-reduction here, since what follows has quadratic complexity.
  Base::omega_reduce();
  y.Base::omega_reduce();
  Sequence new_sequence;
  const Polyhedra_Powerset<PH>& x = *this;
  for (const_iterator xi = x.begin(), x_end = x.end(),
	 y_begin = y.begin(), y_end = y.end(); xi != x_end; ) {
    for (const_iterator yi = y_begin; yi != y_end; ++yi) {
      CS zi = *xi;
      zi.concatenate_assign(*yi);
      assert(!zi.is_bottom());
      new_sequence.push_back(zi);
    }
    ++xi;
    if (abandon_expensive_computations && xi != x_end && y_begin != y_end) {
      // Hurry up!
      PH xph = xi->element();
      for (++xi; xi != x_end; ++xi)
	xph.poly_hull_assign(xi->element());
      const_iterator yi = y_begin;
      PH yph = yi->element();
      for (++yi; yi != y_end; ++yi)
	yph.poly_hull_assign(yi->element());
      xph.concatenate_assign(yph);
      std::swap(Base::sequence, new_sequence);
      add_disjunct(xph);
      goto done;
    }
  }
  std::swap(Base::sequence, new_sequence);
 done:
  space_dim += y.space_dim;
  assert(OK());
}

template <typename PH>
void
Polyhedra_Powerset<PH>::add_constraint(const Constraint& c) {
  for (iterator xi = Base::begin(), x_end = Base::end(); xi != x_end; ++xi)
    xi->element().add_constraint(c);
  Base::reduced = false;
}

template <typename PH>
bool
Polyhedra_Powerset<PH>::add_constraint_and_minimize(const Constraint& c) {
  for (iterator xi = Base::begin(), xin = xi, x_end = Base::end(); xi != x_end; xi = xin) {
    ++xin;
    if (!xi->element().add_constraint_and_minimize(c)) {
      erase(xi);
      x_end = Base::end();
    }
    else
      Base::reduced = false;
  }
  return !Base::empty();
}

template <typename PH>
void
Polyhedra_Powerset<PH>::add_constraints(const ConSys& cs) {
  for (iterator xi = Base::begin(), x_end = Base::end(); xi != x_end; ++xi)
    xi->element().add_constraints(cs);
  Base::reduced = false;
}

template <typename PH>
bool
Polyhedra_Powerset<PH>::add_constraints_and_minimize(const ConSys& cs) {
  for (iterator xi = Base::begin(),
	 xin = xi, x_end = Base::end(); xi != x_end; xi = xin) {
    ++xin;
    if (!xi->element().add_constraints_and_minimize(cs)) {
      erase(xi);
      x_end = Base::end();
    }
    else
      Base::reduced = false;
  }
  return !Base::empty();
}

template <typename PH>
void
Polyhedra_Powerset<PH>::add_dimensions_and_embed(dimension_type m) {
  for (iterator i = Base::begin(), send = Base::end(); i != send; ++i)
    i->add_dimensions_and_embed(m);
  space_dim += m;
  assert(OK());
}

template <typename PH>
void
Polyhedra_Powerset<PH>::add_dimensions_and_project(dimension_type m) {
  for (iterator i = Base::begin(), send = Base::end(); i != send; ++i)
    i->add_dimensions_and_project(m);
  space_dim += m;
  assert(OK());
}

template <typename PH>
void
Polyhedra_Powerset<PH>::remove_dimensions(const Variables_Set& to_be_removed) {
  Variables_Set::size_type num_removed = to_be_removed.size();
  if (num_removed > 0) {
    for (iterator i = Base::begin(), send = Base::end(); i != send; ++i) {
      i->remove_dimensions(to_be_removed);
      Base::reduced = false;
    }
    space_dim -= num_removed;
    assert(OK());
  }
}

template <typename PH>
void
Polyhedra_Powerset<PH>::remove_higher_dimensions(dimension_type
						 new_dimension) {
  if (new_dimension < space_dim) {
    for (iterator i = Base::begin(), send = Base::end(); i != send; ++i) {
      i->remove_higher_dimensions(new_dimension);
      Base::reduced = false;
    }
    space_dim = new_dimension;
    assert(OK());
  }
}

template <typename PH>
bool
Polyhedra_Powerset<PH>::
geometrically_covers(const Polyhedra_Powerset& y) const {
  for (const_iterator yi = y.Base::begin(),
	 y_end = y.Base::end(); yi != y_end; ++yi)
    if (!check_containment(yi->element(), *this))
      return false;
  return true;
}

template <typename PH>
bool
Polyhedra_Powerset<PH>::
geometrically_equals(const Polyhedra_Powerset& y) const {
  const Polyhedra_Powerset& x = * this;
  return x.geometrically_covers(y) && y.geometrically_covers(x);
}

template <typename PH>
template <typename PartialFunction>
void
Polyhedra_Powerset<PH>::map_dimensions(const PartialFunction& pfunc) {
  if (Base::is_bottom()) {
    dimension_type n = 0;
    for (dimension_type i = space_dim; i-- > 0; ) {
      dimension_type new_i;
      if (pfunc.maps(i, new_i))
	++n;
    }
    space_dim = n;
  }
  else {
    iterator sbegin = Base::begin();
    for (iterator i = sbegin, send = Base::end(); i != send; ++i)
      i->map_dimensions(pfunc);
    space_dim = sbegin->space_dimension();
    Base::reduced = false;
  }
  assert(OK());
}

template <typename PH>
void
Polyhedra_Powerset<PH>::pairwise_reduce() {
  // It is wise to omega-reduce before pairwise-reducing.
  Base::omega_reduce();

  size_type n = Base::size();
  size_type deleted;
  do {
    Sequence new_sequence;
    std::deque<bool> marked(n, false);
    deleted = 0;
    iterator sbegin = Base::begin();
    iterator send = Base::end();
    unsigned i_index = 0;
    for (iterator i = sbegin; i != send; ++i, ++i_index) {
      if (marked[i_index])
	continue;
      PH& pi = i->element();
      const_iterator j = i;
      int j_index = i_index;
      for (++j, ++j_index; j != send; ++j, ++j_index) {
	if (marked[j_index])
	  continue;
	const PH& pj = j->element();
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
    std::swap(Base::sequence, new_sequence);
    n -= deleted;
  } while (deleted > 0);
  assert(OK());
}

template <typename PH>
template <typename Widening>
void
Polyhedra_Powerset<PH>::
BGP99_heuristics_assign(const Polyhedra_Powerset& y, Widening wf) {
#ifndef NDEBUG
  {
    // We assume that y entails *this.
    const Polyhedra_Powerset<PH> x_copy = *this;
    const Polyhedra_Powerset<PH> y_copy = y;
    assert(y_copy.definitely_entails(x_copy));
  }
#endif

  size_type n = Base::size();
  Sequence new_sequence;
  std::deque<bool> marked(n, false);
  const_iterator sbegin = Base::begin();
  const_iterator send = Base::end();
  unsigned i_index = 0;
  for (const_iterator i = sbegin; i != send; ++i, ++i_index)
    for (const_iterator j = y.Base::begin(),
	   y_end = y.Base::end(); j != y_end; ++j) {
      const PH& pi = i->element();
      const PH& pj = j->element();
      if (pi.contains(pj)) {
	PH pi_copy = pi;
	wf(pi_copy, pj);
	add_non_bottom_disjunct(new_sequence, pi_copy);
	marked[i_index] = true;
      }
    }
  iterator nsbegin = new_sequence.begin();
  iterator nsend = new_sequence.end();
  i_index = 0;
  for (const_iterator i = sbegin; i != send; ++i, ++i_index)
    if (!marked[i_index])
      add_non_bottom_disjunct(new_sequence, *i, nsbegin, nsend);
  std::swap(Base::sequence, new_sequence);
  assert(OK());
  assert(Base::is_omega_reduced());
}

template <typename PH>
template <typename Widening>
void
Polyhedra_Powerset<PH>::
BGP99_extrapolation_assign(const Polyhedra_Powerset& y,
			   Widening wf,
			   unsigned max_disjuncts) {
  // `x' is the current iteration value.
  Polyhedra_Powerset<PH>& x = *this;

#ifndef NDEBUG
  {
    // We assume that y entails *this.
    const Polyhedra_Powerset<PH> x_copy = x;
    const Polyhedra_Powerset<PH> y_copy = y;
    assert(y_copy.definitely_entails(x_copy));
  }
#endif

  x.pairwise_reduce();
  if (max_disjuncts != 0)
    x.collapse(max_disjuncts);
  x.BGP99_heuristics_assign(y, wf);
}

template <typename PH>
template <typename Cert>
void
Polyhedra_Powerset<PH>::
collect_certificates(std::map<Cert, size_type,
		              typename Cert::Compare>& cert_ms) const {
  assert(Base::is_omega_reduced());
  assert(cert_ms.size() == 0);
  for (const_iterator i = Base::begin(),
	 iend = Base::end(); i != iend; i++) {
    Cert ph_cert(i->element());
    ++cert_ms[ph_cert];
  }
}

template <typename PH>
template <typename Cert>
bool
Polyhedra_Powerset<PH>::
is_cert_multiset_stabilizing(const std::map<Cert, size_type,
			                    typename Cert::Compare>& y_cert_ms
			     ) const {
  typedef std::map<Cert, size_type, typename Cert::Compare> Cert_Multiset;
  Cert_Multiset x_cert_ms;
  collect_certificates(x_cert_ms);
  typename Cert_Multiset::const_iterator
    xi = x_cert_ms.begin(),
    xend = x_cert_ms.end(),
    yi = y_cert_ms.begin(),
    yend = y_cert_ms.end();
  while (xi != xend && yi != yend) {
    const Cert& xi_cert = xi->first;  
    const Cert& yi_cert = yi->first;
    switch (xi_cert.compare(yi_cert)) {
    case 0:
      // xi_cert == yi_cert: check the number of multiset occurrences.
      {
	const size_type& xi_count = xi->second;
	const size_type& yi_count = yi->second;
	if (xi_count == yi_count) {
	  // Same number of occurrences: compare the next pair.
	  ++xi;
	  ++yi;
	}
	else
	  // Different number of occurrences: can decide ordering.
	  return xi_count < yi_count;
	break;
      }
    case 1:
      // xi_cert > yi_cert: it is not stabilizing.
      return false;

    case -1:
      // xi_cert < yi_cert: it is stabilizing.
      return true;
    }
  }
  // Here xi == xend or yi == yend.
  // Stabilization is achieved if `y_cert_ms' still has other elements.
  return (yi != yend);
}

template <typename PH>
template <typename Cert, typename Widening>
void
Polyhedra_Powerset<PH>::BHZ03_widening_assign(const Polyhedra_Powerset& y,
					      Widening wf) {
  // `x' is the current iteration value.
  Polyhedra_Powerset<PH>& x = *this;

#ifndef NDEBUG
  {
    // We assume that y entails *this.
    const Polyhedra_Powerset<PH> x_copy = x;
    const Polyhedra_Powerset<PH> y_copy = y;
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
    x_hull.poly_hull_assign(i->element());

  // Compute the poly-hull of `y'.
  PH y_hull(y.space_dim, PH::EMPTY);
  for (const_iterator i = y.begin(), iend = y.end(); i != iend; ++i)
    y_hull.poly_hull_assign(i->element());
  // Compute the certificate for `y_hull'.
  const Cert y_hull_cert(y_hull);

  // If the hull is stabilizing, do nothing.
  int hull_stabilization = y_hull_cert.compare(x_hull);
  if (hull_stabilization == 1)
    return;

  // Multiset ordering is only useful when `y' is not a singleton. 
  const bool y_is_not_a_singleton = y.size() > 1;

  // The multiset certificate for `y':
  // we want to be lazy about its computation.
  typedef std::map<Cert, size_type, typename Cert::Compare> Cert_Multiset;
  Cert_Multiset y_cert_ms;
  bool y_cert_ms_computed = false;

  if (hull_stabilization == 0 && y_is_not_a_singleton) {
    // Collect the multiset certificate for `y'.
    y.collect_certificates(y_cert_ms);
    y_cert_ms_computed = true;
    // If multiset ordering is stabilizing, do nothing.
    if (x.is_cert_multiset_stabilizing(y_cert_ms))
      return;
  }

  // Second widening technique: try the BGP99 powerset heuristics.
  Polyhedra_Powerset<PH> bgp99_heuristics = x;
  bgp99_heuristics.BGP99_heuristics_assign(y, wf);

  // Compute the poly-hull of `bgp99_heuristics'.
  PH bgp99_heuristics_hull(x.space_dim, PH::EMPTY);
  for (const_iterator i = bgp99_heuristics.begin(),
	 iend = bgp99_heuristics.end(); i != iend; ++i)
    bgp99_heuristics_hull.poly_hull_assign(i->element());
  
  // Check for stabilization and, if successful,
  // commit to the result of the extrapolation.
  hull_stabilization = y_hull_cert.compare(bgp99_heuristics_hull);
  if (hull_stabilization == 1) {
    // The poly-hull is stabilizing.
    std::swap(x, bgp99_heuristics);
    return;
  }
  else if (hull_stabilization == 0 && y_is_not_a_singleton) {
    // If not already done, compute multiset certificate for `y'.
    if (!y_cert_ms_computed) {
      y.collect_certificates(y_cert_ms);
      y_cert_ms_computed = true;
    }
    if (bgp99_heuristics.is_cert_multiset_stabilizing(y_cert_ms)) {
      std::swap(x, bgp99_heuristics);
      return;
    }
    // Third widening technique: pairwise-reduction on `bgp99_heuristics'.
    // Note that pairwise-reduction does not affect the computation
    // of the poly-hulls, so that we only have to check the multiset
    // certificate relation.
    Polyhedra_Powerset<PH> reduced_bgp99_heuristics(bgp99_heuristics);
    reduced_bgp99_heuristics.pairwise_reduce();
    if (reduced_bgp99_heuristics.is_cert_multiset_stabilizing(y_cert_ms)) {
      std::swap(x, reduced_bgp99_heuristics);
      return;
    }
  }

  // Fourth widening technique: this is applicable only when
  // `y_hull' is a proper subset of `bgp99_heuristics_hull'.
  if (bgp99_heuristics_hull.strictly_contains(y_hull)) {
    // Compute (y_hull \widen bgp99_heuristics_hull).
    PH ph = bgp99_heuristics_hull;
    wf(ph, y_hull);
    // Compute the poly-difference between `ph' and `bgp99_heuristics_hull'.
    ph.poly_difference_assign(bgp99_heuristics_hull);
    x.add_disjunct(ph);
    return;
  }

  // Fallback to the computation of the poly-hull.
  Polyhedra_Powerset<PH> x_hull_singleton(x.space_dim, PH::EMPTY);
  x_hull_singleton.add_disjunct(x_hull);
  std::swap(x, x_hull_singleton);
}

template <typename PH>
template <typename Widening>
inline void
Polyhedra_Powerset<PH>::
BHZ03_widening_assign(const Polyhedra_Powerset& y, Widening wf) {
  BHZ03_widening_assign<BHRZ03_Certificate>(y, wf);
}

template <typename PH>
void
Polyhedra_Powerset<PH>::ascii_dump(std::ostream& s) const {
  s << "size " << Base::size()
    << "\nspace_dim " << space_dim
    << std::endl;
  for (const_iterator i = Base::begin(), send = Base::end(); i != send; ++i)
    i->element().ascii_dump(s);
}

template <typename PH>
bool
Polyhedra_Powerset<PH>::ascii_load(std::istream& s) {
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

  Polyhedra_Powerset new_pps(space_dim, Polyhedron::EMPTY);
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
Polyhedra_Powerset<PH>::OK() const {
  for (const_iterator i = Base::begin(), send = Base::end(); i != send; ++i)
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

namespace {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \brief
//! Partitions polyhedron \p qq according to constraint \p c.
/*! \relates Polyhedra_Powerset
  On exit, the intersection of \p qq and constraint \p c is stored
  in \p qq, whereas the intersection of \p qq with the negation of \p c
  is added as a new disjunct of the powerset \p r.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename PH>
void
linear_partition_aux(const Constraint& c,
		     PH& qq,
		     Polyhedra_Powerset<NNC_Polyhedron>& r) {
  LinExpression le(c);
  Constraint neg_c = c.is_strict_inequality() ? (le <= 0) : (le < 0);
  NNC_Polyhedron qqq(qq);
  if (qqq.add_constraint_and_minimize(neg_c))
    r.add_disjunct(qqq);
  qq.add_constraint(c);
}

} // namespace

/*! \relates Polyhedra_Powerset */
template <typename PH>
std::pair<PH, Polyhedra_Powerset<NNC_Polyhedron> >
linear_partition(const PH& p, const PH& q) {
  Polyhedra_Powerset<NNC_Polyhedron> r(p.space_dimension(),
				       Polyhedron::EMPTY);
  PH qq = q;
  const ConSys& pcs = p.constraints();
  for (ConSys::const_iterator i = pcs.begin(),
	 pcs_end = pcs.end(); i != pcs_end; ++i) {
    const Constraint c = *i;
    if (c.is_equality()) {
      LinExpression le(c);
      linear_partition_aux(le <= 0, qq, r);
      linear_partition_aux(le >= 0, qq, r);
    }
    else
      linear_partition_aux(c, qq, r);
  }
  return std::pair<PH, Polyhedra_Powerset<NNC_Polyhedron> >(qq, r);
}

template <>
inline void
Polyhedra_Powerset<NNC_Polyhedron>
::poly_difference_assign(const Polyhedra_Powerset& y) {
  // Ensure omega-reduction.
  Base::omega_reduce();
  y.Base::omega_reduce();
  Sequence new_sequence = Base::sequence;
  for (const_iterator yi = y.begin(), y_end = y.end(); yi != y_end; ++yi) {
    const NNC_Polyhedron& py = yi->element();
    Sequence tmp_sequence;
    for (const_iterator nsi = new_sequence.begin(),
	   ns_end = new_sequence.end(); nsi != ns_end; ++nsi) {
      std::pair<NNC_Polyhedron, Polyhedra_Powerset<NNC_Polyhedron> > partition
	= linear_partition(py, nsi->element());
      const Polyhedra_Powerset<NNC_Polyhedron>& residues = partition.second;
      // Append the contents of `residues' to `tmp_sequence'.
      std::copy(residues.begin(), residues.end(), back_inserter(tmp_sequence));
    }
    std::swap(tmp_sequence, new_sequence);
  }
  std::swap(Base::sequence, new_sequence);
  Base::reduced = false;
  assert(OK());
}

template <>
inline void
Polyhedra_Powerset<C_Polyhedron>
::poly_difference_assign(const Polyhedra_Powerset& y) {
  Polyhedra_Powerset<NNC_Polyhedron> nnc_this = *this;
  Polyhedra_Powerset<NNC_Polyhedron> nnc_y = y;
  nnc_this.poly_difference_assign(nnc_y);
  *this = nnc_this;
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::Polyhedra_Powerset */
template <typename PH>
inline void
swap(Parma_Polyhedra_Library::Polyhedra_Powerset<PH>& x,
     Parma_Polyhedra_Library::Polyhedra_Powerset<PH>& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Polyhedra_Powerset_inlines_hh)
