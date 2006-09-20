/* Pointset_Ask_Tell class implementation: non-inline template functions.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Pointset_Ask_Tell_templates_hh
#define PPL_Pointset_Ask_Tell_templates_hh 1

#include "Constraint.defs.hh"
#include "Constraint_System.defs.hh"
#include "Constraint_System.inlines.hh"
#include "C_Polyhedron.defs.hh"
#include "NNC_Polyhedron.defs.hh"
#include <algorithm>
#include <deque>
#include <string>
#include <iostream>
#include <sstream>
#include <stdexcept>

namespace Parma_Polyhedra_Library {

template <typename PH>
void
Pointset_Ask_Tell<PH>::add_disjunct(const PH& ph) {
  Pointset_Ask_Tell& x = *this;
  if (x.space_dimension() != ph.space_dimension()) {
    std::ostringstream s;
    s << "PPL::Pointset_Ask_Tell<PH>::add_disjunct(ph):\n"
      << "this->space_dimension() == " << x.space_dimension() << ", "
      << "ph.space_dimension() == " << ph.space_dimension() << ".";
    throw std::invalid_argument(s.str());
  }
  x.sequence.push_back(Determinate<PH>(ph));
  x.reduced = false;
  assert(x.OK());
}

template <>
template <typename QH>
Pointset_Ask_Tell<NNC_Polyhedron>
::Pointset_Ask_Tell(const Pointset_Ask_Tell<QH>& y)
  : Base(), space_dim(y.space_dimension()) {
  Pointset_Ask_Tell& x = *this;
  for (typename Pointset_Ask_Tell<QH>::const_iterator i = y.begin(),
	 y_end = y.end(); i != y_end; ++i) {
    Determinate<NNC_Polyhedron>
      nnc_ask(NNC_Polyhedron(i->ask().element().constraints()));
    Determinate<NNC_Polyhedron>
      nnc_tell(NNC_Polyhedron(i->tell().element().constraints()));
    x.sequence.push_back(Pair(nnc_ask, nnc_tell));
  }
  // FIXME: the following is a bug!
  x.normalized = y.normalized;
  assert(x.OK());
}

template <>
template <typename QH>
Pointset_Ask_Tell<C_Polyhedron>
::Pointset_Ask_Tell(const Pointset_Ask_Tell<QH>& y)
  : Base(), space_dim(y.space_dimension()) {
  Pointset_Ask_Tell& x = *this;
  for (typename Pointset_Ask_Tell<QH>::const_iterator i = y.begin(),
	 y_end = y.end(); i != y_end; ++i) {
    Determinate<C_Polyhedron>
      c_ask(C_Polyhedron(i->ask().element().constraints()));
    Determinate<C_Polyhedron>
      c_tell(C_Polyhedron(i->tell().element().constraints()));
    x.sequence.push_back(Pair(c_ask, c_tell));
  }

  // FIXME: the following comment should be rephrased!
  // Note: this might be non-reduced even when `y' is known to be
  // omega-reduced, because the constructor of C_Polyhedron, by
  // enforcing topological closure, may have made different elements
  // comparable.
  x.normalized = false;
  assert(x.OK());
}

template <typename PH>
void
Pointset_Ask_Tell<PH>::concatenate_assign(const Pointset_Ask_Tell& y) {
  Pointset_Ask_Tell& x = *this;
  for (const_iterator yi = y.begin(), y_end = y.end(); yi != y_end; ++yi) {
    CS ask(PH(space_dim, UNIVERSE));
    ask.concatenate_assign(yi->ask());
    CS tell(PH(space_dim, UNIVERSE));
    tell.concatenate_assign(yi->tell());
    // FIXME: why the following does not work?
    //x.sequence.push_back(Pair(ask, tell));
    x.sequence.push_back(Ask_Tell_Pair<CS>(ask, tell));
  }
  space_dim += y.space_dim;
  if (x.normalized)
    x.normalized = y.normalized;
  assert(x.OK());
}

template <typename PH>
void
Pointset_Ask_Tell<PH>::add_constraint(const Constraint& c) {
  Pointset_Ask_Tell& x = *this;
  for (Sequence_iterator si = x.sequence.begin(),
	 s_end = x.sequence.end(); si != s_end; ++si)
    si->element().add_constraint(c);
  x.reduced = false;
  assert(x.OK());
}

template <typename PH>
bool
Pointset_Ask_Tell<PH>::add_constraint_and_minimize(const Constraint& c) {
  Pointset_Ask_Tell& x = *this;
  for (Sequence_iterator si = x.sequence.begin(),
	 s_end = x.sequence.end(); si != s_end; )
    if (!si->element().add_constraint_and_minimize(c))
      si = x.sequence.erase(si);
    else {
      x.reduced = false;
      ++si;
    }
  assert(x.OK());
  return !x.empty();
}

template <typename PH>
void
Pointset_Ask_Tell<PH>::add_constraints(const Constraint_System& cs) {
  Pointset_Ask_Tell& x = *this;
  for (Sequence_iterator si = x.sequence.begin(),
	 s_end = x.sequence.end(); si != s_end; ++si)
    si->element().add_constraints(cs);
  x.reduced = false;
  assert(x.OK());
}

template <typename PH>
bool
Pointset_Ask_Tell<PH>::
add_constraints_and_minimize(const Constraint_System& cs) {
  Pointset_Ask_Tell& x = *this;
  for (Sequence_iterator si = x.sequence.begin(),
	 s_end = x.sequence.end(); si != s_end; )
    if (!si->element().add_constraints_and_minimize(cs))
      si = x.sequence.erase(si);
    else {
      x.reduced = false;
      ++si;
    }
  assert(x.OK());
  return !x.empty();
}

template <typename PH>
void
Pointset_Ask_Tell<PH>::add_space_dimensions_and_embed(dimension_type m) {
  Pointset_Ask_Tell& x = *this;
  for (Sequence_iterator si = x.sequence.begin(),
	 s_end = x.sequence.end(); si != s_end; ++si)
    si->element().add_space_dimensions_and_embed(m);
  x.space_dim += m;
  assert(x.OK());
}

template <typename PH>
void
Pointset_Ask_Tell<PH>::add_space_dimensions_and_project(dimension_type m) {
  Pointset_Ask_Tell& x = *this;
  for (Sequence_iterator si = x.sequence.begin(),
	 s_end = x.sequence.end(); si != s_end; ++si)
    si->element().add_space_dimensions_and_project(m);
  x.space_dim += m;
  assert(x.OK());
}

template <typename PH>
void
Pointset_Ask_Tell<PH>::
remove_space_dimensions(const Variables_Set& to_be_removed) {
  Pointset_Ask_Tell& x = *this;
  Variables_Set::size_type num_removed = to_be_removed.size();
  if (num_removed > 0) {
    for (Sequence_iterator si = x.sequence.begin(),
	   s_end = x.sequence.end(); si != s_end; ++si) {
      si->ask().element().remove_space_dimensions(to_be_removed);
      si->tell().element().remove_space_dimensions(to_be_removed);
      x.normalized = false;
    }
    x.space_dim -= num_removed;
    assert(x.OK());
  }
}

template <typename PH>
void
Pointset_Ask_Tell<PH>::remove_higher_space_dimensions(dimension_type
						       new_dimension) {
  Pointset_Ask_Tell& x = *this;
  if (new_dimension < x.space_dim) {
    for (Sequence_iterator si = x.sequence.begin(),
	   s_end = x.sequence.end(); si != s_end; ++si) {
      si->ask().element().remove_higher_space_dimensions(new_dimension);
      si->tell().element().remove_higher_space_dimensions(new_dimension);
      x.reduced = false;
    }
    x.space_dim = new_dimension;
    assert(x.OK());
  }
}

template <typename PH>
template <typename Partial_Function>
void
Pointset_Ask_Tell<PH>::map_space_dimensions(const Partial_Function& pfunc) {
  Pointset_Ask_Tell& x = *this;
  if (x.is_bottom()) {
    dimension_type n = 0;
    for (dimension_type i = x.space_dim; i-- > 0; ) {
      dimension_type new_i;
      if (pfunc.maps(i, new_i))
	++n;
    }
    x.space_dim = n;
  }
  else {
    Sequence_iterator s_begin = x.sequence.begin();
    for (Sequence_iterator si = s_begin,
	   s_end = x.sequence.end(); si != s_end; ++si)
      si->element().map_space_dimensions(pfunc);
    x.space_dim = s_begin->element().space_dimension();
    x.reduced = false;
  }
  assert(x.OK());
}

template <typename PH>
void
Pointset_Ask_Tell<PH>::pairwise_reduce() {
  Pointset_Ask_Tell& x = *this;
  // It is wise to omega-reduce before pairwise-reducing.
  x.omega_reduce();

  size_type n = x.size();
  size_type deleted;
  do {
    Pointset_Ask_Tell new_x(x.space_dim, EMPTY);
    std::deque<bool> marked(n, false);
    deleted = 0;
    Sequence_iterator s_begin = x.sequence.begin();
    Sequence_iterator s_end = x.sequence.end();
    unsigned si_index = 0;
    for (Sequence_iterator si = s_begin; si != s_end; ++si, ++si_index) {
      if (marked[si_index])
	continue;
      PH& pi = si->element();
      Sequence_const_iterator sj = si;
      unsigned sj_index = si_index;
      for (++sj, ++sj_index; sj != s_end; ++sj, ++sj_index) {
	if (marked[sj_index])
	  continue;
	const PH& pj = sj->element();
	if (pi.upper_bound_assign_if_exact(pj)) {
	  marked[si_index] = marked[sj_index] = true;
	  // FIXME: check whether the preservation of reduction was
	  // actually meant here.
	  new_x.add_non_bottom_disjunct_preserve_reduction(pi);
	  ++deleted;
	  goto next;
	}
      }
    next:
      ;
    }
    iterator nx_begin = new_x.begin();
    iterator nx_end = new_x.end();
    unsigned xi_index = 0;
    for (const_iterator xi = x.begin(),
	   x_end = x.end(); xi != x_end; ++xi, ++xi_index)
      if (!marked[xi_index])
	nx_begin = new_x.add_non_bottom_disjunct_preserve_reduction(*xi,
								    nx_begin,
								    nx_end);
    std::swap(x.sequence, new_x.sequence);
    n -= deleted;
  } while (deleted > 0);
  assert(x.OK());
}

template <typename PH>
template <typename Widening>
void
Pointset_Ask_Tell<PH>::
BGP99_heuristics_assign(const Pointset_Ask_Tell& y, Widening wf) {
  // `x' is the current iteration value.
  Pointset_Ask_Tell& x = *this;

#ifndef NDEBUG
  {
    // We assume that `y' entails `x'.
    const Pointset_Ask_Tell<PH> x_copy = x;
    const Pointset_Ask_Tell<PH> y_copy = y;
    assert(y_copy.definitely_entails(x_copy));
  }
#endif

  size_type n = x.size();
  Pointset_Ask_Tell new_x(x.space_dim, EMPTY);
  std::deque<bool> marked(n, false);
  const_iterator x_begin = x.begin();
  const_iterator x_end = x.end();
  unsigned i_index = 0;
  for (const_iterator i = x_begin,
	 y_begin = y.begin(), y_end = y.end(); i != x_end; ++i, ++i_index)
    for (const_iterator j = y_begin; j != y_end; ++j) {
      const PH& pi = i->element();
      const PH& pj = j->element();
      if (pi.contains(pj)) {
	PH pi_copy = pi;
	wf(pi_copy, pj);
	// FIXME: check whether the preservation of reduction was
	// actually meant here.
	new_x.add_non_bottom_disjunct_preserve_reduction(pi_copy);
	marked[i_index] = true;
      }
    }
  iterator nx_begin = new_x.begin();
  iterator nx_end = new_x.end();
  i_index = 0;
  for (const_iterator i = x_begin; i != x_end; ++i, ++i_index)
    if (!marked[i_index])
      nx_begin = new_x.add_non_bottom_disjunct_preserve_reduction(*i,
								  nx_begin,
								  nx_end);
  std::swap(x.sequence, new_x.sequence);
  assert(x.OK());
  assert(x.is_omega_reduced());
}

template <typename PH>
template <typename Widening>
void
Pointset_Ask_Tell<PH>::
BGP99_extrapolation_assign(const Pointset_Ask_Tell& y,
			   Widening wf,
			   unsigned max_disjuncts) {
  // `x' is the current iteration value.
  Pointset_Ask_Tell& x = *this;

#ifndef NDEBUG
  {
    // We assume that `y' entails `x'.
    const Pointset_Ask_Tell<PH> x_copy = x;
    const Pointset_Ask_Tell<PH> y_copy = y;
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
Pointset_Ask_Tell<PH>::
collect_certificates(std::map<Cert, size_type,
		              typename Cert::Compare>& cert_ms) const {
  const Pointset_Ask_Tell& x = *this;
  assert(x.is_omega_reduced());
  assert(cert_ms.size() == 0);
  for (const_iterator i = x.begin(), end = x.end(); i != end; i++) {
    Cert ph_cert(i->element());
    ++cert_ms[ph_cert];
  }
}

template <typename PH>
template <typename Cert>
bool
Pointset_Ask_Tell<PH>::
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
  return yi != yend;
}

template <typename PH>
template <typename Cert, typename Widening>
void
Pointset_Ask_Tell<PH>::BHZ03_widening_assign(const Pointset_Ask_Tell& y,
					      Widening wf) {
  // `x' is the current iteration value.
  Pointset_Ask_Tell& x = *this;

#ifndef NDEBUG
  {
    // We assume that `y' entails `x'.
    const Pointset_Ask_Tell<PH> x_copy = x;
    const Pointset_Ask_Tell<PH> y_copy = y;
    assert(y_copy.definitely_entails(x_copy));
  }
#endif

  // First widening technique: do nothing.

  // If `y' is the empty collection, do nothing.
  assert(x.size() > 0);
  if (y.size() == 0)
    return;

  // Compute the poly-hull of `x'.
  PH x_hull(x.space_dim, EMPTY);
  for (const_iterator i = x.begin(), x_end = x.end(); i != x_end; ++i)
    x_hull.upper_bound_assign(i->element());

  // Compute the poly-hull of `y'.
  PH y_hull(y.space_dim, EMPTY);
  for (const_iterator i = y.begin(), y_end = y.end(); i != y_end; ++i)
    y_hull.upper_bound_assign(i->element());
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
  Pointset_Ask_Tell<PH> bgp99_heuristics = x;
  bgp99_heuristics.BGP99_heuristics_assign(y, wf);

  // Compute the poly-hull of `bgp99_heuristics'.
  PH bgp99_heuristics_hull(x.space_dim, EMPTY);
  for (const_iterator i = bgp99_heuristics.begin(),
	 bh_end = bgp99_heuristics.end(); i != bh_end; ++i)
    bgp99_heuristics_hull.upper_bound_assign(i->element());

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
    Pointset_Ask_Tell<PH> reduced_bgp99_heuristics(bgp99_heuristics);
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
    // Compute the difference between `ph' and `bgp99_heuristics_hull'.
    ph.difference_assign(bgp99_heuristics_hull);
    x.add_disjunct(ph);
    return;
  }

  // Fall back to the computation of the poly-hull.
  Pointset_Ask_Tell<PH> x_hull_singleton(x.space_dim, EMPTY);
  x_hull_singleton.add_disjunct(x_hull);
  std::swap(x, x_hull_singleton);
}

template <typename PH>
void
Pointset_Ask_Tell<PH>::ascii_dump(std::ostream& s) const {
  const Pointset_Ask_Tell& x = *this;
  s << "size " << x.size()
    << "\nspace_dim " << x.space_dim
    << "\n";
  for (const_iterator xi = x.begin(), x_end = x.end(); xi != x_end; ++xi)
    xi->element().ascii_dump(s);
}

PPL_OUTPUT_TEMPLATE_DEFINITIONS(PH, Pointset_Ask_Tell<PH>)

template <typename PH>
bool
Pointset_Ask_Tell<PH>::ascii_load(std::istream& s) {
  Pointset_Ask_Tell& x = *this;
  std::string str;

  if (!(s >> str) || str != "size")
    return false;

  size_type sz;

  if (!(s >> sz))
    return false;

  if (!(s >> str) || str != "space_dim")
    return false;

  if (!(s >> x.space_dim))
    return false;

  Pointset_Ask_Tell new_x(x.space_dim, EMPTY);
  while (sz-- > 0) {
    PH ph;
    if (!ph.ascii_load(s))
      return false;
    new_x.add_disjunct(ph);
  }
  x.swap(new_x);

  // Check for well-formedness.
  assert(x.OK());
  return true;
}

template <typename PH>
bool
Pointset_Ask_Tell<PH>::OK() const {
  const Pointset_Ask_Tell& x = *this;
  for (const_iterator xi = x.begin(), x_end = x.end(); xi != x_end; ++xi) {
    const PH& ask_i = xi->ask().element();
    const PH& tell_i = xi->tell().element();
    if (ask_i.space_dimension() != x.space_dim
	|| tell_i.space_dimension() != x.space_dim) {
#ifndef NDEBUG
      std::cerr << "Space dimension mismatch: is ("
		<< ask_i.space_dimension()
		<< " -> "
		<< tell_i.space_dimension()
		<< ") in an element of the sequence,\nshould be "
		<< x.space_dim << "."
		<< std::endl;
#endif
      return false;
    }
  }
  return x.Base::OK();
}


namespace Implementation {

namespace Pointset_Ask_Tells {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Partitions polyhedron \p qq according to constraint \p c.
/*! \relates Parma_Polyhedra_Library::Pointset_Ask_Tell
  On exit, the intersection of \p qq and constraint \p c is stored
  in \p qq, whereas the intersection of \p qq with the negation of \p c
  is added as a new disjunct of the powerset \p r.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename PH>
void
linear_partition_aux(const Constraint& c,
		     PH& qq,
		     Pointset_Ask_Tell<NNC_Polyhedron>& r) {
  Linear_Expression le(c);
  Constraint neg_c = c.is_strict_inequality() ? (le <= 0) : (le < 0);
  NNC_Polyhedron qqq(qq);
  if (qqq.add_constraint_and_minimize(neg_c))
    r.add_disjunct(qqq);
  qq.add_constraint(c);
}

} // namespace Pointset_Ask_Tells

} // namespace Implementation


/*! \relates Pointset_Ask_Tell */
template <typename PH>
std::pair<PH, Pointset_Ask_Tell<NNC_Polyhedron> >
linear_partition(const PH& p, const PH& q) {
  using Implementation::Pointset_Ask_Tells::linear_partition_aux;

  Pointset_Ask_Tell<NNC_Polyhedron> r(p.space_dimension(), EMPTY);
  PH qq = q;
  const Constraint_System& pcs = p.constraints();
  for (Constraint_System::const_iterator i = pcs.begin(),
	 pcs_end = pcs.end(); i != pcs_end; ++i) {
    const Constraint c = *i;
    if (c.is_equality()) {
      Linear_Expression le(c);
      linear_partition_aux(le <= 0, qq, r);
      linear_partition_aux(le >= 0, qq, r);
    }
    else
      linear_partition_aux(c, qq, r);
  }
  return std::pair<PH, Pointset_Ask_Tell<NNC_Polyhedron> >(qq, r);
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Pointset_Ask_Tell_templates_hh)
