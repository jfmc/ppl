/* A collection of useful convex polyhedra algorithms: inline functions.
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

#include "NNC_Polyhedron.defs.hh"
#include "Determinate.defs.hh"
#include "PowerSet.defs.hh"
#include "Constraint.defs.defs.hh"
#include "LinExpression.defs.defs.hh"
#include "ConSys.defs.defs.hh"
#include <utility>
#include <cassert>

namespace Parma_Polyhedra_Library {

//! Partitions \p q with respect to \p p.
/*!
  Let \p p and \p q be two polyhedra.
  The function returns an object <CODE>r</CODE> of type
  <CODE>std::pair\<PH, PowerSet\<Determinate\<NNC_Polyhedron\> \> \></CODE>
  such that
  - <CODE>r.first</CODE> is the intersection of \p p and \p q;
  - <CODE>r.second</CODE> has the property that all its elements are
    not empty, pairwise disjoint, and disjoint from \p p;
  - the union of <CODE>r.first</CODE> with all the elements of
    <CODE>r.second</CODE> gives \p q (i.e., <CODE>r</CODE> is the
    representation of a partition of \p q).

  \if Include_Implementation_Details

  See
  <A HREF="http://www.cs.unipr.it/ppl/Documentation/bibliography#Srivastava93">
  this paper</A> for more information about the implementation.
  \endif
*/
template <typename PH>
std::pair<PH, PowerSet<Determinate<NNC_Polyhedron> > >
linear_partition(const PH& p, const PH& q);

template <typename PH>
void
H79_extrapolation_assign(PowerSet<Determinate<PH> >& r,
			 const PowerSet<Determinate<PH> >& q);

template <typename PH>
void
BHRZ03_extrapolation_assign(PowerSet<Determinate<PH> >& r,
			    const PowerSet<Determinate<PH> >& q);

namespace {

template <typename PH>
void
linear_partition_aux(const Constraint& c,
		     PH& qq,
		     PowerSet<Determinate<NNC_Polyhedron> >& r) {
  LinExpression le(c);
  Constraint neg_c = c.is_strict_inequality() ? (le <= 0) : (le < 0);
  NNC_Polyhedron qqq(qq);
  if (qqq.add_constraint_and_minimize(neg_c))
    r.inject(qqq);
  qq.add_constraint(c);
}

} // namespace

template <typename PH>
std::pair<PH, PowerSet<Determinate<NNC_Polyhedron> > >
linear_partition(const PH& p, const PH& q) {
  PowerSet<Determinate<NNC_Polyhedron> > r(p.space_dimension(), false);
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
  return std::pair<PH, PowerSet<Determinate<NNC_Polyhedron> > >(qq, r);
}

namespace {

template <typename PH>
bool
poly_hull_assign_if_exact(PH& p, const PH& q) {
  PH phull = p;
  NNC_Polyhedron nnc_p(p);
  phull.poly_hull_assign(q);
  std::pair<PH, PowerSet<Determinate<NNC_Polyhedron> > >
    partition = linear_partition(q, phull);
  const PowerSet<Determinate<NNC_Polyhedron> >& s = partition.second;
  typedef PowerSet<Determinate<NNC_Polyhedron> >::const_iterator iter;
  for (iter i = s.begin(), s_end = s.end(); i != s_end; ++i)
    // The polyhedral hull is exact if and only if all the elements
    // of the partition of the polyhedral hull of `p' and `q' with
    // respect to `q' are included in `p'
    if (!nnc_p.contains(i->polyhedron()))
      return false;
  p = phull;
  return true;
}

template <typename PH>
void
complete_reduction(PowerSet<Determinate<PH> >& p) {
  size_t n = p.size();
  size_t deleted;
  do {
    PowerSet<Determinate<PH> > q(p.space_dimension(), false);
    std::deque<bool> marked(n, false);
    deleted = 0;
    typedef typename PowerSet<Determinate<PH> >::iterator iter;
    iter p_begin = p.begin();
    iter p_end = p.end();
    unsigned i_index = 0;
    for (iter i = p_begin, j; i != p_end; ++i, ++i_index) {
      if (marked[i_index])
	continue;
      PH& pi = i->polyhedron();
      int j_index = 0;
      for (j = i, ++j; j != p_end; ++j, ++j_index) {
	if (marked[j_index])
	  continue;
	const PH& pj = j->polyhedron();
	if (poly_hull_assign_if_exact(pi, pj)) {
	  marked[i_index] = marked[j_index] = true;
	  q.inject(pi);
	  ++deleted;
	  goto next;
	}
      }
    next:
      ;
    }
    i_index = 0;
    for (iter i = p_begin; i != p_end; ++i, ++i_index)
      if (!marked[i_index])
	q.inject(*i);
    p = q;
    n -= deleted;
  } while (deleted > 0);
  assert(p.OK());
}

template <typename PH>
void
extrapolation_assign(PowerSet<Determinate<PH> >& r,
		     const PowerSet<Determinate<PH> >& q,
		     void (Polyhedron::*
			   extrapolation_assign)(const Polyhedron&,
						 unsigned*)) {
  complete_reduction(r);
  size_t n = r.size();
  PowerSet<Determinate<PH> > p(q.space_dimension(), false);
  std::deque<bool> marked(n, false);
  typedef typename PowerSet<Determinate<PH> >::const_iterator const_iter;
  typedef typename PowerSet<Determinate<PH> >::iterator iter;
  iter r_begin = r.begin();
  iter r_end = r.end();
  unsigned i_index = 0;
  for (iter i = r_begin; i != r_end; ++i, ++i_index)
    for (const_iter j = q.begin(), q_end = q.end(); j != q_end; ++j) {
      PH& ri = i->polyhedron();
      const PH& qj = j->polyhedron();
      if (ri.contains(qj)) {
	(ri.*extrapolation_assign)(qj, 0);
	p.inject(ri);
	marked[i_index] = true;
      }
    }
  i_index = 0;
  for (iter i = r_begin; i != r_end; ++i, ++i_index)
    if (!marked[i_index])
      p.inject(*i);
  r = p;
}

} // namespace

template <typename PH>
void
H79_extrapolation_assign(PowerSet<Determinate<PH> >& r,
			 const PowerSet<Determinate<PH> >& q) {
  extrapolation_assign(r, q, &PH::H79_widening_assign);
}

template <typename PH>
void
BHRZ03_extrapolation_assign(PowerSet<Determinate<PH> >& r,
			    const PowerSet<Determinate<PH> >& q) {
  extrapolation_assign(r, q, &PH::BHRZ03_widening_assign);
}

} // namespace Parma_Polyhedra_Library
