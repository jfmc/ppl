/* A collection of useful convex polyhedra algorithms: inline functions.
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

#include "NNC_Polyhedron.defs.hh"
#include "Determinate.defs.hh"
#include "PowerSet.defs.hh"
#include "Constraint.defs.defs.hh"
#include "LinExpression.defs.defs.hh"
#include "ConSys.defs.defs.hh"
#include <utility>

namespace Parma_Polyhedra_Library {

template <typename PH>
void
linear_partition_aux(const Constraint& c,
		     PH& qq,
		     PowerSet<Determinate<NNC_Polyhedron> >& r) {
  LinExpression le(c);
  Constraint neg_c = c.is_strict_inequality() ? (le <= 0) : (le < 0);
  NNC_Polyhedron qqq(qq);
  // FIXME: add_constraint_and_minimize()?
  qqq.add_constraint(neg_c);
  if (!qqq.check_empty())
    r.inject(qqq);
  qq.add_constraint(c);
}

//! Partitions \p q with respect to \p p.
/*!
  Let \p p and \p q two polyhedra such that
  - \p p and \p q are not disjoint;
  - \p p does not contain \p q.

  The function returns an object <CODE>r</CODE> of type
  <CODE>std::pair<PH, PowerSet<Determinate<NNC_Polyhedron> > ></CODE>
  such that
  - <CODE>r.first</CODE> is the intersection of \p p and \p q;
  - <CODE>r.second</CODE> has the property that all its elements are
    pairwise disjoint and disjoint from \p p;
  - the union of <CODE>r.first</CODE> with all the elements of
    <CODE>r.second</CODE> gives \p q (i.e., <CODE>r</CODE> is the
    representation of a partition of \p q).
*/
template <typename PH>
std::pair<PH, PowerSet<Determinate<NNC_Polyhedron> > >
linear_partition(const PH& p, const PH& q) {
  // FIXME: we may want to do more than simply asserting the preconditions.
  assert(!are_disjoint(p, q));
  assert(!(p >= q));
  PowerSet<Determinate<NNC_Polyhedron> > r(p.space_dimension());
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

} // namespace Parma_Polyhedra_Library
