/* A collection of useful convex polyhedra algorithms: inline functions.
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

#ifndef PPL_algorithms_hh
#define PPL_algorithms_hh 1

#include "NNC_Polyhedron.defs.hh"
#include "Polyhedra_Powerset.defs.hh"
#include <utility>
#include <cassert>

namespace Parma_Polyhedra_Library {

//! If the poly-hull between \p p and \p q is exact it is assigned to \p p.
/*! \relates Polyhedron */
template <typename PH>
bool
poly_hull_assign_if_exact(PH& p, const PH& q);

/*! \relates Polyhedron */
template <typename PH>
bool
poly_hull_assign_if_exact(PH& p, const PH& q) {
  PH phull = p;
  NNC_Polyhedron nnc_p(p);
  phull.poly_hull_assign(q);
  std::pair<PH, Polyhedra_Powerset<NNC_Polyhedron> >
    partition = linear_partition(q, phull);
  const Polyhedra_Powerset<NNC_Polyhedron>& s = partition.second;
  typedef Polyhedra_Powerset<NNC_Polyhedron>::const_iterator iter;
  for (iter i = s.begin(), s_end = s.end(); i != s_end; ++i)
    // The polyhedral hull is exact if and only if all the elements
    // of the partition of the polyhedral hull of `p' and `q' with
    // respect to `q' are included in `p'
    if (!nnc_p.contains(i->element()))
      return false;
  p = phull;
  return true;
}

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! \brief
//! Returns <CODE>true</CODE> if and only if the union of the polyhedra
//! in \p ps contains the polyhedron \p ph.
/*! \relates Polyhedra_Powerset */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename PH>
bool
check_containment(const PH& ph, const Polyhedra_Powerset<PH>& ps);

/*! \relates Polyhedra_Powerset */
template <typename PH>
bool
check_containment(const PH& ph, const Polyhedra_Powerset<PH>& ps) {
  Polyhedra_Powerset<NNC_Polyhedron> tmp(ph.space_dimension(),
					 Polyhedron::EMPTY);
  tmp.add_disjunct(NNC_Polyhedron(ph));
  for (typename Polyhedra_Powerset<PH>::const_iterator i = ps.begin(),
	 ps_end = ps.end(); i != ps_end; ++i) {
    const NNC_Polyhedron pi(i->element());
    for (typename Polyhedra_Powerset<NNC_Polyhedron>::iterator
	   j = tmp.begin(); j != tmp.end(); ) {
      const NNC_Polyhedron& pj = j->element();
      if (pi.contains(pj))
	j = tmp.erase(j);
      else
	++j;
    }
    if (tmp.empty())
      return true;
    else {
      Polyhedra_Powerset<NNC_Polyhedron> new_disjuncts(ph.space_dimension(),
						       Polyhedron::EMPTY);
      for (Polyhedra_Powerset<NNC_Polyhedron>::iterator
	     j = tmp.begin(); j != tmp.end(); ) {
	const NNC_Polyhedron& pj = j->element();
	if (pj.is_disjoint_from(pi))
	  ++j;
	else {
	  std::pair<NNC_Polyhedron, Polyhedra_Powerset<NNC_Polyhedron> >
	    partition = linear_partition(pi, pj);
	  new_disjuncts.upper_bound_assign(partition.second);
	  j = tmp.erase(j);
	}
      }
      tmp.upper_bound_assign(new_disjuncts);
    }
  }
  return false;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_algorithms_hh)
