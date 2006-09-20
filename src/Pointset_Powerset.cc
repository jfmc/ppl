/* Pointset_Powerset class implementation: non-inline functions.
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

#include <config.h>
#include "Pointset_Powerset.defs.hh"
#include <utility>

namespace PPL = Parma_Polyhedra_Library;

template <>
void
PPL::Pointset_Powerset<PPL::NNC_Polyhedron>
::poly_difference_assign(const Pointset_Powerset& y) {
  Pointset_Powerset& x = *this;
  // Ensure omega-reduction.
  x.omega_reduce();
  y.omega_reduce();
  Sequence new_sequence = x.sequence;
  for (const_iterator yi = y.begin(), y_end = y.end(); yi != y_end; ++yi) {
    const NNC_Polyhedron& py = yi->element();
    Sequence tmp_sequence;
    for (Sequence_const_iterator nsi = new_sequence.begin(),
	   ns_end = new_sequence.end(); nsi != ns_end; ++nsi) {
      std::pair<NNC_Polyhedron, Pointset_Powerset<NNC_Polyhedron> > partition
	= linear_partition(py, nsi->element());
      const Pointset_Powerset<NNC_Polyhedron>& residues = partition.second;
      // Append the contents of `residues' to `tmp_sequence'.
      std::copy(residues.begin(), residues.end(), back_inserter(tmp_sequence));
    }
    std::swap(tmp_sequence, new_sequence);
  }
  std::swap(x.sequence, new_sequence);
  x.reduced = false;
  assert(x.OK());
}

template <>
bool
PPL::Pointset_Powerset<PPL::NNC_Polyhedron>
::geometrically_covers(const Pointset_Powerset& y) const {
  const Pointset_Powerset& x = *this;
  for (const_iterator yi = y.begin(), y_end = y.end(); yi != y_end; ++yi)
    if (!check_containment(yi->element(), x))
      return false;
  return true;
}

/*! \relates Parma_Polyhedra_Library::Pointset_Powerset */
bool
PPL::check_containment(const NNC_Polyhedron& ph,
		       const Pointset_Powerset<NNC_Polyhedron>& ps) {
  Pointset_Powerset<NNC_Polyhedron> tmp(ph.space_dimension(), EMPTY);
  tmp.add_disjunct(ph);
  for (Pointset_Powerset<NNC_Polyhedron>::const_iterator
	 i = ps.begin(), ps_end = ps.end(); i != ps_end; ++i) {
    const NNC_Polyhedron& pi = i->element();
    for (Pointset_Powerset<NNC_Polyhedron>::iterator
	   j = tmp.begin(); j != tmp.end(); ) {
      const NNC_Polyhedron& pj = j->element();
      if (pi.contains(pj))
	j = tmp.drop_disjunct(j);
      else
	++j;
    }
    if (tmp.empty())
      return true;
    else {
      Pointset_Powerset<NNC_Polyhedron> new_disjuncts(ph.space_dimension(),
						       EMPTY);
      for (Pointset_Powerset<NNC_Polyhedron>::iterator
	     j = tmp.begin(); j != tmp.end(); ) {
	const NNC_Polyhedron& pj = j->element();
	if (pj.is_disjoint_from(pi))
	  ++j;
	else {
	  std::pair<NNC_Polyhedron, Pointset_Powerset<NNC_Polyhedron> >
	    partition = linear_partition(pi, pj);
	  new_disjuncts.upper_bound_assign(partition.second);
	  j = tmp.drop_disjunct(j);
	}
      }
      tmp.upper_bound_assign(new_disjuncts);
    }
  }
  return false;
}
