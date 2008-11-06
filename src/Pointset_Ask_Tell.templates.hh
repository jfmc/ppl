/* Pointset_Ask_Tell class implementation: non-inline template functions.
   Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
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
#include "Variables_Set.defs.hh"
#include <algorithm>
#include <deque>
#include <string>
#include <iostream>
#include <sstream>
#include <stdexcept>

namespace Parma_Polyhedra_Library {

template <typename PS>
void
Pointset_Ask_Tell<PS>::add_disjunct(const PS& ph) {
  Pointset_Ask_Tell& x = *this;
  if (x.space_dimension() != ph.space_dimension()) {
    std::ostringstream s;
    s << "PPL::Pointset_Ask_Tell<PS>::add_disjunct(ph):\n"
      << "this->space_dimension() == " << x.space_dimension() << ", "
      << "ph.space_dimension() == " << ph.space_dimension() << ".";
    throw std::invalid_argument(s.str());
  }
  x.sequence.push_back(Determinate<PS>(ph));
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
  // FIXME(0.10.1): the following is a bug!
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

  // FIXME(0.10.1): the following comment should be rephrased!
  // Note: this might be non-reduced even when `y' is known to be
  // omega-reduced, because the constructor of C_Polyhedron, by
  // enforcing topological closure, may have made different elements
  // comparable.
  x.normalized = false;
  assert(x.OK());
}

template <typename PS>
void
Pointset_Ask_Tell<PS>::concatenate_assign(const Pointset_Ask_Tell& y) {
  Pointset_Ask_Tell& x = *this;
  for (const_iterator yi = y.begin(), y_end = y.end(); yi != y_end; ++yi) {
    CS ask(PS(space_dim, UNIVERSE));
    ask.concatenate_assign(yi->ask());
    CS tell(PS(space_dim, UNIVERSE));
    tell.concatenate_assign(yi->tell());
    x.sequence.push_back(Pair(ask, tell));
  }
  space_dim += y.space_dim;
  if (x.normalized)
    x.normalized = y.normalized;
  assert(x.OK());
}

template <typename PS>
void
Pointset_Ask_Tell<PS>::add_constraint(const Constraint& c) {
  Pointset_Ask_Tell& x = *this;
  for (Sequence_iterator si = x.sequence.begin(),
	 s_end = x.sequence.end(); si != s_end; ++si)
    si->element().add_constraint(c);
  x.reduced = false;
  assert(x.OK());
}

template <typename PS>
void
Pointset_Ask_Tell<PS>::add_constraints(const Constraint_System& cs) {
  Pointset_Ask_Tell& x = *this;
  for (Sequence_iterator si = x.sequence.begin(),
	 s_end = x.sequence.end(); si != s_end; ++si)
    si->element().add_constraints(cs);
  x.reduced = false;
  assert(x.OK());
}

template <typename PS>
void
Pointset_Ask_Tell<PS>::unconstrain(const Variable var) {
  Pointset_Ask_Tell& x = *this;
  for (Sequence_iterator si = x.sequence.begin(),
	 s_end = x.sequence.end(); si != s_end; ++si)
    si->element().unconstrain(var);
  x.reduced = false;
  assert(x.OK());
}

template <typename PS>
void
Pointset_Ask_Tell<PS>::unconstrain(const Variables_Set& to_be_unconstrained) {
  Pointset_Ask_Tell& x = *this;
  for (Sequence_iterator si = x.sequence.begin(),
	 s_end = x.sequence.end(); si != s_end; ++si)
    si->element().unconstrain(to_be_unconstrained);
  x.reduced = false;
  assert(x.OK());
}

template <typename PS>
void
Pointset_Ask_Tell<PS>::add_space_dimensions_and_embed(dimension_type m) {
  Pointset_Ask_Tell& x = *this;
  for (Sequence_iterator si = x.sequence.begin(),
	 s_end = x.sequence.end(); si != s_end; ++si)
    si->element().add_space_dimensions_and_embed(m);
  x.space_dim += m;
  assert(x.OK());
}

template <typename PS>
void
Pointset_Ask_Tell<PS>::add_space_dimensions_and_project(dimension_type m) {
  Pointset_Ask_Tell& x = *this;
  for (Sequence_iterator si = x.sequence.begin(),
	 s_end = x.sequence.end(); si != s_end; ++si)
    si->element().add_space_dimensions_and_project(m);
  x.space_dim += m;
  assert(x.OK());
}

template <typename PS>
void
Pointset_Ask_Tell<PS>::
remove_space_dimensions(const Variables_Set& to_be_removed) {
  Pointset_Ask_Tell& x = *this;
  Variables_Set::size_type num_removed = to_be_removed.size();
  if (num_removed > 0) {
    for (Sequence_iterator si = x.sequence.begin(),
	   s_end = x.sequence.end(); si != s_end; ) {
      PS& ask = si->ask().element();
      PS& tell = si->tell().element();
      ask.remove_space_dimensions(to_be_removed);
      tell.remove_space_dimensions(to_be_removed);
      if (tell.contains(ask)) {
	si = x.sequence.erase(si);
	s_end = x.sequence.end();
      }
      else {
	x.normalized = false;
	++si;
      }
    }
    x.space_dim -= num_removed;
    assert(x.OK());
  }
}

template <typename PS>
void
Pointset_Ask_Tell<PS>::remove_higher_space_dimensions(dimension_type
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

template <typename PS>
template <typename Partial_Function>
void
Pointset_Ask_Tell<PS>::map_space_dimensions(const Partial_Function& pfunc) {
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

template <typename PS>
void
Pointset_Ask_Tell<PS>::ascii_dump(std::ostream& s) const {
  const Pointset_Ask_Tell& x = *this;
  s << "size " << x.size()
    << "\nspace_dim " << x.space_dim
    << "\n";
  for (const_iterator xi = x.begin(), x_end = x.end(); xi != x_end; ++xi)
    xi->element().ascii_dump(s);
}

PPL_OUTPUT_TEMPLATE_DEFINITIONS(PS, Pointset_Ask_Tell<PS>)

template <typename PS>
bool
Pointset_Ask_Tell<PS>::ascii_load(std::istream& s) {
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
    PS ph;
    if (!ph.ascii_load(s))
      return false;
    new_x.add_disjunct(ph);
  }
  x.swap(new_x);

  // Check invariants.
  assert(x.OK());
  return true;
}

template <typename PS>
bool
Pointset_Ask_Tell<PS>::OK() const {
  const Pointset_Ask_Tell& x = *this;
  for (const_iterator xi = x.begin(), x_end = x.end(); xi != x_end; ++xi) {
    const PS& ask_i = xi->ask().element();
    const PS& tell_i = xi->tell().element();
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

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Pointset_Ask_Tell_templates_hh)
