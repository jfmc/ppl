/* Determinate class implementation: inline functions.
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

#ifndef PPL_Determinate_inlines_hh
#define PPL_Determinate_inlines_hh 1

#include <cassert>

namespace Parma_Polyhedra_Library {

template <typename PH>
Determinate<PH>::Rep::Rep(dimension_type num_dimensions,
			  Polyhedron::Degenerate_Kind kind)
  : references(0), ph(num_dimensions, kind) {
}

template <typename PH>
Determinate<PH>::Rep::Rep(const PH& p)
  : references(0), ph(p) {
}

template <typename PH>
Determinate<PH>::Rep::Rep(const ConSys& cs)
  : references(0), ph(cs) {
}

template <typename PH>
Determinate<PH>::Rep::~Rep() {
  assert(references == 0);
}

template <typename PH>
void
Determinate<PH>::Rep::new_reference() const {
  ++references;
}

template <typename PH>
bool Determinate<PH>::Rep::del_reference() const {
  return --references == 0;
}

template <typename PH>
bool
Determinate<PH>::Rep::is_shared() const {
  return references > 1;
}

template <typename PH>
Determinate<PH>::Determinate(dimension_type num_dimensions, bool universe)
  : prep(new Rep(num_dimensions,
		 universe ? Polyhedron::UNIVERSE : Polyhedron::EMPTY)) {
  prep->new_reference();
}

template <typename PH>
Determinate<PH>::Determinate(const PH& ph)
  : prep(new Rep(ph)) {
  prep->new_reference();
}

template <typename PH>
Determinate<PH>::Determinate(const ConSys& cs)
  : prep(new Rep(cs)) {
  prep->new_reference();
}

template <typename PH>
Determinate<PH>::Determinate(const Determinate& y)
  : prep(y.prep) {
  prep->new_reference();
}

template <typename PH>
Determinate<PH>::~Determinate() {
  if (prep->del_reference())
    delete prep;
}

template <typename PH>
Determinate<PH>&
Determinate<PH>::operator=(const Determinate& y) {
  y.prep->new_reference();
  if (prep->del_reference())
    delete prep;
  prep = y.prep;
  return *this;
}

template <typename PH>
inline void
Determinate<PH>::swap(Determinate& y) {
  std::swap(prep, y.prep);
}

template <typename PH>
void
Determinate<PH>::mutate() {
  if (prep->is_shared()) {
    Rep* new_prep = new Rep(prep->ph);
    (void) prep->del_reference();
    new_prep->new_reference();
    prep = new_prep;
  }
}

template <typename PH>
const PH&
Determinate<PH>::element() const {
  return prep->ph;
}

template <typename PH>
PH&
Determinate<PH>::element() {
  mutate();
  return prep->ph;
}

template <typename PH>
void
Determinate<PH>::upper_bound_assign(const Determinate& y) {
  mutate();
  prep->ph.poly_hull_assign(y.prep->ph);
}

template <typename PH>
void
Determinate<PH>::meet_assign(const Determinate& y) {
  mutate();
  prep->ph.intersection_assign(y.prep->ph);
}

template <typename PH>
void
Determinate<PH>::concatenate_assign(const Determinate& y) {
  mutate();
  prep->ph.concatenate_assign(y.prep->ph);
}

template <typename PH>
bool
Determinate<PH>::definitely_entails(const Determinate& y) const {
  return prep == y.prep || y.prep->ph.contains(prep->ph);
}

template <typename PH>
bool
Determinate<PH>::is_definitely_equivalent_to(const Determinate& y) const {
  return prep == y.prep || prep->ph == y.prep->ph;
}

template <typename PH>
inline bool
Determinate<PH>::is_top() const {
  return prep->ph.is_universe();
}

template <typename PH>
inline bool
Determinate<PH>::is_bottom() const {
  return prep->ph.is_empty();
}

template <typename PH>
bool
Determinate<PH>::OK() const {
  return prep->ph.OK();
}

namespace IO_Operators {

/*! \relates Parma_Polyhedra_Library::Determinate */
template <typename PH>
std::ostream&
operator<<(std::ostream& s, const Determinate<PH>& x) {
  if (x.is_top())
    s << "true";
  else if (x.is_bottom())
    s << "false";
  else {
    const ConSys& cs = x.constraints();
    ConSys::const_iterator i = cs.begin();
    ConSys::const_iterator cs_end = cs.end();
    s << "{ ";
    while (i != cs_end) {
      s << *i++;
      if (i != cs_end)
	s << ", ";
    }
    s << " }";
  }
  return s;
}

} // namespace IO_Operators

/*! \relates Determinate */
template <typename PH>
bool
operator==(const Determinate<PH>& x, const Determinate<PH>& y) {
  return x.prep == y.prep || x.prep->ph == y.prep->ph;
}

/*! \relates Determinate */
template <typename PH>
bool
operator!=(const Determinate<PH>& x, const Determinate<PH>& y) {
  return x.prep != y.prep && x.prep->ph != y.prep->ph;
}

template <typename PH>
dimension_type
Determinate<PH>::space_dimension() const {
  return prep->ph.space_dimension();
}

template <typename PH>
const ConSys&
Determinate<PH>::constraints() const {
  return prep->ph.constraints();
}

template <typename PH>
const ConSys&
Determinate<PH>::minimized_constraints() const {
  return prep->ph.minimized_constraints();
}

template <typename PH>
void
Determinate<PH>::add_constraint(const Constraint& c) {
  mutate();
  prep->ph.add_constraint(c);
}

template <typename PH>
void
Determinate<PH>::add_constraints(ConSys& cs) {
  mutate();
  prep->ph.add_constraints(cs);
}

template <typename PH>
void
Determinate<PH>::add_dimensions_and_embed(dimension_type m) {
  mutate();
  prep->ph.add_dimensions_and_embed(m);
}

template <typename PH>
void
Determinate<PH>::add_dimensions_and_project(dimension_type m) {
  mutate();
  prep->ph.add_dimensions_and_project(m);
}

template <typename PH>
void
Determinate<PH>::remove_dimensions(const Variables_Set& to_be_removed) {
  mutate();
  prep->ph.remove_dimensions(to_be_removed);
}

template <typename PH>
void
Determinate<PH>::remove_higher_dimensions(dimension_type new_dimension) {
  mutate();
  prep->ph.remove_higher_dimensions(new_dimension);
}

template <typename PH>
template <typename PartialFunction>
void
Determinate<PH>::map_dimensions(const PartialFunction& pfunc) {
  mutate();
  prep->ph.map_dimensions(pfunc);
}

} // namespace Parma_Polyhedra_Library


namespace std {

/*! \relates Parma_Polyhedra_Library::Determinate */
template <typename PH>
inline void
swap(Parma_Polyhedra_Library::Determinate<PH>& x,
     Parma_Polyhedra_Library::Determinate<PH>& y) {
  x.swap(y);
}

} // namespace std

#endif // !defined(PPL_Determinate_inlines_hh)
