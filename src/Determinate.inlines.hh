/* Determinate class implementation: inline functions.
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

#ifndef PPL_Determinate_inlines_hh
#define PPL_Determinate_inlines_hh 1

#include <cassert>

namespace Parma_Polyhedra_Library {

template <typename PH>
Determinate<PH>::Rep::Rep(size_t num_dimensions,
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
Determinate<PH>::Determinate(size_t num_dimensions,
			     Polyhedron::Degenerate_Kind kind)
  : prep(new Rep(num_dimensions, kind)) {
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
void
Determinate<PH>::mutate() {
  if (prep->is_shared()) {
    (void) prep->del_reference();
    prep = new Rep(prep->ph);
    prep->new_reference();
  }
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
Determinate<PH>::definitely_entails(const Determinate<PH>& y) const {
  return prep->ph <= y.prep->ph;
}

template <typename PH>
bool
lcompare(const Determinate<PH>& x, const Determinate<PH>& y) {
  return x.prep < y.prep;
}

template <typename PH>
Determinate<PH>
operator*(const Determinate<PH>& x, const Determinate<PH>& y) {
  Determinate<PH> z = x;
  z.meet_assign(y);
  return z;
}

template <typename PH>
Determinate<PH>
operator+(const Determinate<PH>& x, const Determinate<PH>& y) {
  Determinate<PH> z = x;
  z += y;
  return z;
}

template <typename PH>
bool
Determinate<PH>::is_top() const {
  return prep->ph.check_universe();
}

template <typename PH>
bool
Determinate<PH>::is_bottom() const {
  return prep->ph.check_empty();
}

template <typename PH>
bool
Determinate<PH>::OK() const {
  return prep->ph.OK();
}

template <typename PH>
std::ostream&
operator<<(std::ostream& s, const Determinate<PH>& x) {
  if (x.is_top())
    s << "TOP";
  else if (x.is_bottom())
    s << "BOTTOM";
  else
    s << x.prep->ph;
  return s;
}

template <typename PH>
bool
operator==(const Determinate<PH>& x, const Determinate<PH>& y) {
  return x.prep->ph == y.prep->ph;
}

template <typename PH>
bool
operator!=(const Determinate<PH>& x, const Determinate<PH>& y) {
  return x.prep->ph != y.prep->ph;
}

template <typename PH>
size_t
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
const GenSys&
Determinate<PH>::generators() const {
  return prep->ph.generators();
}

template <typename PH>
const GenSys&
Determinate<PH>::minimized_generators() const {
  return prep->ph.minimized_generators();
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
Determinate<PH>::add_dimensions_and_embed(size_t m) {
  mutate();
  prep->ph.add_dimensions_and_embed(m);
}

template <typename PH>
void
Determinate<PH>::add_dimensions_and_project(size_t m) {
  mutate();
  prep->ph.add_dimensions_and_project(m);
}

template <typename PH>
void
Determinate<PH>::remove_dimensions(const std::set<Variable>& to_be_removed) {
  mutate();
  prep->ph.remove_dimensions(to_be_removed);
}

template <typename PH>
void
Determinate<PH>::remove_higher_dimensions(size_t new_dimension) {
  mutate();
  prep->ph.remove_higher_dimensions(new_dimension);
}

template <typename PH>
template <typename PartialFunction>
void
Determinate<PH>::shuffle_dimensions(const PartialFunction& pfunc) {
  mutate();
  prep->ph.shuffle_dimensions(pfunc);
}

template <typename PH>
void
Determinate<PH>::H79_widening_assign(const Determinate& y) {
  mutate();
  prep->ph.H79_widening_assign(y.prep->ph);
}

template <typename PH>
void
Determinate<PH>::limited_H79_widening_assign(const Determinate& y,
					     ConSys& cs) {
  mutate();
  prep->ph.limited_H79_widening_assign(y.prep->ph, cs);
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Determinate_inlines_hh)
