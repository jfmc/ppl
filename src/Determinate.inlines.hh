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

#ifndef _Determinate_inlines_hh
#define _Determinate_inlines_hh 1

#include <cassert>

namespace Parma_Polyhedra_Library {

template <typename PH>
Determinate<PH>::Determinate(const PH& p)
  : prep(new Rep(p)) {
  new_reference();
}

template <typename PH>
Determinate<PH>::Determinate(const Determinate& y)
  : prep(y.prep) {
  new_reference();
}

template <typename PH>
Determinate<PH>::~Determinate() {
  if (del_reference())
    delete prep;
}

template <typename PH>
Determinate<PH>&
Determinate<PH>::operator=(const Determinate& y) {
  prep->new_reference();
  if (del_reference())
    delete prep;
  prep = y.prep;
  return *this;
}

template <typename PH>
void
Determinate<PH>::mutate() {
  if (prep->is_shared()) {
    prep = new Rep(prep->ph);
    new_reference();
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
  assert(prep);
  return prep->ph.check_universe();
}

template <typename PH>
bool
Determinate<PH>::is_bottom() const {
  assert(prep);
  return prep->ph.check_empty();
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
void
Determinate<PH>::add_dimensions_and_embed(size_t dim) {
  mutate();
  prep->ph.add_dimensions_and_embed(dim);
}

template <typename PH>
void
Determinate<PH>::add_dimensions_and_project(size_t dim) {
  mutate();
  prep->ph.add_dimensions_and_project(dim);
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

} // namespace Parma_Polyhedra_Library

#endif // !defined(_Determinate_inlines_hh)
