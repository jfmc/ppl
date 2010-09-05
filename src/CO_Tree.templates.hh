/* CO_Tree class implementation: non-inline template functions.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_CO_Tree_templates_hh
#define PPL_CO_Tree_templates_hh 1

namespace Parma_Polyhedra_Library {

template <typename Archive>
void
CO_Tree::save(Archive & ar, const unsigned int /* version */) const {
  ar & size;
  if (size != 0) {
    for (const_iterator i = begin(), i_end = end(); i != i_end; ++i) {
      // This is needed because archives' operator&() takes a reference, not
      // a const reference.
      dimension_type index = i.index();
      ar & index;
      ar & *i;
    }
  }
}

template <typename Archive>
void
CO_Tree::load(Archive & ar, const unsigned int /* version */) {
  clear();
  destroy();
  init(0);
  dimension_type n;
  ar & n;
  dimension_type index;
  PPL_DIRTY_TEMP_COEFFICIENT(coeff);
  for (dimension_type i = 0; i < n; i++) {
    ar & index;
    ar & coeff;
    insert(index, coeff);
  }
  PPL_ASSERT(OK());
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Sparse_Row_templates_hh)
