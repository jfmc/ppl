/* Sparse_Row class implementation: non-inline template functions.
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

#ifndef PPL_Sparse_Row_templates_hh
#define PPL_Sparse_Row_templates_hh 1

namespace Parma_Polyhedra_Library {


template <typename Func1, typename Func2, typename Func3>
void
Sparse_Row::combine(const Unlimited_Sparse_Row& y,
                    const Func1& f, const Func2& g, const Func3& h) {
  row.combine(y, f, g, h);
}

template <typename Func1, typename Func2, typename Func3>
void
Sparse_Row::combine(const Sparse_Row& y,
                    const Func1& f, const Func2& g, const Func3& h) {
  row.combine(y.row, f, g, h);
}

template <typename Func1, typename Func2, typename Func3>
void
Sparse_Row::combine(const Sparse_Row_Reference& y,
                    const Func1& f, const Func2& g, const Func3& h) {
  row.combine(static_cast<const Unlimited_Sparse_Row&>(y), f, g, h);
}


template <typename Func1, typename Func2, typename Func3>
void
Sparse_Row_Reference::combine(const Unlimited_Sparse_Row& y, const Func1& f,
                              const Func2& g, const Func3& h) {
  row.combine(y, f, g, h);
}

template <typename Func1, typename Func2, typename Func3>
void
Sparse_Row_Reference::combine(const Sparse_Row& y, const Func1& f,
                              const Func2& g, const Func3& h) {
  row.combine(static_cast<const Unlimited_Sparse_Row&>(y), f, g, h);
}

template <typename Func1, typename Func2, typename Func3>
void
Sparse_Row_Reference::combine(const Sparse_Row_Reference& y, const Func1& f,
                              const Func2& g, const Func3& h) {
  row.combine(y.row, f, g, h);
}


} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Sparse_Row_templates_hh)
