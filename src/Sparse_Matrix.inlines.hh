/* Sparse_Matrix class implementation: inline functions.
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

#ifndef PPL_Sparse_Matrix_inlines_hh
#define PPL_Sparse_Matrix_inlines_hh 1

namespace Parma_Polyhedra_Library {

template <typename Func>
inline void
Parma_Polyhedra_Library::Sparse_Matrix::for_each_row(Func func) {
  std::for_each(begin(),end(),func);
}

template <typename Func>
inline void
Parma_Polyhedra_Library::Sparse_Matrix::for_each_row(Func func) const {
  std::for_each(begin(),end(),func);
}

template <typename Func>
inline void
Sparse_Matrix_Row::for_each_nonzero(Func func,const dimension_type n) {
  (void)n;
  std::for_each(begin(),end(),func);
}

template <typename Func>
inline void
Sparse_Matrix_Row::for_each_nonzero(Func func,const dimension_type n)
  const {
  (void)n;
  std::for_each(begin(),end(),func);
}

template <typename Operation1, typename Operation2>
inline
PPL::Sparse_Matrix_Row::unary_compose<Operation1,Operation2>
::unary_compose(const Operation1& x,const Operation2& y)
  : f1(x), f2(y) {
}

template <typename Operation1, typename Operation2>
inline
typename Operation1::result_type
PPL::Sparse_Matrix_Row::unary_compose<Operation1,Operation2>::operator()(
  const typename Operation2::argument_type& x) const {

  return f1(f2(x));
}

template <typename Operation1, typename Operation2>
inline PPL::Sparse_Matrix_Row::unary_compose<Operation1, Operation2>
PPL::Sparse_Matrix_Row::compose1(const Operation1& f1, const Operation2& f2) {
  return unary_compose<Operation1,Operation2>(f1, f2);
}

template <typename Pair>
inline typename Pair::second_type
Sparse_Matrix_Row::select2nd<Pair>::operator()(const Pair& x) const {
  return x.second;
};

} // namespace Parma_Polyhedra_Library


#endif // !defined(PPL_Sparse_Matrix_inlines_hh)
