/* Linear_Expression_Impl class implementation: non-inline template functions.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2011 BUGSENG srl (http://bugseng.com)

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

#ifndef PPL_Linear_Expression_Impl_templates_hh
#define PPL_Linear_Expression_Impl_templates_hh 1

#include "Linear_Expression_Impl.defs.hh"

#include "Dense_Row.defs.hh"
#include "Sparse_Row.defs.hh"

#include "Constraint.defs.hh"
#include "Generator.defs.hh"
#include "Grid_Generator.defs.hh"
#include "Congruence.defs.hh"
#include <deque>
#include <stdexcept>
#include <iostream>

namespace Parma_Polyhedra_Library {

template <>
void
Linear_Expression_Impl<Dense_Row>::remove_space_dimensions(const Variables_Set& vars) {
  PPL_ASSERT(vars.space_dimension() <= space_dimension());
  if (vars.empty())
    return;

  // For each variable to be removed, replace the corresponding coefficient
  // by shifting left the coefficient to the right that will be kept.
  Variables_Set::const_iterator vsi = vars.begin();
  Variables_Set::const_iterator vsi_end = vars.end();
  dimension_type dst_col = *vsi+1;
  dimension_type src_col = dst_col + 1;
  for (++vsi; vsi != vsi_end; ++vsi) {
    const dimension_type vsi_col = *vsi+1;
    // Move all columns in between to the left.
    while (src_col < vsi_col)
      row.swap(dst_col++, src_col++);
    ++src_col;
  }
  // Move any remaining columns.
  const dimension_type sz = row.size();
  while (src_col < sz)
    row.swap(dst_col++, src_col++);

  // The number of remaining coefficients is `dst_col'.
  row.resize(dst_col);
  PPL_ASSERT(OK());
}

template <>
void
Linear_Expression_Impl<Sparse_Row>::remove_space_dimensions(const Variables_Set& vars) {
  PPL_ASSERT(vars.space_dimension() <= space_dimension());
  if (vars.empty())
    return;

  // For each variable to be removed, replace the corresponding coefficient
  // by shifting left the coefficient to the right that will be kept.
  Variables_Set::const_iterator vsi = vars.begin();
  Variables_Set::const_iterator vsi_end = vars.end();
  Sparse_Row::iterator src = row.lower_bound(*vsi + 1);
  const Sparse_Row::iterator& row_end = row.end();
  dimension_type num_removed = 0;
  while (vsi != vsi_end) {
    // Delete the element.
    if (src != row_end && src.index() == *vsi + 1)
      src = row.reset(src);
    num_removed++;
    vsi++;
    if (vsi != vsi_end) {
      // Shift left the coefficients in [src.index(), *vsi + 1) by num_removed
      // positions.
      while (src != row_end && src.index() < *vsi + 1) {
        row.fast_swap(src.index() - num_removed, src);
        ++src;
      }
    } else {
      // Shift left the coefficients in [src.index(), row.size()) by
      // num_removed positions.
      while (src != row_end) {
        row.fast_swap(src.index() - num_removed, src);
        ++src;
      }
    }
  }

  PPL_ASSERT(num_removed == vars.size());

  row.resize(row.size() - num_removed);
  PPL_ASSERT(OK());
}

template <typename Row>
Linear_Expression_Impl<Row>::Linear_Expression_Impl(const Linear_Expression_Impl& e) {
  construct(e);
}

template <typename Row>
template <typename Row2>
Linear_Expression_Impl<Row>::Linear_Expression_Impl(const Linear_Expression_Impl<Row2>& e) {
  construct(e);
}

template <typename Row>
Linear_Expression_Impl<Row>::Linear_Expression_Impl(const Linear_Expression_Interface& e) {
  if (const Linear_Expression_Impl<Dense_Row>* p = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&e)) {
    construct(*p);
  } else if (const Linear_Expression_Impl<Sparse_Row>* p = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&e)) {
    construct(*p);
  } else {
    // Add implementations for other derived classes here.
    PPL_ASSERT(false);
  }
}

template <typename Row>
Linear_Expression_Impl<Row>::Linear_Expression_Impl(const Linear_Expression_Interface& e,
                                                    dimension_type sz) {
  if (const Linear_Expression_Impl<Dense_Row>* p = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&e)) {
    construct(*p, sz);
  } else if (const Linear_Expression_Impl<Sparse_Row>* p = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&e)) {
    construct(*p, sz);
  } else {
    // Add implementations for other derived classes here.
    PPL_ASSERT(false);
  }
}

template <typename Row>
Linear_Expression_Impl<Row>::Linear_Expression_Impl(const Congruence& cg, dimension_type sz) {
  PPL_ASSERT(sz != 0);
  construct(*(cg.expression().impl), sz);
}

template <typename Row>
template <typename Row2>
void
Linear_Expression_Impl<Row>
::linear_combine(const Linear_Expression_Impl<Row2>& y, Variable i) {
  PPL_ASSERT(space_dimension() == y.space_dimension());
  PPL_ASSERT(i.space_dimension() <= space_dimension());
  linear_combine(y, i.space_dimension());
}

template <typename Row>
template <typename Row2>
void
Linear_Expression_Impl<Row>
::linear_combine(const Linear_Expression_Impl<Row2>& y, dimension_type i) {
  Linear_Expression_Impl& x = *this;
  PPL_ASSERT(i < x.space_dimension() + 1);
  PPL_ASSERT(x.space_dimension() == y.space_dimension());
  Coefficient_traits::const_reference x_i = x.row.get(i);
  Coefficient_traits::const_reference y_i = y.row.get(i);
  PPL_ASSERT(x_i != 0);
  PPL_ASSERT(y_i != 0);
  PPL_DIRTY_TEMP_COEFFICIENT(normalized_x_v);
  PPL_DIRTY_TEMP_COEFFICIENT(normalized_y_v);
  normalize2(x_i, y_i, normalized_x_v, normalized_y_v);
  neg_assign(normalized_x_v);
  linear_combine(y, normalized_y_v, normalized_x_v);
  // We cannot use x_i here because it may have been invalidated by
  // linear_combine().
  assert(x.row.get(i) == 0);
  PPL_ASSERT(OK());
}

template <typename Row>
template <typename Row2>
void
Linear_Expression_Impl<Row>
::linear_combine(const Linear_Expression_Impl<Row2>& y,
                 Coefficient_traits::const_reference c1,
                 Coefficient_traits::const_reference c2) {
  PPL_ASSERT(c1 != 0);
  PPL_ASSERT(c2 != 0);
  if (space_dimension() < y.space_dimension())
    set_space_dimension(y.space_dimension());
  linear_combine(y, c1, c2, 0, y.space_dimension() + 1);
  PPL_ASSERT(OK());
}

template <typename Row>
template <typename Row2>
void
Linear_Expression_Impl<Row>
::linear_combine_lax(const Linear_Expression_Impl<Row2>& y,
                     Coefficient_traits::const_reference c1,
                     Coefficient_traits::const_reference c2) {
  if (space_dimension() < y.space_dimension())
    set_space_dimension(y.space_dimension());
  linear_combine_lax(y, c1, c2, 0, y.space_dimension() + 1);
  PPL_ASSERT(OK());
}

template <typename Row>
template <typename Row2>
void
Linear_Expression_Impl<Row>::swap(Linear_Expression_Impl<Row2>& y) {
  std::swap(row, y.row);
  PPL_ASSERT(OK());
  PPL_ASSERT(y.OK());
}

template <typename Row>
template <typename Row2>
int
Linear_Expression_Impl<Row>::compare(const Linear_Expression_Impl<Row2>& y) const {
  const Linear_Expression_Impl& x = *this;
  // Compare all the coefficients of the row starting from position 1.
  // NOTE: x and y may be of different size.
  typename Row::const_iterator i = x.row.lower_bound(1);
  typename Row::const_iterator i_end = x.row.end();
  typename Row2::const_iterator j = y.row.lower_bound(1);
  typename Row2::const_iterator j_end = y.row.end();
  while (i != i_end && j != j_end) {
    if (i.index() < j.index()) {
      int s = sgn(*i);
      if (s != 0)
        return 2*s;
      ++i;
      continue;
    }
    if (i.index() > j.index()) {
      int s = sgn(*j);
      if (s != 0)
        return -2*s;
      ++j;
      continue;
    }
    PPL_ASSERT(i.index() == j.index());
    int s = cmp(*i, *j);
    if (s < 0)
      return -2;
    if (s > 0)
      return 2;
    PPL_ASSERT(s == 0);
    ++i;
    ++j;
  }
  for ( ; i != i_end; ++i) {
    int s = sgn(*i);
    if (s != 0)
      return 2*s;
  }
  for ( ; j != j_end; ++j) {
    int s = sgn(*j);
    if (s != 0)
      return -2*s;
  }

  // If all the coefficients in `x' equal all the coefficients in `y'
  // (starting from position 1) we compare coefficients in position 0,
  // i.e., inhomogeneous terms.
  const int comp = cmp(x.row.get(0), y.row.get(0));
  if (comp > 0)
    return 1;
  if (comp < 0)
    return -1;
  PPL_ASSERT(comp == 0);

  // `x' and `y' are equal.
  return 0;
}

template <typename Row>
Linear_Expression_Impl<Row>::Linear_Expression_Impl(const Constraint& c) {
  // Do not copy the epsilon dimension (if any).
  construct(*(c.expression().impl), c.space_dimension() + 1);
  PPL_ASSERT(OK());
}

template <typename Row>
Linear_Expression_Impl<Row>::Linear_Expression_Impl(const Generator& g) {
  // Do not copy the epsilon dimension (if any).
  construct(*(g.expression().impl), g.space_dimension() + 1);
  // Do not copy the divisor of `g'.
  row.reset(0);
  PPL_ASSERT(OK());
}

template <typename Row>
Linear_Expression_Impl<Row>::Linear_Expression_Impl(const Grid_Generator& g) {
  // NOTE: This does *not* copy the last coefficient.
  construct(*(g.expression().impl), g.expression().space_dimension());
  // Do not copy the divisor of `g'.
  row.reset(0);
  PPL_ASSERT(OK());
}

template <typename Row>
Linear_Expression_Impl<Row>::Linear_Expression_Impl(const Congruence& cg) {
  construct(*(cg.expression().impl));
  PPL_ASSERT(OK());
}

template <typename Row>
Linear_Expression_Impl<Row>::Linear_Expression_Impl(const Variable v) {
  if (v.space_dimension() > max_space_dimension())
    throw std::length_error("Linear_Expression_Impl::"
                            "Linear_Expression_Impl(v):\n"
                            "v exceeds the maximum allowed "
                            "space dimension.");
  set_space_dimension(v.space_dimension());
  (*this) += v;
  PPL_ASSERT(OK());
}

template <typename Row>
template <typename Row2>
bool
Linear_Expression_Impl<Row>::is_equal_to(const Linear_Expression_Impl<Row2>& x) const {
  return row == x.row;
}

template <typename Row>
void
Linear_Expression_Impl<Row>::get_row(Dense_Row& row) const {
  row = this->row;
}

template <typename Row>
void
Linear_Expression_Impl<Row>::get_row(Sparse_Row& row) const {
  row = this->row;
}

template <typename Row>
void
Linear_Expression_Impl<Row>::permute_space_dimensions(const std::vector<Variable>& cycle) {
  const dimension_type n = cycle.size();
  if (n < 2)
    return;

  if (n == 2) {
    row.swap(cycle[0].space_dimension(), cycle[1].space_dimension());
  } else {
    PPL_DIRTY_TEMP_COEFFICIENT(tmp);
    tmp = row.get(cycle.back().space_dimension());
    for (dimension_type i = n - 1; i-- > 0; )
     row.swap(cycle[i + 1].space_dimension(), cycle[i].space_dimension());
    if (tmp == 0)
      row.reset(cycle[0].space_dimension());
    else
      std::swap(tmp, row[cycle[0].space_dimension()]);
  }
  PPL_ASSERT(OK());
}

template <typename Row>
template <typename Row2>
Linear_Expression_Impl<Row>&
Linear_Expression_Impl<Row>::operator+=(const Linear_Expression_Impl<Row2>& e) {
  linear_combine(e, Coefficient_one(), Coefficient_one());
  return *this;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression_Impl */
template <typename Row>
Linear_Expression_Impl<Row>&
Linear_Expression_Impl<Row>::operator+=(const Variable v) {
  const dimension_type v_space_dim = v.space_dimension();
  if (v_space_dim > Linear_Expression_Impl<Row>::max_space_dimension())
    throw std::length_error("Linear_Expression_Impl& "
                            "operator+=(e, v):\n"
                            "v exceeds the maximum allowed space dimension.");
  if (space_dimension() < v_space_dim)
    set_space_dimension(v_space_dim);
  typename Row::iterator itr = row.insert(v_space_dim);
  ++(*itr);
  if (*itr == 0)
    row.reset(itr);
  PPL_ASSERT(OK());
  return *this;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression_Impl */
template <typename Row>
template <typename Row2>
Linear_Expression_Impl<Row>&
Linear_Expression_Impl<Row>::operator-=(const Linear_Expression_Impl<Row2>& e2) {
  linear_combine(e2, Coefficient_one(), -1);
  return *this;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression_Impl */
template <typename Row>
Linear_Expression_Impl<Row>&
Linear_Expression_Impl<Row>::operator-=(const Variable v) {
  const dimension_type v_space_dim = v.space_dimension();
  if (v_space_dim > Linear_Expression_Impl<Row>::max_space_dimension())
    throw std::length_error("Linear_Expression_Impl& "
                            "operator-=(e, v):\n"
                            "v exceeds the maximum allowed space dimension.");
  if (space_dimension() < v_space_dim)
    set_space_dimension(v_space_dim);
  typename Row::iterator itr = row.insert(v_space_dim);
  --(*itr);
  if (*itr == 0)
    row.reset(itr);
  PPL_ASSERT(OK());
  return *this;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression_Impl */
template <typename Row>
Linear_Expression_Impl<Row>&
Linear_Expression_Impl<Row>::operator*=(Coefficient_traits::const_reference n) {
  if (n == 0) {
    row.clear();
    PPL_ASSERT(OK());
    return *this;
  }
  for (typename Row::iterator i = row.begin(), i_end = row.end(); i != i_end; ++i)
    (*i) *= n;
  PPL_ASSERT(OK());
  return *this;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression_Impl */
template <typename Row>
Linear_Expression_Impl<Row>&
Linear_Expression_Impl<Row>::operator/=(Coefficient_traits::const_reference n) {
  typename Row::iterator i = row.begin();
  const typename Row::iterator& i_end = row.end();
  while (i != i_end) {
    (*i) /= n;
    if (*i == 0)
      i = row.reset(i);
    else
      ++i;
  }
  PPL_ASSERT(OK());
  return *this;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression_Impl */
template <typename Row>
void
Linear_Expression_Impl<Row>::negate() {
  for (typename Row::iterator i = row.begin(), i_end = row.end(); i != i_end; ++i)
    neg_assign(*i);
  PPL_ASSERT(OK());
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression_Impl */
template <typename Row>
Linear_Expression_Impl<Row>&
Linear_Expression_Impl<Row>::add_mul_assign(Coefficient_traits::const_reference n,
                                            const Variable v) {
  const dimension_type v_space_dim = v.space_dimension();
  if (v_space_dim > Linear_Expression_Impl<Row>::max_space_dimension())
    throw std::length_error("Linear_Expression_Impl& "
                            "add_mul_assign(e, n, v):\n"
                            "v exceeds the maximum allowed space dimension.");
  if (space_dimension() < v_space_dim)
    set_space_dimension(v_space_dim);
  typename Row::iterator itr = row.insert(v_space_dim);
  (*itr) += n;
  if (*itr == 0)
    row.reset(itr);
  PPL_ASSERT(OK());
  return *this;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression_Impl */
template <typename Row>
Linear_Expression_Impl<Row>&
Linear_Expression_Impl<Row>::sub_mul_assign(Coefficient_traits::const_reference n,
                                            const Variable v) {
  const dimension_type v_space_dim = v.space_dimension();
  if (v_space_dim > Linear_Expression_Impl<Row>::max_space_dimension())
    throw std::length_error("Linear_Expression_Impl& "
                            "sub_mul_assign(e, n, v):\n"
                            "v exceeds the maximum allowed space dimension.");
  if (space_dimension() < v_space_dim)
    set_space_dimension(v_space_dim);
  typename Row::iterator itr = row.insert(v_space_dim);
  (*itr) -= n;
  if (*itr == 0)
    row.reset(itr);
  PPL_ASSERT(OK());
  return *this;
}

template <typename Row>
template <typename Row2>
void
Linear_Expression_Impl<Row>::add_mul_assign(Coefficient_traits::const_reference factor,
                                            const Linear_Expression_Impl<Row2>& y) {
  if (factor != 0)
    linear_combine(y, Coefficient_one(), factor);
}

template <typename Row>
template <typename Row2>
void
Linear_Expression_Impl<Row>::sub_mul_assign(Coefficient_traits::const_reference factor,
                                            const Linear_Expression_Impl<Row2>& y) {
  if (factor != 0)
    linear_combine(y, Coefficient_one(), -factor);
}

template <typename Row>
bool
Linear_Expression_Impl<Row>::OK() const {
  return row.size() != 0;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression_Impl */
template <typename Row>
std::ostream&
Linear_Expression_Impl<Row>::operator<<(std::ostream& s) const {
  const dimension_type num_variables = space_dimension();
  PPL_DIRTY_TEMP_COEFFICIENT(ev);
  bool first = true;
  for (dimension_type v = 0; v < num_variables; ++v) {
    ev = row.get(v+1);
    if (ev != 0) {
      if (!first) {
        if (ev > 0)
          s << " + ";
        else {
          s << " - ";
          neg_assign(ev);
        }
      }
      else
        first = false;
      if (ev == -1)
        s << "-";
      else if (ev != 1)
        s << ev << "*";
      IO_Operators::operator<<(s, Variable(v));
    }
  }
  // Inhomogeneous term.
  PPL_DIRTY_TEMP_COEFFICIENT(it);
  it = row[0];
  if (it != 0) {
    if (!first) {
      if (it > 0)
        s << " + ";
      else {
        s << " - ";
        neg_assign(it);
      }
    }
    else
      first = false;
    s << it;
  }

  if (first)
    // The null linear expression.
    s << Coefficient_zero();
  return s;
}

template <typename Row>
Coefficient&
Linear_Expression_Impl<Row>::operator[](dimension_type i) {
  return row[i];
}

template <typename Row>
const Coefficient&
Linear_Expression_Impl<Row>::operator[](dimension_type i) const {
  return row[i];
}

template <typename Row>
Coefficient_traits::const_reference
Linear_Expression_Impl<Row>::get(dimension_type i) const {
  return row.get(i);
}

template <typename Row>
void
Linear_Expression_Impl<Row>
::set(dimension_type i, Coefficient_traits::const_reference n) {
  if (n == 0)
    row.reset(i);
  else
    row.insert(i, n);
  PPL_ASSERT(OK());
}

template <typename Row>
bool
Linear_Expression_Impl<Row>::all_zeroes(dimension_type start, dimension_type end) const {
  for (typename Row::const_iterator i = row.lower_bound(start), i_end = row.lower_bound(end);
       i != i_end; ++i)
    if (*i != 0)
      return false;
  return true;
}

template <typename Row>
dimension_type
Linear_Expression_Impl<Row>::num_zeroes(dimension_type start, dimension_type end) const {
  PPL_ASSERT(start <= end);
  dimension_type result = end - start;
  for (typename Row::const_iterator i = row.lower_bound(start), i_end = row.lower_bound(end);
       i != i_end; ++i)
    if (*i != 0)
      --result;
  return result;
}

template <typename Row>
Coefficient
Linear_Expression_Impl<Row>::gcd(dimension_type start, dimension_type end) const {
  typename Row::const_iterator i = row.lower_bound(start);
  typename Row::const_iterator i_end = row.lower_bound(end);

  while (1) {
    if (i == i_end)
      return 0;

    if (*i != 0)
      break;

    ++i;
  }

  PPL_ASSERT(*i != 0);

  Coefficient result = *i;
  ++i;

  if (result < 0)
    neg_assign(result);

  for ( ; i != i_end; ++i) {
    if (*i == 0)
      continue;
    gcd_assign(result, *i, result);
    if (result == 1)
      return result;
  }

  return result;
}

template <typename Row>
void
Linear_Expression_Impl<Row>
::exact_div_assign(Coefficient_traits::const_reference c,
                   dimension_type start, dimension_type end) {
  // NOTE: Since all coefficients in [start,end) are multiple of c,
  // each of the resulting coefficients will be nonzero iff the initial
  // coefficient was.
  for (typename Row::iterator
    i = row.lower_bound(start), i_end = row.lower_bound(end); i != i_end; ++i)
    Parma_Polyhedra_Library::exact_div_assign(*i, *i, c);
  PPL_ASSERT(OK());
}

template <typename Row>
void
Linear_Expression_Impl<Row>
::mul_assign(Coefficient_traits::const_reference c,
                   dimension_type start, dimension_type end) {
  if (c == 0) {
    typename Row::iterator i = row.lower_bound(start);
    const typename Row::iterator& i_end = row.end();
    while (i != i_end && i.index() < end)
      i = row.reset(i);
  } else {
    for (typename Row::iterator
      i = row.lower_bound(start), i_end = row.lower_bound(end); i != i_end; ++i)
      (*i) *= c;
  }
  PPL_ASSERT(OK());
}

template <typename Row>
template <typename Row2>
void
Linear_Expression_Impl<Row>
::linear_combine(const Linear_Expression_Impl<Row2>& y,
                 Coefficient_traits::const_reference c1,
                 Coefficient_traits::const_reference c2,
                 dimension_type start, dimension_type end) {
  Parma_Polyhedra_Library::linear_combine(row, y.row, c1, c2, start, end);
  PPL_ASSERT(OK());
}

template <typename Row>
template <typename Row2>
void
Linear_Expression_Impl<Row>
::linear_combine_lax(const Linear_Expression_Impl<Row2>& y,
                     Coefficient_traits::const_reference c1,
                     Coefficient_traits::const_reference c2,
                     dimension_type start, dimension_type end) {
  PPL_ASSERT(start <= end);
  PPL_ASSERT(end <= row.size());
  PPL_ASSERT(end <= y.row.size());
  if (c1 == 0) {
    if (c2 == 0) {
      PPL_ASSERT(c1 == 0);
      PPL_ASSERT(c2 == 0);
      typename Row::iterator i = row.lower_bound(start);
      const typename Row::iterator& i_end = row.end();
      while (i != i_end && i.index() < end)
        i = row.reset(i);
    } else {
      PPL_ASSERT(c1 == 0);
      PPL_ASSERT(c2 != 0);

      typename Row::iterator i = row.lower_bound(start);
      const typename Row::iterator& i_end = row.end();
      typename Row2::const_iterator j = y.row.lower_bound(start);
      typename Row2::const_iterator j_last = y.row.lower_bound(end);

      while (i != i_end && i.index() < end && j != j_last) {
        if (i.index() < j.index()) {
          i = row.reset(i);
          continue;
        }
        if (i.index() > j.index()) {
          i = row.insert(i, j.index(), *j);
          (*i) *= c2;
          ++i;
          ++j;
          continue;
        }
        PPL_ASSERT(i.index() == j.index());
        (*i) = (*j);
        (*i) *= c2;
        ++i;
        ++j;
      }
      while (i != i_end && i.index() < end)
        i = row.reset(i);
      while (j != j_last) {
        i = row.insert(i, j.index(), *j);
        (*i) *= c2;
        // No need to increment i here.
        ++j;
      }
    }
  } else {
    if (c2 == 0) {
      PPL_ASSERT(c1 != 0);
      PPL_ASSERT(c2 == 0);
      for (typename Row::iterator i = row.lower_bound(start),
                                  i_end = row.lower_bound(end);
          i != i_end; ++i)
        (*i) *= c1;
    } else {
      PPL_ASSERT(c1 != 0);
      PPL_ASSERT(c2 != 0);
      Parma_Polyhedra_Library::linear_combine(row, y.row, c1, c2, start, end);
    }
  }
  PPL_ASSERT(OK());
}

template <typename Row>
void
Linear_Expression_Impl<Row>::sign_normalize() {
  typename Row::iterator i = row.lower_bound(1);
  typename Row::iterator i_end = row.end();

  for ( ; i != i_end; ++i)
    if (*i != 0)
      break;

  if (i != i_end && *i < 0) {
    for ( ; i != i_end; ++i)
      neg_assign(*i);
    // Negate the first coefficient, too.
    typename Row::iterator i = row.begin();
    if (i != row.end() && i.index() == 0)
      neg_assign(*i);
  }
  PPL_ASSERT(OK());
}

template <typename Row>
void
Linear_Expression_Impl<Row>::negate(dimension_type first, dimension_type last) {
  PPL_ASSERT(first <= last);
  PPL_ASSERT(last <= row.size());
  typename Row::iterator i = row.lower_bound(first);
  typename Row::iterator i_end = row.lower_bound(last);
  for ( ; i != i_end; ++i)
    neg_assign(*i);
  PPL_ASSERT(OK());
}

template <typename Row>
bool
Linear_Expression_Impl<Row>::all_zeroes(const Variables_Set& vars) const {
  typename Row::const_iterator i = row.begin();
  typename Row::const_iterator i_end = row.end();
  Variables_Set::const_iterator j = vars.begin();
  Variables_Set::const_iterator j_end = vars.end();

  for ( ; j != j_end; j++) {
    i = row.lower_bound(i, *j + 1);
    if (i == i_end)
      break;
    if (i.index() == *j + 1 && *i != 0)
      return false;
  }

  return true;
}

template <typename Row>
bool
Linear_Expression_Impl<Row>
::all_zeroes_except(const Variables_Set& vars, dimension_type start, dimension_type end) const {
  PPL_ASSERT(start <= end);
  if (start == end)
    return true;
  if (start == 0) {
    if (row.get(0) != 0)
      return false;

    start = 1;
  }

  PPL_ASSERT(start != 0);
  PPL_ASSERT(start <= end);
  for (typename Row::const_iterator i = row.lower_bound(start), i_end = row.lower_bound(end); i != i_end; i++)
    if (*i != 0 && vars.count(i.index() - 1) == 0)
      return false;

  return true;
}

template <typename Row>
template <typename Row2, typename Row3>
void
Linear_Expression_Impl<Row>
::modify_according_to_evolution(const Linear_Expression_Impl<Row2>& x,
                                const Linear_Expression_Impl<Row3>& y) {
  PPL_DIRTY_TEMP_COEFFICIENT(tmp);
  std::deque<bool> considered(x.space_dimension() + 1);

  // The following loop is an optimized version of this loop:
  //
  // for (dimension_type k = 1; k < x.space_dimension(); ++k) {
  //   if (considered[k])
  //     continue;
  //
  //   for (dimension_type h = k + 1; h <= x.space_dimension(); ++h) {
  //     if (considered[h])
  //       continue;
  //
  //     tmp = (x[k] * y[h]) - (x[h] * y[k]);
  //
  //     const int clockwise = sgn(tmp);
  //     const int first_or_third_quadrant = sgn(x[k]) * sgn(x[h]);
  //     switch (clockwise * first_or_third_quadrant) {
  //     case -1:
  //       row[k] = 0;
  //       considered[k] = true;
  //       break;
  //     case 1:
  //       row[h] = 0;
  //       considered[h] = true;
  //       break;
  //     default:
  //       break;
  //     }
  //   }
  // }

  typename Row::iterator itr = row.end();

  typename Row2::const_iterator x_end = x.row.end();
  typename Row3::const_iterator y_end = y.row.end();
  typename Row3::const_iterator y_k = y.row.end();
  for (typename Row2::const_iterator x_k = x.row.begin(); x_k != x_end; ++x_k) {
    const dimension_type k = x_k.index();
    if (considered[k])
      continue;

    y_k = y.row.lower_bound(y_k, k);

    if (y_k == y.row.end())
      break;

    // Note that y_k.index() may not be k.

    typename Row3::const_iterator y_h = y_k;

    typename Row2::const_iterator x_h = x_k;
    ++x_h;
    for ( ; x_h != x_end; ++x_h) {
      const dimension_type h = x_h.index();
      if (considered[h])
        continue;

      y_h = y.row.lower_bound(y_h, h);

      // Note that y_k may be y_end, and y_k.index() may not be k.

      if (y_h != y_end && y_h.index() == h)
        tmp = (*x_k) * (*y_h);
      else
        tmp = 0;

      if (y_k.index() == k) {
        // The following line optimizes the computation of
        // tmp -= x[h] * y[k];
        Parma_Polyhedra_Library::sub_mul_assign(tmp, *x_h, *y_k);
      }

      const int clockwise = sgn(tmp);
      const int first_or_third_quadrant = sgn(*x_k) * sgn(*x_h);
      switch (clockwise * first_or_third_quadrant) {
      case -1:
        // Optimized version of row.reset(k)
        itr = row.lower_bound(itr, k);
        if (itr != row.end() && itr.index() == k)
          itr = row.reset(itr);
        considered[k] = true;
        break;
      case 1:
        // Optimized version of row.reset(h)
        itr = row.lower_bound(itr, h);
        if (itr != row.end() && itr.index() == h)
          itr = row.reset(itr);
        considered[h] = true;
        break;
      default:
        break;
      }
    }
  }
  normalize();
  PPL_ASSERT(OK());
}

template <typename Row>
dimension_type
Linear_Expression_Impl<Row>::last_nonzero() const {
  typename Row::const_iterator i = row.begin();
  typename Row::const_iterator i_end = row.end();

  while (1) {
    if (i == i_end)
      return 0;
    --i_end;
    if (*i_end != 0)
      return i_end.index();
  }
}

template <typename Row>
template <typename Row2>
void
Linear_Expression_Impl<Row>::construct(const Linear_Expression_Impl<Row2>& e) {
  row = e.row;
  PPL_ASSERT(OK());
}

template <typename Row>
template <typename Row2>
void
Linear_Expression_Impl<Row>::construct(const Linear_Expression_Impl<Row2>& e,
                                       dimension_type sz) {
  row = Row(e.row, sz, sz);
  PPL_ASSERT(OK());
}

template <typename Row>
template <typename Row2>
void
Linear_Expression_Impl<Row>
::scalar_product_assign(Coefficient& result, const Linear_Expression_Impl<Row2>& y,
                        dimension_type start, dimension_type end) const {
  const Linear_Expression_Impl<Row>& x = *this;
  PPL_ASSERT(start <= end);
  PPL_ASSERT(end <= x.row.size());
  PPL_ASSERT(end <= y.row.size());
  result = 0;
  typename Row ::const_iterator x_i = x.row.lower_bound(start);
  typename Row ::const_iterator x_end = x.row.lower_bound(end);
  typename Row2::const_iterator y_i = y.row.lower_bound(start);
  typename Row2::const_iterator y_end = y.row.lower_bound(end);
  while (x_i != x_end && y_i != y_end) {
    if (x_i.index() == y_i.index()) {
      Parma_Polyhedra_Library::add_mul_assign(result, *x_i, *y_i);
      ++x_i;
      ++y_i;
    } else {
      if (x_i.index() < y_i.index()) {
        PPL_ASSERT(y.row.get(x_i.index()) == 0);
        // (*x_i) * 0 == 0, nothing to do.
        ++x_i;
      } else {
        PPL_ASSERT(x.row.get(y_i.index()) == 0);
        // 0 * (*y_i) == 0, nothing to do.
        ++y_i;
      }
    }
  }
  // In the remaining positions (if any) at most one row is nonzero, so
  // there's nothing left to do.
}

template <typename Row>
template <typename Row2>
int
Linear_Expression_Impl<Row>
::scalar_product_sign(const Linear_Expression_Impl<Row2>& y,
                      dimension_type start, dimension_type end) const {
  PPL_DIRTY_TEMP_COEFFICIENT(result);
  scalar_product_assign(result, y, start, end);
  return sgn(result);
}

template <typename Row>
dimension_type
Linear_Expression_Impl<Row>
::first_nonzero(dimension_type first, dimension_type last) const {
  PPL_ASSERT(first <= last);
  PPL_ASSERT(last <= row.size());
  typename Row::const_iterator i = row.lower_bound(first);
  typename Row::const_iterator i_end = row.lower_bound(last);
  for ( ; i != i_end; ++i)
    if (*i != 0)
      return i.index();

  return last;
}

template <typename Row>
void
Linear_Expression_Impl<Row>
::has_a_free_dimension_helper(std::set<dimension_type>& x) const {
  std::set<dimension_type> result;
  typename Row::const_iterator itr = row.end();
  typename Row::const_iterator itr_end = row.end();
  std::set<dimension_type>::const_iterator i = x.begin();
  std::set<dimension_type>::const_iterator i_end = x.end();
  for ( ; i != i_end; ++i) {
    itr = row.lower_bound(itr, *i);
    if (itr == itr_end || itr.index() != *i || *itr == 0)
      result.insert(*i);
  }
  std::swap(x, result);
}

template <typename Row>
template <typename Row2>
bool
Linear_Expression_Impl<Row>
::is_equal_to(const Linear_Expression_Impl<Row2>& y,
              dimension_type start, dimension_type end) const {
  const Linear_Expression_Impl<Row>& x = *this;
  PPL_ASSERT(start <= end);
  PPL_ASSERT(end <= x.row.size());
  PPL_ASSERT(end <= y.row.size());

  typename Row::const_iterator i = x.row.lower_bound(start);
  typename Row::const_iterator i_end = x.row.lower_bound(end);
  typename Row2::const_iterator j = y.row.lower_bound(start);
  typename Row2::const_iterator j_end = y.row.lower_bound(end);
  while (i != i_end && j != j_end) {
    if (i.index() == j.index()) {
      if (*i != *j)
        return false;
      ++i;
      ++j;
    } else {
      if (i.index() < j.index()) {
        if (*i != 0)
          return false;
        ++i;
      } else {
        PPL_ASSERT(i.index() > j.index());
        if (*j != 0)
          return false;
        ++j;
      }
    }
  }
  for ( ; i != i_end; ++i)
    if (*i != 0)
      return false;
  for ( ; j != j_end; ++j)
    if (*j != 0)
      return false;
  return true;
}

template <typename Row>
template <typename Row2>
bool
Linear_Expression_Impl<Row>
::is_equal_to(const Linear_Expression_Impl<Row2>& y,
              Coefficient_traits::const_reference c1,
              Coefficient_traits::const_reference c2,
              dimension_type start, dimension_type end) const {
  const Linear_Expression_Impl<Row>& x = *this;
  PPL_ASSERT(start <= end);
  PPL_ASSERT(end <= x.row.size());
  PPL_ASSERT(end <= y.row.size());

  // Deal with trivial cases.
  if (c1 == 0) {
    if (c2 == 0)
      return true;
    else
      return y.all_zeroes(start, end);
  }
  if (c2 == 0)
    return x.all_zeroes(start, end);

  PPL_ASSERT(c1 != 0);
  PPL_ASSERT(c2 != 0);
  typename Row::const_iterator i = x.row.lower_bound(start);
  typename Row::const_iterator i_end = x.row.lower_bound(end);
  typename Row2::const_iterator j = y.row.lower_bound(start);
  typename Row2::const_iterator j_end = y.row.lower_bound(end);
  while (i != i_end && j != j_end) {
    if (i.index() == j.index()) {
      if ((*i) * c1 != (*j) * c2)
        return false;
      ++i;
      ++j;
    } else {
      if (i.index() < j.index()) {
        if (*i != 0)
          return false;
        ++i;
      } else {
        PPL_ASSERT(i.index() > j.index());
        if (*j != 0)
          return false;
        ++j;
      }
    }
  }
  for ( ; i != i_end; ++i)
    if (*i != 0)
      return false;
  for ( ; j != j_end; ++j)
    if (*j != 0)
      return false;
  return true;
}

template <>
bool
Linear_Expression_Impl<Dense_Row>::is_unbounded_obj_function(
  const std::vector<std::pair<dimension_type, dimension_type> >& mapping,
  Optimization_Mode optimization_mode) const {

  for (dimension_type i = 1; i < row.size(); ++i) {
    // If a the value of a variable in the objective function is
    // different from zero, the final status is unbounded.
    // In the first part the variable is constrained to be greater or equal
    // than zero.
    Coefficient_traits::const_reference c = row[i];
    if (mapping[i].second != 0) {
      if (c != 0)
        return true;
    } else {
      PPL_ASSERT(mapping[i].second == 0);
      if (optimization_mode == MAXIMIZATION) {
        if (c > 0)
          return true;
      } else {
        PPL_ASSERT(optimization_mode == MINIMIZATION);
        if (c < 0)
          return true;
      }
    }
  }
  
  return false;
}

template <>
bool
Linear_Expression_Impl<Sparse_Row>::is_unbounded_obj_function(
  const std::vector<std::pair<dimension_type, dimension_type> >& mapping,
  Optimization_Mode optimization_mode) const {

  for (Sparse_Row::const_iterator i = row.lower_bound(1), i_end = row.end(); i != i_end; ++i) {
    // If a the value of a variable in the objective function is
    // different from zero, the final status is unbounded.
    // In the first part the variable is constrained to be greater or equal
    // than zero.
    if (mapping[i.index()].second != 0)
      return true;
    if (optimization_mode == MAXIMIZATION) {
      if (*i > 0)
        return true;
    } else {
      PPL_ASSERT(optimization_mode == MINIMIZATION);
      if (*i < 0)
        return true;
    }
  }
  return false;
}

template <typename Row>
void
Linear_Expression_Impl<Row>
::linear_combine(const Linear_Expression_Interface& y, Variable v) {
  if (const Linear_Expression_Impl<Dense_Row>* p = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&y)) {
    linear_combine(*p, v);
  } else if (const Linear_Expression_Impl<Sparse_Row>* p = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&y)) {
    linear_combine(*p, v);
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
  }
}

template <typename Row>
void
Linear_Expression_Impl<Row>
::linear_combine(const Linear_Expression_Interface& y,
                 Coefficient_traits::const_reference c1,
                 Coefficient_traits::const_reference c2) {
  if (const Linear_Expression_Impl<Dense_Row>* p = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&y)) {
    linear_combine(*p, c1, c2);
  } else if (const Linear_Expression_Impl<Sparse_Row>* p = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&y)) {
    linear_combine(*p, c1, c2);
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
  }
}

template <typename Row>
void
Linear_Expression_Impl<Row>
::linear_combine_lax(const Linear_Expression_Interface& y,
                     Coefficient_traits::const_reference c1,
                     Coefficient_traits::const_reference c2) {
  if (const Linear_Expression_Impl<Dense_Row>* p = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&y)) {
    linear_combine_lax(*p, c1, c2);
  } else if (const Linear_Expression_Impl<Sparse_Row>* p = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&y)) {
    linear_combine_lax(*p, c1, c2);
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
  }
}

template <typename Row>
void
Linear_Expression_Impl<Row>
::swap(Linear_Expression_Interface& y) {
  if (Linear_Expression_Impl<Dense_Row>* p = dynamic_cast<Linear_Expression_Impl<Dense_Row>*>(&y)) {
    swap(*p);
  } else if (Linear_Expression_Impl<Sparse_Row>* p = dynamic_cast<Linear_Expression_Impl<Sparse_Row>*>(&y)) {
    swap(*p);
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
  }
}

template <typename Row>
bool
Linear_Expression_Impl<Row>
::is_equal_to(const Linear_Expression_Interface& y) const {
  if (const Linear_Expression_Impl<Dense_Row>* p = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&y)) {
    return is_equal_to(*p);
  } else if (const Linear_Expression_Impl<Sparse_Row>* p = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&y)) {
    return is_equal_to(*p);
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
    return false;
  }
}

template <typename Row>
Linear_Expression_Impl<Row>&
Linear_Expression_Impl<Row>
::operator+=(const Linear_Expression_Interface& y) {
  if (const Linear_Expression_Impl<Dense_Row>* p = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&y)) {
    return operator+=(*p);
  } else if (const Linear_Expression_Impl<Sparse_Row>* p = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&y)) {
    return operator+=(*p);
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
    return *this;
  }
}

template <typename Row>
Linear_Expression_Impl<Row>&
Linear_Expression_Impl<Row>
::operator-=(const Linear_Expression_Interface& y) {
  if (const Linear_Expression_Impl<Dense_Row>* p = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&y)) {
    return operator-=(*p);
  } else if (const Linear_Expression_Impl<Sparse_Row>* p = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&y)) {
    return operator-=(*p);
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
    return *this;
  }
}

template <typename Row>
void
Linear_Expression_Impl<Row>
::add_mul_assign(Coefficient_traits::const_reference factor,
                 const Linear_Expression_Interface& y) {
  if (const Linear_Expression_Impl<Dense_Row>* p = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&y)) {
    add_mul_assign(factor, *p);
  } else if (const Linear_Expression_Impl<Sparse_Row>* p = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&y)) {
    add_mul_assign(factor, *p);
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
  }
}

template <typename Row>
void
Linear_Expression_Impl<Row>
::sub_mul_assign(Coefficient_traits::const_reference factor,
                 const Linear_Expression_Interface& y) {
  if (const Linear_Expression_Impl<Dense_Row>* p = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&y)) {
    sub_mul_assign(factor, *p);
  } else if (const Linear_Expression_Impl<Sparse_Row>* p = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&y)) {
    sub_mul_assign(factor, *p);
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
  }
}

template <typename Row>
void
Linear_Expression_Impl<Row>
::linear_combine(const Linear_Expression_Interface& y, dimension_type i) {
  if (const Linear_Expression_Impl<Dense_Row>* p = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&y)) {
    linear_combine(*p, i);
  } else if (const Linear_Expression_Impl<Sparse_Row>* p = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&y)) {
    linear_combine(*p, i);
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
  }
}

template <typename Row>
void
Linear_Expression_Impl<Row>
::linear_combine(const Linear_Expression_Interface& y,
                 Coefficient_traits::const_reference c1,
                 Coefficient_traits::const_reference c2,
                 dimension_type start, dimension_type end) {
  if (const Linear_Expression_Impl<Dense_Row>* p = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&y)) {
    linear_combine(*p, c1, c2, start, end);
  } else if (const Linear_Expression_Impl<Sparse_Row>* p = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&y)) {
    linear_combine(*p, c1, c2, start, end);
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
  }
}

template <typename Row>
void
Linear_Expression_Impl<Row>
::linear_combine_lax(const Linear_Expression_Interface& y,
                     Coefficient_traits::const_reference c1,
                     Coefficient_traits::const_reference c2,
                     dimension_type start, dimension_type end) {
  if (const Linear_Expression_Impl<Dense_Row>* p = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&y)) {
    linear_combine_lax(*p, c1, c2, start, end);
  } else if (const Linear_Expression_Impl<Sparse_Row>* p = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&y)) {
    linear_combine_lax(*p, c1, c2, start, end);
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
  }
}

template <typename Row>
void
Linear_Expression_Impl<Row>
::modify_according_to_evolution(const Linear_Expression_Interface& x,
                                const Linear_Expression_Interface& y) {
  if (const Linear_Expression_Impl<Dense_Row>* p_x = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&x)) {
    if (const Linear_Expression_Impl<Dense_Row>* p_y = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&y)) {
      modify_according_to_evolution(*p_x, *p_y);
    } else if (const Linear_Expression_Impl<Sparse_Row>* p_y = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&y)) {
      modify_according_to_evolution(*p_x, *p_y);
    } else {
      // Add implementations for new derived classes here.
      PPL_ASSERT(false);
    }
  } else if (const Linear_Expression_Impl<Sparse_Row>* p_x = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&x)) {
    if (const Linear_Expression_Impl<Dense_Row>* p_y = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&y)) {
      modify_according_to_evolution(*p_x, *p_y);
    } else if (const Linear_Expression_Impl<Sparse_Row>* p_y = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&y)) {
      modify_according_to_evolution(*p_x, *p_y);
    } else {
      // Add implementations for new derived classes here.
      PPL_ASSERT(false);
    }
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
  }
}

template <typename Row>
int
Linear_Expression_Impl<Row>::compare(const Linear_Expression_Interface& y) const {
  if (const Linear_Expression_Impl<Dense_Row>* p = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&y)) {
    return compare(*p);
  } else if (const Linear_Expression_Impl<Sparse_Row>* p = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&y)) {
    return compare(*p);
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
    return 0;
  }
}


template <typename Row>
void
Linear_Expression_Impl<Row>::construct(const Linear_Expression_Interface& y) {
  if (const Linear_Expression_Impl<Dense_Row>* p = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&y)) {
    return construct(*p);
  } else if (const Linear_Expression_Impl<Sparse_Row>* p = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&y)) {
    return construct(*p);
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
  }
}

template <typename Row>
void
Linear_Expression_Impl<Row>::construct(const Linear_Expression_Interface& y,
                                       dimension_type sz) {
  if (const Linear_Expression_Impl<Dense_Row>* p = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&y)) {
    return construct(*p, sz);
  } else if (const Linear_Expression_Impl<Sparse_Row>* p = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&y)) {
    return construct(*p, sz);
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
  }
}

template <typename Row>
void
Linear_Expression_Impl<Row>
::scalar_product_assign(Coefficient& result, const Linear_Expression_Interface& y,
                        dimension_type start, dimension_type end) const {
  if (const Linear_Expression_Impl<Dense_Row>* p = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&y)) {
    scalar_product_assign(result, *p, start, end);
  } else if (const Linear_Expression_Impl<Sparse_Row>* p = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&y)) {
    scalar_product_assign(result, *p, start, end);
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
  }
}

template <typename Row>
int
Linear_Expression_Impl<Row>
::scalar_product_sign(const Linear_Expression_Interface& y,
                      dimension_type start, dimension_type end) const {
  if (const Linear_Expression_Impl<Dense_Row>* p = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&y)) {
    return scalar_product_sign(*p, start, end);
  } else if (const Linear_Expression_Impl<Sparse_Row>* p = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&y)) {
    return scalar_product_sign(*p, start, end);
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
    return 0;
  }
}

template <typename Row>
bool
Linear_Expression_Impl<Row>
::is_equal_to(const Linear_Expression_Interface& y,
              dimension_type start, dimension_type end) const {
  if (const Linear_Expression_Impl<Dense_Row>* p = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&y)) {
    return is_equal_to(*p, start, end);
  } else if (const Linear_Expression_Impl<Sparse_Row>* p = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&y)) {
    return is_equal_to(*p, start, end);
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
    return 0;
  }
}

template <typename Row>
bool
Linear_Expression_Impl<Row>
::is_equal_to(const Linear_Expression_Interface& y,
              Coefficient_traits::const_reference c1,
              Coefficient_traits::const_reference c2,
              dimension_type start, dimension_type end) const {
  if (const Linear_Expression_Impl<Dense_Row>* p = dynamic_cast<const Linear_Expression_Impl<Dense_Row>*>(&y)) {
    return is_equal_to(*p, c1, c2, start, end);
  } else if (const Linear_Expression_Impl<Sparse_Row>* p = dynamic_cast<const Linear_Expression_Impl<Sparse_Row>*>(&y)) {
    return is_equal_to(*p, c1, c2, start, end);
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
    return 0;
  }
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Linear_Expression_Impl_templates_hh)
