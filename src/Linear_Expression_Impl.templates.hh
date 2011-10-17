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

#include "Constraint.defs.hh"
#include "Generator.defs.hh"
#include "Grid_Generator.defs.hh"
#include "Congruence.defs.hh"
#include <deque>
#include <stdexcept>
#include <iostream>

namespace Parma_Polyhedra_Library {

template <typename Row>
Linear_Expression_Impl<Row>::Linear_Expression_Impl(const Linear_Expression_Impl& e) {
  construct(e);
}

template <typename Row>
Linear_Expression_Impl<Row>::Linear_Expression_Impl(const Linear_Expression_Interface& e) {
  if (const Linear_Expression_Impl* p = dynamic_cast<const Linear_Expression_Impl*>(&e)) {
    construct(*p);
  } else {
    // Add implementations for other derived classes here.
    PPL_ASSERT(false);
  }
}

template <typename Row>
Linear_Expression_Impl<Row>::Linear_Expression_Impl(const Linear_Expression_Interface& e,
                                                    dimension_type sz) {
  if (const Linear_Expression_Impl* p = dynamic_cast<const Linear_Expression_Impl*>(&e)) {
    construct(*p, sz);
  } else {
    // Add implementations for other derived classes here.
    PPL_ASSERT(false);
  }
}

template <typename Row>
template <typename Row2>
void
Linear_Expression_Impl<Row>
::linear_combine(const Linear_Expression_Impl<Row2>& y, Variable i) {
  linear_combine(y, i.space_dimension());
}

template <typename Row>
template <typename Row2>
void
Linear_Expression_Impl<Row>
::linear_combine(const Linear_Expression_Impl<Row2>& y, dimension_type i) {
  Linear_Expression_Impl& x = *this;
  // We can combine only vector of the same dimension.
  PPL_ASSERT(x.space_dimension() == y.space_dimension());
  PPL_ASSERT(x.row[i] != 0);
  PPL_ASSERT(y.row[i] != 0);
  PPL_DIRTY_TEMP_COEFFICIENT(normalized_x_v);
  PPL_DIRTY_TEMP_COEFFICIENT(normalized_y_v);
  normalize2(x.row[i], y.row[i], normalized_x_v, normalized_y_v);
  neg_assign(normalized_x_v);
  x.row.linear_combine(y.row, normalized_y_v, normalized_x_v);
  assert(x.row[i] == 0);
  PPL_ASSERT(OK());
}

template <typename Row>
template <typename Row2>
void
Linear_Expression_Impl<Row>
::linear_combine(const Linear_Expression_Impl<Row2>& y,
                 Coefficient_traits::const_reference c1,
                 Coefficient_traits::const_reference c2) {
  row.linear_combine(y.row, c1, c2);
  PPL_ASSERT(OK());
}

template <typename Row>
template <typename Row2>
void
Linear_Expression_Impl<Row>::swap(Linear_Expression_Impl<Row2>& y) {
  row.swap(y.row);
}

template <typename Row>
template <typename Row2>
int
Linear_Expression_Impl<Row>::compare(const Linear_Expression_Impl<Row2>& y) const {
  const Linear_Expression_Impl& x = *this;
  // Compare all the coefficients of the row starting from position 1.
  const dimension_type xsz = x.row.size();
  const dimension_type ysz = y.row.size();
  const dimension_type min_sz = std::min(xsz, ysz);
  dimension_type i;
  for (i = 1; i < min_sz; ++i)
    if (const int comp = cmp(x.row[i], y.row[i]))
      // There is at least a different coefficient.
      return (comp > 0) ? 2 : -2;

  // Handle the case where `x' and `y' are of different size.
  if (xsz != ysz) {
    for( ; i < xsz; ++i)
      if (const int sign = sgn(x.row[i]))
        return (sign > 0) ? 2 : -2;
    for( ; i < ysz; ++i)
      if (const int sign = sgn(y.row[i]))
        return (sign < 0) ? 2 : -2;
  }

  // If all the coefficients in `x' equal all the coefficients in `y'
  // (starting from position 1) we compare coefficients in position 0,
  // i.e., inhomogeneous terms.
  if (const int comp = cmp(x.row[0], y.row[0]))
    return (comp > 0) ? 1 : -1;

  // `x' and `y' are equal.
  return 0;
}

template <typename Row>
Linear_Expression_Impl<Row>::Linear_Expression_Impl(const Constraint& c) {
  construct(*(c.expression().impl));
  // Do not copy the epsilon dimension (if any).
  if (c.is_not_necessarily_closed())
    row.resize(row.size() - 1);
  PPL_ASSERT(OK());
}

template <typename Row>
Linear_Expression_Impl<Row>::Linear_Expression_Impl(const Generator& g) {
  construct(*(g.expression().impl));
  // Do not copy the divisor of `g'.
  row[0] = 0;
  // Do not copy the epsilon dimension (if any).
  if (g.is_not_necessarily_closed())
    row.resize(row.size() - 1);
  PPL_ASSERT(OK());
}

template <typename Row>
Linear_Expression_Impl<Row>::Linear_Expression_Impl(const Grid_Generator& g) {
  // NOTE: This does *not* copy the last coefficient.
  construct(*(g.expression().impl), g.expression().space_dimension());
  // Do not copy the divisor of `g'.
  row[0] = 0;
  PPL_ASSERT(OK());
}

template <typename Row>
Linear_Expression_Impl<Row>::Linear_Expression_Impl(const Congruence& cg) {
  construct(*(cg.expression().impl));
  PPL_ASSERT(OK());
}

template <typename Row>
Linear_Expression_Impl<Row>::Linear_Expression_Impl(const Variable v)
  : row(v.space_dimension() <= max_space_dimension()
               ? v.space_dimension() + 1
               : (throw std::length_error("Linear_Expression_Impl::"
                                          "Linear_Expression_Impl(v):\n"
                                          "v exceeds the maximum allowed "
                                          "space dimension."),
                  v.space_dimension() + 1)) {
  ++(row[v.space_dimension()]);
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
Linear_Expression_Impl<Row>::remove_space_dimensions(const Variables_Set& vars) {
  PPL_ASSERT(vars.space_dimension() <= space_dimension());
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
    tmp = row[cycle.back().space_dimension()];
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
Linear_Expression_Impl<Row>::operator+=(const Linear_Expression_Impl<Row2>& e2) {
  Linear_Expression_Impl& e1 = *this;
  dimension_type e1_size = e1.row.size();
  dimension_type e2_size = e2.row.size();
  if (e1_size >= e2_size)
    for (dimension_type i = e2_size; i-- > 0; )
      e1.row[i] += e2.row[i];
  else {
    Linear_Expression_Impl new_e(e2);
    for (dimension_type i = e1_size; i-- > 0; )
      new_e.row[i] += e1.row[i];
    e1.swap(new_e);
  }
  return e1;
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
  const dimension_type e_size = row.size();
  if (e_size <= v_space_dim) {
    Linear_Expression_Impl new_e(*this, v_space_dim+1);
    swap(new_e);
  }
  ++(row[v_space_dim]);
  return *this;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression_Impl */
template <typename Row>
template <typename Row2>
Linear_Expression_Impl<Row>&
Linear_Expression_Impl<Row>::operator-=(const Linear_Expression_Impl<Row2>& e2) {
  Linear_Expression_Impl& e1 = *this;
  dimension_type e1_size = e1.row.size();
  dimension_type e2_size = e2.row.size();
  if (e1_size >= e2_size)
    for (dimension_type i = e2_size; i-- > 0; )
      e1.row[i] -= e2.row[i];
  else {
    Linear_Expression_Impl new_e(e1, e2_size);
    for (dimension_type i = e2_size; i-- > 0; )
      new_e.row[i] -= e2.row[i];
    e1.swap(new_e);
  }
  return e1;
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
  const dimension_type e_size = row.size();
  if (e_size <= v_space_dim) {
    Linear_Expression_Impl new_e(*this, v_space_dim+1);
    swap(new_e);
  }
  --row[v_space_dim];
  return *this;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression_Impl */
template <typename Row>
Linear_Expression_Impl<Row>&
Linear_Expression_Impl<Row>::operator*=(Coefficient_traits::const_reference n) {
  for (dimension_type i = row.size(); i-- > 0; )
    row[i] *= n;
  return *this;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression_Impl */
template <typename Row>
Linear_Expression_Impl<Row>&
Linear_Expression_Impl<Row>::operator/=(Coefficient_traits::const_reference n) {
  for (dimension_type i = row.size(); i-- > 0; )
    row[i] /= n;
  return *this;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression_Impl */
template <typename Row>
void
Linear_Expression_Impl<Row>::negate() {
  for (dimension_type i = row.size(); i-- > 0; )
    neg_assign(row[i]);
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
  if (row.size() <= v_space_dim) {
    Linear_Expression_Impl new_e(*this, v_space_dim+1);
    swap(new_e);
  }
  row[v_space_dim] += n;
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
  if (row.size() <= v_space_dim) {
    Linear_Expression_Impl new_e(*this, v_space_dim+1);
    swap(new_e);
  }
  row[v_space_dim] -= n;
  return *this;
}

template <typename Row>
template <typename Row2>
Linear_Expression_Impl<Row>&
Linear_Expression_Impl<Row>::sub_mul_assign(Coefficient_traits::const_reference n,
                                            const Linear_Expression_Impl<Row2>& y,
                                            dimension_type start, dimension_type end) {
  for (dimension_type i = start; i < end; i++)
    row[i] -= n*y.row[i];
  return *this;
}

template <typename Row>
template <typename Row2>
void
Linear_Expression_Impl<Row>::add_mul_assign(Coefficient_traits::const_reference factor,
                                            const Linear_Expression_Impl<Row2>& e2) {
  Linear_Expression_Impl& e1 = *this;
  if (e2.space_dimension() > e1.space_dimension())
    e1.set_space_dimension(e2.space_dimension());
  PPL_ASSERT(e1.space_dimension() >= e2.space_dimension());
  for (dimension_type i = 0; i < e2.row.size(); i++)
    e1[i] += factor * e2[i];
}

template <typename Row>
template <typename Row2>
void
Linear_Expression_Impl<Row>::sub_mul_assign(Coefficient_traits::const_reference factor,
                                            const Linear_Expression_Impl<Row2>& e2) {
  Linear_Expression_Impl& e1 = *this;
  if (e2.space_dimension() > e1.space_dimension())
    e1.set_space_dimension(e2.space_dimension());
  PPL_ASSERT(e1.space_dimension() >= e2.space_dimension());
  for (dimension_type i = 0; i < e2.row.size(); i++)
    e1[i] -= factor * e2[i];
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
    ev = row[v+1];
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
const Coefficient&
Linear_Expression_Impl<Row>::get(dimension_type i) const {
  return row.get(i);
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
  for (dimension_type i = start; i < end; ++i) {
    Coefficient& x = row[i];
    Parma_Polyhedra_Library::exact_div_assign(x, x, c);
  }
}

template <typename Row>
template <typename Row2>
void
Linear_Expression_Impl<Row>
::linear_combine(const Linear_Expression_Impl<Row2>& y,
                 Coefficient_traits::const_reference c1,
                 Coefficient_traits::const_reference c2,
                 dimension_type start, dimension_type end) {
  row.linear_combine(y.row, c1, c2, start, end);
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
}

template <typename Row>
void
Linear_Expression_Impl<Row>::negate(dimension_type first, dimension_type last) {
  typename Row::iterator i = row.lower_bound(first);
  typename Row::iterator i_end = row.lower_bound(last);
  for ( ; i != i_end; ++i)
    neg_assign(*i);
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

  typename Row::const_iterator x_end = x.row.end();
  typename Row::const_iterator y_end = y.row.end();
  typename Row::const_iterator y_k = y.row.end();
  for (typename Row::const_iterator x_k = x.row.begin(); x_k != x_end; ++x_k) {
    const dimension_type k = x_k.index();
    if (considered[k])
      continue;

    y_k = y.row.lower_bound(y_k, k);

    if (y_k == y.row.end())
      break;

    // Note that y_k.index() may not be k.

    typename Row::const_iterator y_h = y_k;

    typename Row::const_iterator x_h = x_k;
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
        row[k] = 0;
        considered[k] = true;
        break;
      case 1:
        row[h] = 0;
        considered[h] = true;
        break;
      default:
        break;
      }
    }
  }
  normalize();
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
}

template <typename Row>
template <typename Row2>
void
Linear_Expression_Impl<Row>::construct(const Linear_Expression_Impl<Row2>& e,
                                       dimension_type sz) {
  row = Row(e.row, sz, sz);
}

template <typename Row>
void
Linear_Expression_Impl<Row>
::linear_combine(const Linear_Expression_Interface& y, Variable v) {
  if (const Linear_Expression_Impl<Row>* p = dynamic_cast<const Linear_Expression_Impl<Row>*>(&y)) {
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
  if (const Linear_Expression_Impl<Row>* p = dynamic_cast<const Linear_Expression_Impl<Row>*>(&y)) {
    linear_combine(*p, c1, c2);
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
  }
}

template <typename Row>
void
Linear_Expression_Impl<Row>
::swap(Linear_Expression_Interface& y) {
  if (Linear_Expression_Impl<Row>* p = dynamic_cast<Linear_Expression_Impl<Row>*>(&y)) {
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
  if (const Linear_Expression_Impl<Row>* p = dynamic_cast<const Linear_Expression_Impl<Row>*>(&y)) {
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
  if (const Linear_Expression_Impl<Row>* p = dynamic_cast<const Linear_Expression_Impl<Row>*>(&y)) {
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
  if (const Linear_Expression_Impl<Row>* p = dynamic_cast<const Linear_Expression_Impl<Row>*>(&y)) {
    return operator-=(*p);
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
    return *this;
  }
}

template <typename Row>
Linear_Expression_Impl<Row>&
Linear_Expression_Impl<Row>
::sub_mul_assign(Coefficient_traits::const_reference n,
                 const Linear_Expression_Interface& y,
                 dimension_type start, dimension_type end) {
  if (const Linear_Expression_Impl<Row>* p = dynamic_cast<const Linear_Expression_Impl<Row>*>(&y)) {
    return sub_mul_assign(n, *p, start, end);
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
  if (const Linear_Expression_Impl<Row>* p = dynamic_cast<const Linear_Expression_Impl<Row>*>(&y)) {
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
  if (const Linear_Expression_Impl<Row>* p = dynamic_cast<const Linear_Expression_Impl<Row>*>(&y)) {
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
  if (const Linear_Expression_Impl<Row>* p = dynamic_cast<const Linear_Expression_Impl<Row>*>(&y)) {
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
  if (const Linear_Expression_Impl<Row>* p = dynamic_cast<const Linear_Expression_Impl<Row>*>(&y)) {
    linear_combine(*p, c1, c2, start, end);
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
  if (const Linear_Expression_Impl<Row>* p_x = dynamic_cast<const Linear_Expression_Impl<Row>*>(&x)) {
    if (const Linear_Expression_Impl<Row>* p_y = dynamic_cast<const Linear_Expression_Impl<Row>*>(&y)) {
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
  if (const Linear_Expression_Impl<Row>* p = dynamic_cast<const Linear_Expression_Impl<Row>*>(&y)) {
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
  if (const Linear_Expression_Impl<Row>* p = dynamic_cast<const Linear_Expression_Impl<Row>*>(&y)) {
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
  if (const Linear_Expression_Impl<Row>* p = dynamic_cast<const Linear_Expression_Impl<Row>*>(&y)) {
    return construct(*p, sz);
  } else {
    // Add implementations for new derived classes here.
    PPL_ASSERT(false);
  }
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Linear_Expression_Impl_templates_hh)
