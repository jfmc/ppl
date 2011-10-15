/* Linear_Expression_Impl class implementation (non-inline functions).
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


#include <ppl-config.h>

#include "Linear_Expression_Impl.defs.hh"
#include "Constraint.defs.hh"
#include "Generator.defs.hh"
#include "Grid_Generator.defs.hh"
#include "Congruence.defs.hh"
#include <deque>
#include <stdexcept>
#include <iostream>


namespace PPL = Parma_Polyhedra_Library;

int
PPL::Linear_Expression_Impl::compare(const Linear_Expression_Impl& y) const {
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

PPL::Linear_Expression_Impl::Linear_Expression_Impl(const Constraint& c)
  : row(c.expression().impl->row) {
  // Do not copy the epsilon dimension (if any).
  if (c.is_not_necessarily_closed())
    row.resize(row.size() - 1);
  PPL_ASSERT(OK());
}

PPL::Linear_Expression_Impl::Linear_Expression_Impl(const Generator& g)
  : row(g.expression().impl->row) {
  // Do not copy the divisor of `g'.
  row[0] = 0;
  // Do not copy the epsilon dimension (if any).
  if (g.is_not_necessarily_closed())
    row.resize(row.size() - 1);
  PPL_ASSERT(OK());
}

PPL::Linear_Expression_Impl::Linear_Expression_Impl(const Grid_Generator& g)
  : row(g.expression().impl->row) {
  // Do not copy the divisor of `g'.
  row[0] = 0;
  // Do not copy the epsilon dimension (if any).
  if (g.is_not_necessarily_closed())
    row.resize(row.size() - 1);
  PPL_ASSERT(OK());
}

PPL::Linear_Expression_Impl::Linear_Expression_Impl(const Congruence& cg)
  : row(cg.space_dimension() + 1) {
  for (dimension_type i = row.size() - 1; i-- > 0; )
    row[i + 1] = cg.coefficient(Variable(i));
  row[0] = cg.inhomogeneous_term();
  PPL_ASSERT(OK());
}

PPL::Linear_Expression_Impl::Linear_Expression_Impl(const Variable v)
  : row(v.space_dimension() <= max_space_dimension()
	       ? v.space_dimension() + 1
	       : (throw std::length_error("PPL::Linear_Expression_Impl::"
					  "Linear_Expression_Impl(v):\n"
					  "v exceeds the maximum allowed "
					  "space dimension."),
		  v.space_dimension() + 1)) {
  ++(row[v.space_dimension()]);
  PPL_ASSERT(OK());
}

bool
PPL::Linear_Expression_Impl::is_equal_to(const Linear_Expression_Impl& x) const {
  return row == x.row;
}

void
PPL::Linear_Expression_Impl::remove_space_dimensions(const Variables_Set& vars) {
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

void
PPL::Linear_Expression_Impl::permute_space_dimensions(const std::vector<Variable>& cycle) {
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

PPL::Linear_Expression_Impl&
PPL::Linear_Expression_Impl::operator+=(const Linear_Expression_Impl& e2) {
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
PPL::Linear_Expression_Impl&
PPL::Linear_Expression_Impl::operator+=(const Variable v) {
  const dimension_type v_space_dim = v.space_dimension();
  if (v_space_dim > Linear_Expression_Impl::max_space_dimension())
    throw std::length_error("Linear_Expression_Impl& "
                            "PPL::operator+=(e, v):\n"
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
PPL::Linear_Expression_Impl&
PPL::Linear_Expression_Impl::operator-=(const Linear_Expression_Impl& e2) {
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
PPL::Linear_Expression_Impl&
PPL::Linear_Expression_Impl::operator-=(const Variable v) {
  const dimension_type v_space_dim = v.space_dimension();
  if (v_space_dim > Linear_Expression_Impl::max_space_dimension())
    throw std::length_error("Linear_Expression_Impl& "
                            "PPL::operator-=(e, v):\n"
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
PPL::Linear_Expression_Impl&
PPL::Linear_Expression_Impl::operator*=(Coefficient_traits::const_reference n) {
  for (dimension_type i = row.size(); i-- > 0; )
    row[i] *= n;
  return *this;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression_Impl */
void
PPL::Linear_Expression_Impl::negate() {
  for (dimension_type i = row.size(); i-- > 0; )
    neg_assign(row[i]);
  PPL_ASSERT(OK());
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression_Impl */
PPL::Linear_Expression_Impl&
PPL::Linear_Expression_Impl::add_mul_assign(Coefficient_traits::const_reference n,
                                            const Variable v) {
  const dimension_type v_space_dim = v.space_dimension();
  if (v_space_dim > Linear_Expression_Impl::max_space_dimension())
    throw std::length_error("Linear_Expression_Impl& "
                            "PPL::add_mul_assign(e, n, v):\n"
			    "v exceeds the maximum allowed space dimension.");
  if (row.size() <= v_space_dim) {
    Linear_Expression_Impl new_e(*this, v_space_dim+1);
    swap(new_e);
  }
  row[v_space_dim] += n;
  return *this;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression_Impl */
PPL::Linear_Expression_Impl&
PPL::Linear_Expression_Impl::sub_mul_assign(Coefficient_traits::const_reference n,
                                            const Variable v) {
  const dimension_type v_space_dim = v.space_dimension();
  if (v_space_dim > Linear_Expression_Impl::max_space_dimension())
    throw std::length_error("Linear_Expression_Impl& "
                            "PPL::sub_mul_assign(e, n, v):\n"
			    "v exceeds the maximum allowed space dimension.");
  if (row.size() <= v_space_dim) {
    Linear_Expression_Impl new_e(*this, v_space_dim+1);
    swap(new_e);
  }
  row[v_space_dim] -= n;
  return *this;
}

PPL::Linear_Expression_Impl&
PPL::Linear_Expression_Impl::sub_mul_assign(Coefficient_traits::const_reference n,
                                            const Linear_Expression_Impl& y,
                                            dimension_type start, dimension_type end) {
  for (dimension_type i = start; i < end; i++)
    row[i] -= n*y.row[i];
  return *this;
}

void
PPL::Linear_Expression_Impl::add_mul_assign(Coefficient_traits::const_reference factor,
                                            const Linear_Expression_Impl& e2) {
  Linear_Expression_Impl& e1 = *this;
  if (e2.space_dimension() > e1.space_dimension())
    e1.set_space_dimension(e2.space_dimension());
  PPL_ASSERT(e1.space_dimension() >= e2.space_dimension());
  for (dimension_type i = 0; i < e2.space_dimension(); i++)
    e1[i] += factor * e2[i];
}

void
PPL::Linear_Expression_Impl::sub_mul_assign(Coefficient_traits::const_reference factor,
                                            const Linear_Expression_Impl& e2) {
  Linear_Expression_Impl& e1 = *this;
  if (e2.space_dimension() > e1.space_dimension())
    e1.set_space_dimension(e2.space_dimension());
  PPL_ASSERT(e1.space_dimension() >= e2.space_dimension());
  for (dimension_type i = 0; i < e2.space_dimension(); i++)
    e1[i] -= factor * e2[i];
}

bool
PPL::Linear_Expression_Impl::OK() const {
  return row.size() != 0;
}

/*! \relates Parma_Polyhedra_Library::Linear_Expression_Impl */
std::ostream&
PPL::Linear_Expression_Impl::operator<<(std::ostream& s) const {
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
      PPL::IO_Operators::operator<<(s, Variable(v));
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

PPL::Coefficient&
PPL::Linear_Expression_Impl::operator[](dimension_type i) {
  return row[i];
}

const PPL::Coefficient&
PPL::Linear_Expression_Impl::operator[](dimension_type i) const {
  return row[i];
}

const PPL::Coefficient&
PPL::Linear_Expression_Impl::get(dimension_type i) const {
  return row.get(i);
}

bool
PPL::Linear_Expression_Impl::all_zeroes(dimension_type start, dimension_type end) const {
  for (Dense_Row::const_iterator i = row.lower_bound(start), i_end = row.lower_bound(end);
       i != i_end; ++i)
    if (*i != 0)
      return false;
  return true;
}

PPL::Coefficient
PPL::Linear_Expression_Impl::gcd(dimension_type start, dimension_type end) const {
  Dense_Row::const_iterator i = row.lower_bound(start);
  Dense_Row::const_iterator i_end = row.lower_bound(end);

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

void
PPL::Linear_Expression_Impl
::exact_div_assign(Coefficient_traits::const_reference c,
                   dimension_type start, dimension_type end) {
  for (dimension_type i = start; i < end; ++i) {
    Coefficient& x = row[i];
    PPL::exact_div_assign(x, x, c);
  }
}

void
PPL::Linear_Expression_Impl::linear_combine(const Linear_Expression_Impl& y,
                                       Coefficient_traits::const_reference c1,
                                       Coefficient_traits::const_reference c2,
                                       dimension_type start,
                                       dimension_type end) {
  row.linear_combine(y.row, c1, c2, start, end);
}

void
PPL::Linear_Expression_Impl::sign_normalize() {
  Dense_Row::iterator i = row.lower_bound(1);
  Dense_Row::iterator i_end = row.end();
  
  for ( ; i != i_end; ++i)
    if (*i != 0)
      break;
  
  if (i != i_end && *i < 0) {
    for ( ; i != i_end; ++i)
      neg_assign(*i);
    // Negate the first coefficient, too.
    Dense_Row::iterator i = row.begin();
    if (i != row.end() && i.index() == 0)
      neg_assign(*i);
  }
}

void
PPL::Linear_Expression_Impl::negate(dimension_type first, dimension_type last) {
  Dense_Row::iterator i = row.lower_bound(first);
  Dense_Row::iterator i_end = row.lower_bound(last);
  for ( ; i != i_end; ++i)
    neg_assign(*i);
}

bool
PPL::Linear_Expression_Impl::all_zeroes(const Variables_Set& vars) const {
  Dense_Row::const_iterator i = row.begin();
  Dense_Row::const_iterator i_end = row.end();
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

void
PPL::Linear_Expression_Impl
::modify_according_to_evolution(const Linear_Expression_Impl& x,
                                const Linear_Expression_Impl& y) {
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
  
  Dense_Row::const_iterator x_end = x.row.end();
  Dense_Row::const_iterator y_end = y.row.end();
  Dense_Row::const_iterator y_k = y.row.end();
  for (Dense_Row::const_iterator x_k = x.row.begin(); x_k != x_end; ++x_k) {
    const dimension_type k = x_k.index();
    if (considered[k])
      continue;

    y_k = y.row.lower_bound(y_k, k);

    if (y_k == y.row.end())
      break;

    // Note that y_k.index() may not be k.

    Dense_Row::const_iterator y_h = y_k;

    Dense_Row::const_iterator x_h = x_k;
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
        PPL::sub_mul_assign(tmp, *x_h, *y_k);
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

PPL::dimension_type
PPL::Linear_Expression_Impl::last_nonzero() const {
  Dense_Row::const_iterator i = row.begin();
  Dense_Row::const_iterator i_end = row.end();

  while (1) {
    if (i == i_end)
      return 0;
    --i_end;
    if (*i_end != 0)
      return i_end.index();
  }
}

PPL_OUTPUT_DEFINITIONS(Linear_Expression_Impl)
