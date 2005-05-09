/* Congruence_System class implementation (non-inline functions).
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

#include <config.h>

#include "Congruence_System.defs.hh"
#include "Congruence_System.inlines.hh"
#include "Congruence.defs.hh"
#include "Generator.defs.hh"
#include <cassert>
#include <string>
#include <vector>
#include <iostream>
#include <stdexcept>

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Congruence_System::normalize_moduli() {
  // FIX Add a flag to save doing this often.  Clear the flag when
  //     congruences are added, and set it on conversion.
  dimension_type row = num_rows();
  if (row) {
    // Calculate the LCM of all the moduli.
    TEMP_INTEGER(lcm);
    // Find first congruence.
    while (1) {
      lcm = operator[](--row).modulus();
      if (lcm > 0)
	break;
      if (row == 0)
	// All rows are equalities.
	return;
    }
    while (row > 0) {
      TEMP_INTEGER(modulus);
      modulus = operator[](--row).modulus();
      if (modulus > 0)
	lcm_assign(lcm, modulus);
    }

    // Represent every row using the LCM as the modulus.
    dimension_type row_size = operator[](0).size();
    for (dimension_type row = num_rows(); row-- > 0; ) {
      TEMP_INTEGER(modulus);
      modulus = operator[](row).modulus();
      if (modulus <= 0 || modulus == lcm)
	continue;
      TEMP_INTEGER(factor);
      factor = lcm / modulus;
      for (dimension_type col = row_size; col-- > 0; )
	operator[](row)[col] *= factor;
      operator[](row)[row_size-1] = lcm;
    }
  }
}

bool
PPL::Congruence_System::
adjust_space_dimension(const dimension_type new_space_dim) {
  assert(space_dimension() <= new_space_dim);

  dimension_type cols_to_add = new_space_dim - space_dimension();
  dimension_type old_num_cols = num_columns();

  if (num_rows()) {
    if (cols_to_add) {
      add_zero_columns(cols_to_add);
      // Move the moduli.
      swap_columns(num_columns() - 1, old_num_cols - 1);
    }
  }
  else
    // Empty system; possibly add modulus and constant term.
    add_zero_columns(cols_to_add + (num_columns() < 2 ? 2 : 0));

  assert(OK());
  return true;
}

void
PPL::Congruence_System::insert(const Congruence& cg) {
  const dimension_type old_num_rows = num_rows();
  const dimension_type old_num_columns = num_columns();
  const dimension_type cg_size = cg.size();

  if (cg_size > old_num_columns) {
    // Resize the system, if necessary.
    add_zero_columns(cg_size - old_num_columns);
    if (old_num_rows != 0)
      // Move the moduli to the last column.
      swap_columns(old_num_columns - 1, cg_size - 1);
    add_row(cg);
  }
  else if (cg_size < old_num_columns) {
    // Create a resized copy of `cg'.
    Congruence rc(cg, old_num_columns, row_capacity);
    // Move the modulus to its place.
    std::swap(rc[cg_size - 1], rc[old_num_columns - 1]);
    add_row(rc);
  }
  else
    // Here cg_size == old_num_columns.
    add_row(cg);

  static_cast<Congruence&>(operator[](rows.size()-1)).strong_normalize();

  assert(OK());
}

void
PPL::Congruence_System::add_rows(const Congruence_System& y) {
  Congruence_System& x = *this;
  assert(x.row_size == y.row_size);

  const dimension_type x_n_rows = x.num_rows();
  const dimension_type y_n_rows = y.num_rows();
  // Grow to the required size.
  add_zero_rows(y_n_rows, Row::Flags());

  // Copy the rows of `y', forcing size and capacity.
  for (dimension_type i = y_n_rows; i-- > 0; ) {
    Row copy(y[i], x.row_size, x.row_capacity);
    std::swap(copy, x[x_n_rows+i]);
  }
  assert(OK());
}

bool
PPL::Congruence_System::has_linear_equalities() const {
  const Congruence_System& cgs = *this;
  dimension_type modulus_index = cgs.num_columns() - 1;
  for (dimension_type i = cgs.num_rows(); i-- > 0; )
    // FIX
    // Optimized type checking: we already know the topology;
    // also, equalities have the epsilon coefficient equal to zero.
    // NOTE: the constraint eps_leq_one should not be considered
    //       a strict inequality.
    if (cgs[i][modulus_index] == 0)
      return true;
  return false;
}

void
PPL::Congruence_System::const_iterator::skip_forward() {
  const Matrix::const_iterator csp_end = csp->end();
  while (i != csp_end && (*this)->is_trivial_true())
    ++i;
}

PPL::dimension_type
PPL::Congruence_System::num_equalities() const {
#if 0 // FIX
  // We are sure that we call this method only when the matrix has no
  // pending rows.
  assert(num_pending_rows() == 0);
#endif
  const Congruence_System& cgs = *this;
  dimension_type n = 0;
#if 0 // FIX
  // If the Matrix happens to be sorted, take advantage of the fact
  // that FIX inequalities are at the bottom of the system.
  if (is_sorted())
    for (dimension_type i = num_rows(); i > 0 && cgs[--i].is_equality(); )
      ++n;
  else
#endif
    for (dimension_type i = num_rows(); i-- > 0 ; )
      if (cgs[i].is_equality())
	++n;
  return n;
}

PPL::dimension_type
PPL::Congruence_System::num_non_equalities() const {
#if 0 // FIX
  // We are sure that we call this method only when the matrix has no
  // pending rows.
  assert(num_pending_rows() == 0);
#endif
  const Congruence_System& cgs = *this;
  dimension_type n = 0;
  for (dimension_type i = num_rows(); i-- > 0 ; )
    if (cgs[i].is_equality() == false)
      ++n;
  return n;
}

// FIX saturates_every_congruence?
// FIX all/every_congruence/s_saturated_by?
// FIX saturated_by?
bool
PPL::Congruence_System::saturates_all_congruences(const Generator& g) const {
  assert(g.space_dimension() <= space_dimension());

  // Setting `sp_fp' to the appropriate scalar product operator.
  // This also avoids problems when having _legal_ topology mismatches
  // (which could also cause a mismatch in the number of columns).
  int (*sps)(const Linear_Row&, const Congruence&);
  //if (g.type() == Generator::CLOSURE_POINT) // FIX (first, and had cases swapped)
  if (g.is_necessarily_closed())
    sps = PPL::scalar_product_sign;
  else
    sps = PPL::reduced_scalar_product_sign;

  const Congruence_System& cgs = *this;
  for (dimension_type i = cgs.num_rows(); i-- > 0; )
    if (sps(g, cgs[i]) != 0)
      return false;
  return true;
}

bool
PPL::Congruence_System::satisfies_all_congruences(const Generator& g) const {
  assert(g.space_dimension() <= space_dimension());

  // Almost identical to the double-argument version below.

  // Setting `spa_fp' to the appropriate scalar product operator.
  // This also avoids problems when having _legal_ topology mismatches
  // (which could also cause a mismatch in the number of columns).
  void (*spa_fp)(Coefficient&, const Linear_Row&, const Congruence&);
  //if (g.type() == Generator::CLOSURE_POINT) // FIX (first, and had cases swapped)
  if (g.is_necessarily_closed())
    spa_fp = PPL::scalar_product_assign;
  else
    spa_fp = PPL::reduced_scalar_product_assign;

  const Congruence_System& cgs = *this;
  for (dimension_type i = cgs.num_rows(); i-- > 0; ) {
    // FIX skip virtual rows
    TEMP_INTEGER(sp);
    spa_fp(sp, g, cgs[i]);
    if (cgs[i].is_equality()) {
      // FIX a guess
      if (sp != 0) {
	std::cout << "satisfies_all_cgs... done (eq false i = " << i << ")." << std::endl;
	return false;
      }
    }
    else
      if (sp % cgs[i].modulus() != 0) {
	std::cout << "satisfies_all_cgs... done (false i = " << i << ")." << std::endl;
	return false;
      }
  }
  return true;
}

bool
PPL::Congruence_System::satisfies_all_congruences(const Generator& g,
						  const Generator& ref) const {
  assert(g.space_dimension() <= space_dimension());

  // Almost identical to the single-argument version above.

  // Setting `spa_fp' to the appropriate scalar product operator.
  // This also avoids problems when having _legal_ topology mismatches
  // (which could also cause a mismatch in the number of columns).
  void (*spa_fp)(Coefficient&, const Linear_Row&, const Congruence&,
		 const Linear_Row&);
  //if (g.type() == Generator::CLOSURE_POINT) // FIX (first, and had cases swapped)
  if (g.is_necessarily_closed())
    spa_fp = PPL::scalar_product_assign;
  else
    spa_fp = PPL::reduced_scalar_product_assign;

  const Congruence_System& cgs = *this;
  for (dimension_type i = cgs.num_rows(); i-- > 0; ) {
    // FIX is this correct?
    TEMP_INTEGER(sp);
    spa_fp(sp, g, cgs[i], ref);
    if (cgs[i].is_equality()) {
      // FIX a guess
      if (sp != 0) {
	std::cout << "satisfies_all_cgs... done (eq false i = " << i << ", sp = " << sp << ")." << std::endl;
	return false;
      }
    }
    else
      if (sp % cgs[i].modulus() != 0) {
	std::cout << "satisfies_all_cgs... done (false i = " << i << ")." << std::endl;
	return false;
      }
  }
  return true;
}

#if 0
// FIX complete
void
PPL::Congruence_System::affine_preimage(dimension_type v,
					const Linear_Expression& expr,
					Coefficient_traits::const_reference denominator) {
  // `v' is the index of a column corresponding to
  // a "user" variable (i.e., it cannot be the inhomogeneous term,
  // nor the epsilon dimension of NNC polyhedra).
  assert(v > 0 && v <= space_dimension());
  assert(expr.space_dimension() <= space_dimension());
  assert(denominator > 0);

  const dimension_type n_columns = num_columns();
  const dimension_type n_rows = num_rows();
  const dimension_type expr_size = expr.size();
  const bool not_invertible = (v >= expr_size || expr[v] == 0);
  Congruence_System& x = *this;

  if (denominator != 1)
    for (dimension_type i = n_rows; i-- > 0; ) {
      Congruence& row = x[i];
      Coefficient& row_v = row[v];
      if (row_v != 0) {
	for (dimension_type j = n_columns; j-- > 0; )
	  if (j != v) {
	    Coefficient& row_j = row[j];
	    row_j *= denominator;
	    if (j < expr_size)
	      add_mul_assign(row_j, row_v, expr[j]);
	  }
	if (not_invertible)
	  row_v = 0;
	else
	  row_v *= expr[v];
      }
    }
  else
    // Here `denominator' == 1: optimized computation
    // only considering columns having indexes < expr_size.
    for (dimension_type i = n_rows; i-- > 0; ) {
      Congruence& row = x[i];
      Coefficient& row_v = row[v];
      if (row_v != 0) {
	for (dimension_type j = expr_size; j-- > 0; )
	  if (j != v)
	    add_mul_assign(row[j], row_v, expr[j]);
	if (not_invertible)
	  row_v = 0;
	else
	  row_v *= expr[v];
      }
    }
  // Strong normalization also resets the sortedness flag.
  x.strong_normalize();
}
#endif

void
PPL::Congruence_System::ascii_dump(std::ostream& s) const {
  const Congruence_System& x = *this;
  dimension_type x_num_rows = x.num_rows();
  dimension_type x_num_columns = x.num_columns();
  s << x_num_rows << " x " << x_num_columns // << ' '
    //<< (x.is_sorted() ? "(sorted)" : "(not_sorted)")
    << std::endl
    //<< "index_first_pending " << x.first_pending_row()
    //<< std::endl
    ;
  if (x_num_rows && x_num_columns)
    for (dimension_type i = 0; i < x_num_rows; ++i)
      x[i].ascii_dump(s);
}

bool
PPL::Congruence_System::ascii_load(std::istream& s) {
  std::string str;
  dimension_type nrows;
  dimension_type ncols;
  if ((s >> nrows) == false)
    return false;
  if ((s >> str) == false) // FIX == false?
    return false;
  if ((s >> ncols) == false)
    return false;
  resize_no_copy(nrows, ncols);

#if 0
  dimension_type index;
  if (!(s >> str) || str != "index_first_pending")
    return false;
  if (!(s >> index))
    return false;
  set_index_first_pending_row(index);
#endif

  // FIX freeing on failure?

  Congruence_System& x = *this;
  for (dimension_type i = 0; i < x.num_rows(); ++i)
    x[i].ascii_load(s);

  // Check for well-formedness.
  assert(OK());
  return true;
}

bool
PPL::Congruence_System::OK() const {
  // A Congruence_System must be a valid Matrix.
  if (!Matrix::OK())
    return false;

  if (num_rows()) {
    if (num_columns() < 2)
      return false;
  }
#if 0 // FIX
  else
    if (num_columns() > 2)
      return false;
#endif

  // Checking each congruence in the system.
  const Congruence_System& x = *this;
  for (dimension_type i = num_rows(); i-- > 0; ) {
    const Congruence& cg = x[i];
    if (!cg.OK())
      return false;
    // FIX check that strong normalized
  }

  // All checks passed.
  return true;
}

/*! \relates Parma_Polyhedra_Library::Congruence_System */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s, const Congruence_System& cgs) {
  Congruence_System::const_iterator i = cgs.begin();
  const Congruence_System::const_iterator cgs_end = cgs.end();
  if (i == cgs_end)
    return s << "true";
  while (1) {
    s << *i++;
    if (i == cgs_end)
      return s;
    s << ", ";
  }
}

/*! \relates Parma_Polyhedra_Library::Congruence_System */
bool
PPL::operator==(const Congruence_System& x, const Congruence_System& y) {
  if (x.num_columns() == y.num_columns()) {
    dimension_type num_rows = x.num_rows();
    if (num_rows == y.num_rows()) {
      while (num_rows) {
	if (x[--num_rows] == y[num_rows])
	  continue;
	return false;
      }
      return true;
    }
  }
  return false;
}
