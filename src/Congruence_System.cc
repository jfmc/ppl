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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

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
    while (true) {
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
increase_space_dimension(const dimension_type new_space_dim) {
  assert(space_dimension() <= new_space_dim);

  dimension_type cols_to_add = new_space_dim - space_dimension();

  if (cols_to_add) {
    if (num_rows()) {
      dimension_type old_num_cols = num_columns();
      add_zero_columns(cols_to_add);
      // Move the moduli.
      swap_columns(num_columns() - 1, old_num_cols - 1);
    }
    else
      // Empty system; possibly add modulus and constant term.
      add_zero_columns(cols_to_add - num_columns() + 2);
  }

  assert(OK());
  return true;
}

void
PPL::Congruence_System::insert_verbatim(const Congruence& cg) {
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
}

void
PPL::Congruence_System::insert(const Congruence& cg) {
  insert_verbatim(cg);
  static_cast<Congruence&>(operator[](rows.size()-1)).strong_normalize();
  assert(OK());
}

void
PPL::Congruence_System::insert(const Constraint& c) {
  // FIX insert makes a copy of this copy (cg)
  Congruence cg(c);
  insert(cg);
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
  const Congruence_System& cgs = *this;
  dimension_type n = 0;
  for (dimension_type i = num_rows(); i-- > 0 ; )
    if (cgs[i].is_equality())
      ++n;
  return n;
}

PPL::dimension_type
PPL::Congruence_System::num_proper_congruences() const {
  const Congruence_System& cgs = *this;
  dimension_type n = 0;
  for (dimension_type i = num_rows(); i-- > 0 ; ) {
    const Congruence& cg = cgs[i];
    if (cg.is_proper_congruence())
      ++n;
  }
  return n;
}

bool
PPL::Congruence_System::
satisfies_all_congruences(const Generator& g,
			  Coefficient_traits::const_reference divisor) const {
  assert(g.space_dimension() <= space_dimension());
  assert(divisor >= 0);

  // Setting `spa_fp' to the appropriate scalar product operator.
  // This also avoids problems when having _legal_ topology mismatches
  // (which could also cause a mismatch in the number of columns).
  void (*spa_fp)(Coefficient&, const Linear_Row&, const Congruence&);
  if (g.is_necessarily_closed())
    spa_fp = PPL::scalar_product_assign;
  else
    spa_fp = PPL::reduced_scalar_product_assign;

  const Congruence_System& cgs = *this;
  for (dimension_type i = cgs.num_rows(); i-- > 0; ) {
    TEMP_INTEGER(sp);
    const Congruence& cg = cgs[i];
    spa_fp(sp, g, cg);
    if (cg.is_equality() || g.is_line()) {
      if (sp != 0)
	return false;
    }
    // FIX compare divisor before loop
    else if (sp % (divisor > 1 ? cg.modulus() * divisor : cg.modulus()) != 0)
      return false;
  }
  return true;
}

void
PPL::Congruence_System::
affine_preimage(dimension_type v,
		const Linear_Expression& expr,
		Coefficient_traits::const_reference denominator) {
  // `v' is the index of a column corresponding to a "user" variable
  // (i.e., it cannot be the inhomogeneous term).
  assert(v > 0 && v <= space_dimension());
  assert(expr.space_dimension() <= space_dimension());
  assert(denominator > 0);

  const dimension_type n_columns = num_columns();
  const dimension_type n_rows = num_rows();
  const dimension_type expr_size = expr.size();
  const bool not_invertible = (v >= expr_size || expr[v] == 0);
  Congruence_System& x = *this;

  if (denominator == 1)
    // Optimized computation only considering columns having indexes <
    // expr_size.
    for (dimension_type i = n_rows; i-- > 0; ) {
      Congruence& row = x[i];
      Coefficient& row_v = row[v];
      if (row_v != 0) {
	for (dimension_type j = expr_size; j-- > 0; )
	  if (j != v)
	    // row[j] = row[j] + row_v * expr[j]
	    add_mul_assign(row[j], row_v, expr[j]);
	if (not_invertible)
	  row_v = 0;
	else
	  row_v *= expr[v];
      }
    }
  else
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
}

void
PPL::Congruence_System::ascii_dump(std::ostream& s) const {
  const Congruence_System& x = *this;
  dimension_type x_num_rows = x.num_rows();
  dimension_type x_num_columns = x.num_columns();
  s << x_num_rows << " x " << x_num_columns
    << std::endl;
  if (x_num_rows && x_num_columns)
    for (dimension_type i = 0; i < x_num_rows; ++i)
      x[i].ascii_dump(s);
}

void
PPL::Congruence_System::ascii_dump() const {
  ascii_dump(std::cerr);
}

bool
PPL::Congruence_System::ascii_load(std::istream& s) {
  std::string str;
  dimension_type nrows;
  dimension_type ncols;
  if (!(s >> nrows))
    return false;
  if (!(s >> str))
    return false;
  if (!(s >> ncols))
    return false;
  resize_no_copy(nrows, ncols);

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
    if (num_columns() < 2) {
#ifndef NDEBUG
      std::cerr << "Congruence_System has rows and fewer than two columns."
		<< std::endl;
#endif
      return false;
    }
  }

  // Checking each congruence in the system.
  const Congruence_System& x = *this;
  for (dimension_type i = num_rows(); i-- > 0; ) {
    const Congruence& cg = x[i];
    if (!cg.OK())
      return false;
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
  while (true) {
    Congruence cg = *i++;
    cg.strong_normalize();
    s << cg;
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
      while (num_rows--) {
	if (x[num_rows] == y[num_rows])
	  continue;
	return false;
      }
      return true;
    }
  }
  return false;
}
