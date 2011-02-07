/* Constraint_System class implementation (non-inline functions).
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

#include "Constraint_System.defs.hh"
#include "Constraint_System.inlines.hh"
#include "Generator.defs.hh"
#include "Scalar_Products.defs.hh"
#include "Scalar_Products.inlines.hh"
#include "Congruence_System.defs.hh"
#include "Congruence_System.inlines.hh"
#include "assert.hh"
#include <string>
#include <vector>
#include <iostream>
#include <stdexcept>

namespace PPL = Parma_Polyhedra_Library;

PPL::Constraint_System::Constraint_System(const Congruence_System& cgs)
  : Base(NECESSARILY_CLOSED, 0, cgs.space_dimension() + 1) {
  for (Congruence_System::const_iterator i = cgs.begin(),
	 cgs_end = cgs.end(); i != cgs_end; ++i)
    if (i->is_equality())
      // TODO: Consider adding a recycling_insert to save the extra copy here.
      insert(Constraint(*i));
}

bool
PPL::Constraint_System::
adjust_topology_and_space_dimension(const Topology new_topology,
				    const dimension_type new_space_dim) {
  PPL_ASSERT(space_dimension() <= new_space_dim);

  const dimension_type old_space_dim = space_dimension();
  const Topology old_topology = topology();
  dimension_type cols_to_be_added = new_space_dim - old_space_dim;

  // Dealing with empty constraint systems first.
  if (num_rows() == 0) {
    if (num_columns() == 0)
      if (new_topology == NECESSARILY_CLOSED) {
	add_zero_columns(++cols_to_be_added);
	set_necessarily_closed();
      }
      else {
	cols_to_be_added += 2;
	add_zero_columns(cols_to_be_added);
	set_not_necessarily_closed();
      }
    else
      // Here `num_columns() > 0'.
      if (old_topology != new_topology)
	if (new_topology == NECESSARILY_CLOSED) {
	  switch (cols_to_be_added) {
	  case 0:
	    remove_trailing_columns(1);
	    break;
	  case 1:
	    // Nothing to do.
	    break;
	  default:
	    add_zero_columns(--cols_to_be_added);
	  }
	  set_necessarily_closed();
	}
	else {
	  // Here old_topology == NECESSARILY_CLOSED
	  //  and new_topology == NOT_NECESSARILY_CLOSED.
	  add_zero_columns(++cols_to_be_added);
	  set_not_necessarily_closed();
	}
      else {
	// Here topologies agree.
	if (cols_to_be_added > 0)
	  add_zero_columns(cols_to_be_added);
      }
    PPL_ASSERT(OK());
    return true;
  }

  // Here the constraint system is not empty.
  if (cols_to_be_added > 0)
    if (old_topology != new_topology)
      if (new_topology == NECESSARILY_CLOSED) {
	// A NOT_NECESSARILY_CLOSED constraint system
	// can be converted to a NECESSARILY_CLOSED one
	// only if it does not contain strict inequalities.
	if (has_strict_inequalities())
	  return false;
	// Since there were no strict inequalities,
	// the only constraints that may have a non-zero epsilon coefficient
	// are the eps-leq-one and the eps-geq-zero constraints.
	// If they are present, we erase these rows, so that the
	// epsilon column will only contain zeroes: as a consequence,
	// we just decrement the number of columns to be added.
	Constraint_System& cs = *this;
	const dimension_type eps_index = old_space_dim + 1;
  const dimension_type old_cs_num_rows = cs.num_rows();
  dimension_type cs_num_rows = old_cs_num_rows;
	bool was_sorted = cs.is_sorted();
	if (was_sorted)
	  cs.set_sorted(false);

	// If we have no pending rows, we only check if
	// we must erase some rows.
	if (cs.num_pending_rows() == 0) {
	  for (dimension_type i = cs_num_rows; i-- > 0; )
	    if (cs[i][eps_index] != 0) {
	      --cs_num_rows;
              cs.swap_rows(i, cs_num_rows);
	    }
	  cs.remove_trailing_rows(old_cs_num_rows - cs_num_rows);
	  cs.unset_pending_rows();
	}
	else {
	  // There are pending rows, and we cannot swap them
	  // into the non-pending part of the matrix.
	  // Thus, we first work on the non-pending part as if it was
	  // an independent matrix; then we work on the pending part.
	  const dimension_type old_first_pending = cs.first_pending_row();
	  dimension_type new_first_pending = old_first_pending;
	  for (dimension_type i = new_first_pending; i-- > 0; )
	    if (cs[i][eps_index] != 0) {
	      --new_first_pending;
              cs.swap_rows(i, new_first_pending);
	    }
	  const dimension_type num_swaps
	    = old_first_pending - new_first_pending;
          cs.set_index_first_pending_row(new_first_pending);
	  // Move the swapped rows to the real end of the matrix.
	  for (dimension_type i = num_swaps; i-- > 0; )
            cs.swap_rows(old_first_pending - i, cs_num_rows - i);
	  cs_num_rows -= num_swaps;
	  // Now iterate through the pending rows.
	  for (dimension_type i = cs_num_rows; i-- > new_first_pending; )
	    if (cs[i][eps_index] != 0) {
	      --cs_num_rows;
              cs.swap_rows(i, cs_num_rows);
	    }
	  cs.remove_trailing_rows(old_cs_num_rows - cs_num_rows);
	}

	// If `cs' was sorted we sort it again.
	if (was_sorted)
	  cs.sort_rows();
	if (--cols_to_be_added > 0)
	  add_zero_columns(cols_to_be_added);
	set_necessarily_closed();
      }
      else {
	// A NECESSARILY_CLOSED constraint system is converted to
	// a NOT_NECESSARILY_CLOSED one by adding a further column
	// of zeroes for the epsilon coefficients.
	add_zero_columns(++cols_to_be_added);
	set_not_necessarily_closed();
      }
    else {
      // Topologies agree: first add the required zero columns ...
      add_zero_columns(cols_to_be_added);
      // ... and, if needed, move the epsilon coefficients
      // to the new last column.
      if (old_topology == NOT_NECESSARILY_CLOSED)
	swap_columns(old_space_dim + 1, new_space_dim + 1);
    }
  else
    // Here `cols_to_be_added == 0'.
    if (old_topology != new_topology) {
      if (new_topology == NECESSARILY_CLOSED) {
	// A NOT_NECESSARILY_CLOSED constraint system
	// can be converted to a NECESSARILY_CLOSED one
	// only if it does not contain strict inequalities.
	if (has_strict_inequalities())
	  return false;
	// We just remove the column of the epsilon coefficients.
	remove_trailing_columns(1);
	set_necessarily_closed();
      }
      else {
	// We just add the column of the epsilon coefficients.
	add_zero_columns(1);
	set_not_necessarily_closed();
      }
    }
  // We successfully adjusted space dimensions and topology.
  PPL_ASSERT(OK());
  return true;
}

bool
PPL::Constraint_System::has_equalities() const {
  const Constraint_System& cs = *this;
  // We verify if the system has equalities also in the pending part.
  for (dimension_type i = cs.num_rows(); i-- > 0; )
    if (cs[i].is_equality())
      return true;
  return false;
}

bool
PPL::Constraint_System::has_strict_inequalities() const {
  if (is_necessarily_closed())
    return false;
  const Constraint_System& cs = *this;
  const dimension_type eps_index = cs.num_columns() - 1;
  // We verify if the system has strict inequalities
  // also in the pending part.
  for (dimension_type i = cs.num_rows(); i-- > 0; ) {
    const Constraint& c = cs[i];
    // Optimized type checking: we already know the topology;
    // also, equalities have the epsilon coefficient equal to zero.
    // NOTE: the constraint eps_leq_one should not be considered
    //       a strict inequality.
    if (c[eps_index] < 0 && !c.is_tautological())
      return true;
  }
  return false;
}

void
PPL::Constraint_System::insert(const Constraint& c) {
  // We are sure that the matrix has no pending rows
  // and that the new row is not a pending constraint.
  PPL_ASSERT(num_pending_rows() == 0);
  if (topology() == c.topology())
    Base::insert(c);
  else
    // `*this' and `c' have different topologies.
    if (is_necessarily_closed()) {
      // Padding the matrix with a columns of zeroes
      // corresponding to the epsilon coefficients.
      add_zero_columns(1);
      set_not_necessarily_closed();
      Base::insert(c);
    }
    else {
      // Here `*this' is NNC and `c' is necessarily closed.
      // Copying the constraint adding the epsilon coefficient
      // and the missing space dimensions, if any.
      // FIXME: provide a resizing copy constructor taking
      // topology and the space dimension.
      const dimension_type new_size = 2 + std::max(c.space_dimension(),
						   space_dimension());
      Constraint tmp_c(c, new_size);
      tmp_c.set_not_necessarily_closed();
      Base::insert(tmp_c);
    }
  PPL_ASSERT(OK());
}

void
PPL::Constraint_System::insert_pending(const Constraint& c) {
  if (topology() == c.topology())
    Base::insert_pending(c);
  else
    // `*this' and `c' have different topologies.
    if (is_necessarily_closed()) {
      // Padding the matrix with a columns of zeroes
      // corresponding to the epsilon coefficients.
      add_zero_columns(1);
      set_not_necessarily_closed();
      Base::insert_pending(c);
    }
    else {
      // Here `*this' is NNC and `c' is necessarily closed.
      // Copying the constraint adding the epsilon coefficient
      // and the missing space dimensions, if any.
      const dimension_type new_size = 2 + std::max(c.space_dimension(),
						   space_dimension());
      Constraint tmp_c(c, new_size);
      tmp_c.set_not_necessarily_closed();
      Base::insert_pending(tmp_c);
    }
  PPL_ASSERT(OK());
}

PPL::dimension_type
PPL::Constraint_System::num_inequalities() const {
  // We are sure that we call this method only when
  // the matrix has no pending rows.
  PPL_ASSERT(num_pending_rows() == 0);
  const Constraint_System& cs = *this;
  dimension_type n = 0;
  // If the Base happens to be sorted, take advantage of the fact
  // that inequalities are at the bottom of the system.
  if (is_sorted())
    for (dimension_type i = num_rows(); i > 0 && cs[--i].is_inequality(); )
      ++n;
  else
    for (dimension_type i = num_rows(); i-- > 0 ; )
      if (cs[i].is_inequality())
	++n;
  return n;
}

PPL::dimension_type
PPL::Constraint_System::num_equalities() const {
  // We are sure that we call this method only when
  // the matrix has no pending rows.
  PPL_ASSERT(num_pending_rows() == 0);
  return num_rows() - num_inequalities();
}

void
PPL::Constraint_System_const_iterator::skip_forward() {
  const Constraint_System::Base::const_iterator csp_end = csp->end();
  while (i != csp_end && (*this)->is_tautological())
    ++i;
}

bool
PPL::Constraint_System::satisfies_all_constraints(const Generator& g) const {
  PPL_ASSERT(g.space_dimension() <= space_dimension());

  // Setting `sps' to the appropriate scalar product sign operator.
  // This also avoids problems when having _legal_ topology mismatches
  // (which could also cause a mismatch in the number of columns).
  Topology_Adjusted_Scalar_Product_Sign sps(g);

  const Constraint_System& cs = *this;
  if (cs.is_necessarily_closed()) {
    if (g.is_line()) {
      // Lines must saturate all constraints.
      for (dimension_type i = cs.num_rows(); i-- > 0; )
	if (sps(g, cs[i]) != 0)
	  return false;
    }
    else
      // `g' is either a ray, a point or a closure point.
      for (dimension_type i = cs.num_rows(); i-- > 0; ) {
	const Constraint& c = cs[i];
	const int sp_sign = sps(g, c);
	if (c.is_inequality()) {
	  // As `cs' is necessarily closed,
	  // `c' is a non-strict inequality.
	  if (sp_sign < 0)
	    return false;
	}
	else
	  // `c' is an equality.
	  if (sp_sign != 0)
	    return false;
      }
  }
  else
    // `cs' is not necessarily closed.
    switch (g.type()) {

    case Generator::LINE:
      // Lines must saturate all constraints.
      for (dimension_type i = cs.num_rows(); i-- > 0; )
	if (sps(g, cs[i]) != 0)
	  return false;
      break;

    case Generator::POINT:
      // Have to perform the special test
      // when dealing with a strict inequality.
      for (dimension_type i = cs.num_rows(); i-- > 0; ) {
	const Constraint& c = cs[i];
	const int sp_sign = sps(g, c);
	switch (c.type()) {
	case Constraint::EQUALITY:
	  if (sp_sign != 0)
	    return false;
	  break;
	case Constraint::NONSTRICT_INEQUALITY:
	  if (sp_sign < 0)
	    return false;
	  break;
	case Constraint::STRICT_INEQUALITY:
	  if (sp_sign <= 0)
	    return false;
	  break;
	}
      }
      break;

    case Generator::RAY:
      // Intentionally fall through.
    case Generator::CLOSURE_POINT:
      for (dimension_type i = cs.num_rows(); i-- > 0; ) {
	const Constraint& c = cs[i];
	const int sp_sign = sps(g, c);
	if (c.is_inequality()) {
	  // Constraint `c' is either a strict or a non-strict inequality.
	  if (sp_sign < 0)
	    return false;
	}
	else
	  // Constraint `c' is an equality.
	  if (sp_sign != 0)
	    return false;
      }
      break;
    }

  // If we reach this point, `g' satisfies all constraints.
  return true;
}


void
PPL::Constraint_System
::affine_preimage(const dimension_type v,
		  const Linear_Expression& expr,
		  Coefficient_traits::const_reference denominator) {
  Constraint_System& x = *this;
  // `v' is the index of a column corresponding to
  // a "user" variable (i.e., it cannot be the inhomogeneous term,
  // nor the epsilon dimension of NNC polyhedra).
  PPL_ASSERT(v > 0 && v <= x.space_dimension());
  PPL_ASSERT(expr.space_dimension() <= x.space_dimension());
  PPL_ASSERT(denominator > 0);

  const dimension_type n_columns = x.num_columns();
  const dimension_type n_rows = x.num_rows();
  const dimension_type expr_size = expr.size();
  const bool not_invertible = (v >= expr_size || expr[v] == 0);

  // TODO: Check if it is correct to arrive at this point with
  // num_pending_rows() != 0.
  const dimension_type pending_index = first_pending_row();

  // Avoid triggering assertions in x.release_rows().
  x.unset_pending_rows();
  
  Swapping_Vector<Linear_Row> rows;
  // Release the rows from the linear system so they can be modified.
  x.release_rows(rows);

  if (denominator != 1) {
    for (dimension_type i = n_rows; i-- > 0; ) {
      Linear_Row& row = rows[i];
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
  } else {
    // Here `denominator' == 1: optimized computation
    // only considering columns having indexes < expr_size.
    for (dimension_type i = n_rows; i-- > 0; ) {
      Linear_Row& row = rows[i];
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
  }

  // Put the rows back in the Linear_System.
  x.take_ownership_of_rows(rows);
  x.set_index_first_pending_row(pending_index);

  // Strong normalization also resets the sortedness flag.
  x.strong_normalize();
}

void
PPL::Constraint_System::ascii_dump(std::ostream& s) const {
  const Constraint_System& x = *this;
  const dimension_type x_num_rows = x.num_rows();
  const dimension_type x_num_columns = x.num_columns();
  s << "topology " << (is_necessarily_closed()
		       ? "NECESSARILY_CLOSED"
		       : "NOT_NECESSARILY_CLOSED")
    << "\n"
    << x_num_rows << " x " << x_num_columns << ' '
    << (x.is_sorted() ? "(sorted)" : "(not_sorted)")
    << "\n"
    << "index_first_pending " << x.first_pending_row()
    << "\n";
  for (dimension_type i = 0; i < x_num_rows; ++i) {
    const Constraint& c = x[i];
    for (dimension_type j = 0; j < x_num_columns; ++j)
      s << c[j] << ' ';
    switch (c.type()) {
    case Constraint::EQUALITY:
      s << "=";
      break;
    case Constraint::NONSTRICT_INEQUALITY:
      s << ">=";
      break;
    case Constraint::STRICT_INEQUALITY:
      s << ">";
      break;
    }
    s << "\n";
  }
}

PPL_OUTPUT_DEFINITIONS(Constraint_System)

bool
PPL::Constraint_System::ascii_load(std::istream& s) {
  std::string str;
  if (!(s >> str) || str != "topology")
    return false;
  if (!(s >> str))
    return false;
  if (str == "NECESSARILY_CLOSED")
    set_necessarily_closed();
  else {
    if (str != "NOT_NECESSARILY_CLOSED")
      return false;
    set_not_necessarily_closed();
  }

  dimension_type nrows;
  dimension_type ncols;
  if (!(s >> nrows))
    return false;
  if (!(s >> str) || str != "x")
    return false;
  if (!(s >> ncols))
      return false;
  resize_no_copy(0, ncols);

  if (!(s >> str) || (str != "(sorted)" && str != "(not_sorted)"))
    return false;
  set_sorted(str == "(sorted)");
  dimension_type pending_index;
  if (!(s >> str) || str != "index_first_pending")
    return false;
  if (!(s >> pending_index))
    return false;

  Constraint_System& x = *this;
  for (dimension_type i = 0; i < nrows; ++i) {
    Constraint row(ncols);
    for (dimension_type j = 0; j < x.num_columns(); ++j)
      if (!(s >> row[j]))
	return false;

    if (!(s >> str))
      return false;
    if (str == "=")
      row.set_is_equality();
    else if (str == ">=" || str == ">")
      row.set_is_inequality();
    else
      return false;

    row.set_topology(topology());

    // Checking for equality of actual and declared types.
    switch (row.type()) {
    case Constraint::EQUALITY:
      if (str != "=")
	return false;
      break;
    case Constraint::NONSTRICT_INEQUALITY:
      if (str != ">=")
	return false;
      break;
    case Constraint::STRICT_INEQUALITY:
      if (str != ">")
	return false;
      break;
    }
    insert_pending_recycled(row);
  }
  set_index_first_pending_row(pending_index);
  // Check invariants.
  PPL_ASSERT(OK());
  return true;
}

const PPL::Constraint_System* PPL::Constraint_System::zero_dim_empty_p = 0;

void
PPL::Constraint_System::initialize() {
  PPL_ASSERT(zero_dim_empty_p == 0);
  zero_dim_empty_p
    = new Constraint_System(Constraint::zero_dim_false());
}

void
PPL::Constraint_System::finalize() {
  PPL_ASSERT(zero_dim_empty_p != 0);
  delete zero_dim_empty_p;
  zero_dim_empty_p = 0;
}

bool
PPL::Constraint_System::OK() const {
  // A Constraint_System must be a valid Linear_System; do not check for
  // strong normalization, since this will be done when
  // checking each individual constraint.
  if (!Base::OK(false))
    return false;

  // Checking each constraint in the system.
  const Constraint_System& x = *this;
  for (dimension_type i = num_rows(); i-- > 0; )
    if (!x[i].OK())
      return false;

  // All checks passed.
  return true;
}

/*! \relates Parma_Polyhedra_Library::Constraint_System */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s, const Constraint_System& cs) {
  Constraint_System_const_iterator i = cs.begin();
  const Constraint_System_const_iterator cs_end = cs.end();
  if (i == cs_end)
    s << "true";
  else {
    while (i != cs_end) {
      s << *i++;
      if (i != cs_end)
	s << ", ";
    }
  }
  return s;
}
