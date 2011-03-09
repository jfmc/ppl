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
  : sys(NECESSARILY_CLOSED, cgs.space_dimension()) {
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

  if (sys.topology() == NOT_NECESSARILY_CLOSED
      && new_topology == NECESSARILY_CLOSED) {
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
    bool was_sorted = sys.is_sorted();
    const dimension_type eps_index = space_dimension() + 1;

    // Note that num_rows() is *not* constant, because it is decreased by
    // remove_row().
    for (dimension_type i = 0; i < num_rows(); )
      if (sys[i][eps_index] != 0)
        sys.remove_row(i, false);
      else
        ++i;

    // If `cs' was sorted we sort it again.
    if (was_sorted)
      sys.sort_rows();
  }

  sys.set_topology(new_topology);
  sys.set_space_dimension(new_space_dim);

  // We successfully adjusted space dimensions and topology.
  PPL_ASSERT(OK());
  return true;
}

bool
PPL::Constraint_System::has_equalities() const {
  // We verify if the system has equalities also in the pending part.
  for (dimension_type i = sys.num_rows(); i-- > 0; )
    if (sys[i].is_equality())
      return true;
  return false;
}

bool
PPL::Constraint_System::has_strict_inequalities() const {
  if (sys.is_necessarily_closed())
    return false;
  const dimension_type eps_index = sys.num_columns() - 1;
  // We verify if the system has strict inequalities
  // also in the pending part.
  for (dimension_type i = sys.num_rows(); i-- > 0; ) {
    const Constraint& c = sys[i];
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
  PPL_ASSERT(sys.num_pending_rows() == 0);
  if (sys.topology() == c.topology())
    sys.insert(c);
  else
    // `*this' and `c' have different topologies.
    if (sys.is_necessarily_closed()) {
      sys.set_not_necessarily_closed();
      sys.insert(c);
    }
    else {
      const dimension_type new_size = 2 + std::max(c.space_dimension(),
						   space_dimension());
      Constraint tmp_c(c, new_size);
      tmp_c.set_not_necessarily_closed();
      sys.insert(tmp_c);
    }
  PPL_ASSERT(OK());
}

void
PPL::Constraint_System::insert_pending(const Constraint& c) {
  if (sys.topology() == c.topology())
    sys.insert_pending(c);
  else
    // `*this' and `c' have different topologies.
    if (sys.is_necessarily_closed()) {
      sys.set_not_necessarily_closed();
      sys.insert_pending(c);
    }
    else {
      // Here `*this' is NNC and `c' is necessarily closed.
      // Copying the constraint adding the epsilon coefficient
      // and the missing space dimensions, if any.
      const dimension_type new_size = 2 + std::max(c.space_dimension(),
						   space_dimension());
      Constraint tmp_c(c, new_size);
      tmp_c.set_not_necessarily_closed();
      sys.insert_pending(tmp_c);
    }
  PPL_ASSERT(OK());
}

PPL::dimension_type
PPL::Constraint_System::num_inequalities() const {
  // We are sure that we call this method only when
  // the matrix has no pending rows.
  PPL_ASSERT(sys.num_pending_rows() == 0);
  const Constraint_System& cs = *this;
  dimension_type n = 0;
  // If the Base happens to be sorted, take advantage of the fact
  // that inequalities are at the bottom of the system.
  if (sys.is_sorted())
    for (dimension_type i = sys.num_rows(); i > 0 && cs[--i].is_inequality(); )
      ++n;
  else
    for (dimension_type i = sys.num_rows(); i-- > 0 ; )
      if (cs[i].is_inequality())
	++n;
  return n;
}

PPL::dimension_type
PPL::Constraint_System::num_equalities() const {
  // We are sure that we call this method only when
  // the matrix has no pending rows.
  PPL_ASSERT(sys.num_pending_rows() == 0);
  return sys.num_rows() - num_inequalities();
}

void
PPL::Constraint_System_const_iterator::skip_forward() {
  const Linear_System<Constraint>::const_iterator csp_end = csp->end();
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

  if (sys.is_necessarily_closed()) {
    if (g.is_line()) {
      // Lines must saturate all constraints.
      for (dimension_type i = sys.num_rows(); i-- > 0; )
	if (sps(g, sys[i]) != 0)
	  return false;
    }
    else
      // `g' is either a ray, a point or a closure point.
      for (dimension_type i = sys.num_rows(); i-- > 0; ) {
        const Constraint& c = sys[i];
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
      for (dimension_type i = sys.num_rows(); i-- > 0; )
	if (sps(g, sys[i]) != 0)
	  return false;

      break;

    case Generator::POINT:
      // Have to perform the special test
      // when dealing with a strict inequality.
      for (dimension_type i = sys.num_rows(); i-- > 0; ) {
        const Constraint& c = sys[i];
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
      for (dimension_type i = sys.num_rows(); i-- > 0; ) {
        const Constraint& c = sys[i];
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
  // `v' is the index of a column corresponding to
  // a "user" variable (i.e., it cannot be the inhomogeneous term,
  // nor the epsilon dimension of NNC polyhedra).
  PPL_ASSERT(v > 0 && v <= sys.space_dimension());
  PPL_ASSERT(expr.space_dimension() <= sys.space_dimension());
  PPL_ASSERT(denominator > 0);

  const dimension_type n_columns = sys.num_columns();
  const dimension_type n_rows = sys.num_rows();
  const dimension_type expr_size = expr.get_linear_row().size();
  const bool not_invertible = (v >= expr_size || expr.get_linear_row()[v] == 0);

  // TODO: Check if it is correct to arrive at this point with
  // num_pending_rows() != 0.
  const dimension_type pending_index = first_pending_row();

  // Avoid triggering assertions in x.release_rows().
  sys.set_sorted(false);
  sys.unset_pending_rows();
  
  Swapping_Vector<Constraint> rows;
  // Release the rows from the linear system so they can be modified.
  sys.release_rows(rows);

  if (denominator != 1) {
    for (dimension_type i = n_rows; i-- > 0; ) {
      Constraint& row = rows[i];
      Coefficient& row_v = row[v];
      if (row_v != 0) {
	for (dimension_type j = n_columns; j-- > 0; )
	  if (j != v) {
	    Coefficient& row_j = row[j];
	    row_j *= denominator;
	    if (j < expr_size)
	      add_mul_assign(row_j, row_v, expr.get_linear_row()[j]);
	  }
	if (not_invertible)
	  row_v = 0;
	else
	  row_v *= expr.get_linear_row()[v];
      }
    }
  } else {
    // Here `denominator' == 1: optimized computation
    // only considering columns having indexes < expr_size.
    for (dimension_type i = n_rows; i-- > 0; ) {
      Constraint& row = rows[i];
      Coefficient& row_v = row[v];
      if (row_v != 0) {
	for (dimension_type j = expr_size; j-- > 0; )
	  if (j != v)
	    add_mul_assign(row[j], row_v, expr.get_linear_row()[j]);
	if (not_invertible)
	  row_v = 0;
	else
	  row_v *= expr.get_linear_row()[v];
      }
    }
  }

  // Put the rows back in the Linear_System.
  sys.take_ownership_of_rows(rows);
  sys.set_index_first_pending_row(pending_index);

  // Strong normalization also resets the sortedness flag.
  sys.strong_normalize();
}

void
PPL::Constraint_System::ascii_dump(std::ostream& s) const {
  const dimension_type x_num_rows = sys.num_rows();
  const dimension_type x_num_columns = sys.num_columns();
  s << "topology " << (sys.is_necessarily_closed()
		       ? "NECESSARILY_CLOSED"
		       : "NOT_NECESSARILY_CLOSED")
    << "\n"
    << x_num_rows << " x " << x_num_columns << ' '
    << (sys.is_sorted() ? "(sorted)" : "(not_sorted)")
    << "\n"
    << "index_first_pending " << sys.first_pending_row()
    << "\n";

  for (dimension_type i = 0; i < x_num_rows; ++i)
    sys[i].ascii_dump(s);
}

PPL_OUTPUT_DEFINITIONS(Constraint_System)

bool
PPL::Constraint_System::ascii_load(std::istream& s) {
  std::string str;
  std::string str2;
  if (!(s >> str) || str != "topology")
    return false;
  if (!(s >> str))
    return false;

  sys.clear();

  if (str == "NECESSARILY_CLOSED")
    sys.set_necessarily_closed();
  else {
    if (str != "NOT_NECESSARILY_CLOSED")
      return false;
    sys.set_not_necessarily_closed();
  }

  dimension_type nrows;
  dimension_type ncols;
  if (!(s >> nrows))
    return false;
  if (!(s >> str) || str != "x")
    return false;
  if (!(s >> ncols))
      return false;

  if (sys.topology() == NECESSARILY_CLOSED) {
    PPL_ASSERT(ncols >= 1);
    sys.set_space_dimension(ncols - 1);
  } else {
    PPL_ASSERT(ncols >= 2);
    sys.set_space_dimension(ncols - 2);
  }

  if (!(s >> str) || (str != "(sorted)" && str != "(not_sorted)"))
    return false;

  // Set sortedness later, so insert_pending_recycled() will have no effect on
  // it.
  bool sorted = (str == "(sorted)");
  set_sorted(false);
  dimension_type pending_index;
  if (!(s >> str) || str != "index_first_pending")
    return false;
  if (!(s >> pending_index))
    return false;

  for (dimension_type i = 0; i < nrows; ++i) {
    Constraint c;
    if (!c.ascii_load(s))
      return false;

    sys.insert_pending_recycled(c);
  }
  sys.set_index_first_pending_row(pending_index);
  sys.set_sorted(sorted);
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
