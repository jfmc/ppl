/* Polyhedron class implementation (non-inline functions).
   Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "Polyhedron.defs.hh"

#include "Generator.defs.hh"
#include "Constraint.defs.hh"
#include <cassert>
#include <string>
#include <iostream>
#include <sstream>
#include <stdexcept>

#define BE_LAZY

namespace PPL = Parma_Polyhedra_Library;

/*!
  Updates the constraints as necessary, then returns a constant
  reference to the system of constraints.
*/
const PPL::ConSys&
PPL::Polyhedron::constraints() const {

  if (is_empty()) {
    assert(con_sys.num_columns() == 0 && con_sys.num_rows() == 0);
    // FIXME: does the user want an inconsistent constraint
    // of the actual space dimension ?
    return ConSys::zero_dim_empty();
  }

  if (space_dimension() == 0) {
    // Zero-dim universe.
    assert(con_sys.num_columns() == 0 && con_sys.num_rows() == 0);
    return con_sys;
  }

  if (!constraints_are_up_to_date())
    update_constraints();

  // We insist in returning a sorted system of constraints.
  if (!con_sys.is_sorted()) {
    if (sat_c_is_up_to_date()) {
      const_cast<Polyhedron&>(*this).obtain_sorted_constraints_with_sat_c();
      if (sat_g_is_up_to_date())
	const_cast<SatMatrix&>(sat_g).transpose_assign(sat_c);
    }
    else {
      const_cast<ConSys&>(con_sys).sort_rows();
      if (sat_g_is_up_to_date()) {
#ifndef NDEBUG
	const_cast<Polyhedron&>(*this).clear_sat_g_up_to_date();
#endif
	const_cast<Polyhedron&>(*this).update_sat_g();
      }
    }
  }
  return con_sys;
}


/*!
  Updates the generators as necessary, then returns a constant
  reference to the system of generators.
*/
const PPL::GenSys&
PPL::Polyhedron::generators() const {

  if (is_empty()) {
    assert(gen_sys.num_columns() == 0 && gen_sys.num_rows() == 0);
    return gen_sys;
  }

  if (space_dimension() == 0) {
    assert(gen_sys.num_columns() == 0 && gen_sys.num_rows() == 0);
    return GenSys::zero_dim_univ();
  }

  if (!generators_are_up_to_date())
    update_generators();

  // We insist in returning a sorted system of generators.
  if (!gen_sys.is_sorted()) {
    if (sat_g_is_up_to_date()) {
      const_cast<Polyhedron&>(*this).obtain_sorted_generators_with_sat_g();
      if (sat_c_is_up_to_date())
	const_cast<SatMatrix&>(sat_c).transpose_assign(sat_g);
    }
    else {
      const_cast<GenSys&>(gen_sys).sort_rows();
      if (sat_c_is_up_to_date()) {
#ifndef NDEBUG
	const_cast<Polyhedron&>(*this).clear_sat_c_up_to_date();
#endif
	const_cast<Polyhedron&>(*this).update_sat_c();
      }
    }
  }
  return gen_sys;
}


PPL::Polyhedron::Polyhedron(size_t num_dimensions, Degenerate_Kind kind)
  : con_sys(),
    gen_sys(),
    sat_c(),
    sat_g() {
  if (kind == EMPTY)
    status.set_empty();
  else
    if (num_dimensions > 0) {
      // The only constraint is the positivity one.
      con_sys.resize_no_copy(1, num_dimensions+1);
      con_sys[0][0] = 1;
      con_sys[0].set_is_inequality();
      // The system of constraints only composed by the positivity
      // constraint is in the minimal form.
      set_constraints_minimized();
    }
  space_dim = num_dimensions;
}


PPL::Polyhedron::Polyhedron(const Polyhedron& y)
  : space_dim(y.space_dim),
    status(y.status) {
  if (y.constraints_are_up_to_date())
    con_sys = y.con_sys;
  if (y.generators_are_up_to_date())
    gen_sys = y.gen_sys;
  if (y.sat_c_is_up_to_date())
    sat_c = y.sat_c;
  if (y.sat_g_is_up_to_date())
      sat_g = y.sat_g;
}

/*!
  Builds a polyhedron satisfying the given system of constraints \p cs.
*/
PPL::Polyhedron::Polyhedron(ConSys& cs)
  : con_sys(),
    gen_sys(),
    sat_c(),
    sat_g() {

  if (cs.num_columns() > 1) {
    // The following swap destroys the given argument `cs';
    // that is why the formal parameter is not declared const.
    std::swap(con_sys, cs);
    // Adding the positivity constraint.
    con_sys.add_row(Row::RAY_OR_VERTEX_OR_INEQUALITY);
    con_sys[con_sys.num_rows()-1][0] = 1;
    set_constraints_up_to_date();
    // The first column is not a dimension.
    space_dim = con_sys.num_columns() - 1;
    return;
  }

  // As cs.num_columns <= 1, it is a zero-dim space polyhedron.
  space_dim = 0;
  if (cs.num_columns() == 1)
    // Checking for an inconsistent constraint.
    for (size_t i = cs.num_rows(); i-- > 0; ) {
      const Row& r = cs[i];
      if (r[0] != 0 && (r.is_line_or_equality() || r[0] < 0))
	// Inconsistent constraint found.
	set_empty();
    }
}


/*!
  Builds a polyhedron generated by the given system of generators \p gs.
*/
PPL::Polyhedron::Polyhedron(GenSys& gs)
  : con_sys(),
    gen_sys(),
    sat_c(),
    sat_g() {

  if (gs.num_columns() > 1) {
    assert(gs.num_rows() > 0);
    // Checking if the matrix of generators contains a vertex:
    // we speculatively use an increasing index (instead of
    // the standard decreasing one) because we hope this way
    // vertices can be found earlier (we suppose users will
    // insert vertices first in a system of generators).
    size_t i = 0;
    size_t iend = gs.num_rows();
    for ( ; i < iend; ++i) {
      if (gs[i][0] != 0)
	break;
    }
    if (i == iend)
      throw std::invalid_argument("PPL::Polyhedron::Polyhedron(gs): "
				  "non-empty gs with no vertices");
    
    // The following swap destroys the given argument `gs';
    // that is why the formal parameter is not declared const.
    std::swap(gen_sys, gs);
    set_generators_up_to_date();
    space_dim = gen_sys.num_columns() - 1;
    return;
  }

  // Here gs.num_columns() <= 1.
  space_dim = 0;
  if (gs.num_rows() == 0)
    status.set_empty();
  else
    // It has to be a vertex.
    assert(gs[0][0] != 0);
}


PPL::Polyhedron&
PPL::Polyhedron::operator =(const Polyhedron& y) {
  space_dim = y.space_dim;
  status = y.status;
  if (y.constraints_are_up_to_date())
    con_sys = y.con_sys;
  if (y.generators_are_up_to_date())
    gen_sys = y.gen_sys;
  if (y.sat_c_is_up_to_date())
    sat_c = y.sat_c;
  if (y.sat_g_is_up_to_date())
    sat_g = y.sat_g;

  return *this;
}


//! Destructor.
PPL::Polyhedron::~Polyhedron() {
}


/*!
  Clears the polyhedron since it is empty.
*/
void
PPL::Polyhedron::set_empty() {
  status.set_empty();
  // The polyhedron is empty: we can thus throw away everything.
  con_sys.clear();
  gen_sys.clear();
  sat_c.clear();
  sat_g.clear();
}

/*!
  Updates constraints starting from the system of generators
  and minimizes them. The resulting system of constraints will not
  be sorted: we only know that the equalities are in the upper
  part of the matrix and the inequalities in the lower one.
*/
void
PPL::Polyhedron::update_constraints() const {
  assert(space_dimension() > 0);
  assert(!is_empty());
  assert(generators_are_up_to_date());

  Polyhedron& x = const_cast<Polyhedron&>(*this);
  minimize(false, x.gen_sys, x.con_sys, x.sat_c);
  x.set_sat_c_up_to_date();
  x.set_constraints_minimized();
  x.set_generators_minimized();
}

/*!
  Updates generators starting from the system of constraints
  and minimizes them. The resulting system of generators will not
  be sorted: we only know that the lines are in the upper part of
  the matrix and the rays and the vertices are in the lower one.
*/
bool
PPL::Polyhedron::update_generators() const {
  assert(space_dimension() > 0);
  assert(!is_empty());
  assert(constraints_are_up_to_date());

  Polyhedron& x = const_cast<Polyhedron&>(*this);
  // If the system of constraints is not consistent the 
  // polyhedron is empty.
  bool empty = minimize(true, x.con_sys, x.gen_sys, x.sat_g);
  if (empty)
    x.set_empty();
  else {
    x.set_sat_g_up_to_date();
    x.set_constraints_minimized();
    x.set_generators_minimized();
  }
  return !empty;
}


/*!
  If required, updates generators from constraints or vice versa
  and minimizes them.
*/
void
PPL::Polyhedron::minimize() const {
  // 0-dim space or empty polyhedra are already minimized.
  if (space_dimension() == 0
      || is_empty()
      || (constraints_are_minimized() && generators_are_minimized()))
    return;
  // If constraints or generators are up-to-date, invoking
  // update_generators() or update_constraints(), respectively,
  // minimizes both constraints and generators.
  // If both are up-to-date it does not matter whether we use
  // update_generators() or update_constraints():
  // both minimize constraints and generators.
  else if (constraints_are_up_to_date())
    update_generators();
  else {
    assert(generators_are_up_to_date());
    update_constraints();
  }

  assert(OK());
}

/*!
  Sorts the matrix of constraints keeping \p sat_g consistent; 
  if \p sat_g is not up-to-date it will be obtained from \p sat_c 
  and then sorted together with \p con_sys.
*/
void
PPL::Polyhedron::obtain_sorted_constraints() {
  assert(constraints_are_up_to_date());

  if (!con_sys.is_sorted())
    if (sat_g_is_up_to_date()) {
      // Sorting constraints keeping `sat_g' consistent.
      con_sys.sort_and_remove_with_sat(sat_g);
      // `sat_c' is not up-to-date anymore.
      clear_sat_c_up_to_date();
    }
    else if (sat_c_is_up_to_date()) {
      // Using `sat_c' to obtain `sat_g', then it is like previous case.
      sat_g.transpose_assign(sat_c);
      con_sys.sort_and_remove_with_sat(sat_g);
      set_sat_g_up_to_date();
      clear_sat_c_up_to_date();
    }
    else
      // If neither `sat_g' nor `sat_c' are up-to-date, we just sort
      // the constraints.
      con_sys.sort_rows();
  assert(con_sys.check_sorted());
}

/*!
  Sorts the matrix of generators keeping \p sat_c consistent; 
  if \p sat_c is not up-to-date it will be obtained from \p sat_g 
  and then sorted together with \p gen_sys.
*/
void
PPL::Polyhedron::obtain_sorted_generators() {
  assert(generators_are_up_to_date());
  if (!gen_sys.is_sorted())
    if (sat_c_is_up_to_date()) {
      // Sorting generators keeping 'sat_c' consistent.
      gen_sys.sort_and_remove_with_sat(sat_c);
      // `sat_g' is not up-to-date anymore.
      clear_sat_g_up_to_date();
    }
    else if (sat_g_is_up_to_date()) {
      // Obtaining `sat_c' from `sat_g' and proceeding like previous case.
      sat_c.transpose_assign(sat_g);
      gen_sys.sort_and_remove_with_sat(sat_c);
      set_sat_c_up_to_date();
      clear_sat_g_up_to_date();
    }
    else
      // If neither `sat_g' nor `sat_c' are up-to-date, we just sort
      // the generators.
      gen_sys.sort_rows();
  assert(gen_sys.check_sorted());
}


/*!
  Sorts the matrix of constraints keeping \p sat_g consistent and then
  obtains \p sat_c from \p sat_g.
*/
void
PPL::Polyhedron::obtain_sorted_constraints_with_sat_c() {
  assert(constraints_are_up_to_date());

  assert(constraints_are_minimized());

  // At least one of the saturation matrices must be up-to-date.
  if (!sat_c_is_up_to_date() && !sat_g_is_up_to_date())
    update_sat_c();

  if (con_sys.is_sorted()) {
    if (sat_c_is_up_to_date())
      // If constraints are already sorted and sat_c is up to
      // date there is nothing to do.
      return;
  }
  else {
    if (!sat_g_is_up_to_date()) {
      // If constraints are not sorted and sat_g is not up-to-date
      // we obtain sat_g from sat_c (that has to be up-to-date)...
      sat_g.transpose_assign(sat_c);
      set_sat_g_up_to_date();
    }
    // ...and sort it together with constraints.
    con_sys.sort_and_remove_with_sat(sat_g);
  }
  // Obtaining sat_c from sat_g.
  sat_c.transpose_assign(sat_g);
  set_sat_c_up_to_date();
  // Constraints are sorted now.
  con_sys.set_sorted(true);
  assert(con_sys.check_sorted());
}


/*!
  Sorts the matrix of generators keeping \p sat_c consistent and then
  obtains \p sat_g from \p sat_c.
*/
void
PPL::Polyhedron::obtain_sorted_generators_with_sat_g() {
  assert(generators_are_up_to_date());

  // At least one of the saturation matrices must be up-to-date.
  if (!sat_c_is_up_to_date() && !sat_g_is_up_to_date())
    update_sat_g();

  if (gen_sys.is_sorted()) {
    if (sat_g_is_up_to_date())
      // If generators are already sorted and sat_g is up to
      // date there is nothing to do.
      return;
  }
  else {
    if (!sat_c_is_up_to_date()) {
      // If generators are not sorted and sat_c is not up-to-date
      // we obtain sat_c from sat_g (that has to be up-to-date) ...
      sat_c.transpose_assign(sat_g);
      set_sat_c_up_to_date();
    }
    // ...and sort it together with generators.
    gen_sys.sort_and_remove_with_sat(sat_c);
  }
  // Obtaining sat_g from sat_c.
  sat_g.transpose_assign(sat_c);
  set_sat_g_up_to_date();
  // Generators are sorted now.
  gen_sys.set_sorted(true);
  assert(gen_sys.check_sorted());
}

/*!
  Computes \p sat_c starting from the systems of generators
  and constraints and assuming that both of them are up-to-date:
  \f[
    \begin{cases}
    sat\_c[i][j] = 0, \quad \text{if } G[i] \cdot C^\mathrm{T}[j] = 0; \\
    sat\_c[i][j] = 1, \quad \text{if } G[i] \cdot C^\mathrm{T}[j] > 0.
    \end{cases}
  \f]
*/
void
PPL::Polyhedron::update_sat_c() const {
  assert(constraints_are_minimized());
  assert(generators_are_minimized());
  assert(!sat_c_is_up_to_date());

  size_t csr = con_sys.num_rows();
  size_t gsr = gen_sys.num_rows();
  Polyhedron& x = const_cast<Polyhedron&>(*this);

  // Recall that the columns of `sat_c' represent the constraints and
  // its rows represent the generators: this is because it is resized
  // as follow.
  x.sat_c.resize(gsr, csr);
  for (size_t i = gsr; i-- > 0; )
    for (size_t j = csr; j-- > 0; ) {
      int sp_sign = sgn(con_sys[j] * gen_sys[i]);
      // The negativity of this scalar product would mean
      // that the generator `gen_sys[i]' violates the constraint
      // `con_sys[j]' and it is not possible because both generators
      // and constraints are up-to-date.
      assert(sp_sign >= 0);
      if (sp_sign > 0)
	// `gen_sys[i]' satisfies (without saturate) `con_sys[j]'.
	x.sat_c[i].set(j);
      else
	// `gen_sys[i]' saturates `con_sys[j]'.
	x.sat_c[i].clear(j);
    }
  x.set_sat_c_up_to_date();
}

/*!
  Computes \p sat_g starting from the systems of generators
  and constraints and assuming both of them are up-to-date:
  \f[
  \begin{cases}
  sat\_g[i][j] = 0, \quad \text{if } C[i] \cdot G^\mathrm{T}[j] = 0; \\
  sat\_g[i][j] = 1, \quad \text{if } C[i] \cdot G^\mathrm{T}[j] > 0.
  \end{cases}
  \f]
*/
void
PPL::Polyhedron::update_sat_g() const {
  assert(constraints_are_minimized());
  assert(generators_are_minimized());
  assert(!sat_g_is_up_to_date());

  size_t csr = con_sys.num_rows();
  size_t gsr = gen_sys.num_rows();
  Polyhedron& x = const_cast<Polyhedron&>(*this);

  // Recalling that the columns of `sat_g' represent generators and its
  // rows represent the constraints, we resize it as follow.
  x.sat_g.resize(csr, gsr);
  for (size_t i = csr; i-- > 0; )
    for (size_t j = gsr; j-- > 0; ) {
      int sp_sign = sgn(con_sys[i] * gen_sys[j]);
      // The negativity of this scalar product would mean
      // that the generator `gen_sys[j]' violates the constraint
      // `con_sys[i]' and it is not possible because both generators
      // and constraints are up-to-date.
      assert(sp_sign >= 0);
      if (sp_sign > 0)
	// `gen_sys[j]' satisfies (without saturate) `con_sys[i]'.
	x.sat_g[i].set(j);
      else
	// `gen_sys[j]' saturates `con_sys[i]'.
	x.sat_g[i].clear(j);
    }
  x.set_sat_g_up_to_date();
}

/*!
  Returns <CODE>true</CODE> if and only if
  \p x is contained in \p y.
*/
bool
PPL::operator <=(const Polyhedron& x, const Polyhedron& y) {
  if (x.space_dimension() != y.space_dimension())
    throw std::invalid_argument("PPL::operator <=(ph1, ph2): "
				"ph1 and ph2 are dimension-incompatible");
  if (x.is_empty())
    return true;
  else if (y.is_empty())
    return x.check_empty();
  else if (x.space_dimension() == 0)
    return true;
  if (!(x.generators_are_minimized()))
    x.minimize();
  if (!(y.constraints_are_minimized()))
    y.minimize();
  // `x' is contained in `y' if and only if all the generators of `x'
  // satisfy or saturate all the inequalities and saturate all the
  // equalities of `y'; this comes from
  // the definition of a polyhedron as the set of vector that satisfy
  // a given system of constraints and the fact that all vectors in `x'
  // can be obtained as a combination of its generators.
  for (size_t i = x.gen_sys.num_rows(); i-- > 0; )
    for (size_t j = y.con_sys.num_rows(); j-- > 0; )
      if (y.con_sys[j].is_inequality()) {
	if (y.con_sys[j] * x.gen_sys[i] < 0)
	  return false;
      }
      else if (y.con_sys[j] * x.gen_sys[i] != 0)
	return false;
  return true;
}

static void
throw_different_dimensions(const char* method,
			   const PPL::Polyhedron& x,
			   const PPL::Polyhedron& y) {
  std::string what;
  std::ostringstream s(what);
  s << method << ":" << std::endl
    << "this->space_dimension() == " << x.space_dimension()
    << ", y->space_dimension() == " << y.space_dimension();
  throw std::invalid_argument(s.str());
}

/*!
  The intersection of \p *this with \p y (that are assumed to
  have the same dimension) is assigned to \p *this.
*/
void
PPL::Polyhedron::intersection_assign(const Polyhedron& y) {
  Polyhedron& x = *this;

  // Dimension-compatibility check.
  if (x.space_dimension() != y.space_dimension())
    throw_different_dimensions("PPL::Polyhedron::intersection_assign(y)",
			       x, y);

  // If one of the two polyhedra is empty, the intersection is empty.
  if (x.is_empty())
    return;
  if (y.is_empty()) {
    x.set_empty();
    return;
  }

  // If both polyhedra are zero-dimensional,
  // then at this point they are necessarily non-empty,
  // so that their intersection is non-empty too.
  if (x.space_dimension() == 0)
    return;

  // add_and_minimize() requires x to be up-to-date
  // and to have sorted constraints...
  x.minimize();
  // ... and y to have updated and sorted constraints.
  if (!y.constraints_are_up_to_date())
    y.update_constraints();
  
  // After minimize() is not assured constraints of x to be sorted.
  x.obtain_sorted_constraints_with_sat_c();
  // After update_constraint() is not assured constraint of y to be sorted.
  const_cast<Polyhedron&>(y).obtain_sorted_constraints();

  bool empty = add_and_minimize(true,
				x.con_sys, x.gen_sys, x.sat_c,
				y.con_sys);

  if (empty)
    x.set_empty();
  else {
    // On exit of the function intersection_assign() the polyhedron
    // is up-to-date and sat_c is meaningful.
    x.set_sat_c_up_to_date();
    x.clear_sat_g_up_to_date();
  }
}

/*!
  The convex hull between \p *this and \p y (that are assumed to
  have the same dimension) is assigned to \p *this.
*/
void
PPL::Polyhedron::convex_hull_assign(const Polyhedron& y) {
  Polyhedron& x = *this;

  // Dimension-compatibility check.
  if (x.space_dimension() != y.space_dimension())
    throw_different_dimensions("PPL::Polyhedron::convex_hull_assign(y)",
			       x, y);

  // Convex hull between a polyhedron `p' and an empty polyhedron is `p'.
  if (y.is_empty())
    return;
  else if (x.is_empty()) {
    x = y;
    return;
  }

  // If both polyhedra are zero-dimensional,
  // then at this point they are necessarily universe polyhedra,
  // so that their convex-hull is the universe polyhedron too.
  if (x.space_dimension() == 0)
    return;

  // The function add_and_minimize() requires `x' to be up-to-date and
  // to have sorted generators...
  x.minimize();
  // ...and `y' to have updated and sorted generators.
  if (!y.generators_are_up_to_date())
    y.update_generators();
   
  x.obtain_sorted_generators_with_sat_g();

  const_cast<Polyhedron&>(y).obtain_sorted_generators();

  add_and_minimize(false,
		   x.gen_sys, x.con_sys, x.sat_g,
		   y.gen_sys);

  x.set_sat_g_up_to_date();
  x.clear_sat_c_up_to_date();

  assert(OK());
}

/*!
  The convex hull between \p *this and \p y (that are assumed to
  have the same dimension) is assigned to \p *this.
  The result is not minimized.
*/
void
PPL::Polyhedron::convex_hull_assign_lazy(const Polyhedron& y) {
  Polyhedron& x = *this;

  // Dimension-compatibility check.
  if (x.space_dimension() != y.space_dimension())
    throw_different_dimensions("PPL::Polyhedron::convex_hull_assign_lazy(y)",
			       x, y);

  // Convex hull lazy between a polyhedron `p' and
  // an empty polyhedron is `p'.
  if (y.is_empty())
    return;
  else if (x.is_empty()) {
    x = y;
    return;
  }

  // If both polyhedra are zero-dimensional,
  // then at this point they are necessarily universe polyhedra,
  // so that their convex-hull is the universe polyhedron too.
  if (x.space_dimension() == 0)
    return;

  if (!x.generators_are_up_to_date())
    x.update_generators();
  if (!y.generators_are_up_to_date())
    y.update_generators();

  // Matrix::merge_rows_assign() requires both matrices to be sorted.
  if (!x.gen_sys.is_sorted())
    x.gen_sys.sort_rows();
  if (!y.gen_sys.is_sorted())
    const_cast<Polyhedron&>(y).gen_sys.sort_rows();

  x.gen_sys.merge_rows_assign(y.gen_sys);

  // After adding new generators, constraints are no longer up-to-date.
  x.clear_constraints_up_to_date();
  // It is lazy, do not minimize.
  x.clear_generators_minimized();
}

/*!
  \param mat1      The matrix to which columns are added.
  \param mat2      The matrix to which rows and columns are added.
  \param sat1      The saturation matrix whose columns represent
                   constraints or generators if \p mat1 is a matrix
		   of constraints or generators respectively.
		   On entry it is up-to-date.
  \param sat2      The saturation matrix which columns represent
                   constraints or generators if \p mat2 is a matrix
		   of constraints or generators respectively.
  \param add_dim   The number of dimensions to add.

  Adds new dimensions to the Polyhedron modifying the matrices.
  This function is invoked only by <CODE>add_dimensions_and_embed()</CODE>
  and <CODE>add_dimensions_and_project()</CODE> passing the matrix of
  constraints and that of generators (and the corresponding saturation
  matrices) in different order (see those methods for details).
  Also, this method is invoked only when the polyhedron has at
  least the system of constraints or generators up-to-date.
*/
void
PPL::Polyhedron::add_dimensions(Matrix& mat1,
				Matrix& mat2,
				SatMatrix& sat1,
				SatMatrix& sat2,
				size_t add_dim) {
  assert(add_dim != 0);

  mat1.add_zero_columns(add_dim);
  mat2.add_rows_and_columns(add_dim);
  // The resulting saturation matrix will be the follow:
  // from row    0    to      add_dim-1       : only zeroes
  //          add_dim     add_dim+num_rows-1  : old saturation matrix

  // In fact all the old generators saturate all the new constraints
  // because the polyhedron has not been embedded in the new space.
  sat1.resize(sat1.num_rows() + add_dim, sat1.num_columns());
  // The old matrix is copied at the end of the new matrix.
  for (size_t i = sat1.num_rows() - add_dim; i != 0; ) {
    --i;
    std::swap(sat1[i], sat1[i+add_dim]);
  }
  // Computes the sat_c, too.
  sat2.transpose_assign(sat1);
}


/*!
  Adds \p dim new dimensions and embeds the old polyhedron
  in the new space. The new dimensions are the last and the corresponding
  coefficients are in the last columns of the new matrices.
  The new polyhedron is characterized by
  a system of constraints in which the variables running through
  the new dimensions are not constrained, and by a system of generators
  containing the old one and the lines that are parallel to the axes
  corresponding to the new dimensions.
  For example if the old polyhedron is a line lying on the \f$xy\f$ plane and
  we add a third dimension \f$z\f$, then the operation results in
  a plane parallel to the \f$z\f$ axis that contains the line fixed on
  the \f$xy\f$ plane.
*/
void
PPL::Polyhedron::add_dimensions_and_embed(size_t dim) {
  // Adding no dimensions to any polyhedron is a no-op.
  if (dim == 0)
    return;

  // Adding dimensions to an empty polyhedron is obtained
  // by merely adjusting `space_dim'.
  if (is_empty()) {
    space_dim += dim;
    return;
  }

  if (space_dimension() == 0) {
    assert(status.test_zero_dim_univ());
    // The system of constraints describing the universe polyhedron 
    // only has the positivity constraint; it is in minimal form.
    con_sys.resize_no_copy(1, dim + 1);
    con_sys[0][0] = 1;
    con_sys[0].set_is_inequality();
    set_constraints_minimized();
#ifndef BE_LAZY
    // The system of generators describing the universe polyhedron 
    // has the origin of the space as a (non-proper) vertex
    // and `dim' rows corresponding to the lines parallel to
    // the Cartesian axes; it is in minimal form.
    // We want a sorted system of generators, thus we create
    // the ``specular'' identity matrix having dim+1 rows.
    gen_sys.add_rows_and_columns(dim + 1);
    set_generators_minimized();
#endif
  }
  // To embed an n-dimension space polyhedron in a (n+dim)-dimension space,
  // we just add `dim' zero-columns to the rows in the matrix of constraints;
  // in contrast, the matrix of generators needs additional rows,
  // corresponding to the vectors of the canonical basis
  // for the added dimensions. That is, for each new dimension `x[k]'
  // we add the line having that direction. This is done by invoking
  // the function add_dimensions() giving the matrix of generators
  // as the second argument.
  else if (constraints_are_up_to_date() && generators_are_up_to_date()) {
    // sat_c is necessary in add_dimensions(...).
    if (!sat_c_is_up_to_date())
      update_sat_c();
    // Adds rows and/or columns to both matrices (constraints and generators).
    add_dimensions(con_sys, gen_sys, sat_c, sat_g, dim);
  }
  else if (constraints_are_up_to_date())
    // Only constraints are up-to-date: we do not need to modify generators.
    con_sys.add_zero_columns(dim);
  else {
    // Only generators are up-to-date: we do not need to modify constraints.
    assert(generators_are_up_to_date());
    gen_sys.add_rows_and_columns(dim);
  }
  // Update the space dimension.
  space_dim += dim;

  assert(OK());
}

/*!
  Adds \p dim new dimensions to the old polyhedron and projects
  it onto the old space.  The new dimensions are the last and the
  corresponding coefficients are in the last columns of the new matrices.
  The new polyhedron is characterized by a system of constraints
  containing the old one and other \p dim equalities that constrain
  the new variable to be zero. So the new matrix of generators is made
  by adding \p dim zero-columns to the old matrix.
  For example if we have a line lying on the plane \f$xy\f$, we
  add a third dimension \f$z\f$ and project the line in \f$\Rset^3\f$,
  we still obtain the line on the plane \f$xy\f$ but now the new
  considered space has one more dimension.
*/
void
PPL::Polyhedron::add_dimensions_and_project(size_t dim) {
  // Adding no dimensions to any polyhedron is a no-op.
  if (dim == 0)
    return;

  // Adding dimensions to an empty polyhedron is obtained
  // by merely adjusting `space_dim'.
  if (is_empty()) {
    space_dim += dim;
    return;
  }

  if (space_dimension() == 0) {
    assert(status.test_zero_dim_univ());
#ifndef BE_LAZY
    // We create a specular identity matrix having dim+1 rows.
    // The last row corresponds to the positivity constraint
    // and is immediately erased to achieve minimality.
    // All the other rows are the constraints x[k] = 0 defining
    // the polyhedron given by the origin of the space.
    con_sys.add_rows_and_columns(dim + 1);
    con_sys.erase_to_end(con_sys.num_rows() - 1);
    set_constraints_minimized();
#endif
    // The system of generator for this polyhedron has only
    // the origin as a vertex.
    gen_sys.resize_no_copy(1, dim + 1);
    gen_sys[0][0] = 1;
    gen_sys[0].set_is_ray_or_vertex();
    set_generators_minimized();
  }
  // To project an n-dimension space polyhedron in a (n+dim)-dimension space,
  // we just add to the matrix of generators `dim' zero-columns;
  // In contrast, in the matrix of constraints, new rows are needed
  // in order to avoid embedding the old polyhedron in the new space.
  // Thus, for each new dimensions `x[k]', we add the constraint
  // x[k] = 0; this is done by invoking the function add_dimensions()
  // giving the matrix of constraints as the second argument.
  else if (constraints_are_up_to_date() && generators_are_up_to_date()) {
    // sat_g is necessary in add_dimensions(...).
    if (!sat_g_is_up_to_date())
      update_sat_g();
    // Adds rows and/or columns to both matrices (constraints and generators).
    add_dimensions(gen_sys, con_sys, sat_g, sat_c, dim);
  }
  else if (constraints_are_up_to_date())
    // Only constraints are up-to-date: we do not need to modify generators.
    con_sys.add_rows_and_columns(dim);
  else {
    // Only generators are up-to-date: we do not need to modify constraints.
    assert(generators_are_up_to_date());
    gen_sys.add_zero_columns(dim);
  }

  // Now we update the space dimension.
  space_dim += dim;

  assert(OK());
}

/*!
  Removes the dimensions corresponding to the elements of
  the set \p dims_to_remove.
*/
void
PPL::Polyhedron::remove_dimensions(const std::set<Variable>& to_be_removed) {
  // The removal of no dimensions from any polyhedron is a no-op.
  // Note that this case also captures the only legal removal of
  // dimensions from a polyhedron in a 0-dim space.
  if (to_be_removed.empty())
    return;

  // Checking for dimension-compatibility: the variable having
  // maximum cardinality is the one occurring last in the set.
  if (to_be_removed.rbegin()->id() >= space_dimension())
    throw std::invalid_argument("void PPL::Polyhedron::remove_dimensions"
				"(vs): dimension-incompatible");

  // Removing dimensions from the empty polyhedron
  // just updates the space_dim member.
  if (is_empty()) {
    space_dim -= to_be_removed.size();
    return;
  }

  if (!generators_are_up_to_date())
    update_generators();

  // Emptyness could have been just detected.
  if (is_empty()) {
    space_dim -= to_be_removed.size();
    return;
  }

  // For each variable to be removed, we fill the corresponding column
  // by shifting left those columns that will not be removed.
  std::set<Variable>::const_iterator tbr = to_be_removed.begin();
  std::set<Variable>::const_iterator tbr_end = to_be_removed.end();
  size_t dst_col = tbr->id() + 1;
  size_t src_col = dst_col + 1;
  size_t nrows = gen_sys.num_rows();
  for (tbr++; tbr != tbr_end; tbr++) {
    size_t tbr_col = tbr->id() + 1;
    // All columns in between are moved toward left.
    while (src_col < tbr_col) {
      for (size_t r = nrows; r-- > 0; )
	std::swap(gen_sys[r][dst_col], gen_sys[r][src_col]);
      dst_col++;
      src_col++;
    }
    src_col++;
  }
  // Moving the remaining columns.
  size_t ncols = gen_sys.num_columns();
  while (src_col < ncols) {
    for (size_t r = nrows; r-- > 0; )
      std::swap(gen_sys[r][dst_col], gen_sys[r][src_col]);
    src_col++;
    dst_col++;
  }
  // The number of remaining columns is dst_col.
  gen_sys.resize(nrows, dst_col);

  // Constraints are no longer up-to-date.
  clear_constraints_up_to_date();

  // Updating the space dimension.
  if (gen_sys.num_columns() > 1)
    space_dim = gen_sys.num_columns() - 1;
  else {
    // If less than 2 columns are left,
    // then polyhedron is the zero-dimension universe.
    space_dim = 0;
    set_zero_dim_univ();
    // A zero-dimension universe polyhedron must have
    // both `con_sys' and `gen_sys' with no rows.
    gen_sys.clear();
    con_sys.clear();
  }

}

static void
throw_different_dimensions(const char* method,
			   const PPL::Polyhedron& x,
			   const PPL::Matrix& y) {
  std::string what;
  std::ostringstream s(what);
  s << method << ":" << std::endl
    << "this->space_dimension() == " << x.space_dimension()
    << ", system->space_dimension() == " ;
  size_t y_num_columns = y.num_columns();
  if (y_num_columns == 0)
    s << y_num_columns;
  else
    s << y_num_columns - 1;
  throw std::invalid_argument(s.str());
}


/*!
  Adds further constraints to a polyhedron and computes the new polyhedron
  satisfying all the constraints.
*/
bool
PPL::Polyhedron::add_constraints(ConSys& cs) {
  size_t cs_num_columns = cs.num_columns();
 
  // Dimension-consistency check:
  // the dimension of `cs' can not be greater than space_dimension().
  if (space_dimension() < cs_num_columns - 1)
    throw_different_dimensions("PPL::Polyhedron::add_constraints(c)",
			       *this, cs);

  // Adding no constraints is the same as checking for emptyness.
  if (cs.num_rows() == 0)
    return !check_empty();

  // Dealing with zero-dim space polyhedra first.
  if (space_dimension() == 0) {
    assert(cs_num_columns == 1);
    // Checking for an inconsistent constraint.
    if (cs.begin() == cs.end())
      return true;
    // Inconsistent constraint found.
    status.set_empty();
    return false;
  }

  // We use `check_empty()' because we want the flag EMPTY
  // to precisely represents the status of the polyhedron
  // (i.e., if it is false the polyhedron is really NOT empty)
  // and because, for a non-empty polyhedron, we need both
  // the system of generators and constraints minimal.
  if (check_empty())
    return false;

  // Polyhedron::add_and_minimize() requires that
  // the matrix of constraints to add is sorted.
  if (!cs.is_sorted())
    cs.sort_rows();

  // If needed, we extend `cs' to the right space dimension.
  if (space_dimension() > cs_num_columns - 1)
    cs.add_zero_columns(space_dimension() - cs_num_columns + 1);

  obtain_sorted_constraints_with_sat_c();

  bool empty = add_and_minimize(true, con_sys, gen_sys,
				sat_c, cs);
  if (empty)
    set_empty();
  else {
    // On exit of the function add_and_minimize() `sat_c' is up-to-date
    // while `sat_g' is not up-to-date anymore.
    set_sat_c_up_to_date();
    clear_sat_g_up_to_date();
  }
  assert(OK());

  return !empty;
}


static void
throw_different_dimensions(const char* method,
			   const PPL::Polyhedron& x,
			   const PPL::Row& y) {
  std::string what;
  std::ostringstream s(what);
  s << method << ":" << std::endl
    << "this->space_dimension() == " << x.space_dimension()
    << ", y->space_dimension() == " << y.size() - 1;
  throw std::invalid_argument(s.str());
}

/*!
  This function inserts a new constraint \p c into the system of
  constraints of the polyhedron \p *this.
*/
void
PPL::Polyhedron::insert(const Constraint& c) {

  // Dimension-consistency check:
  // the dimension of `c' can not be greater than space_dimension().
  if (space_dimension() < c.size() - 1)
    throw_different_dimensions("PPL::Polyhedron::insert(c)",
			       *this, c);

  // Adding a new constraint to an empty polyhedron
  // results in an empty polyhedron.
  if (is_empty())
    return;

  // Dealing with a zero-dim space polyhedron first.
  if (space_dimension() == 0) {
    // For dimension-compatibility, `c' has 1 column.
    assert(c.size() == 1);
    if (!c.is_trivial())
      set_empty();
    return;
  }

  if (!constraints_are_up_to_date())
    update_constraints();

  // Here we know that the system of constraints has at least a row.
  con_sys.insert(c);
  
  // After adding new constraints, generators are no more up-to-date.
  clear_constraints_minimized();
  clear_generators_up_to_date();

  // The constraint system may have become unsatisfiable.
  // Do not check for satisfiability.
  assert(OK(false));
}

/*!
  This function inserts a new generator \p g into the system of
  generators of the polyhedron \p *this. The new generator
  can be redundant.
*/
void
PPL::Polyhedron::insert(const Generator& g) {

  // Dimension-consistency check:
  // the dimension of `g' can not be greater than space_dimension().
  if (space_dimension() < g.size() - 1)
    throw_different_dimensions("PPL::Polyhedron::insert(g)",
			       *this, g);

  // If the dimension-compatibility check is passed,
  // we have space_dimension() > 0.
  if (generators_are_up_to_date()) {
    gen_sys.insert(g);
    // After adding the new generator, constraints are no longer up-to-date.
    clear_generators_minimized();
    clear_constraints_up_to_date();
    return;
  }

  // Generators are not up-to-date: now calling `check_empty' because,
  // if the polyhedron is not empty, we need the generators anyway.
  if (check_empty()) {
    // Polyhedron is empty:
    // the specification says we can only insert a vertex.
    if (g.type() != Generator::VERTEX)
      throw std::invalid_argument("void PPL::Polyhedron::insert(g): "
				  "*this is empty and g is not a vertex");
    // FIXME: why do we need the following clear() ?
    // Would not be an assertion sufficient ?
    gen_sys.clear();
    gen_sys.insert(g);
    // Resize `gen_sys' to have space_dimension() + 1 columns.
    if (gen_sys.num_columns() != space_dimension() + 1)
      gen_sys.add_zero_columns(space_dimension()
			       - gen_sys.num_columns() + 1);
    // No longer empty, generators up-to-date and minimized.
    clear_empty();
    set_generators_up_to_date();
    return;
  }

  // Polyhedron is NOT empty and with generators up-to-date.
  gen_sys.insert(g);
  // Generators no longer minimized, constraints no longer up-to-date.
  clear_generators_minimized();
  clear_constraints_up_to_date();

}


/*!
  Adds specified constraints to a Polyhedron without minimizing.
*/
void
PPL::Polyhedron::add_constraints_lazy(ConSys& cs) {
  size_t cs_num_columns = cs.num_columns();  
  // Dimension-consistency check:
  // the dimension of `cs' can not be greater than space_dimension().
  if (space_dimension() < cs_num_columns - 1)
    throw_different_dimensions("PPL::Polyhedron::add_constraints_lazy(c)",
			       *this, cs);

  // Adding zero-dimension constraints is a no-op.
  // (By dimension-compatibility, this also capture the case
  // when the polyhedron space is zero-dim.)
  if (cs_num_columns == 0)
    return;

  if (cs_num_columns == 1 && space_dimension() == 0) {
    // Checking for an inconsistent constraint.
    if (cs.begin() != cs.end())
      // Inconsistent constraint found.
      status.set_empty();
    return;
  }

  // We only need that the system of constraints is up-to-date.
  if (!constraints_are_up_to_date())
    minimize();
  
  if (is_empty())
    return;

  // Matrix::merge_rows_assign() requires both matrices to be sorted.
  if (!cs.is_sorted())
    cs.sort_rows();

  // If needed, we extend `cs' to the right space dimension.
  if (space_dimension() > cs_num_columns - 1)
    cs.add_zero_columns(space_dimension() - cs_num_columns + 1);
 
  con_sys.sort_rows();

  con_sys.merge_rows_assign(cs);
  // After adding new constraints, generators are no more up-to-date.
  clear_constraints_minimized();
  clear_generators_up_to_date();

  // The constraint system may have become unsatisfiable.
  // Do not check for satisfiability.
  assert(OK(false));
}

void
PPL::Polyhedron::add_dimensions_and_constraints_lazy(const ConSys& cs) {
  space_dim = space_dimension() + cs.num_columns() - 1;
  // If the polyhedron is empty, we only modify the dimension of
  // the space.
  if (is_empty())
    return;

  if (!constraints_are_up_to_date())
    update_constraints();
 
  size_t old_num_rows = con_sys.num_rows();
  size_t old_num_columns = con_sys.num_columns();

  // The new system of constraints has the matrix of constraints
  // of the old polyhedron in the upper left hand side and the
  // coefficients of the variables of `cs' in the lower right hand side.
  con_sys.resize(old_num_rows + cs.num_rows(), space_dim + 1);
  for (size_t i = cs.num_rows(); i-- > 0; ) {
    Constraint& c = con_sys[old_num_rows + i];
      c[0] = cs[i][0];
      if (cs[i].is_equality())
	c.set_is_equality();
      for (size_t j = cs.num_columns(); j-- > 1; )
      c[old_num_columns - 1 + j] = cs[i][j];
  }
 
  clear_constraints_minimized();
  clear_generators_up_to_date();
  clear_sat_g_up_to_date();
  clear_sat_c_up_to_date();
}
  
/*!
  Adds further generators to a Polyhedron.
*/
void
PPL::Polyhedron::add_generators(GenSys& gs) {
  size_t gs_num_columns = gs.num_columns();

  // Dimension-consistency check:
  // the dimension of `gs' can not be greater than space_dimension().
  if (space_dimension() < gs_num_columns - 1)
    throw_different_dimensions("PPL::Polyhedron::add_generators(g)",
			       *this, gs);

  // Adding zero-dimension generators is a no-op.
  // (By dimension-compatibility, this also capture the case
  // when the polyhedron space is zero-dim.)
  if (gs_num_columns == 0)
    // FIXME: what if polyhedron is empty ?
    return;

  if (gs_num_columns == 1 && space_dimension() == 0) {
    set_zero_dim_univ();
    return;
  }

  if (!gs.is_sorted())
    gs.sort_rows();
  
  // If needed, we extend `gs' to the right space dimension.
  if (space_dimension() > gs_num_columns - 1)
    gs.add_zero_columns(space_dimension() - gs_num_columns + 1);

  // We use `check_empty()' because we want the flag EMPTY
  // to precisely represents the status of the polyhedron
  // (i.e., if it is false the polyhedron is really NOT empty)
  // and because, for a non-empty polyhedron, we need both
  // the system of generators and constraints minimal.
  if (!check_empty()) {
    obtain_sorted_generators_with_sat_g();
    add_and_minimize(false, gen_sys, con_sys, sat_g, gs);
    clear_sat_c_up_to_date();
  }
  else {
    // The polyhedron is no longer empty and generators are up-to-date.
    std::swap(gen_sys, gs);
    clear_empty();
    set_generators_up_to_date();
    minimize();
  }
}

std::ostream&
PPL::operator <<(std::ostream& s, const PPL::Polyhedron& p) {
  using std::endl;

  s << "space_dim "
    << p.space_dimension()
    << endl
    << p.status
    << endl
    << "con_sys ("
    << (p.constraints_are_up_to_date() ? "" : "not_")
    << "up-to-date)"
    << endl
    << p.con_sys
    << endl
    << "gen_sys ("
    << (p.generators_are_up_to_date() ? "" : "not_")
    << "up-to-date)"
    << endl
    << p.gen_sys
    << endl
    << "sat_c"
    << endl
    << p.sat_c
    << endl
    << "sat_g"
    << endl
    << p.sat_g
    << endl;

  return s;
}

std::istream&
PPL::operator >>(std::istream& s, PPL::Polyhedron& p) {
  std::string str;

  s >> str;
  assert(str == "space_dim");
  s >> p.space_dim;

  s >> p.status;

  s >> str;
  assert(str == "con_sys");
  s >> str;
  assert(str == "(not_up-to-date)" || str == "(up-to-date)");
  s >> p.con_sys;
  s >> str;
  assert(str == "gen_sys");
  s >> str;
  assert(str == "(not_up-to-date)" || str == "(up-to-date)");
  s >> p.gen_sys;
  s >> str;
  assert(str == "sat_c");
  s >> p.sat_c;
  s >> str;
  assert(str == "sat_g");
  s >> p.sat_g;

  return s;
}


/*!
  When considering the generators of a polyhedron, the
  affine transformation
  \f[
    \frac{\sum_{i=0}^{n-1} a_i x_i + b}{denominator}
  \f]
  is assigned to \p var where \p expr is
  \f$\sum_{i=0}^{n-1} a_i x_i + b\f$
  (\f$b\f$ is the inhomogeneous term).

  If constraints are up-to-date, it uses the specialized function
  substitute_variable() (for the system of constraints)
  and inverse transformation to reach the same result.
  To obtain the inverse transformation, , we use the following observation.

  Observation:
  -# The affine transformation is invertible if the coefficient of \p var
     in this transformation (i.e. \f$a_{var}\f$) is different from zero.
  -# If the transformation is invertible, then we can write
     \f[
  	\text{denominator} * {x'}_{var}
	                         = \sum_{i = 0}^{n - 1} a_i x_i + b
	                         = a_{var} x_{var} +
			           \sum_{i \neq var} a_i x_i + b,
     \f]
     so that the inverse transformation is
     \f[
	a_{var} x_{var} = \text{denominator} * {x'}_{var} -
	              \sum_{i \neq j} a_i x_i - b.
     \f]

  Then, if the transformation is invertible, all the entities that
  were up-to-date remain up-to-date. Otherwise only generators remain
  up-to-date.

  In other words, if \f$R\f$ is a \f$m_1 \times n_1\f$ matrix representing
  the rays of the polyhedron, \f$V\f$ is a \f$m_2 \times n_2\f$
  matrix representing the vertices of the polyhedron and
  \f[
    P = \bigl\{\,\vect{x} = (x_0, \ldots, x_{n-1})^\mathrm{T} \bigm|
               \vect{x} = \vect{\lambda} R + \vect{\mu} V,
               \vect{\lambda} \in \Rset^{m_1}_+,
               \vect{\mu} \in \Rset^{m_2}_+,
	       \sum_{i = 0}^{m_1 - 1} \lambda_i = 1\,\bigr\}
  \f]
  and \f$T\f$ is the affine transformation to apply to \f$P\f$, then
  the resulting polyhedron is
  \f[
    P' = \bigl\{\,(x_0, \ldots, T(x_0, \ldots, x_{n-1}), \ldots,
                  x_{n-1})^\mathrm{T} \bigm| (x_0, \ldots, x_{n-1})
		  ^\mathrm{T} \in P\,\bigr\}.
  \f]

  Affine transformations are, for example:
  - translations
  - rotations
  - symmetries.
*/
void
PPL::Polyhedron::assign_variable(const Variable& var,
				 const LinExpression& expr,
				 const Integer& denominator) {
  if (denominator == 0)
    throw std::invalid_argument("void PPL::Polyhedron::assign_variable"
				"(v, e, d): d == 0");
  Polyhedron& x = *this;
  size_t num_columns = x.space_dimension() + 1;
  size_t num_var = var.id() + 1;
  if (num_columns != expr.size())
    throw std::invalid_argument("PPL::Polyhedron::assign_variable"
				"(v, e, d): dim(e) != dim(*this)");

  // Index of var must be in the range of the variables of generators.
  if (!(num_var < num_columns))
    throw std::invalid_argument("PPL::Polyhedron::assign_variable(v, e, d):"
				" v is not in *this");

  if (expr[num_var] != 0) {
    // The transformation is invertible.
    if (generators_are_up_to_date())
      x.gen_sys.assign_variable(num_var, expr, denominator);
    if (constraints_are_up_to_date()) {
      // To build the inverse transformation,
      // after copying and negating `expr',
      // we exchange the roles of `expr[num_var]' and `denominator'.
      LinExpression inverse = -expr;
      inverse[num_var] = denominator;
      x.con_sys.substitute_variable(num_var, inverse, expr[num_var]);
    }
    x.clear_constraints_minimized();
    x.clear_generators_minimized();
  }
  // The transformation is not invertible.
  else {
    if (!generators_are_up_to_date())
      x.minimize();
    x.gen_sys.assign_variable(num_var, expr, denominator);
    x.clear_constraints_up_to_date();
    x.clear_generators_minimized();
  }
}



/*!
  When considering constraints of a polyhedron, the affine transformation
  \f[
  \frac{\sum_{i=0}^{n-1} a_i x_i + b}{denominator},
  \f]
  is assigned to \p vars where \p expr is
  \f$\sum_{i=0}^{n-1} a_i x_i + b\f$
  (\f$b\f$ is the inhomogeneous term).

  If generators are up-to-date, then the specialized function
  assign_variable() is used (for the system of generators)
  and inverse transformation to reach the same result.
  To obtain the inverse transformation, we use the following observation.

  Observation:
  -# The affine transformation is invertible if the
     coefficient of `var' in this transformation (i.e. \f$a_{var}\f$)
     is different from zero.
  -# If the transformation is invertible, then we can write
     \f[
  	\text{denominator} * {x'}_{var}
	                         = \sum_{i = 0}^{n - 1} a_i x_i + b
                                 = a_{var} x_{var} +
			           \sum_{i \neq var} a_i x_i + b,
     \f],
     the inverse transformation is
     \f[
	a_{var} x_{var} = \text{denominator} * {x'}_{var} -
	                  \sum_{i \neq j} a_i x_i - b.
     \f].

  Then, if the transformation is invertible, all the entities that
  were up-to-date remain up-to-date. Otherwise only constraints remain
  up-to-date.

  In other words, if \f$A\f$ is a \f$m \times n\f$ matrix representing
  the constraints of the polyhedron, \f$T\f$ is the affine transformation
  to apply to \f$P\f$ and
  \f[
    P = \bigl\{\,\vect{x} = (x_0, \ldots, x_{n-1})^\mathrm{T} \bigm|
                 A\vect{x} \geq \vect{0}\,\bigr\}.
  \f]
  The resulting polyhedron is
  \f[
    P' = \bigl\{\,\vect{x} = (x_0, \ldots, x_{n-1}))^\mathrm{T} \bigm|
                 A'\vect{x} \geq \vect{0}\,\bigr\},
  \f]
  where \f$A'\f$ is defined as follows:
  \f[
    {a'}_{ij} =
    \begin{cases}
    a_{ij} * \text{denominator} + a_{i\text{var}} * \text{expr}[j]
    \quad \text{for } j \neq \text{var}; \\
    \text{expr}[\text{var}] * a_{i\text{var}}.
    \end{cases}
  \f]
*/
void
PPL::Polyhedron::substitute_variable(const Variable& var,
				     const LinExpression& expr,
				     const Integer& denominator) {
  if (denominator == 0)
    throw std::invalid_argument("void PPL::Polyhedron::substitute_variable"
				"(v, e, d): d == 0");

  Polyhedron& x = *this;
  size_t num_columns = x.space_dimension() + 1;
  size_t num_var = var.id() + 1;
  if (num_columns != expr.size())
    throw std::invalid_argument("PPL::Polyhedron::substitute_variable"
				"(v, e, d): dim(e) != dim(*this)");

  // Index of var must be in the range of the variables of generators.
  if (!(num_var < num_columns))
    throw std::invalid_argument("PPL::Polyhedron::substitute_variable"
				"(v, e, d): v is not in *this");

  // The transformation is invertible.
  if (expr[num_var] != 0) {
    if (constraints_are_up_to_date())
      x.con_sys.substitute_variable(num_var, expr, denominator);
    if (generators_are_up_to_date()) {
      // To build the inverse transformation,
      // after copying and negating `expr',
      // we exchange the roles of `expr[num_var]' and `denominator'.
      LinExpression inverse = -expr;
      inverse[num_var] = denominator;
      x.gen_sys.assign_variable(num_var, inverse, expr[num_var]);
    }
    x.clear_constraints_minimized();
    x.clear_generators_minimized();
  }
  // The transformation is not invertible.
  else {
    if (!constraints_are_up_to_date())
      x.minimize();
    x.con_sys.substitute_variable(num_var, expr, denominator);
    x.clear_generators_up_to_date();
    x.clear_constraints_minimized();
  }
}

/*!
  Returns the relation between \p *this and the constraint \p c.
  (See the function
  \p GenSys::satisfy(const Constraint& con)
  for more details).
*/
PPL::GenSys_Con_Rel
PPL::Polyhedron::satisfies(const Constraint& c) {
  if (is_empty())
    return ALL_SATURATE;
  else {
    if (space_dimension() == 0)
      throw_different_dimensions("PPL::Polyhedron::satisfies(c)",
				 *this, c);

    if (!generators_are_up_to_date())
      update_generators();
    if (gen_sys.num_columns() != c.size())
      throw_different_dimensions("PPL::Polyhedron::satisfies(c)",
				 *this, c);
    return gen_sys.satisfy(c);
  }
}

/*!
  Returns <CODE>true</CODE> if the generator \p g satisfies all
  the constraints representing \p *this, <CODE>false</CODE> otherwise.
*/
bool
PPL::Polyhedron::includes(const Generator& g) {
  if(is_empty())
    return false;
  if (space_dimension() == 0)
    throw_different_dimensions("PPL::Polyhedron::includes(g)",
				*this, g);
  else {
    if(!constraints_are_up_to_date())
      update_constraints();
    if (con_sys.num_columns() != g.size())
      throw_different_dimensions("PPL::Polyhedron::includes(g)",
				 *this, g);

    return con_sys.satisfies_all_constraints(g);
  }
  // Just to avoid a gcc warning.
  abort();
}

/*!
  This function computes the widening between \p *this and
  \p y and the result is assigned to the polyhedron \p *this.
  The resulting polyhedron has a system of constraints
  which is up-to-date for just those constraints that are common
  to both the polyhedra.
  For this function, it is required that the polyhedron \p y is contained
  in \p *this.
*/
void
PPL::Polyhedron::widening_assign(const Polyhedron& y) {
  Polyhedron& x = *this;

#ifndef NDEBUG
  {
    // We assume that y is contained or equal to x.
    Polyhedron x_copy = x;
    Polyhedron y_copy = y;
    assert(y_copy <= x_copy);
  }
#endif

  // For each x, x.widening_assign(z) = z.widening_assign(x) = z,
  // if z is a zero-dimensional polyhedron.
  if (x.space_dimension() == 0)
    return;
  else if (y.space_dimension() == 0) {
    x.set_zero_dim_univ();
    return;
  }
  if (y.is_empty())
    return;
  if (x.is_empty())
    return;
  // The polyhedrons must have the same dimensions if neither
  // `x' nor `y' is zero-dimensional.
  if (x.space_dimension() != y.space_dimension())
    throw_different_dimensions("PPL::Polyhedron::widening_assign(y)",
			       *this, y);
  // For this function, we need the minimal system of generators and
  // the saturation matrix `sat_g' of the polyhedron `y'. To obtain
  // the saturation matrix, the minimal system of constraints and
  // the minimal system of generators are necessary.
  y.minimize();
  // After the minimize, a polyhedron can become empty if the system
  // of constraints is unsatisfiable.
  // If the polyhedron `y' is empty, the widened polyhedron is `x'.
  if (y.is_empty())
    return;
  // We only need that the system of constraints is up-to-date
  // because
  // - if the system of constraints is insoluble, this means that
  //   also `y' is empty and so the resulting polyhedron is `x';
  // - if some constraints are redundant, these do not influence the
  //   resulting polyhedron.
  //   If a constraint is a combination of other two, it can be also in
  //   the resulting system, if the two constraints are common to both
  //   polyhedron. The redundant constraints is `redundant' also in
  //   the new polyhedron.
  //   If a constraint is redundant in the sense that it does not
  //   satisfy the saturation rule (see in the Introduction), it can not
  //   be put into the new system, because of the way that we use to
  //   choose the constraints.
  if (!x.constraints_are_up_to_date())
    x.update_constraints();
  // This function requires the saturation matrix `sat_g' of
  // the polyhedron `y' to choose which constraints of the
  // polyhedron `x' must be also constraints of the widened
  // polyhedron.
  if (!y.sat_g_is_up_to_date())
    y.update_sat_g();
  // The saturation matrix sat_g is copied in a temporary one:
  // in this way, the new saturation matrix can be sorted
  // without modifying the constant polyhedron `y'.
  SatMatrix tmp_sat_g = y.sat_g;
  tmp_sat_g.sort_rows();
  // Now, we start to build the system of constraints of the
  // widened polyhedron. The first constraint is the positivity
  // one: its presence is necessary to have a correct polyhedron.
  ConSys new_con_sys(1, x.con_sys.num_columns());
  // Hand-made positivity constraint.
  new_con_sys[0][0] = 1;
  new_con_sys[0].set_is_inequality();
  // A one-row ConSys is sorted.
  new_con_sys.set_sorted(true);
  // The size of `buffer' will reach sat.num_columns() bit.
  SatRow buffer;
  // We choose a constraint of `x' if its behavior with the
  // generators of `y' is the same that a constraints of `y' has.
  // This means that we verify if the saturation row built starting
  // from a constraint of `x' and all the generators of `y' is
  // a row of the saturation matrix `sat_g' of `y'(if it happens,
  // the constraint of `x' is also a constraint of `y').
  // In this way a constraint of `x' that does not verify the
  // saturation rule (see in the Introduction) can be put into the
  // resulting polyhedron, because `sat_g' is built staring from
  // a minimized polyhedron.
  for (size_t i = x.con_sys.num_rows(); i-- > 0; ) {
    buffer.clear();
    // The saturation row `buffer' is built considering the `i'-th
    // constraint of the polyhedron `x' and the generators of the
    // polyhedron `y'.
    for (size_t j = tmp_sat_g.num_columns(); j-- > 0; ) {
      int sp_sgn = sgn(y.gen_sys[j] * x.con_sys[i]);
      // We are assuming that y <= x.
      assert(sp_sgn >= 0);
      if (sp_sgn > 0)
	buffer.set(j);
    }
    // We verify if `buffer' is a row of the saturation matrix
    // `sat_g' of the polyhedron `y': to do this check, we use
    // the saturation matrix `tmp_sat_g' (that is sorted) in order to have
    // faster comparisons.
    if (tmp_sat_g.sorted_contains(buffer))
      new_con_sys.add_row(x.con_sys[i]);
  }

  std::swap(x.con_sys, new_con_sys);

  // Update the status of x.
  x.set_constraints_up_to_date();
  x.clear_constraints_minimized();
  x.clear_generators_up_to_date();

  assert(OK());
}


/*!
  This function adds to the widened polyhedron \p *this (obtained
  starting from \p *this and \p y) the constraints of the matrix
  \p, constraints that are verified by the polyhedron \p y and by
  the initial polyhedra \p *this.
  Returns <CODE>true</CODE> if the widened polyhedron \p *this is
  not empty.
*/
bool
PPL::Polyhedron::limited_widening_assign(const Polyhedron& y,
					 ConSys& cs) {
  Polyhedron& x = *this;

#ifndef NDEBUG
  {
    Polyhedron x_copy = x;
    Polyhedron y_copy = y;
    // We assume that y <= x.
    // This assertion is right (will never be violated).
    assert(y_copy <= x_copy);
  }
#endif

  if (y.is_empty())
    return !(x.is_empty());
  if (x.is_empty())
    return false;

  // For each x,
  // x.limited_widening_assign(z,...) = z.limited_widening_assign(x,...) = z,
  // if z is a zero-dimensional polyhedron.
  if (x.space_dimension() == 0)
    return true;
  else if (y.space_dimension() == 0) {
    x.set_zero_dim_univ();
    return true;
  }
  // The two polyhedrons and must have the same dimension, if neither
  // `x' nor `y' is zero-dimensional.
  if (x.space_dimension() != y.space_dimension())
    throw_different_dimensions("PPL::Polyhedron::limited_widening_assign(y,c)",
			       *this, y);
  // \p cs must be in the same space of the two polyhedrons:
  // this means that the number of variables of \p cs
  // is equal to the dimension of the polyhedrons.
  if (x.space_dimension() != cs.num_columns() - 1)
    throw_different_dimensions("PPL::Polyhedron::limited_widening_assign(y,c)",
			       *this, cs);

  y.minimize();
  // This function needs that the generators of `x' are up-to-date,
  // because we use these to choose which constraints of the matrix
  // \p cs must be added to the resulting polyhedron.
  if (!x.generators_are_up_to_date())
    x.update_generators();
  // After the minimize, a polyhedron can become empty if the system
  // of constraints is unsatisfiable.
  if (!y.is_empty()) {
    size_t con_nbrows = cs.num_rows();
    size_t nbrows = 0;
    for (size_t i = 0; i < con_nbrows; ++i) {
      // The constraints to add must be saturated by both the
      // polyhedrons. To choose them, we only use the generators
      // of the greater polyhedron `x', because those of `y'
      // are points also of `x' (`y' is contained in `x') and
      // so they verify the chosen constraints, too.
      GenSys_Con_Rel satisfy
	= x.gen_sys.satisfy(cs[i]);
      if (satisfy == ALL_SATURATE || satisfy == ALL_SATISFY)
	// The chosen constraints are put at the top of the
	// matrix \p cs.
	std::swap(cs[nbrows], cs[i]);
      ++nbrows;
    }
    x.widening_assign(y);

    // We erase the constraints that are not saturated or satisfied
    // by the generators of `x' and `y' and that are put at the end
    // of the matrix \p cs.
    cs.erase_to_end(nbrows);

    cs.sort_rows();
    x.con_sys.sort_rows();
    // The system of constraints of the resulting polyhedron is
    // composed by the constraints of the widened polyhedron `x'
    // and by those of the new `cs'.
    x.con_sys.merge_rows_assign(cs);
    // Only the system of constraints is up-to-date.
    x.set_constraints_up_to_date();
    x.clear_constraints_minimized();
    x.clear_generators_up_to_date();
  }
  if (x.is_empty())
    return false;
  else
    return true;
}

/*!
  Returns <CODE>true</CODE> if and only if \p *this represents
  the universal polyhedron: to be universal, \p this must contain
  just the positivity constraint.
*/
bool
PPL::Polyhedron::check_universe() const {
  if (!constraints_are_minimized())
    minimize();
  if (con_sys.num_rows() != 1)
    return false;
  if (!con_sys[0].is_inequality())
    return false;
  if (con_sys[0][0] <= 0)
    return false;
  for (size_t i = con_sys.num_columns(); i-- > 1; )
    if (con_sys[0][i] != 0)
      return false;
  return true;
}



/*!
  Checks if \p *this is really a polyhedron, i.e., excludes all the extreme
  cases.

  For this purpose we check several things and in particular we check
  whether
  - the system of constraints and the system of generators satisfy the
    dimensional rules,
  - the system of constraints and the system of generators are really
    minimized, when they are declared minimal.
*/
bool
PPL::Polyhedron::OK(bool check_not_empty) const {
  using std::endl;
  using std::cerr;

  // Check whether the saturation matrices are well-formed.
  if (!sat_c.OK())
    goto bomb;
  if (!sat_g.OK())
    goto bomb;

  // Checks the possible meaningful status combinations.
  if (!status.OK()) {
    cerr << "Wrong status!" << endl;
    goto bomb;
  }
  // An empty polyhedron is allowed.
  if (is_empty())
    return true;
  // A zero-dimensional polyhedron is allowed if
  // the system of constraint `con_sys' and the system of generators
  // `gen_sys' have no rows.
  if (space_dimension() == 0)
    if (!(gen_sys.num_rows() == 0 && con_sys.num_rows() == 0))
      cerr << "Polyhedron zeor-dimensional"
	   << endl
	   << "and with constraints or generator not clear"
	   << endl;

  // A polyhedron is defined by a system of constraints or a system
  // of generators.
  if (!(constraints_are_up_to_date() || generators_are_up_to_date())) {
    cerr << "Polyhedron not empty, not zero-dimensional"
	 << endl
	 << "and with neither constraints nor generators up-to-date!"
	 << endl;
    goto bomb;
  }
  // Here we check if the size of the matrices is consistent.
  // Let us suppose that all the matrices are up-to-date; this means:
  // `con_sys' : number of constraints x space_dimension() + 1
  // `gen_sys' : number of generators  x space_dimension() + 1
  // `sat_c'   : number of generators  x number of constraints
  // `sat_g'   : number of constraints x number of generators
  if (constraints_are_up_to_date()) {
    if (con_sys.num_columns() != space_dimension() + 1) {
      cerr << "Incompatible size! (con_sys and space_dim)";
      goto bomb;
    }
    if (sat_c_is_up_to_date())
      if (con_sys.num_rows() != sat_c.num_columns()) {
	cerr << "Incompatible size! (con_sys and sat_c)";
	goto bomb;
      }
    if (sat_g_is_up_to_date())
      if (con_sys.num_rows() != sat_g.num_rows()) {
	cerr << "Incompatible size! (con_sys and sat_g)";
	goto bomb;
      }
    if (generators_are_up_to_date())
      if (con_sys.num_columns() != gen_sys.num_columns()) {
	cerr << "Incompatible size! (con_sys and gen_sys)";
	goto bomb;
      }
  }

  if (generators_are_up_to_date()) {
    if (gen_sys.num_columns() != space_dimension() + 1) {
      cerr << "Incompatible size! (gen_sys and space_dim)";
      goto bomb;
    }
    if (sat_c_is_up_to_date())
      if (gen_sys.num_rows() != sat_c.num_rows()) {
	cerr << "Incompatible size! (gen_sys and sat_c)";
	goto bomb;
      }
    if (sat_g_is_up_to_date())
      if (gen_sys.num_rows() != sat_g.num_columns()) {
	cerr << "Incompatible size! (gen_sys and sat_g)";
	goto bomb;
      }
  }

  if (generators_are_up_to_date()) {
    // Check if the system of generators is well-formed.
    if (!gen_sys.OK())
      goto bomb;

    if (generators_are_minimized()) {
      // If the system of generators is minimized, the number of lines,
      // rays and vertices of the polyhedron must be the same
      // of the temporary minimized one. If it does not happen
      // the polyhedron is not ok.
      ConSys new_con_sys;
      GenSys copy_of_gen_sys = gen_sys;
      SatMatrix new_sat_c;
      minimize(false, copy_of_gen_sys, new_con_sys, new_sat_c);
      size_t copy_num_lines = copy_of_gen_sys.num_lines();
      if (!(gen_sys.num_rows() == copy_of_gen_sys.num_rows() &&
	    gen_sys.num_lines() == copy_num_lines &&
	    gen_sys.num_rays() == copy_of_gen_sys.num_rays())) {
	cerr << "Generators are declared minimized, but they are not!"
	     << endl
	     << "Here is the minimized form of the generators:"
	     << endl
	     << copy_of_gen_sys
	     << endl;
	goto bomb;
      }
      // A minimal system of generators is unique up to positive scaling if
      // the cone is pointed.
      // So, we verify if the cone is pointed (i.e. there are no lines)
      // and after normalizing and sorting a copy of the matrix `gen_sys'
      // of the polyhedron (we use a copy not to modify the polyhedron's
      // matrix) and the matrix `copy_of_gen_sys' that has been just
      // minimized, we check if the two matrices are identical. If they
      // are different it means that the generators of the polyhedron
      // are declared minimized, but they are not: the polyhedron is not
      // ok.
      if (copy_num_lines == 0) {
	copy_of_gen_sys.normalize();
	copy_of_gen_sys.sort_rows();
	GenSys tmp_gen = gen_sys;
	tmp_gen.normalize();
	tmp_gen.sort_rows();
	if (copy_of_gen_sys != tmp_gen) {
	  cerr << "Generators are declared minimized, but they are not!"
	       << endl
	       << "(we are in the case:"
	       << endl
	       << "dimension of lineality space equal to 0)"
	       << endl
	       << "Here is the minimized form of the generators:"
	       << endl
	       << copy_of_gen_sys
	       << endl;
	    goto bomb;
	}
      }
    }
  }

  if (constraints_are_up_to_date()) {
    // Check if the system of constraints is well-formed.
    if (!con_sys.OK())
      goto bomb;

    ConSys copy_of_con_sys = con_sys;
    GenSys new_gen_sys;
    SatMatrix new_sat_g;

    if (minimize(true, copy_of_con_sys, new_gen_sys, new_sat_g)) {
      if (check_not_empty) {
	// Want to know the satisfiability of the constraints.
	cerr << "Insoluble system of constraints!" << endl;
	goto bomb;
      }
      else
	// The polyhedron is empty, there is nothing else to check.
	return true;
    }

    if (constraints_are_minimized()) {
      // If the constraints are minimized, the number of equalities
      // and of inequalities of the system of the polyhedron must be
      // the same of the temporary minimized one.
      // If it does not happen, the polyhedron is not ok.
      if (!(con_sys.num_rows() == copy_of_con_sys.num_rows() &&
  	    con_sys.num_equalities() == copy_of_con_sys.num_equalities())) {
	cerr << "Constraints are declared minimized, but they are not!"
	     << endl
	     << "Here is the minimized form of the constraints:"
	     << endl
	     << copy_of_con_sys
	     << endl;
	goto bomb;
      }
      // The matrix `copy_of_con_sys' has the form that is obtained
      // after the functions gauss() and back_substitute().
      // A system of constraints can be minimal even if it does not
      // have this form. So, to verify if the polyhedron is correct,
      // we copy the matrix `con_sys' in a temporary one that then
      // is modified using the functions gauss() and back_substitute().
      // If the temporary matrix and `copy_of_con_sys' are different,
      // the polyhedron is not ok.
      copy_of_con_sys.normalize();
      copy_of_con_sys.sort_rows();
      ConSys tmp_con = con_sys;
      tmp_con.sort_rows();
      tmp_con.back_substitute(tmp_con.gauss());
      tmp_con.normalize();
      tmp_con.sort_rows();
      if (tmp_con != copy_of_con_sys) {
	cerr << "Constraints are declared minimized, but they are not!"
	     << endl
	     << "Here is the minimized form of the constraints:"
	     << endl
	     << copy_of_con_sys
	     << endl;
	goto bomb;
      }
    }
  }

  // If the polyhedron has both the system of constraints and
  // the system of generators, they must have the same number of columns.
  if (constraints_are_up_to_date() && generators_are_up_to_date()
      && con_sys.num_columns() != gen_sys.num_columns()) {
    cerr << "Constraints and generators are dimension-incompatible:" << endl
	 << con_sys.num_columns()-1 << " and " << gen_sys.num_columns()-1
	 << endl;
    goto bomb;
  }

  if (sat_c_is_up_to_date())
    for (size_t i = sat_c.num_rows(); i-- > 0; ) {
      Generator tmp_gen = gen_sys[i];
      SatRow tmp_sat = sat_c[i];
      for (size_t j = sat_c.num_columns(); j-- > 0; )
	if (sgn(tmp_gen * con_sys[j]) != tmp_sat[j] ) {
	  cerr << "sat_c is declared up-to-date, but it is not!" << endl;
	  goto bomb;
	}
    }

  if (sat_g_is_up_to_date())
    for (size_t i = sat_g.num_rows(); i-- > 0; ) {
      Constraint tmp_con = con_sys[i];
      SatRow tmp_sat = sat_g[i];
      for (size_t j = sat_g.num_columns(); j-- > 0; )
	if (sgn(tmp_con * gen_sys[j]) != tmp_sat[j] ) {
	  cerr << "sat_g is declared up-to-date, but it is not!" << endl;
	  goto bomb;
	}
    }

  return true;

 bomb:
  cerr << "Here is the guilty polyhedron:"
       << endl
       << *this
       << endl;
  return false;
}
