/* Grid class implementation
   (non-inline operators that may change the dimension of the vector space).
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "Grid.defs.hh"
#include <cassert>

#define BE_LAZY 1

namespace PPL = Parma_Polyhedra_Library;

// Used for add_space_dimensions_and_embed.
inline void
PPL::Grid::add_space_dimensions(Congruence_System& cgs,
				Generator_System& gs,
				dimension_type dims) {
  assert(cgs.num_columns() - 1 == gs.num_columns());
  assert(dims > 0);

  dimension_type tem = cgs.num_columns() - 1;
  cgs.add_zero_columns(dims);
  // Move the moduli.
  cgs.swap_columns(tem, tem + dims);

  if (congruences_are_minimized() || generators_are_minimized())
    dim_kinds.resize(tem + dims, CON_VIRTUAL /* a.k.a. LINE */);

  gs.add_zero_rows_and_columns(dims, dims,
			       Linear_Row::Flags(NECESSARILY_CLOSED,
						 Linear_Row::LINE_OR_EQUALITY));
  dimension_type num_rows = gs.num_rows();
  dimension_type col_num = gs.num_columns() - dims;
  for (dimension_type row_num = num_rows - dims;
       row_num < num_rows; ++row_num, ++col_num) {
    Generator& gen = gs[row_num];
    gen[col_num] = 1;
  }
  gen_sys.unset_pending_rows();
}

// Used for add_space_dimensions_and_project.
inline void
PPL::Grid::add_space_dimensions(Generator_System& gs,
				Congruence_System& cgs,
				dimension_type dims) {
  assert(cgs.num_columns() - 1 == gs.num_columns());
  assert(dims > 0);

  dimension_type num_cols = cgs.num_columns() - 1;
  cgs.add_zero_rows_and_columns(dims, dims, Row::Flags());
  // Move the moduli.
  dimension_type last_col = num_cols + dims;
  cgs.swap_columns(num_cols, last_col);
  dimension_type num_rows = cgs.num_rows();
  dimension_type col_num = cgs.num_columns() - dims - 1;
  for (dimension_type row_num = num_rows - dims;
       row_num < num_rows; ++row_num, ++col_num) {
    Congruence& cg = cgs[row_num];
    cg.set_is_equality();
    cg[col_num] = 1;
  }
  // TODO: consider the option of staying minimal by prepending the
  //       new rows
  clear_congruences_minimized();

  gs.add_zero_columns(dims);
  if (generators_are_minimized())
    dim_kinds.resize(last_col, EQUALITY /* a.k.a GEN_VIRTUAL */);
}

// (o is a point)       y
//
//                      |   |   |
//                 =>   |   |   |
//                      |   |   |
// o---o---o-- x        |---|---|-- x
// 0 1 2 3 4 5          0 1 2 3 4 5
//     Q^1                   Q^2
void
PPL::Grid::add_space_dimensions_and_embed(dimension_type m) {
  if (m == 0)
    return;

  // The space dimension of the resulting grid must be at most the
  // maximum allowed space dimension.
  if (m > max_space_dimension() - space_dimension())
    throw_space_dimension_overflow("add_space_dimensions_and_embed(m)",
				   "adding m new space dimensions exceeds "
				   "the maximum allowed space dimension");

  // Adding dimensions to an empty grid is obtained by adjusting
  // `space_dim' and clearing `con_sys' (since it can contain the
  // integrality congruence of the current dimension).
  if (marked_empty()) {
    space_dim += m;
    con_sys.clear();
    gen_sys.clear();
    return;
  }

  // The case of a zero-dimension space grid.
  if (space_dim == 0) {
    // Since it is not empty, it has to be the universe grid.
    assert(status.test_zero_dim_univ());
    // Swap *this with a newly created `m'-dimensional universe grid.
    Grid gr(m, UNIVERSE);
    swap(gr);
    return;
  }

  // To embed an n-dimension space grid in a (n+m)-dimension space, we
  // add `m' zero-columns to the rows in the system of congruences; in
  // contrast, the system of generators needs additional rows,
  // corresponding to the vectors of the canonical basis for the added
  // dimensions. That is, for each new dimension we add the line
  // having that direction. This is done by invoking the function
  // add_space_dimensions().
  if (congruences_are_up_to_date())
    if (generators_are_up_to_date())
      // Adds rows and/or columns to both matrices.
      add_space_dimensions(con_sys, gen_sys, m);
    else {
      // Only congruences are up-to-date, so modify only them.
      con_sys.add_zero_columns(m);
      dimension_type size = con_sys.num_columns() - 1;
      // Move the moduli.
      con_sys.swap_columns(size - m, size);
      if (congruences_are_minimized())
	dim_kinds.resize(size, CON_VIRTUAL);
    }
  else {
    // Only generators are up-to-date, so modify only them.
    assert(generators_are_up_to_date());
    // A very long method invocation follows.
    gen_sys.add_zero_rows_and_columns
      (m, m, Linear_Row::Flags(NECESSARILY_CLOSED,
			       Linear_Row::LINE_OR_EQUALITY));
    dimension_type num_rows = gen_sys.num_rows();
    dimension_type col_num = gen_sys.num_columns() - m;
    for (dimension_type row_num = num_rows - m;
	 row_num < num_rows; ++row_num, ++col_num) {
      Generator& gen = gen_sys[row_num];
      gen[col_num] = 1;
    }
    // The grid does not support pending rows.
    gen_sys.unset_pending_rows();
    if (generators_are_minimized())
      dim_kinds.resize(col_num, LINE);
  }
  // Update the space dimension.
  space_dim += m;

  // Note: we do not check for satisfiability, because the system of
  // congruences may be unsatisfiable.
  assert(OK());
}

// (o is a point)       y
//
//
//                 =>
//
// o---o---o-- x        o---o---o-- x
// 0 1 2 3 4 5          0 1 2 3 4 5
//     Q^1                   Q^2
void
PPL::Grid::add_space_dimensions_and_project(dimension_type m) {
  if (m == 0)
    return;

  // The space dimension of the resulting grid should be at most the
  // maximum allowed space dimension.
  if (m > max_space_dimension() - space_dimension())
    throw_space_dimension_overflow("add_space_dimensions_and_project(m)",
				   "adding m new space dimensions exceeds "
				   "the maximum allowed space dimension");

  // Adding dimensions to an empty grid is obtained by merely
  // adjusting `space_dim'.
  if (marked_empty()) {
    space_dim += m;
    con_sys.clear();
    gen_sys.clear();
    return;
  }

  if (space_dim == 0) {
    assert(status.test_zero_dim_univ());
    // Swap *this with a newly created `n'-dimensional universe grid.
    Grid gr(m, UNIVERSE);
    swap(gr);
    return;
  }

  // To project an n-dimension space grid in a (n+m)-dimension space,
  // we just add to the system of generators `m' zero-columns; in
  // contrast, in the system of congruences, new rows are needed in
  // order to avoid embedding the old grid in the new space.  Thus,
  // for each new dimensions `x[k]', we add the constraint x[k] = 0;
  // this is done by invoking the function add_space_dimensions()
  // giving the system of constraints as the second argument.
  if (congruences_are_up_to_date())
    if (generators_are_up_to_date())
      // Adds rows and/or columns to both matrices.
      add_space_dimensions(gen_sys, con_sys, m);
    else {
      // Only congruences are up-to-date so modify only them.
      dimension_type num_cols = con_sys.num_columns() - 1;
      con_sys.add_zero_rows_and_columns(m, m, Row::Flags());
      // Swap the modulus and the new last column.
      con_sys.swap_columns(num_cols, num_cols + m);
      // Limit the added dimensions.
      dimension_type num_rows = con_sys.num_rows();
      dimension_type col_num = con_sys.num_columns() - m - 1;
      for (dimension_type row_num = num_rows - m;
	   row_num < num_rows; ++row_num, ++col_num) {
	Congruence& cg = con_sys[row_num];
	cg.set_is_equality();
	cg[col_num] = 1;
      }
      // TODO: consider the option of staying minimal by prepending
      //       the new rows, as in add_space_dimensions(gs,cgs,m).
      clear_congruences_minimized();
    }
  else {
    // Only generators are up-to-date so modify only them.
    assert(generators_are_up_to_date());
    gen_sys.add_zero_columns(m);
    if (generators_are_minimized())
      dim_kinds.resize(gen_sys.num_columns(), EQUALITY);
  }
  // Now update the space dimension.
  space_dim += m;

  // Note: we do not check for satisfiability, because the system of
  // congruences may be unsatisfiable.
  assert(OK());
}

void
PPL::Grid::concatenate_assign(const Grid& y) {
  // The space dimension of the resulting grid must be at most the
  // maximum allowed space dimension.
  if (y.space_dim > max_space_dimension() - space_dimension())
    throw_space_dimension_overflow("concatenate_assign(y)",
				   "concatenation exceeds the maximum "
				   "allowed space dimension");

  const dimension_type added_columns = y.space_dim;

  // If `*this' or `y' are empty polyhedra, it is sufficient to adjust
  // the dimension of the space.
  if (marked_empty() || y.marked_empty()) {
    space_dim += added_columns;
    set_empty();
    return;
  }

  // If `y' is a non-empty 0-dim space grid, the result is `*this'.
  if (added_columns == 0)
    return;

  // If `*this' is a non-empty 0-dim space grid, the result is `y'.
  if (space_dim == 0) {
    *this = y;
    return;
  }

  // TODO: this implementation is just an executable specification.
  Congruence_System cgs = y.congruences();

  if (!congruences_are_up_to_date())
    // FIX check if now empty? (also in ph?)
    update_congruences();

  // The matrix for the new system of congruences is obtained by
  // leaving the old system in the upper left-hand side and placing
  // the constraints of `cgs' in the lower right-hand side.
  dimension_type old_num_rows = con_sys.num_rows();
  dimension_type old_num_columns = con_sys.num_columns();
  dimension_type added_rows = cgs.num_rows();

  // We already dealt with the cases of an empty or zero-dim `y' grid;
  // also, `cgs' contains the low-level constraints, at least.
  assert(added_rows > 0 && added_columns > 0);

  con_sys.add_zero_rows_and_columns(added_rows, added_columns,
				    Row::Flags());
  dimension_type cgs_num_columns = cgs.num_columns();
  dimension_type old_modi = old_num_columns - 1;
  dimension_type modi = con_sys.num_columns() - 1;

  // Swap the modulus and the new last column, in the old rows.
  for (dimension_type i = 0; i < old_num_rows; ++i) {
    Congruence& cg = con_sys[i];
    std::swap(cg[old_modi], cg[modi]);
  }

  // Move the congruences from `cgs' to `con_sys', shifting the
  // coefficients along into the appropriate columns.
  for (dimension_type i = added_rows; i-- > 0; ) {
    Congruence& cg_old = cgs[i];
    Congruence& cg_new = con_sys[old_num_rows + i];
    // The inhomogeneous term is moved to the same column.
    std::swap(cg_new[0], cg_old[0]);
    // All homogeneous terms are shifted by `space_dim' columns.
    for (dimension_type j = 1; j < cgs_num_columns; ++j)
      std::swap(cg_old[j], cg_new[space_dim + j]);
  }

  clear_congruences_minimized();
  clear_generators_up_to_date();
  // Update space dimension.
  space_dim += added_columns;

  // Check that the system is OK, taking into account that the system
  // of congruences may now be empty.
  assert(OK());
}

void
PPL::Grid::remove_space_dimensions(const Variables_Set& to_be_removed) {
  // The removal of no dimensions from any grid is a no-op.  This case
  // also captures the only legal removal of dimensions from a grid in
  // a 0-dim space.
  if (to_be_removed.empty()) {
    assert(OK());
    return;
  }

  // Dimension-compatibility check: the variable having maximum space
  // dimension is the one occurring last in the set.
  const dimension_type
    min_space_dim = to_be_removed.rbegin()->space_dimension();
  if (space_dim < min_space_dim)
    throw_dimension_incompatible("remove_space_dimensions(vs)", min_space_dim);

  const dimension_type new_space_dim = space_dim - to_be_removed.size();

  if (marked_empty()
      || (!generators_are_up_to_date() && !update_generators())) {
    // FIX?  mark empty? keep empty con_sys?
    con_sys.clear();
    gen_sys.clear();
    // Update the space dimension.
    space_dim = new_space_dim;
    assert(OK());
    return;
  }

  // Removing _all_ dimensions from a non-empty grid obtains the
  // zero-dimensional universe grid.
  if (new_space_dim == 0) {
    set_zero_dim_univ();
    return;
  }

  // FIXME: provide a method in Linear_System that removes a set
  // of columns and restores strong-normalization only at the end.

  // For each variable to be removed, replace the corresponding column
  // by shifting left the columns to the right that will be kept.
  Variables_Set::const_iterator tbr = to_be_removed.begin();
  Variables_Set::const_iterator tbr_end = to_be_removed.end();
  dimension_type dst_col = tbr->space_dimension();
  dimension_type src_col = dst_col + 1;
  for (++tbr; tbr != tbr_end; ++tbr) {
    dimension_type tbr_col = tbr->space_dimension();
    // Move all columns in between to the left.
    while (src_col < tbr_col)
      // FIXME: consider whether Linear_System must have a swap_columns()
      // method.  If the answer is "no", remove this Matrix:: qualification.
      gen_sys.Matrix::swap_columns(dst_col++, src_col++);
    ++src_col;
  }
  // Move any remaining columns.
  const dimension_type gen_sys_num_columns = gen_sys.num_columns();
  while (src_col < gen_sys_num_columns)
    // FIXME: consider whether Linear_System must have a swap_columns()
    // method.  If the answer is "no", remove this Matrix:: qualification.
    gen_sys.Matrix::swap_columns(dst_col++, src_col++);

  // The number of remaining columns is `dst_col'.
  gen_sys.Matrix::remove_trailing_columns(gen_sys_num_columns - dst_col);

  // FIX confirm OK to leave parameters that have zeros in all cols
  // FIX confirm OK if vars include first col

  clear_congruences_up_to_date();
  clear_generators_minimized();

  // Update the space dimension.
  space_dim = new_space_dim;

  assert(OK(true));
}

void
PPL::Grid::remove_higher_space_dimensions(dimension_type new_dimension) {
  // Dimension-compatibility check.
  if (new_dimension > space_dim)
    throw_dimension_incompatible("remove_higher_space_dimensions(nd)",
				 new_dimension);

  // The removal of no dimensions from any grid is a no-op.
  // Note that this case also captures the only legal removal of
  // dimensions from a grid in a 0-dim space.
  if (new_dimension == space_dim) {
    assert(OK());
    return;
  }

  if (marked_empty()
      || (!generators_are_up_to_date() && !update_generators())) {
    // Removing dimensions from the empty grid just updates the space
    // dimension.
    space_dim = new_dimension;
    // FIX?  mark empty? keep empty con_sys?
    con_sys.clear();
    gen_sys.clear();
    assert(OK());
    return;
  }

  if (new_dimension == 0) {
    // Removing all dimensions from a non-empty grid just returns the
    // zero-dimensional universe grid.
    set_zero_dim_univ();
    return;
  }

  gen_sys.Matrix::remove_trailing_columns(space_dim - new_dimension);
  if (generators_are_minimized()) {
    gen_sys.erase_to_end(new_dimension + 1);
    gen_sys.unset_pending_rows();
    dim_kinds.erase(dim_kinds.begin() + new_dimension + 1, dim_kinds.end());
  }

  clear_congruences_up_to_date();

  // Update the space dimension.
  space_dim = new_dimension;

  assert(OK(true));
}

void
PPL::Grid::expand_space_dimension(Variable var, dimension_type m) {
  // FIXME: this implementation is _really_ an executable specification.

  // `var' must be one of the dimensions of the vector space.
  if (var.space_dimension() > space_dim)
    throw_dimension_incompatible("expand_space_dimension(v, m)", "v", var);

  // Adding 0 dimensions leaves the same grid.
  if (m == 0)
    return;

  // The resulting space dimension must be at most the maximum.
  if (m > max_space_dimension() - space_dimension())
    throw_space_dimension_overflow("expand_dimension(v, m)",
				   "adding m new space dimensions exceeds "
				   "the maximum allowed space dimension");

  // Save the number of dimensions before adding new ones.
  dimension_type old_dim = space_dim;

  // Add the required new dimensions.
  add_space_dimensions_and_embed(m);

  const dimension_type src_d = var.id();
  const Congruence_System& cgs = congruences();
  Congruence_System new_congruences;
  for (Congruence_System::const_iterator i = cgs.begin(),
	 cgs_end = cgs.end(); i != cgs_end; ++i) {
    const Congruence& cg = *i;

    // Only consider congruences that constrain `var'.
    if (cg.coefficient(var) == 0)
      continue;

    // Each relevant congruence results in `m' new congruences.
    for (dimension_type dst_d = old_dim; dst_d < old_dim+m; ++dst_d) {
      Linear_Expression e;
      for (dimension_type j = old_dim; j-- > 0; )
	e +=
	  cg.coefficient(Variable(j))
	  * (j == src_d ? Variable(dst_d) : Variable(j));
      new_congruences.insert_verbatim((e + cg.inhomogeneous_term() %= 0)
				      / cg.modulus());
    }
  }
  add_congruences(new_congruences);
  assert(OK());
}
#if 0
void
PPL::Grid::fold_space_dimensions(const Variables_Set& to_be_folded,
				 Variable var) {
  // FIXME: this implementation is _really_ an executable specification.

  // `var' should be one of the dimensions of the grid.
  if (var.space_dimension() > space_dim)
    throw_dimension_incompatible("fold_space_dimensions(tbf, v)", "v", var);

  // The folding of no dimensions is a no-op.
  if (to_be_folded.empty())
    return;

  // All variables in `to_be_folded' should be dimensions of the grid.
  if (to_be_folded.rbegin()->space_dimension() > space_dim)
    throw_dimension_incompatible("fold_space_dimensions(tbf, v)",
				 "*tbf.rbegin()",
				 *to_be_folded.rbegin());

  // Moreover, `var' should not occur in `to_be_folded'.
  if (to_be_folded.find(var) != to_be_folded.end())
    throw_invalid_argument("fold_space_dimensions(tbf, v)",
			   "v should not occur in tbf");

  for (Variables_Set::const_iterator i = to_be_folded.begin(),
	 tbf_end = to_be_folded.end(); i != tbf_end; ++i) {
    Grid copy = *this;
    copy.affine_image(var, Linear_Expression(*i));
    poly_hull_assign(copy);
  }
  remove_space_dimensions(to_be_folded);
  assert(OK());
}
#endif
