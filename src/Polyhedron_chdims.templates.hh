/* Polyhedron class implementation
   (non-inline template operators that may change the dimension of the vector
   space).
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

#ifndef PPL_Polyhedron_chdims_templates_hh
#define PPL_Polyhedron_chdims_templates_hh 1

namespace Parma_Polyhedra_Library {

template <typename Linear_System1, typename Linear_System2>
void
Polyhedron::add_space_dimensions(Linear_System1& sys1,
                                 Linear_System2& sys2,
                                 Bit_Matrix& sat1,
                                 Bit_Matrix& sat2,
                                 dimension_type add_dim) {

  typedef typename Linear_System2::internal_row_type sys2_internal_row_type;

  PPL_ASSERT(sys1.topology() == sys2.topology());
  PPL_ASSERT(sys1.num_columns() == sys2.num_columns());
  PPL_ASSERT(add_dim != 0);

  sys1.add_zero_columns(add_dim);
  dimension_type old_index = sys2.first_pending_row();
  sys2.add_universe_rows_and_columns(add_dim);
  // The added rows are in the non-pending part.
  sys2.set_index_first_pending_row(old_index + add_dim);

  // The resulting saturation matrix will be as follows:
  // from row    0    to      add_dim-1       : only zeroes
  //          add_dim     add_dim+num_rows-1  : old saturation matrix

  // In fact all the old generators saturate all the new constraints
  // because the polyhedron has not been embedded in the new space.
  sat1.resize(sat1.num_rows() + add_dim, sat1.num_columns());
  // The old matrix is moved to the end of the new matrix.
  for (dimension_type i = sat1.num_rows() - add_dim; i-- > 0; )
    std::swap(sat1[i], sat1[i+add_dim]);
  // Computes the "sat_c", too.
  sat2.transpose_assign(sat1);

  if (!sys1.is_necessarily_closed()) {
    // Moving the epsilon coefficients to the new last column.
    dimension_type new_eps_index = sys1.num_columns() - 1;
    dimension_type old_eps_index = new_eps_index - add_dim;
    // This swap preserves sortedness of `sys1'.
    sys1.swap_columns(old_eps_index, new_eps_index);

    // Try to preserve sortedness of `sys2'.
    if (!sys2.is_sorted())
      sys2.swap_columns(old_eps_index, new_eps_index);
    else {
      Swapping_Vector<sys2_internal_row_type> rows;
      sys2.release_rows(rows);

      for (dimension_type i = rows.size(); i-- > add_dim; ) {
        sys2_internal_row_type& r = rows[i];
        std::swap(r[old_eps_index], r[new_eps_index]);
      }
      // The upper-right corner of `sys2' contains the J matrix:
      // swap coefficients to preserve sortedness.
      for (dimension_type i = add_dim; i-- > 0; ++old_eps_index) {
        sys2_internal_row_type& r = rows[i];
        std::swap(r[old_eps_index], r[old_eps_index + 1]);
      }

      sys2.take_ownership_of_rows(rows);
      sys2.set_sorted(true);
    }
    // NOTE: since we swapped columns in both `sys1' and `sys2',
    // no swapping is required for `sat1' and `sat2'.
  }
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Polyhedron_chdims_templates_hh)
