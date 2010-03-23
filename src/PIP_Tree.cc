/* PIP_Tree related class implementation: non-inline functions.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

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
#include "PIP_Tree.defs.hh"
#include "PIP_Problem.defs.hh"

#include <algorithm>
#include <memory>
#include <map>

namespace Parma_Polyhedra_Library {

namespace {

// Calculate positive modulo of x % y
inline void
mod_assign(Coefficient& z,
           Coefficient_traits::const_reference x,
           Coefficient_traits::const_reference y) {
  z = x % y;
  if (z < 0)
    z += y;
}

#ifndef PPL_SPARSE_BACKEND_SLOW_INSERTIONS

// Compute x += c * y
inline void
add_mul_assign_row(PIP_Tree_Node::matrix_row_reference_type x,
                   Coefficient_traits::const_reference c,
                   PIP_Tree_Node::matrix_row_const_reference_type y) {
  PIP_Tree_Node::matrix_row_iterator i = x.begin();
  PIP_Tree_Node::matrix_row_iterator last_i = x.begin();
  PIP_Tree_Node::matrix_row_iterator i_end = x.end();
  PIP_Tree_Node::matrix_const_row_const_iterator j = y.begin();
  PIP_Tree_Node::matrix_const_row_const_iterator j_end = y.end();
  if (i != i_end && j != j_end) {
    if ((*i).first == (*j).first) {
      add_mul_assign((*i).second, c, (*j).second);
      last_i = i;
      ++i;
      ++j;
    } else
      if ((*i).first < (*j).first) {
        // We should do (*i).second += c*0, so do nothing.
        last_i = i;
        ++i;
      } else {
        last_i = x.find_create((*j).first, (*j).second);
        (*last_i).second *= c;
#ifdef PPL_SPARSE_BACKEND_INVALIDATES_REFERENCES
        i = last_i;
        ++i;
        i_end = x.end();
        if (& static_cast<PIP_Tree_Node::
                          matrix_row_const_reference_type>(x) == &y) {
          j = last_i;
          j_end = y.end();
        }
#endif
        ++j;
      }
  } else
    if (j != j_end) {
      last_i = x.find_create((*j).first, (*j).second);
      neg_assign((*last_i).second);
#ifdef PPL_SPARSE_BACKEND_INVALIDATES_REFERENCES
      i = last_i;
      ++i;
      i_end = x.end();
      if (& static_cast<PIP_Tree_Node::
                        matrix_row_const_reference_type>(x) == &y) {
        j = last_i;
        j_end = y.end();
      }
#endif
      ++j;
    }
  while (i != i_end && j != j_end)
    if ((*i).first == (*j).first) {
      add_mul_assign((*i).second, c, (*j).second);
      last_i = i;
      ++i;
      ++j;
    } else
      if ((*i).first < (*j).first) {
        // We should do (*i).second += c*0, so do nothing.
        last_i = i;
        ++i;
      } else {
        last_i = x.find_create((*j).first ,(*j).second, last_i);
        (*last_i).second *= c;
#ifdef PPL_SPARSE_BACKEND_INVALIDATES_REFERENCES
        i = last_i;
        ++i;
        i_end = x.end();
        if (& static_cast<PIP_Tree_Node::
                          matrix_row_const_reference_type>(x) == &y) {
          j = last_i;
          j_end = y.end();
        }
#endif
        ++j;
      }
  while (j != j_end) {
    last_i = x.find_create((*j).first, (*j).second, last_i);
    (*last_i).second *= c;
    ++j;
  }
}

#else // !defined(PPL_SPARSE_BACKEND_SLOW_INSERTIONS)

// Compute x += c * y
inline void
add_mul_assign_row(PIP_Tree_Node::matrix_row_reference_type x,
                   Coefficient_traits::const_reference c,
                   PIP_Tree_Node::matrix_row_const_reference_type y) {
  PIP_Tree_Node::matrix_row_copy_type row(x.size());
  PIP_Tree_Node::matrix_row_copy_iterator itr = row.end();
  PIP_Tree_Node::matrix_row_const_iterator i = x.begin();
  PIP_Tree_Node::matrix_row_const_iterator i_end = x.end();
  PIP_Tree_Node::matrix_const_row_const_iterator j = y.begin();
  PIP_Tree_Node::matrix_const_row_const_iterator j_end = y.end();
  if (i == i_end && j == j_end)
    return;
  if (j == j_end
      || (i != i_end && (*i).first < (*j).first)) {
    itr = row.find_create(*i);
    ++i;
  } else
      if (i == i_end
          || (j != j_end && (*i).first > (*j).first)) {
        itr = row.find_create(*j);
        (*itr).second *= c;
        ++j;
      } else {
        PPL_ASSERT(i != i_end);
        PPL_ASSERT(j != j_end);
        PPL_ASSERT((*i).first == (*j).first);
        itr = row.find_create(*j);
        (*itr).second *= c;
        (*itr).second += (*i).second;
        ++i;
        ++j;
      }
  PPL_ASSERT(itr != row.end());
  while (i != i_end && j != j_end) {
    if ((*i).first < (*j).first) {
      itr = row.find_create(*i,itr);
      ++i;
    } else {
      if ((*i).first > (*j).first) {
        itr = row.find_create(*j,itr);
        (*itr).second *= c;
        ++j;
      } else {
        PPL_ASSERT((*i).first == (*j).first);
        itr = row.find_create(*j,itr);
        (*itr).second *= c;
        (*itr).second += (*i).second;
        ++i;
        ++j;
      }
    }
  }
  while (i != i_end) {
    itr = row.find_create(*i,itr);
    ++i;
  }
  PPL_ASSERT(i == i_end);
  while (j != j_end) {
    itr = row.find_create(*j,itr);
    (*itr).second *= c;
    ++j;
  }
  PPL_ASSERT(j == j_end);
  std::swap(row,x);
}

#endif // !defined(PPL_SPARSE_BACKEND_SLOW_INSERTIONS)

#ifndef PPL_SPARSE_BACKEND_SLOW_INSERTIONS

// Compute x -= y
inline void
sub_assign(PIP_Tree_Node::matrix_row_reference_type x,
           PIP_Tree_Node::matrix_row_const_reference_type y) {
  PIP_Tree_Node::matrix_row_iterator i = x.begin();
  PIP_Tree_Node::matrix_row_iterator last_i = x.begin();
  PIP_Tree_Node::matrix_row_iterator i_end = x.end();
  PIP_Tree_Node::matrix_const_row_const_iterator j = y.begin();
  PIP_Tree_Node::matrix_const_row_const_iterator j_end = y.end();
  if (i != i_end && j != j_end) {
    if ((*i).first == (*j).first) {
      (*i).second -= (*j).second;
      last_i = i;
      ++i;
      ++j;
    } else
      if ((*i).first < (*j).first) {
        // We should do (*i).second -= 0, so do nothing.
        last_i = i;
        ++i;
      } else {
        last_i = x.find_create((*j).first, (*j).second);
        neg_assign((*last_i).second);
#ifdef PPL_SPARSE_BACKEND_INVALIDATES_REFERENCES
        i = last_i;
        ++i;
        i_end = x.end();
        if (& static_cast<PIP_Tree_Node::
                          matrix_row_const_reference_type>(x) == &y) {
          j = last_i;
          j_end = y.end();
        }
#endif
        ++j;
      }
  } else
    if (j != j_end) {
      last_i = x.find_create((*j).first, (*j).second);
      neg_assign((*last_i).second);
#ifdef PPL_SPARSE_BACKEND_INVALIDATES_REFERENCES
      i = last_i;
      ++i;
      i_end = x.end();
      if (& static_cast<PIP_Tree_Node::
                        matrix_row_const_reference_type>(x) == &y) {
        j = last_i;
        j_end = y.end();
      }
#endif
      ++j;
    }
  while (i != i_end && j != j_end)
    if ((*i).first == (*j).first) {
      (*i).second -= (*j).second;
      last_i = i;
      ++i;
      ++j;
    } else
      if ((*i).first < (*j).first) {
        // We should do (*i).second += c*0, so do nothing.
        last_i = i;
        ++i;
      } else {
        last_i = x.find_create((*j).first, (*j).second, last_i);
        neg_assign((*last_i).second);
#ifdef PPL_SPARSE_BACKEND_INVALIDATES_REFERENCES
        i = last_i;
        ++i;
        i_end = x.end();
        if (& static_cast<PIP_Tree_Node::
                          matrix_row_const_reference_type>(x) == &y) {
          j = last_i;
          j_end = y.end();
        }
#endif
        ++j;
      }
  while (j != j_end) {
    last_i = x.find_create((*j).first, (*j).second, last_i);
    neg_assign((*last_i).second);
    ++j;
  }
}

#else // !defined(PPL_SPARSE_BACKEND_SLOW_INSERTIONS)

// Compute x -= y
inline void
sub_assign(PIP_Tree_Node::matrix_row_reference_type x,
           PIP_Tree_Node::matrix_row_const_reference_type y) {
  PIP_Tree_Node::matrix_row_copy_type row(x.size());
  PIP_Tree_Node::matrix_row_copy_iterator itr = row.end();
  PIP_Tree_Node::matrix_row_const_iterator i = x.begin();
  PIP_Tree_Node::matrix_row_const_iterator i_end = x.end();
  PIP_Tree_Node::matrix_const_row_const_iterator j = y.begin();
  PIP_Tree_Node::matrix_const_row_const_iterator j_end = y.end();
  if (i == i_end && j == j_end)
    return;
  if (j == j_end
      || (i != i_end && (*i).first < (*j).first)) {
    itr = row.find_create(*i);
    ++i;
  } else
      if (i == i_end
          || (j != j_end && (*i).first > (*j).first)) {
        itr = row.find_create(*j);
        neg_assign((*itr).second);
        ++j;
      } else {
        PPL_ASSERT(i != i_end);
        PPL_ASSERT(j != j_end);
        PPL_ASSERT((*i).first == (*j).first);
        itr = row.find_create(*i);
        (*itr).second -= (*j).second;
        ++i;
        ++j;
      }
  PPL_ASSERT(itr != row.end());
  while (i != i_end && j != j_end) {
    if ((*i).first < (*j).first) {
      itr = row.find_create(*i,itr);
      ++i;
    } else {
      if ((*i).first > (*j).first) {
        itr = row.find_create(*j,itr);
        neg_assign((*itr).second);
        ++j;
      } else {
        PPL_ASSERT((*i).first == (*j).first);
        itr = row.find_create(*i,itr);
        (*itr).second -= (*j).second;
        ++i;
        ++j;
      }
    }
  }
  while (i != i_end) {
    itr = row.find_create(*i,itr);
    ++i;
  }
  PPL_ASSERT(i == i_end);
  while (j != j_end) {
    itr = row.find_create(*j,itr);
    neg_assign((*itr).second);
    ++j;
  }
  PPL_ASSERT(j == j_end);
  std::swap(row,x);
}

#endif // !defined(PPL_SPARSE_BACKEND_SLOW_INSERTIONS)

// Merge constraint system to a Matrix-form context such as x = x U y
void
merge_assign(PIP_Tree_Node::matrix_type& x,
             const Constraint_System& y,
             const Variables_Set& parameters) {
  PPL_ASSERT(parameters.size() == x.num_columns() - 1);
  const dimension_type new_rows = std::distance(y.begin(), y.end());
  if (new_rows == 0)
    return;
  const dimension_type old_num_rows = x.num_rows();
  x.add_zero_rows(new_rows);

  // Compute once for all.
  const dimension_type cs_space_dim = y.space_dimension();
  const Variables_Set::const_iterator param_begin = parameters.begin();
  const Variables_Set::const_iterator param_end = parameters.end();

  dimension_type i = old_num_rows;
  for (Constraint_System::const_iterator y_i = y.begin(),
         y_end = y.end(); y_i != y_end; ++y_i, ++i) {
    PPL_ASSERT(y_i->is_nonstrict_inequality());
    PIP_Tree_Node::matrix_row_reference_type x_i = x[i];
    const Coefficient& inhomogeneous_term = y_i->inhomogeneous_term();
    PIP_Tree_Node::matrix_row_iterator itr = x_i.end();
    Variables_Set::const_iterator pj = parameters.begin();
    dimension_type j = 1;
    if (inhomogeneous_term != 0)
      itr = x_i.find_create(0, inhomogeneous_term);
    else
      if (pj != param_end) {
        Variable vj(*pj);
        if (vj.space_dimension() <= cs_space_dim) {
          const Coefficient& c = y_i->coefficient(vj);
          if (c != 0) {
            itr = x_i.find_create(j, c);
            ++pj;
            ++j;
          }
        }
      }
    PPL_ASSERT(pj == param_end || itr != x_i.end());
    for ( ; pj != param_end; ++pj, ++j) {
      Variable vj(*pj);
      if (vj.space_dimension() > cs_space_dim)
        break;
      const Coefficient& c = y_i->coefficient(vj);
      if (c != 0)
        itr = x_i.find_create(j, c, itr);
    }
  }
}

#ifndef USE_PPL_SPARSE_MATRIX

inline void
neg_assign_row(PIP_Tree_Node::matrix_row_reference_type x,
               PIP_Tree_Node::matrix_row_const_reference_type y) {
  for (dimension_type i = x.size(); i-- > 0; )
    neg_assign(x[i], y[i]);
}

#else

// Assigns to row x the negation of row y.
inline void
neg_assign_row(PIP_Tree_Node::matrix_row_reference_type x,
               PIP_Tree_Node::matrix_row_const_reference_type y) {
  x = y;
  PIP_Tree_Node::matrix_row_iterator i = x.begin();
  PIP_Tree_Node::matrix_row_iterator i_end = x.end();
  for ( ; i!=i_end; ++i)
    neg_assign((*i).second);
}

#endif // !defined(USE_PPL_SPARSE_MATRIX)

// Given context row \p y and denominator \p den,
// to be interpreted as expression expr = y / den,
// assigns to context row \p x a new value such that
//     x / den == - expr - 1.
inline void
complement_assign(PIP_Tree_Node::matrix_row_reference_type x,
                  PIP_Tree_Node::matrix_row_const_reference_type y,
                  Coefficient_traits::const_reference den) {
  PPL_ASSERT(den > 0);
  neg_assign_row(x, y);
  Coefficient& x_0 = x[0];
  if (den == 1)
    --x_0;
  else {
    PPL_DIRTY_TEMP_COEFFICIENT(mod);
    mod_assign(mod, x_0, den);
    x_0 -= (mod == 0) ? den : mod;
  }
}

// Add to `context' the columns for new artificial parameters.
inline void
add_artificial_parameters(PIP_Tree_Node::matrix_type& context,
                          const dimension_type num_art_params) {
  if (num_art_params > 0)
    context.add_zero_columns(num_art_params);
}

// Add to `params' the indices of new artificial parameters.
inline void
add_artificial_parameters(Variables_Set& params,
                          const dimension_type space_dim,
                          const dimension_type num_art_params) {
  for (dimension_type i = 0; i < num_art_params; ++i)
    params.insert(space_dim + i);
}

// Update `context', `params' and `space_dim' to account for
// the addition of the new artificial parameters.
inline void
add_artificial_parameters(PIP_Tree_Node::matrix_type& context,
                          Variables_Set& params,
                          dimension_type& space_dim,
                          const dimension_type num_art_params) {
  add_artificial_parameters(context, num_art_params);
  add_artificial_parameters(params, space_dim, num_art_params);
  space_dim += num_art_params;
}

/* Compares two columns lexicographically in revised simplex tableau
  - Returns true if (column ja)*(-cst_a)/pivot_a[ja]
                    << (column jb)*(-cst_b)/pivot_b[jb]
  - Returns false otherwise
*/
bool
column_lower(const PIP_Tree_Node::matrix_type& tableau,
             const std::vector<dimension_type>& mapping,
             const std::vector<bool>& basis,
             PIP_Tree_Node::matrix_row_const_reference_type pivot_a,
             const dimension_type ja,
             PIP_Tree_Node::matrix_row_const_reference_type pivot_b,
             const dimension_type jb,
             Coefficient_traits::const_reference cst_a = -1,
             Coefficient_traits::const_reference cst_b = -1) {
  const Coefficient& sij_a = pivot_a.get(ja);
  const Coefficient& sij_b = pivot_b.get(jb);
  PPL_ASSERT(sij_a > 0);
  PPL_ASSERT(sij_b > 0);

  PPL_DIRTY_TEMP_COEFFICIENT(lhs_coeff);
  PPL_DIRTY_TEMP_COEFFICIENT(rhs_coeff);
  lhs_coeff = cst_a * sij_b;
  rhs_coeff = cst_b * sij_a;

  const int lhs_coeff_sign = sgn(lhs_coeff);
  const int rhs_coeff_sign = sgn(rhs_coeff);

  if (ja == jb) {
    // Same column: just compare the ratios.
    // This works since all columns are lexico-positive.
    // return cst_a * sij_b > cst_b * sij_a;
    return lhs_coeff > rhs_coeff;
  }

  PPL_DIRTY_TEMP_COEFFICIENT(lhs);
  PPL_DIRTY_TEMP_COEFFICIENT(rhs);
  const dimension_type num_vars = mapping.size();
  dimension_type k = 0;
  // While loop guard is: (k < num_rows && lhs == rhs).
  // Return value is false, if k >= num_rows; lhs < rhs, otherwise.
  // Try to optimize the computation of lhs and rhs.
  while (true) {
    const dimension_type mk = mapping[k];
    const bool in_base = basis[k];
    if (++k >= num_vars)
      return false;
    if (in_base) {
      // Reconstitute the identity submatrix part of tableau.
      if (mk == ja) {
        // Optimizing for: lhs == lhs_coeff && rhs == 0;
        if (lhs_coeff == 0)
          continue;
        else
          return lhs_coeff > 0;
      }
      if (mk == jb) {
        // Optimizing for: lhs == 0 && rhs == rhs_coeff;
        if (rhs_coeff == 0)
          continue;
        else
          return 0 > rhs_coeff;
      }
      // Optimizing for: lhs == 0 && rhs == 0;
      continue;
    } else {
      // Not in base.
      PIP_Tree_Node::matrix_row_const_reference_type t_mk = tableau[mk];
      const Coefficient* t_mk_ja;
      const Coefficient* t_mk_jb;
      t_mk.get2(ja, jb, t_mk_ja, t_mk_jb);
      if (t_mk_ja == &Coefficient_zero())
        if (t_mk_jb == &Coefficient_zero())
          continue;
        else {
          const int rhs_sign = rhs_coeff_sign * sgn(*t_mk_jb);
          if (0 == rhs_sign)
            continue;
          else
            return 0 > rhs_sign;
        }
      else
        if (t_mk_jb == &Coefficient_zero()) {
          const int lhs_sign = lhs_coeff_sign * sgn(*t_mk_ja);
          if (lhs_sign == 0)
            continue;
          else
            return lhs_sign > 0;
        } else {
          lhs = lhs_coeff * *t_mk_ja;
          rhs = rhs_coeff * *t_mk_jb;
          if (lhs == rhs)
            continue;
          else
            return lhs > rhs;
        }
    }
  }
  // This point should be unreachable.
  throw std::runtime_error("PPL internal error");
}

/* Find the column j in revised simplex tableau such as
  - j is in candidates
  - (column j) / pivot_row[j] is lexico-minimal
  When this function returns, candidates contains the minimum(s) column(s)
  index(es).
*/
void
find_lexico_minimum_column_in_set(std::set<dimension_type>& candidates,
                                  const PIP_Tree_Node::matrix_type& tableau,
                                  const std::vector<dimension_type>& mapping,
                                  const std::vector<bool>& basis,
                                  PIP_Tree_Node::
                                  matrix_row_const_reference_type pivot_row) {
  const dimension_type num_vars = mapping.size();

  PPL_ASSERT(!candidates.empty());
  for (dimension_type var_index = 0; var_index < num_vars; ++var_index) {
    std::set<dimension_type> new_candidates;
    std::set<dimension_type>::iterator i = candidates.begin();
    std::set<dimension_type>::iterator i_end = candidates.end();
    PPL_ASSERT(!candidates.empty());
    new_candidates.insert(*i);
    dimension_type min_column = *i;
    ++i;
    if (i == i_end)
      // Only one candidate left, so it is the minimum.
      break;
    PIP_Tree_Node::matrix_const_row_const_iterator pivot_itr
      = pivot_row.find(min_column);
    PIP_Tree_Node::matrix_const_row_const_iterator pivot_end
      = pivot_row.end();
    PPL_ASSERT(pivot_itr != pivot_end);
    const Coefficient* sij_b = &((*pivot_itr).second);
    ++pivot_itr;
    const dimension_type row_index = mapping[var_index];
    const bool in_base = basis[var_index];
    if (in_base) {
      for ( ; i != i_end; ++i) {
        pivot_itr = pivot_row.find(*i, pivot_itr);
        PPL_ASSERT(pivot_itr != pivot_end);
        const Coefficient& sij_a = (*pivot_itr).second;
        ++pivot_itr;
        PPL_ASSERT(sij_a > 0);
        PPL_ASSERT(*sij_b > 0);

        bool found_better_candidate = false;
        // Reconstitute the identity submatrix part of tableau.
        if (row_index == *i) {
          // Optimizing for: lhs == lhs_coeff && rhs == 0;
          if (*sij_b == 0)
            new_candidates.insert(*i);
          else {
            if (*sij_b < 0)
              found_better_candidate = true;
          }
        } else
          if (row_index == min_column) {
            // Optimizing for: lhs == 0 && rhs == rhs_coeff;
            if (sij_a == 0)
              new_candidates.insert(*i);
            else {
              if (0 < sij_a)
                found_better_candidate = true;
            }
          } else
            // Optimizing for: lhs == 0 && rhs == 0;
            new_candidates.insert(*i);
        if (found_better_candidate) {
          new_candidates.clear();
          min_column = *i;
          sij_b = &sij_a;
          new_candidates.insert(min_column);
        }
      }
    } else {
      // Not in base.
      PIP_Tree_Node::matrix_row_const_reference_type row = tableau[row_index];
      PIP_Tree_Node::matrix_const_row_const_iterator row_itr
        = row.lower_bound(min_column);
      PIP_Tree_Node::matrix_const_row_const_iterator row_end = row.end();
      const Coefficient* row_jb;
      if (row_itr == row_end || (*row_itr).first > min_column)
        row_jb = &(Coefficient_zero());
      else {
        PPL_ASSERT((*row_itr).first == min_column);
        row_jb = &((*row_itr).second);
        ++row_itr;
      }
      for ( ; i != i_end; ++i) {
        pivot_itr = pivot_row.find(*i, pivot_itr);
        PPL_ASSERT(pivot_itr != pivot_end);
        const Coefficient& sij_a = (*pivot_itr).second;
        PPL_ASSERT(sij_a > 0);
        PPL_ASSERT(*sij_b > 0);

        PPL_DIRTY_TEMP_COEFFICIENT(lhs);
        PPL_DIRTY_TEMP_COEFFICIENT(rhs);
        if (row_itr != row_end && (*row_itr).first < *i)
          row_itr = row.lower_bound(*i, row_itr);
        const Coefficient* row_ja;
        if (row_itr == row_end || (*row_itr).first > *i)
          row_ja = &(Coefficient_zero());
        else {
          PPL_ASSERT((*row_itr).first == *i);
          row_ja = &((*row_itr).second);
          ++row_itr;
        }
        // lhs is actually the left-hand side with toggled sign.
        // rhs is actually the right-hand side with toggled sign.
        lhs = *sij_b * *row_ja;
        rhs = sij_a * *row_jb;
        if (lhs == rhs)
          new_candidates.insert(*i);
        else
          if (lhs < rhs) {
            new_candidates.clear();
            min_column = *i;
            row_jb = row_ja;
            sij_b = &sij_a;
            new_candidates.insert(min_column);
          }
      }
    }
    std::swap(candidates, new_candidates);
  }
}

/* Find the column j in revised simplex tableau such as
  - pivot_row[j] is positive
  - (column j) / pivot_row[j] is lexico-minimal
*/
bool
find_lexico_minimum_column(const PIP_Tree_Node::matrix_type& tableau,
                           const std::vector<dimension_type>& mapping,
                           const std::vector<bool>& basis,
                           PIP_Tree_Node::matrix_row_const_reference_type
                             pivot_row,
                           const dimension_type start_j,
                           dimension_type& j_out) {
  const dimension_type num_cols = tableau.num_columns();
  std::set<dimension_type> candidates;
  for (dimension_type j = start_j; j < num_cols; ++j) {
    const Coefficient& c = pivot_row[j];
    if (c > 0)
      candidates.insert(j);
  }
  if (candidates.empty()) {
    j_out = num_cols;
    return false;
  }

  find_lexico_minimum_column_in_set(candidates, tableau, mapping, basis,
                                    pivot_row);
  PPL_ASSERT(!candidates.empty());
  j_out = *(candidates.begin());

  return true;
}

// Divide all coefficients in row x and denominator y by their GCD.
void
row_normalize(PIP_Tree_Node::matrix_row_reference_type x, Coefficient& den) {
  if (den == 1)
    return;
  PPL_DIRTY_TEMP_COEFFICIENT(gcd);
  gcd = den;
  for (PIP_Tree_Node::matrix_row_const_iterator
    i = x.begin(), i_end = x.end(); i != i_end; ++i) {
    const Coefficient& x_i = (*i).second;
    if (x_i != 0) {
      gcd_assign(gcd, x_i, gcd);
      if (gcd == 1)
        return;
    }
  }
  // Divide the coefficients by the GCD.
  for (PIP_Tree_Node::matrix_row_iterator
    i = x.begin(), i_end = x.end(); i != i_end; ++i) {
    Coefficient& x_i = (*i).second;
    exact_div_assign(x_i, x_i, gcd);
  }
  // Divide the denominator by the GCD.
  exact_div_assign(den, den, gcd);
}

// This is here because it is used as a template argument in
// compatibility_check_find_pivot, so it must be a global declaration.
struct compatibility_check_find_pivot_map_data {
  dimension_type row_index;
  // We cache pointers to cost and value to avoid calling get() multiple
  // times.
  const Coefficient* cost;
  const Coefficient* value;
  bool operator==(const compatibility_check_find_pivot_map_data& x) const {
    return row_index == x.row_index;
  }
  // Needed by std::set to sort the values.
  bool operator<(const compatibility_check_find_pivot_map_data& x) const {
    return row_index < x.row_index;
  }
};

void
compatibility_check_find_pivot_in_set(std::set<std::pair<dimension_type,
                                      compatibility_check_find_pivot_map_data
                                      > >& candidates,
                                      const PIP_Tree_Node::matrix_type& s,
                                      const std::vector<dimension_type>&
                                        mapping,
                                      const std::vector<bool>& basis) {
  typedef compatibility_check_find_pivot_map_data map_data;
  typedef std::set<std::pair<dimension_type, map_data> > candidates_t;
  const dimension_type num_vars = mapping.size();
  for (dimension_type var_index = 0; var_index < num_vars; ++var_index) {
    const dimension_type row_index = mapping[var_index];
    const bool in_base = basis[var_index];
    candidates_t new_candidates;
    candidates_t::iterator i = candidates.begin();
    candidates_t::iterator i_end = candidates.end();
    PPL_ASSERT(i != i_end);
    dimension_type pi = i->second.row_index;
    dimension_type pj = i->first;
    const Coefficient* cost = i->second.cost;
    const Coefficient* value = i->second.value;
    new_candidates.insert(*i);
    if (in_base) {
      for (++i; i != i_end; ++i) {
        bool found_better_pivot = false;

        const dimension_type challenger_i = i->second.row_index;
        const dimension_type challenger_j = i->first;
        const Coefficient* challenger_cost = i->second.cost;
        const Coefficient* challenger_value = i->second.value;
        PPL_ASSERT(*value > 0);
        PPL_ASSERT(*challenger_value > 0);
        PPL_ASSERT(pj < challenger_j);

        const int lhs_coeff_sgn = sgn(*cost) * sgn(*challenger_value);
        const int rhs_coeff_sgn = sgn(*challenger_cost) * sgn(*value);

        PPL_ASSERT(pj != challenger_j);

        // Reconstitute the identity submatrix part of tableau.
        if (row_index == pj) {
          // Optimizing for: lhs == lhs_coeff && rhs == 0;
          if (lhs_coeff_sgn == 0)
            new_candidates.insert(*i);
          else
            found_better_pivot = lhs_coeff_sgn > 0;
        } else {
          if (row_index == challenger_j) {
            // Optimizing for: lhs == 0 && rhs == rhs_coeff;
            if (rhs_coeff_sgn == 0)
              new_candidates.insert(*i);
            else
              found_better_pivot = 0 > rhs_coeff_sgn;
          } else
            // Optimizing for: lhs == 0 && rhs == 0;
            new_candidates.insert(*i);
        }
        if (found_better_pivot) {
          pi = challenger_i;
          pj = challenger_j;
          cost = challenger_cost;
          value = challenger_value;
          new_candidates.clear();
          new_candidates.insert(*i);
        }
      }
    } else {
      // Not in base.
      PIP_Tree_Node::matrix_row_const_reference_type row = s[row_index];
      PIP_Tree_Node::matrix_const_row_const_iterator row_itr
        = row.lower_bound(pj);
      PIP_Tree_Node::matrix_const_row_const_iterator row_end = row.end();
      const Coefficient* row_value;
      if (row_itr != row_end && (*row_itr).first == pj) {
        row_value = &((*row_itr).second);
        ++row_itr;
      } else
        row_value = &(Coefficient_zero());
      for (++i; i != i_end; ++i) {
        const dimension_type challenger_i = i->second.row_index;
        const dimension_type challenger_j = i->first;
        const Coefficient* challenger_cost = i->second.cost;
        const Coefficient* challenger_value = i->second.value;
        PPL_ASSERT(*value > 0);
        PPL_ASSERT(*challenger_value > 0);
        PPL_ASSERT(pj < challenger_j);

        PPL_DIRTY_TEMP_COEFFICIENT(lhs);
        lhs = *cost;
        lhs *= *challenger_value;
        PPL_DIRTY_TEMP_COEFFICIENT(rhs);
        rhs = *challenger_cost;
        rhs *= *value;

        const Coefficient* row_challenger_value;
        // row_challenger_value = &(row.get(challenger_j));
        if (row_itr != row_end) {
          if ((*row_itr).first < challenger_j) {
            row_itr = row.lower_bound(challenger_j,row_itr);
            if (row_itr != row_end && (*row_itr).first == challenger_j) {
              row_challenger_value = &((*row_itr).second);
              ++row_itr;
            } else
              row_challenger_value = &(Coefficient_zero());
          } else {
            if ((*row_itr).first == challenger_j) {
              row_challenger_value = &((*row_itr).second);
              ++row_itr;
            } else {
              PPL_ASSERT((*row_itr).first > challenger_j);
              row_challenger_value = &(Coefficient_zero());
            }
          }
        } else
          row_challenger_value = &(Coefficient_zero());

        lhs *= *row_value;
        rhs *= *row_challenger_value;

        if (lhs == rhs)
          new_candidates.insert(*i);
        else {
          if (lhs > rhs) {
            pi = challenger_i;
            pj = challenger_j;
            cost = challenger_cost;
            value = challenger_value;
            row_value = row_challenger_value;
            new_candidates.clear();
            new_candidates.insert(*i);
          }
        }
      }
    }
    std::swap(candidates, new_candidates);
  }
}

// Returns false if there isn't a posivive pivot candidate.
// Otherwise, it sets pi, pj to the coordinates of the pivot in s.
bool
compatibility_check_find_pivot(const PIP_Tree_Node::matrix_type& s,
                               const std::vector<dimension_type>& mapping,
                               const std::vector<bool>& basis,
                               dimension_type& pi, dimension_type& pj) {
  // Look for a negative RHS (i.e., constant term, stored in column 0),
  // maximizing pivot column.
  const dimension_type num_rows = s.num_rows();
  typedef compatibility_check_find_pivot_map_data map_data;
  typedef std::set<std::pair<dimension_type, map_data> > candidates_t;
  typedef std::map<dimension_type,map_data> candidates_map_t;
  candidates_map_t candidates_map;
  for (dimension_type i = 0; i < num_rows; ++i) {
    PIP_Tree_Node::matrix_row_const_reference_type s_i = s[i];
    if (s_i.get(0) < 0) {
      dimension_type j;
      if (!find_lexico_minimum_column(s, mapping, basis, s_i, 1, j)) {
        // No positive pivot candidate: unfeasible problem.
        return false;
      }
      candidates_map_t::iterator itr = candidates_map.find(j);
      if (itr == candidates_map.end()) {
        map_data& current_data = candidates_map[j];
        current_data.row_index = i;
        s[i].get2(0, j, current_data.cost, current_data.value);
      } else {
        map_data& current_data = candidates_map[j];

        PIP_Tree_Node::matrix_row_const_reference_type row_b = s[i];

        Coefficient_traits::const_reference cost_b = row_b.get(0);
        const Coefficient& value_b = row_b.get(j);

        PPL_ASSERT(*(current_data.value) > 0);
        PPL_ASSERT(value_b > 0);

        Coefficient lhs_coeff = *(current_data.cost);
        lhs_coeff *= value_b;
        Coefficient rhs_coeff = cost_b;
        rhs_coeff *= *(current_data.value);

        // Same column: just compare the ratios.
        // This works since all columns are lexico-positive.
        // return cst_a * sij_b > cst_b * sij_a;
        if (lhs_coeff > rhs_coeff) {
          // Found better pivot
          current_data.row_index = i;
          current_data.cost = &(cost_b);
          current_data.value = &(value_b);
        }
        // Otherwise, keep current pivot for this column.
      }
    }
  }
  candidates_t candidates;
  candidates_map_t::iterator i = candidates_map.begin();
  candidates_map_t::iterator i_end = candidates_map.end();
  for ( ; i != i_end; ++i)
    candidates.insert(*i);
  if (!candidates.empty()) {
    compatibility_check_find_pivot_in_set(candidates, s, mapping, basis);
    PPL_ASSERT(!candidates.empty());
    pi = candidates.begin()->second.row_index;
    pj = candidates.begin()->first;
  } else {
    pi = s.num_rows();
    pj = 0;
  }

  return true;
}

} // namespace

namespace IO_Operators {

std::ostream&
operator<<(std::ostream& os, const PIP_Tree_Node& x) {
  x.print(os);
  return os;
}

std::ostream&
operator<<(std::ostream& os, const PIP_Tree_Node::Artificial_Parameter& x) {
  const Linear_Expression& expr = static_cast<const Linear_Expression&>(x);
  os << "(" << expr << ") div " << x.denominator();
  return os;
}

} // namespace IO_Operators

PIP_Tree_Node::PIP_Tree_Node(const PIP_Problem* owner)
  : owner_(owner),
    parent_(0),
    constraints_(),
    artificial_parameters() {
}

PIP_Tree_Node::PIP_Tree_Node(const PIP_Tree_Node& y)
  : owner_(y.owner_),
    parent_(0), // NOTE: parent is not copied.
    constraints_(y.constraints_),
    artificial_parameters(y.artificial_parameters) {
}

bool
PIP_Tree_Node::Artificial_Parameter
::operator==(const PIP_Tree_Node::Artificial_Parameter& y) const {
  const Artificial_Parameter& x = *this;
  if (x.space_dimension() != y.space_dimension())
    return false;
  if (x.denom != y.denom)
    return false;
  if (x.inhomogeneous_term() != y.inhomogeneous_term())
    return false;
  for (dimension_type i = x.space_dimension(); i-- > 0; )
    if (x.coefficient(Variable(i)) != y.coefficient(Variable(i)))
      return false;
  return true;
}

bool
PIP_Tree_Node::Artificial_Parameter
::operator!=(const PIP_Tree_Node::Artificial_Parameter& y) const {
  return !operator==(y);
}

bool
PIP_Tree_Node::Artificial_Parameter::OK() const {
  if (denom <= 0) {
#ifndef NDEBUG
    std::cerr << "PIP_Tree_Node::Artificial_Parameter "
              << "has a non-positive denominator.\n";
#endif
    return false;
  }
  return true;
}

void
PIP_Tree_Node::Artificial_Parameter::ascii_dump(std::ostream& s) const {
  s << "artificial_parameter ";
  Linear_Expression::ascii_dump(s);
  s << " / " << denom << "\n";
}

bool
PIP_Tree_Node::Artificial_Parameter::ascii_load(std::istream& s) {
  std::string str;
  if (!(s >> str) || str != "artificial_parameter")
    return false;
  if (!Linear_Expression::ascii_load(s))
    return false;
  if (!(s >> str) || str != "/")
    return false;
  if (!(s >> denom))
    return false;
  PPL_ASSERT(OK());
  return true;
}

PPL_OUTPUT_DEFINITIONS(PIP_Tree_Node::Artificial_Parameter)

PIP_Solution_Node::PIP_Solution_Node(const PIP_Problem* owner)
  : PIP_Tree_Node(owner),
    tableau(),
    basis(),
    mapping(),
    var_row(),
    var_column(),
    special_equality_row(0),
    big_dimension(not_a_dimension()),
    sign(),
    solution(),
    solution_valid(false) {
}

PIP_Solution_Node::PIP_Solution_Node(const PIP_Solution_Node& y)
  : PIP_Tree_Node(y),
    tableau(y.tableau),
    basis(y.basis),
    mapping(y.mapping),
    var_row(y.var_row),
    var_column(y.var_column),
    special_equality_row(y.special_equality_row),
    big_dimension(y.big_dimension),
    sign(y.sign),
    solution(y.solution),
    solution_valid(y.solution_valid) {
}

PIP_Solution_Node::PIP_Solution_Node(const PIP_Solution_Node& y,
                                     No_Constraints)
  : PIP_Tree_Node(y.owner_), // NOTE: only copy owner.
    tableau(y.tableau),
    basis(y.basis),
    mapping(y.mapping),
    var_row(y.var_row),
    var_column(y.var_column),
    special_equality_row(y.special_equality_row),
    big_dimension(y.big_dimension),
    sign(y.sign),
    solution(y.solution),
    solution_valid(y.solution_valid) {
}

PIP_Solution_Node::~PIP_Solution_Node() {
}

PIP_Decision_Node::PIP_Decision_Node(const PIP_Problem* owner,
                                     PIP_Tree_Node* fcp,
                                     PIP_Tree_Node* tcp)
  : PIP_Tree_Node(owner),
    false_child(fcp),
    true_child(tcp) {
  if (false_child != 0)
    false_child->set_parent(this);
  if (true_child != 0)
    true_child->set_parent(this);
}

PIP_Decision_Node::PIP_Decision_Node(const PIP_Decision_Node& y)
  : PIP_Tree_Node(y),
    false_child(0),
    true_child(0) {
  if (y.false_child != 0) {
    false_child = y.false_child->clone();
    false_child->set_parent(this);
  }
  // Protect false_child from exception safety issues via std::auto_ptr.
  std::auto_ptr<PIP_Tree_Node> wrapped_node(false_child);
  if (y.true_child != 0) {
    true_child = y.true_child->clone();
    true_child->set_parent(this);
  }
  // It is now safe to release false_child.
  wrapped_node.release();
}

PIP_Decision_Node::~PIP_Decision_Node() {
  delete false_child;
  delete true_child;
}

void
PIP_Solution_Node::set_owner(const PIP_Problem* owner) {
  owner_ = owner;
}

void
PIP_Decision_Node::set_owner(const PIP_Problem* owner) {
  owner_ = owner;
  if (false_child)
    false_child->set_owner(owner);
  if (true_child)
    true_child->set_owner(owner);
}

bool
PIP_Solution_Node::check_ownership(const PIP_Problem* owner) const {
  return get_owner() == owner;
}

bool
PIP_Decision_Node::check_ownership(const PIP_Problem* owner) const {
  return get_owner() == owner
    && (!false_child || false_child->check_ownership(owner))
    && (!true_child || true_child->check_ownership(owner));
}

const PIP_Solution_Node*
PIP_Tree_Node::as_solution() const {
  return 0;
}

const PIP_Decision_Node*
PIP_Tree_Node::as_decision() const {
  return 0;
}

const PIP_Solution_Node*
PIP_Solution_Node::as_solution() const {
  return this;
}

const PIP_Decision_Node*
PIP_Decision_Node::as_decision() const {
  return this;
}

bool
PIP_Solution_Node::Tableau::OK() const {
  if (s.num_rows() != t.num_rows()) {
#ifndef NDEBUG
    std::cerr << "PIP_Solution_Node::Tableau matrices "
              << "have a different number of rows.\n";
#endif
    return false;
  }

  if (!s.OK() || !t.OK()) {
#ifndef NDEBUG
    std::cerr << "A PIP_Solution_Node::Tableau matrix is broken.\n";
#endif
    return false;
  }

  if (denom <= 0) {
#ifndef NDEBUG
    std::cerr << "PIP_Solution_Node::Tableau with non-positive denominator.\n";
#endif
    return false;
  }

  // All tests passed.
  return true;
}

bool
PIP_Tree_Node::OK() const {
#ifndef NDEBUG
  using std::endl;
  using std::cerr;
#endif

  const Constraint_System::const_iterator begin = constraints_.begin();
  const Constraint_System::const_iterator end = constraints_.end();

  // Parameter constraint system should contain no strict inequalities.
  for (Constraint_System::const_iterator ci = begin; ci != end; ++ci)
    if (ci->is_strict_inequality()) {
#ifndef NDEBUG
      cerr << "The feasible region of the PIP_Problem parameter context"
           << "is defined by a constraint system containing strict "
           << "inequalities."
	   << endl;
      ascii_dump(cerr);
#endif
      return false;
    }
  return true;
}

void
PIP_Tree_Node
::add_constraint(matrix_row_const_reference_type row,
                 const Variables_Set& parameters) {
  const dimension_type num_params = parameters.size();

  // Compute the expression for the parameter constraint.
  Linear_Expression expr = Linear_Expression(row.get(0));
  Variables_Set::const_iterator j = parameters.begin();
  if (!parameters.empty()) {
    // Needed to avoid reallocations in expr when iterating upward.
    add_mul_assign(expr, 0, Variable(*(parameters.rbegin())));
    // The number of increments of j plus one.
    dimension_type j_index = 1;
    matrix_const_row_const_iterator i = row.lower_bound(1);
    matrix_const_row_const_iterator i_end = row.end();
    // NOTE: iterating in [1..num_params].
    for ( ; i != i_end; ++i) {
      if ((*i).first > num_params)
        break;
      std::advance(j, (*i).first - j_index);
      j_index = (*i).first;
      add_mul_assign(expr, (*i).second, Variable(*j));
    }
  }
  // Add the parameter constraint.
  constraints_.insert(expr >= 0);
}

void
PIP_Tree_Node::parent_merge() {
  const PIP_Decision_Node& parent = *parent_;

  // Merge the parent's artificial parameters.
  artificial_parameters.insert(artificial_parameters.begin(),
                               parent.art_parameter_begin(),
                               parent.art_parameter_end());

  PPL_ASSERT(OK());
}

bool
PIP_Solution_Node::OK() const {
#ifndef NDEBUG
  using std::cerr;
#endif
  if (!PIP_Tree_Node::OK())
    return false;

  // Check that every member used is OK.

  if (!tableau.OK())
    return false;

  // Check coherency of basis, mapping, var_row and var_column
  if (basis.size() != mapping.size()) {
#ifndef NDEBUG
    cerr << "The PIP_Solution_Node::basis and PIP_Solution_Node::mapping "
         << "vectors do not have the same number of elements.\n";
#endif
    return false;
  }
  if (basis.size() != var_row.size() + var_column.size()) {
#ifndef NDEBUG
    cerr << "The sum of number of elements in the PIP_Solution_Node::var_row "
         << "and PIP_Solution_Node::var_column vectors is different from the "
         << "number of elements in the PIP_Solution_Node::basis vector.\n";
#endif
    return false;
  }
  if (var_column.size() != tableau.s.num_columns()) {
#ifndef NDEBUG
    cerr << "The number of elements in the PIP_Solution_Node::var_column "
         << "vector is different from the number of columns in the "
         << "PIP_Solution_Node::tableau.s Matrix.\n";
#endif
    return false;
  }
  if (var_row.size() != tableau.s.num_rows()) {
#ifndef NDEBUG
    cerr << "The number of elements in the PIP_Solution_Node::var_row "
         << "vector is different from the number of rows in the "
         << "PIP_Solution_Node::tableau.s Matrix.\n";
#endif
    return false;
  }
  for (dimension_type i = mapping.size(); i-- > 0; ) {
    const dimension_type rowcol = mapping[i];
    if (basis[i] && var_column[rowcol] != i) {
#ifndef NDEBUG
      cerr << "Variable " << i << " is basic and corresponds to column "
           << rowcol << " but PIP_Solution_Node::var_column[" << rowcol
           << "] does not correspond to variable " << i << ".\n";
#endif
      return false;
    }
    if (!basis[i] && var_row[rowcol] != i) {
#ifndef NDEBUG
      cerr << "Variable " << i << " is nonbasic and corresponds to row "
           << rowcol << " but PIP_Solution_Node::var_row[" << rowcol
           << "] does not correspond to variable " << i << ".\n";
#endif
      return false;
    }
  }
  // All checks passed.
  return true;
}

bool
PIP_Decision_Node::OK() const {
  // Perform base class well-formedness check on this node.
  if (!PIP_Tree_Node::OK())
    return false;

  // Recursively check if child nodes are well-formed.
  if (false_child && !false_child->OK())
    return false;
  if (true_child && !true_child->OK())
    return false;

  // Decision nodes should always have a true child.
  if (!true_child) {
#ifndef NDEBUG
    std::cerr << "PIP_Decision_Node with no 'true' child.\n";
#endif
    return false;
  }

  // Decision nodes with a false child must have exactly one constraint.
  if (false_child) {
    dimension_type
      dist = std::distance(constraints_.begin(), constraints_.end());
    if (dist != 1) {
#ifndef NDEBUG
      std::cerr << "PIP_Decision_Node with a 'false' child has "
                << dist << " parametric constraints (should be 1).\n";
#endif
      return false;
    }
  }

  // All checks passed.
  return true;
}

void
PIP_Decision_Node::update_tableau(const PIP_Problem& pip,
                                  const dimension_type external_space_dim,
                                  const dimension_type first_pending_constraint,
                                  const Constraint_Sequence& input_cs,
                                  const Variables_Set& parameters) {
  true_child->update_tableau(pip,
                             external_space_dim,
                             first_pending_constraint,
                             input_cs,
                             parameters);
  if (false_child)
    false_child->update_tableau(pip,
                                external_space_dim,
                                first_pending_constraint,
                                input_cs,
                                parameters);
  PPL_ASSERT(OK());
}

PIP_Tree_Node*
PIP_Decision_Node::solve(const PIP_Problem& pip,
                         const bool check_feasible_context,
                         const matrix_type& context,
                         const Variables_Set& params,
                         dimension_type space_dim) {
  PPL_ASSERT(true_child != 0);
  matrix_type context_true(context);
  Variables_Set all_params(params);
  const dimension_type num_art_params = artificial_parameters.size();
  add_artificial_parameters(context_true, all_params, space_dim,
                            num_art_params);
  merge_assign(context_true, constraints_, all_params);
  bool has_false_child = (false_child != 0);
  bool has_true_child = (true_child != 0);
  true_child = true_child->solve(pip, check_feasible_context,
                                 context_true, all_params, space_dim);

  if (has_false_child) {
    // Decision nodes with false child must have exactly one constraint
    PPL_ASSERT(1 == std::distance(constraints_.begin(), constraints_.end()));
    // NOTE: modify context_true in place, complementing its last constraint.
    matrix_type& context_false = context_true;
    matrix_row_reference_type last
     = context_false[context_false.num_rows() - 1];
    complement_assign(last, last, 1);
    false_child = false_child->solve(pip, check_feasible_context,
                                     context_false, all_params, space_dim);
  }

  if (true_child == 0 && false_child == 0) {
    // No childs: the whole subtree is unfeasible.
    delete this;
    return 0;
  }

  PIP_Tree_Node* node = this;
  if (has_false_child && false_child == 0) {
    // False child has become unfeasible: merge this node's artificials with
    // the true child, while removing the local parameter constraints, which
    // are no longer discriminative.
    true_child->parent_merge();
    true_child->set_parent(parent());
    node = true_child;
    true_child = 0;
    delete this;
  }
  else if (has_true_child && true_child == 0) {
    // True child has become unfeasible: merge this node's artificials
    // with the false child.
    false_child->parent_merge();
    false_child->set_parent(parent());
    node = false_child;
    false_child = 0;
    delete this;
  }
  else if (check_feasible_context) {
    // Test all constraints for redundancy with the context, and eliminate
    // them if not necessary.
    Constraint_System cs;
    cs.swap(constraints_);
    const Constraint_System::const_iterator end = cs.end();
    for (Constraint_System::const_iterator ci = cs.begin(); ci != end; ++ci) {
      matrix_type ctx_copy(context);
      merge_assign(ctx_copy, Constraint_System(*ci), all_params);
      matrix_row_reference_type last = ctx_copy[ctx_copy.num_rows()-1];
      complement_assign(last, last, 1);
      if (compatibility_check(ctx_copy)) {
        // The constraint is not redundant with the context: we must keep it.
        constraints_.insert(*ci);
      }
    }
    // If the constraints set has become empty, only keep the true child.
    if (constraints_.empty()) {
      true_child->parent_merge();
      true_child->set_parent(parent());
      node = true_child;
      true_child = 0;
      delete this;
    }
  }
  PPL_ASSERT(node->OK());
  return node;
}

void
PIP_Decision_Node::ascii_dump(std::ostream& s) const {
  // Dump base class info.
  PIP_Tree_Node::ascii_dump(s);

  // Dump true child (if any).
  s << "\ntrue_child: ";
  if (true_child == 0) {
    // Note: this branch should normally be unreachable code, since a
    // well-formed decision node always has a true child. We keep this code
    // for debugging purposes (since we want to dump broken nodes).
    s << "BOTTOM\n";
  }
  else if (const PIP_Decision_Node* dec = true_child->as_decision()) {
    s << "DECISION\n";
    dec->ascii_dump(s);
  }
  else {
    const PIP_Solution_Node* sol = true_child->as_solution();
    PPL_ASSERT(sol != 0);
    s << "SOLUTION\n";
    sol->ascii_dump(s);
  }

  // Dump false child (if any).
  s << "\nfalse_child: ";
  if (false_child == 0)
    s << "BOTTOM\n";
  else if (const PIP_Decision_Node* dec = false_child->as_decision()) {
    // Note: this branch should normally be unreachable code.
    // Since a well-formed decision node having a false child should have
    // a single context constraint, its false child will have no context
    // constraints at all, so that no further branch is possible.
    // We keep this code for debugging purposes.
    s << "DECISION\n";
    dec->ascii_dump(s);
  }
  else {
    const PIP_Solution_Node* sol = false_child->as_solution();
    PPL_ASSERT(sol != 0);
    s << "SOLUTION\n";
    sol->ascii_dump(s);
  }
}

bool
PIP_Decision_Node::ascii_load(std::istream& s) {
  std::string str;

  // Load base class info.
  if (!PIP_Tree_Node::ascii_load(s))
    return false;

  // Release the "true" subtree (if any).
  delete true_child;
  true_child = 0;

  // Load true child (if any).
  if (!(s >> str) || str != "true_child:")
    return false;
  if (!(s >> str))
    return false;
  if (str == "BOTTOM")
    // Note: normally unreachable code (see comment on ascii_dump).
    true_child = 0;
  else if (str == "DECISION") {
    PIP_Decision_Node* dec = new PIP_Decision_Node(0, 0, 0);
    true_child = dec;
    if (!dec->ascii_load(s))
      return false;
  }
  else if (str == "SOLUTION") {
    PIP_Solution_Node* sol = new PIP_Solution_Node(0);
    true_child = sol;
    if (!sol->ascii_load(s))
      return false;
  }
  else
    // Unknown node kind.
    return false;

  // Release the "false" subtree (if any).
  delete false_child;
  false_child = 0;

  // Load false child (if any).
  if (!(s >> str) || str != "false_child:")
    return false;
  if (!(s >> str))
    return false;
  if (str == "BOTTOM")
    false_child = 0;
  else if (str == "DECISION") {
    // Note: normally unreachable code (see comment on ascii_dump).
    PIP_Decision_Node* dec = new PIP_Decision_Node(0, 0, 0);
    false_child = dec;
    if (!dec->ascii_load(s))
      return false;
  }
  else if (str == "SOLUTION") {
    PIP_Solution_Node* sol = new PIP_Solution_Node(0);
    false_child = sol;
    if (!sol->ascii_load(s))
      return false;
  }
  else
    // Unknown node kind.
    return false;

  // Loaded all info.
  PPL_ASSERT(OK());
  return true;
}


void
PIP_Solution_Node::Tableau::normalize() {
  if (denom == 1)
    return;

  const dimension_type num_rows = s.num_rows();

  // Compute global gcd.
  PPL_DIRTY_TEMP_COEFFICIENT(gcd);
  gcd = denom;
  for (dimension_type i = num_rows; i-- > 0; ) {
    matrix_row_const_reference_type s_i = s[i];
    matrix_const_row_const_iterator j = s_i.begin();
    matrix_const_row_const_iterator j_end = s_i.end();
    for ( ; j != j_end; ++j) {
      const Coefficient& s_ij = (*j).second;
      if (s_ij != 0) {
        gcd_assign(gcd, s_ij, gcd);
        if (gcd == 1)
          return;
      }
    }
    matrix_row_const_reference_type t_i = t[i];
    j = t_i.begin();
    j_end = t_i.end();
    for ( ; j != j_end; ++j) {
      const Coefficient& t_ij = (*j).second;
      if (t_ij != 0) {
        gcd_assign(gcd, t_ij, gcd);
        if (gcd == 1)
          return;
      }
    }
  }
  PPL_ASSERT(gcd > 1);
  // Normalize all coefficients.
  for (dimension_type i = num_rows; i-- > 0; ) {
    matrix_row_reference_type s_i = s[i];
    matrix_row_iterator j = s_i.begin();
    matrix_row_iterator j_end = s_i.end();
    for ( ; j != j_end; ++j) {
      Coefficient& s_ij = (*j).second;
      exact_div_assign(s_ij, s_ij, gcd);
    }
    matrix_row_reference_type t_i = t[i];
    j = t_i.begin();
    j_end = t_i.end();
    for ( ; j != j_end; ++j) {
      Coefficient& t_ij = (*j).second;
      exact_div_assign(t_ij, t_ij, gcd);
    }
  }
  // Normalize denominator.
  exact_div_assign(denom, denom, gcd);
}

void
PIP_Solution_Node::Tableau::scale(Coefficient_traits::const_reference ratio) {
  for (dimension_type i = s.num_rows(); i-- > 0; ) {
    {
      matrix_row_reference_type s_i = s[i];
      matrix_row_iterator j = s_i.begin();
      matrix_row_iterator j_end = s_i.end();
      for ( ; j != j_end; ++j)
        (*j).second *= ratio;
    }
    {
      matrix_row_reference_type t_i = t[i];
      matrix_row_iterator j = t_i.begin();
      matrix_row_iterator j_end = t_i.end();
      for ( ; j != j_end; ++j)
        (*j).second *= ratio;
    }
  }
  denom *= ratio;
}

bool
PIP_Solution_Node::Tableau
::is_better_pivot(const std::vector<dimension_type>& mapping,
                  const std::vector<bool>& basis,
                  const dimension_type row_0,
                  const dimension_type col_0,
                  const dimension_type row_1,
                  const dimension_type col_1) const {
  const dimension_type num_params = t.num_columns();
  const dimension_type num_rows = s.num_rows();
  matrix_row_const_reference_type s_0 = s[row_0];
  matrix_row_const_reference_type s_1 = s[row_1];
  const Coefficient& s_0_0 = s_0.get(col_0);
  const Coefficient& s_1_1 = s_1.get(col_1);
  matrix_row_const_reference_type t_0 = t[row_0];
  matrix_row_const_reference_type t_1 = t[row_1];
  PPL_DIRTY_TEMP_COEFFICIENT(product_0);
  PPL_DIRTY_TEMP_COEFFICIENT(product_1);
  // On exit from the loop, if j_mismatch == num_params then
  // no column mismatch was found.
  dimension_type j_mismatch = num_params;
  std::vector<Coefficient> coeff_0(num_params,0);
  std::vector<Coefficient> coeff_1(num_params,0);
  {
    matrix_row_const_iterator j = t_0.begin();
    matrix_row_const_iterator j_end = t_0.end();
    for ( ; j != j_end; ++j)
      coeff_0[(*j).first] = (*j).second * s_1_1;
    j = t_1.begin();
    j_end = t_1.end();
    for ( ; j != j_end; ++j)
      coeff_1[(*j).first] = (*j).second * s_0_0;
  }
  for (dimension_type i = 0; i < num_rows; ++i) {
    matrix_row_const_reference_type s_i = s[i];
    const Coefficient* s_i_col_0;
    const Coefficient* s_i_col_1;
    s_i.get2(col_0, col_1, s_i_col_0, s_i_col_1);
    for (dimension_type j = 0; j < num_params; ++j) {
      product_0 = coeff_0[j] * *s_i_col_0;
      product_1 = coeff_1[j] * *s_i_col_1;
      if (product_0 != product_1) {
        // Mismatch found: exit from both loops.
        j_mismatch = j;
        goto end_loop;
      }
    }
  }

 end_loop:
  return (j_mismatch != num_params)
    && column_lower(s, mapping, basis, s_0, col_0, s_1, col_1,
                    t_0.get(j_mismatch), t_1.get(j_mismatch));
}

void
PIP_Tree_Node::ascii_dump(std::ostream& s) const {
  s << "constraints_\n";
  constraints_.ascii_dump(s);
  dimension_type artificial_parameters_size = artificial_parameters.size();
  s << "\nartificial_parameters( " << artificial_parameters_size << " )\n";
  for (dimension_type i = 0; i < artificial_parameters_size; ++i)
    artificial_parameters[i].ascii_dump(s);
}

bool
PIP_Tree_Node::ascii_load(std::istream& s) {
  std::string str;
  if (!(s >> str) || str != "constraints_")
    return false;
  constraints_.ascii_load(s);

  if (!(s >> str) || str != "artificial_parameters(")
    return false;
  dimension_type artificial_parameters_size;
  if (!(s >> artificial_parameters_size))
    return false;
  if (!(s >> str) || str != ")")
    return false;
  Artificial_Parameter ap;
  for (dimension_type i = 0; i < artificial_parameters_size; ++i) {
    if (!ap.ascii_load(s))
      return false;
    artificial_parameters.push_back(ap);
  }

  // Note: do not assert OK() here.
  // The node invariants should be checked on derived nodes.
  return true;
}

PIP_Tree_Node*
PIP_Solution_Node::clone() const {
  return new PIP_Solution_Node(*this);
}

PIP_Tree_Node*
PIP_Decision_Node::clone() const {
  return new PIP_Decision_Node(*this);
}

void
PIP_Solution_Node::Tableau::ascii_dump(std::ostream& st) const {
  st << "denominator " << denom << "\n";
  st << "variables ";
  s.ascii_dump(st);
  st << "parameters ";
  t.ascii_dump(st);
}

bool
PIP_Solution_Node::Tableau::ascii_load(std::istream& st) {
  std::string str;
  if (!(st >> str) || str != "denominator")
    return false;
  Coefficient den;
  if (!(st >> den))
    return false;
  denom = den;
  if (!(st >> str) || str != "variables")
    return false;
  if (!s.ascii_load(st))
    return false;
  if (!(st >> str) || str != "parameters")
    return false;
  if (!t.ascii_load(st))
    return false;
  PPL_ASSERT(OK());
  return true;
}

void
PIP_Solution_Node::ascii_dump(std::ostream& s) const {
  PIP_Tree_Node::ascii_dump(s);

  s << "\ntableau\n";
  tableau.ascii_dump(s);

  s << "\nbasis ";
  dimension_type basis_size = basis.size();
  s << basis_size;
  for (dimension_type i = 0; i < basis_size; ++i)
    s << (basis[i] ? " true" : " false");

  s << "\nmapping ";
  dimension_type mapping_size = mapping.size();
  s << mapping_size;
  for (dimension_type i = 0; i < mapping_size; ++i)
    s << " " << mapping[i];

  s << "\nvar_row ";
  dimension_type var_row_size = var_row.size();
  s << var_row_size;
  for (dimension_type i = 0; i < var_row_size; ++i)
    s << " " << var_row[i];

  s << "\nvar_column ";
  dimension_type var_column_size = var_column.size();
  s << var_column_size;
  for (dimension_type i = 0; i < var_column_size; ++i)
    s << " " << var_column[i];
  s << "\n";

  s << "special_equality_row " << special_equality_row << "\n";
  s << "big_dimension " << big_dimension << "\n";

  s << "sign ";
  dimension_type sign_size = sign.size();
  s << sign_size;
  for (dimension_type i = 0; i < sign_size; ++i) {
    s << " ";
    switch (sign[i]) {
    case UNKNOWN:
      s << "UNKNOWN";
      break;
    case ZERO:
      s << "ZERO";
      break;
    case POSITIVE:
      s << "POSITIVE";
      break;
    case NEGATIVE:
      s << "NEGATIVE";
      break;
    case MIXED:
      s << "MIXED";
      break;
    }
  }
  s << "\n";

  dimension_type solution_size = solution.size();
  s << "solution " << solution_size << "\n";
  for (dimension_type i = 0; i < solution_size; ++i)
    solution[i].ascii_dump(s);
  s << "\n";

  s << "solution_valid " << (solution_valid ? "true" : "false") << "\n";
}

bool
PIP_Solution_Node::ascii_load(std::istream& s) {
  if (!PIP_Tree_Node::ascii_load(s))
    return false;

  std::string str;
  if (!(s >> str) || str != "tableau")
    return false;
  if (!tableau.ascii_load(s))
    return false;

  if (!(s >> str) || str != "basis")
    return false;
  dimension_type basis_size;
  if (!(s >> basis_size))
    return false;
  basis.clear();
  for (dimension_type i = 0; i < basis_size; ++i) {
    if (!(s >> str))
      return false;
    bool val = false;
    if (str == "true")
      val = true;
    else if (str != "false")
      return false;
    basis.push_back(val);
  }

  if (!(s >> str) || str != "mapping")
    return false;
  dimension_type mapping_size;
  if (!(s >> mapping_size))
    return false;
  mapping.clear();
  for (dimension_type i = 0; i < mapping_size; ++i) {
    dimension_type val;
    if (!(s >> val))
      return false;
    mapping.push_back(val);
  }

  if (!(s >> str) || str != "var_row")
    return false;
  dimension_type var_row_size;
  if (!(s >> var_row_size))
    return false;
  var_row.clear();
  for (dimension_type i = 0; i < var_row_size; ++i) {
    dimension_type val;
    if (!(s >> val))
      return false;
    var_row.push_back(val);
  }

  if (!(s >> str) || str != "var_column")
    return false;
  dimension_type var_column_size;
  if (!(s >> var_column_size))
    return false;
  var_column.clear();
  for (dimension_type i = 0; i < var_column_size; ++i) {
    dimension_type val;
    if (!(s >> val))
      return false;
    var_column.push_back(val);
  }

  if (!(s >> str) || str != "special_equality_row")
    return false;
  if (!(s >> special_equality_row))
    return false;

  if (!(s >> str) || str != "big_dimension")
    return false;
  if (!(s >> big_dimension))
    return false;

  if (!(s >> str) || str != "sign")
    return false;
  dimension_type sign_size;
  if (!(s >> sign_size))
    return false;
  sign.clear();
  for (dimension_type i = 0; i < sign_size; ++i) {
    if (!(s >> str))
      return false;
    Row_Sign val;
    if (str == "UNKNOWN")
      val = UNKNOWN;
    else if (str == "ZERO")
      val = ZERO;
    else if (str == "POSITIVE")
      val = POSITIVE;
    else if (str == "NEGATIVE")
      val = NEGATIVE;
    else if (str == "MIXED")
      val = MIXED;
    else
      return false;
    sign.push_back(val);
  }

  if (!(s >> str) || str != "solution")
    return false;
  dimension_type solution_size;
  if (!(s >> solution_size))
    return false;
  solution.clear();
  for (dimension_type i = 0; i < solution_size; ++i) {
    Linear_Expression val;
    if (!val.ascii_load(s))
      return false;
    solution.push_back(val);
  }

  if (!(s >> str) || str != "solution_valid")
    return false;
  if (!(s >> str))
    return false;
  if (str == "true")
    solution_valid = true;
  else if (str == "false")
    solution_valid = false;
  else
    return false;

  PPL_ASSERT(OK());
  return true;
}

PIP_Solution_Node::Row_Sign
PIP_Solution_Node::row_sign(matrix_row_const_reference_type x,
                            const dimension_type big_dimension) {
  if (big_dimension != not_a_dimension()) {
    // If a big parameter has been set and its coefficient is not zero,
    // then return the sign of the coefficient.
    const Coefficient& x_big = x.get(big_dimension);
    if (x_big > 0)
      return POSITIVE;
    if (x_big < 0)
      return NEGATIVE;
    // Otherwise x_big == 0, then no big parameter involved.
  }

  PIP_Solution_Node::Row_Sign sign = ZERO;
  for (matrix_const_row_const_iterator i = x.begin(), i_end = x.end();
       i != i_end; ++i) {
    const Coefficient& x_i = (*i).second;
    if (x_i > 0) {
      if (sign == NEGATIVE)
        return MIXED;
      sign = POSITIVE;
    }
    else if (x_i < 0) {
      if (sign == POSITIVE)
        return MIXED;
      sign = NEGATIVE;
    }
  }
  return sign;
}

bool
PIP_Tree_Node::compatibility_check(const matrix_type& context,
                                   matrix_row_const_reference_type row) {
  // CHECKME: do `context' and `row' have compatible (row) capacity?
  matrix_type s(context);
  s.add_row(row);
  return compatibility_check(s);
}

bool
PIP_Tree_Node::compatibility_check(matrix_type& s) {
  PPL_ASSERT(s.OK());
  // Note: num_rows may increase.
  dimension_type num_rows = s.num_rows();
  const dimension_type num_cols = s.num_columns();
  const dimension_type num_vars = num_cols - 1;

  std::vector<Coefficient> scaling(num_rows, 1);
  std::vector<bool> basis;
  basis.reserve(num_vars + num_rows);
  std::vector<dimension_type> mapping;
  mapping.reserve(num_vars + num_rows);
  std::vector<dimension_type> var_row;
  var_row.reserve(num_rows);
  std::vector<dimension_type> var_column;
  var_column.reserve(num_cols);

  // Column 0 is the constant term, not a variable
  var_column.push_back(not_a_dimension());
  for (dimension_type j = 1; j <= num_vars; ++j) {
    basis.push_back(true);
    mapping.push_back(j);
    var_column.push_back(j - 1);
  }
  for (dimension_type i = 0; i < num_rows; ++i) {
    basis.push_back(false);
    mapping.push_back(i);
    var_row.push_back(i + num_vars);
  }

  // Scaling factor (i.e., denominator) for pivot coefficients.
  PPL_DIRTY_TEMP_COEFFICIENT(pivot_den);
  // Allocate once and for all: short life temporaries.
  PPL_DIRTY_TEMP_COEFFICIENT(product);
  PPL_DIRTY_TEMP_COEFFICIENT(gcd);
  PPL_DIRTY_TEMP_COEFFICIENT(scale_factor);

  // Perform simplex pivots on the context
  // until we find an empty solution or an optimum.
  while (true) {
    // Check if the client has requested abandoning all expensive
    // computations. If so, the exception specified by the client
    // is thrown now.
    maybe_abandon();

    // pi is the pivot's row index.
    dimension_type pi = num_rows;
    // pj is the pivot's column index.
    dimension_type pj = 0;

    bool found_positive_pivot_candidate
      = compatibility_check_find_pivot(s, mapping, basis, pi, pj);

    if (!found_positive_pivot_candidate)
      return false;

    if (pj == 0) {
      // No negative RHS: fractional optimum found.
      // If it is integer, then the test is successful.
      // Otherwise, generate a new cut.
      bool all_integer_vars = true;
      // NOTE: iterating downwards would be correct, but it would change
      // the ordering of cut generation.
      for (dimension_type i = 0; i < num_vars; ++i) {
        if (basis[i])
          // Basic variable = 0, hence integer.
          continue;
        // Not a basic variable.
        const dimension_type mi = mapping[i];
        const Coefficient& den = scaling[mi];
        if (s[mi].get(0) % den == 0)
          continue;
        // Here constant term is not integer.
        all_integer_vars = false;
        // Generate a new cut.
        var_row.push_back(mapping.size());
        basis.push_back(false);
        mapping.push_back(num_rows);
        s.add_zero_rows(1);
        matrix_row_reference_type cut = s[num_rows];
        ++num_rows;
        matrix_row_const_reference_type s_mi = s[mi];
        matrix_row_iterator cut_i = cut.begin();
        matrix_row_iterator cut_end = cut.end();
        matrix_const_row_const_iterator row_i = s_mi.begin();
        matrix_const_row_const_iterator row_end = s_mi.end();
        if (row_i != row_end) {
          cut_i = cut.find_create((*row_i).first,(*row_i).second);
          mod_assign((*cut_i).second,(*cut_i).second,den);
          for (++row_i; row_i != row_end; ++row_i) {
            cut_i = cut.find_create((*row_i).first,(*row_i).second,cut_i);
            mod_assign((*cut_i).second,(*cut_i).second,den);
          }
        }
        cut[0] -= den;
        scaling.push_back(den);
      }
      // Check if an integer solution was found.
      if (all_integer_vars)
        return true;
      else
        continue;
    }

    // Here we have a positive s[pi][pj] pivot.

    // Normalize the tableau before pivoting.
    for (dimension_type i = num_rows; i-- > 0; )
      row_normalize(s[i], scaling[i]);

    // Update basis.
    {
      const dimension_type var_pi = var_row[pi];
      const dimension_type var_pj = var_column[pj];
      var_row[pi] = var_pj;
      var_column[pj] = var_pi;
      basis[var_pi] = true;
      basis[var_pj] = false;
      mapping[var_pi] = pj;
      mapping[var_pj] = pi;
    }

    // Create an identity row corresponding to basic variable pj.
    s.add_zero_rows(1);
    matrix_row_reference_type pivot = s[num_rows];
    pivot[pj] = 1;

    // Swap identity row with the pivot row previously found.
    std::swap(pivot, s[pi]);
    // Save original pivot scaling factor in a temporary,
    // then reset scaling factor for identity row.
    pivot_den = scaling[pi];
    scaling[pi] = 1;

    // Perform a pivot operation on the matrix.
    const Coefficient& pivot_pj = pivot.get(pj);
    {
      matrix_const_row_const_iterator j = pivot.begin();
      matrix_const_row_const_iterator j_end = pivot.end();
      for ( ; j!=j_end; ++j ) {
        if ((*j).first == pj)
          continue;
        const Coefficient& pivot_j = (*j).second;
        // Do nothing if the j-th pivot element is zero.
        if (pivot_j == 0)
          continue;
        for (dimension_type i = num_rows; i-- > 0; ) {
          matrix_row_reference_type s_i = s[i];
          product = s_i.get(pj) * pivot_j;
          if (product % pivot_pj != 0) {
            // Must scale row s_i to stay in integer case.
            gcd_assign(gcd, product, pivot_pj);
            exact_div_assign(scale_factor, pivot_pj, gcd);
            matrix_row_iterator k = s_i.begin();
            matrix_row_iterator k_end = s_i.end();
            for ( ; k != k_end; ++k )
              (*k).second *= scale_factor;
            product *= scale_factor;
            scaling[i] *= scale_factor;
          }
          PPL_ASSERT(product % pivot_pj == 0);
          exact_div_assign(product, product, pivot_pj);
          s_i[(*j).first] -= product;
        }
      }
    }
    // Update column only if pivot coordinate != 1.
    if (pivot_pj != pivot_den) {
      for (dimension_type i = num_rows; i-- > 0; ) {
        matrix_row_reference_type s_i = s[i];
        Coefficient& s_i_pj = s_i[pj];
        product = s_i_pj * pivot_den;
        if (product % pivot_pj != 0) {
          // As above, perform row scaling.
          gcd_assign(gcd, product, pivot_pj);
          exact_div_assign(scale_factor, pivot_pj, gcd);
          matrix_row_iterator k = s_i.begin();
          matrix_row_iterator k_end = s_i.end();
          for ( ; k != k_end; ++k )
            (*k).second *= scale_factor;
          product *= scale_factor;
          scaling[i] *= scale_factor;
        }
        PPL_ASSERT(product % pivot_pj == 0);
        exact_div_assign(s_i_pj, product, pivot_pj);
      }
    }
    // Drop pivot to restore proper matrix size.
    s.erase_to_end(num_rows);
  }

  // This point should be unreachable.
  throw std::runtime_error("PPL internal error");
}

void
PIP_Solution_Node::update_tableau(const PIP_Problem& pip,
                                  const dimension_type external_space_dim,
                                  const dimension_type first_pending_constraint,
                                  const Constraint_Sequence& input_cs,
                                  const Variables_Set& parameters) {
  // Make sure a parameter column exists, for the inhomogeneous term.
  if (tableau.t.num_columns() == 0)
    tableau.t.add_zero_columns(1);

  // NOTE: here 'params' stands for problem (i.e., non artificial) parameters.
  const dimension_type old_num_vars = tableau.s.num_columns();
  const dimension_type old_num_params
    = pip.internal_space_dim - old_num_vars;
  const dimension_type num_added_dims
    = pip.external_space_dim - pip.internal_space_dim;
  const dimension_type new_num_params = parameters.size();
  const dimension_type num_added_params = new_num_params - old_num_params;
  const dimension_type num_added_vars = num_added_dims - num_added_params;

  const dimension_type old_num_art_params
    = tableau.t.num_columns() - 1 - old_num_params;

  // Resize the two tableau matrices.
  if (num_added_vars > 0)
    tableau.s.add_zero_columns(num_added_vars);
  if (num_added_params > 0)
    tableau.t.add_zero_columns(num_added_params);

  if (num_added_params > 0 && old_num_art_params > 0) {
    // Shift to the right the columns of artificial parameters.
    std::vector<dimension_type> swaps;
    swaps.reserve(3 * old_num_art_params);
    const dimension_type first_ap = 1 + old_num_params;
    for (dimension_type i = 0; i < old_num_art_params; ++i) {
      dimension_type old_ap = first_ap + i;
      dimension_type new_ap = old_ap + num_added_params;
      swaps.push_back(old_ap);
      swaps.push_back(new_ap);
      swaps.push_back(0);
    }
    tableau.t.permute_columns(swaps);
  }

  dimension_type new_var_column = old_num_vars;
  const dimension_type initial_space_dim = old_num_vars + old_num_params;
  for (dimension_type i = initial_space_dim; i < external_space_dim; ++i) {
    if (parameters.count(i) == 0) {
      // A new problem variable.
      if (tableau.s.num_rows() == 0) {
        // No rows have been added yet
        basis.push_back(true);
        mapping.push_back(new_var_column);
      }
      else {
        /*
          Need to insert the original variable id
          before the slack variable id's to respect variable ordering.
        */
        basis.insert(basis.begin() + new_var_column, true);
        mapping.insert(mapping.begin() + new_var_column, new_var_column);
        // Update variable id's of slack variables.
        for (dimension_type j = var_row.size(); j-- > 0; )
          if (var_row[j] >= new_var_column)
            ++var_row[j];
        for (dimension_type j = var_column.size(); j-- > 0; )
          if (var_column[j] >= new_var_column)
            ++var_column[j];
        if (special_equality_row > 0)
          ++special_equality_row;
      }
      var_column.push_back(new_var_column);
      ++new_var_column;
    }
  }

  if (big_dimension == not_a_dimension()
      && pip.big_parameter_dimension != not_a_dimension()) {
    // Compute the column number of big parameter in tableau.t matrix.
    Variables_Set::const_iterator pos
      = parameters.find(pip.big_parameter_dimension);
    big_dimension = std::distance(parameters.begin(), pos) + 1;
  }

  const Coefficient& denom = tableau.denominator();
  for (Constraint_Sequence::const_iterator
         c_iter = input_cs.begin() + first_pending_constraint,
         c_end = input_cs.end(); c_iter != c_end; ++c_iter) {
    const Constraint& constraint = *c_iter;
    // (Tentatively) Add new rows to s and t matrices.
    // These will be removed at the end if they turn out to be useless.
    const dimension_type row_id = tableau.s.num_rows();
    tableau.s.add_zero_rows(1);
    tableau.t.add_zero_rows(1);
    matrix_row_reference_type v_row = tableau.s[row_id];
    matrix_row_reference_type p_row = tableau.t[row_id];

    // Setting the inhomogeneus term.
    if (constraint.inhomogeneous_term() != 0) {
      Coefficient& p_row0 = p_row[0];
      p_row0 = constraint.inhomogeneous_term();
      if (constraint.is_strict_inequality())
        // Transform (expr > 0) into (expr - 1 >= 0).
        --p_row0;
      p_row0 *= denom;
    } else
      if (constraint.is_strict_inequality()) {
        Coefficient& p_row0 = p_row[0];
        // Transform (expr > 0) into (expr - 1 >= 0).
        --p_row0;
        p_row0 *= denom;
      }

    {
      dimension_type p_index = 1;
      dimension_type v_index = 0;
      matrix_row_iterator p_row_itr = p_row.end();
      dimension_type i = 0;
      dimension_type i_end = constraint.space_dimension();
      // Used to minimize the numer of (slow) insertions in v_row that don't
      // use a iterator as a hint.
      std::map<dimension_type,Coefficient> v_row_map;
      for ( ; i != i_end; ++i) {
        const bool is_parameter = (1 == parameters.count(i));
        const Coefficient& coeff_i = constraint.coefficient(Variable(i));
        if (coeff_i == 0) {
          // Optimize computation below: only update p/v index.
          if (is_parameter)
            ++p_index;
          else
            ++v_index;
          // Jump to next iteration.
          continue;
        }

        if (is_parameter) {
          p_row_itr = p_row.find_create(p_index, coeff_i * denom);
          ++p_index;
          ++i;
          break;
        }
        else {
          const dimension_type mv = mapping[v_index];
          if (basis[v_index])
            // Basic variable : add coeff_i * x_i
            add_mul_assign(v_row_map[mv], coeff_i, denom);
          else {
            // Dump v_row_map to v_row.
            {
              std::map<dimension_type,Coefficient>::const_iterator j
                = v_row_map.begin();
              std::map<dimension_type,Coefficient>::const_iterator j_end
                = v_row_map.end();
              if (j != j_end) {
                matrix_row_iterator itr = v_row.find_create(j->first,
                                                            j->second);
                ++j;
                for ( ; j != j_end; ++j)
                  itr = v_row.find_create(j->first, j->second,itr);
              }
            }
            v_row_map.clear();
            // Non-basic variable : add coeff_i * row_i
            add_mul_assign_row(v_row, coeff_i, tableau.s[mv]);
            add_mul_assign_row(p_row, coeff_i, tableau.t[mv]);
          }
          ++v_index;
        }
      }
      for ( ; i != i_end; ++i) {
        const bool is_parameter = (1 == parameters.count(i));
        const Coefficient& coeff_i = constraint.coefficient(Variable(i));
        if (coeff_i == 0) {
          // Optimize computation below: only update p/v index.
          if (is_parameter)
            ++p_index;
          else
            ++v_index;
          // Jump to next iteration.
          continue;
        }

        if (is_parameter) {
          p_row_itr = p_row.find_create(p_index,coeff_i * denom,p_row_itr);
          ++p_index;
        }
        else {
          const dimension_type mv = mapping[v_index];
          if (basis[v_index])
            // Basic variable : add coeff_i * x_i
            add_mul_assign(v_row_map[mv], coeff_i, denom);
          else {
            // Dump v_row_map to v_row.
            {
              std::map<dimension_type,Coefficient>::const_iterator j
                = v_row_map.begin();
              std::map<dimension_type,Coefficient>::const_iterator j_end
                = v_row_map.end();
              if (j != j_end) {
                matrix_row_iterator itr = v_row.find_create(j->first,
                                                            j->second);
                ++j;
                for ( ; j != j_end; ++j)
                  itr = v_row.find_create(j->first, j->second,itr);
              }
            }
            v_row_map.clear();
            // Non-basic variable : add coeff_i * row_i
            add_mul_assign_row(v_row, coeff_i, tableau.s[mv]);
            add_mul_assign_row(p_row, coeff_i, tableau.t[mv]);
          }
          ++v_index;
        }
      }
      // Dump v_row_map to v_row.
      {
        std::map<dimension_type,Coefficient>::const_iterator j
          = v_row_map.begin();
        std::map<dimension_type,Coefficient>::const_iterator j_end
          = v_row_map.end();
        if (j != j_end) {
          matrix_row_iterator itr = v_row.find_create(j->first, j->second);
          ++j;
          for ( ; j != j_end; ++j)
            itr = v_row.find_create(j->first, j->second,itr);
        }
      }
    }

    if (row_sign(v_row, not_a_dimension()) == ZERO) {
      // Parametric-only constraints have already been inserted in
      // initial context, so no need to insert them in the tableau.
      tableau.s.erase_to_end(row_id);
      tableau.t.erase_to_end(row_id);
    }
    else {
      const dimension_type var_id = mapping.size();
      sign.push_back(row_sign(p_row, big_dimension));
      basis.push_back(false);
      mapping.push_back(row_id);
      var_row.push_back(var_id);
      if (constraint.is_equality()) {
        // Handle equality constraints.
        // After having added the f_i(x,p) >= 0 constraint,
        // we must add -f_i(x,p) to the special equality row.
        if (special_equality_row == 0 || basis[special_equality_row]) {
          // The special constraint has not been created yet
          // FIXME: for now, we don't handle the case where the variable
          // is basic, and we just create a new row.
          // This might be faster however.
          tableau.s.add_zero_rows(1);
          tableau.t.add_zero_rows(1);
          // NOTE: addition of rows invalidates references v_row and p_row
          // due to possible matrix reallocations: recompute them.
          neg_assign_row(tableau.s[1 + row_id], tableau.s[row_id]);
          neg_assign_row(tableau.t[1 + row_id], tableau.t[row_id]);
          sign.push_back(row_sign(tableau.t[1 + row_id], big_dimension));
          special_equality_row = mapping.size();
          basis.push_back(false);
          mapping.push_back(1 + row_id);
          var_row.push_back(1 + var_id);
        } else {
          // The special constraint already exists and is nonbasic.
          const dimension_type m_eq = mapping[special_equality_row];
          sub_assign(tableau.s[m_eq], v_row);
          sub_assign(tableau.t[m_eq], p_row);
        }
      }
    }
  }
  PPL_ASSERT(OK());
}

PIP_Tree_Node*
PIP_Solution_Node::solve(const PIP_Problem& pip,
                         const bool check_feasible_context,
                         const matrix_type& ctx,
                         const Variables_Set& params,
                         dimension_type space_dim) {
  // Reset current solution as invalid.
  solution_valid = false;

  matrix_type context(ctx);
  Variables_Set all_params(params);
  const dimension_type num_art_params = artificial_parameters.size();
  add_artificial_parameters(context, all_params, space_dim, num_art_params);
  merge_assign(context, constraints_, all_params);

  // If needed, (re-)check feasibility of context.
  if (check_feasible_context) {
    matrix_type ctx_copy(context);
    if (!compatibility_check(ctx_copy)) {
      delete this;
      return 0;
    }
  }

  const dimension_type not_a_dim = not_a_dimension();

  // Main loop of the simplex algorithm.
  while (true) {
    // Check if the client has requested abandoning all expensive
    // computations. If so, the exception specified by the client
    // is thrown now.
    maybe_abandon();

    PPL_ASSERT(OK());

    const dimension_type num_rows = tableau.t.num_rows();
    const dimension_type num_vars = tableau.s.num_columns();
    const dimension_type num_params = tableau.t.num_columns();
    const Coefficient& tableau_den = tableau.denominator();

#ifdef NOISY_PIP
    tableau.ascii_dump(std::cerr);
    std::cerr << "context ";
    context.ascii_dump(std::cerr);
#endif

    // (Re-) Compute parameter row signs.
    // While at it, keep track of the first parameter rows
    // having negative and mixed sign.
    dimension_type first_negative = not_a_dim;
    dimension_type first_mixed = not_a_dim;
    for (dimension_type i = 0; i < num_rows; ++i) {
      Row_Sign& sign_i = sign[i];
      if (sign_i == UNKNOWN || sign_i == MIXED)
        sign_i = row_sign(tableau.t[i], big_dimension);

      if (sign_i == NEGATIVE && first_negative == not_a_dim)
        first_negative = i;
      else if (sign_i == MIXED && first_mixed == not_a_dim)
        first_mixed = i;
    }

    // If no negative parameter row was found, try to refine the sign of
    // mixed rows using compatibility checks with the current context.
    if (first_negative == not_a_dim && first_mixed != not_a_dim) {
      for (dimension_type i = first_mixed; i < num_rows; ++i) {
        // Consider mixed sign parameter rows only.
        if (sign[i] != MIXED)
          continue;
        matrix_row_const_reference_type t_i = tableau.t[i];
        Row_Sign new_sign = ZERO;
        // Check compatibility for constraint t_i(z) >= 0.
        if (compatibility_check(context, t_i))
          new_sign = POSITIVE;
        // Check compatibility for constraint t_i(z) < 0,
        // i.e., -t_i(z) - 1 >= 0.
        matrix_row_copy_type t_i_compl(num_params);
        complement_assign(t_i_compl, t_i, tableau_den);
        if (compatibility_check(context, t_i_compl))
          new_sign = (new_sign == POSITIVE) ? MIXED : NEGATIVE;
        // Update sign for parameter row i.
        sign[i] = new_sign;
        // Maybe update first_negative and first_mixed.
        if (new_sign == NEGATIVE && first_negative == not_a_dim) {
          first_negative = i;
          if (i == first_mixed)
            first_mixed = not_a_dim;
        }
        else if (new_sign == MIXED) {
          if (first_mixed == not_a_dim)
            first_mixed = i;
        }
        else if (i == first_mixed)
          first_mixed = not_a_dim;
      }
    }

    // If there still is no negative parameter row and a mixed sign
    // parameter row (first_mixed) such that:
    //  - it has at least one positive variable coefficient;
    //  - constraint t_i(z) > 0 is not compatible with the context;
    // then this parameter row can be considered negative.
    if (first_negative == not_a_dim && first_mixed != not_a_dim) {
      for (dimension_type i = first_mixed; i < num_rows; ++i) {
        // Consider mixed sign parameter rows only.
        if (sign[i] != MIXED)
          continue;
        // Check for a positive variable coefficient.
        matrix_row_const_reference_type s_i = tableau.s[i];
        bool has_positive = false;
        {
          matrix_const_row_const_iterator j = s_i.begin();
          matrix_const_row_const_iterator j_end = s_i.end();
          for ( ; j != j_end; ++j)
            if ((*j).second > 0) {
              has_positive = true;
              break;
            }
        }
        if (!has_positive)
          continue;
        // Check compatibility of constraint t_i(z) > 0.
        matrix_row_copy_type row(tableau.t[i]);
        PPL_DIRTY_TEMP_COEFFICIENT(mod);
        Coefficient& row0 = row[0];
        mod_assign(mod, row0, tableau_den);
        row0 -= (mod == 0) ? tableau_den : mod;
        const bool compatible = compatibility_check(context, row);
        // Maybe update sign (and first_* indices).
        if (compatible) {
          // Sign is still mixed.
          if (first_mixed == not_a_dim)
            first_mixed = i;
        }
        else {
          // Sign becomes negative (i.e., no longer mixed).
          sign[i] = NEGATIVE;
          if (first_negative == not_a_dim)
            first_negative = i;
          if (first_mixed == i)
            first_mixed = not_a_dim;
        }
      }
    }

#ifdef NOISY_PIP
    std::cerr << "sign =";
    for (dimension_type i = 0; i < sign.size(); ++i)
      std::cerr << " " << "?0+-*"[sign[i]];
    std::cerr << std::endl;
#endif

    // If we have found a negative parameter row, then
    // either the problem is unfeasible, or a pivoting step is required.
    if (first_negative != not_a_dim) {

      // Search for the best pivot row.
      dimension_type pi = not_a_dim;
      dimension_type pj = not_a_dim;
      for (dimension_type i = first_negative; i < num_rows; ++i) {
        if (sign[i] != NEGATIVE)
          continue;
        dimension_type j;
        if (!find_lexico_minimum_column(tableau.s, mapping, basis,
                                        tableau.s[i], 0, j)) {
          // No positive s_ij was found: problem is unfeasible.
#ifdef NOISY_PIP
          std::cerr << "No positive pivot found: Solution = _|_\n";
#endif
          delete this;
          return 0;
        }
        if (pj == not_a_dim
            || tableau.is_better_pivot(mapping, basis, i, j, pi, pj)) {
          // Update pivot indices.
          pi = i;
          pj = j;
          if (pip.control_parameters[PIP_Problem::PIVOT_ROW_STRATEGY]
              == PIP_Problem::PIVOT_ROW_STRATEGY_FIRST)
            // Stop at first valid row.
            break;
        }
      }

#ifdef NOISY_PIP
      std::cerr << "Pivot (pi, pj) = (" << pi << ", " << pj << ")\n";
#endif

      // Normalize the tableau before pivoting.
      tableau.normalize();

      // Perform pivot operation.

      // Update basis.
      {
        const dimension_type var_pi = var_row[pi];
        const dimension_type var_pj = var_column[pj];
        var_row[pi] = var_pj;
        var_column[pj] = var_pi;
        basis[var_pi] = true;
        basis[var_pj] = false;
        mapping[var_pi] = pj;
        mapping[var_pj] = pi;
      }

      PPL_DIRTY_TEMP_COEFFICIENT(product);
      PPL_DIRTY_TEMP_COEFFICIENT(gcd);
      PPL_DIRTY_TEMP_COEFFICIENT(scale_factor);

      // Creating identity rows corresponding to basic variable pj:
      // 1. add them to tableau so as to have proper size and capacity;
      tableau.s.add_zero_rows(1);
      tableau.t.add_zero_rows(1);
      // 2. swap the rows just added with empty ones.
      matrix_row_copy_type s_pivot(0);
      matrix_row_copy_type t_pivot(0);
      s_pivot.swap(tableau.s[num_rows]);
      t_pivot.swap(tableau.t[num_rows]);
      // 3. drop rows previously added at end of tableau.
      tableau.s.erase_to_end(num_rows);
      tableau.t.erase_to_end(num_rows);

      // Save current pivot denominator.
      PPL_DIRTY_TEMP_COEFFICIENT(pivot_den);
      pivot_den = tableau.denominator();
      // Let the (scaled) pivot coordinate be 1.
      s_pivot[pj] = pivot_den;

      // Swap identity row with the pivot row previously found.
      s_pivot.swap(tableau.s[pi]);
      t_pivot.swap(tableau.t[pi]);
      sign[pi] = ZERO;

      PPL_DIRTY_TEMP_COEFFICIENT(s_pivot_pj);
      s_pivot_pj = s_pivot.get(pj);

      // Compute columns s[*][j] :
      // s[i][j] -= s[i][pj] * s_pivot[j] / s_pivot_pj;
      for (dimension_type i = num_rows; i-- > 0; ) {
        matrix_row_reference_type s_i = tableau.s[i];
        matrix_const_row_const_iterator j = s_pivot.begin();
        matrix_const_row_const_iterator j_end = s_pivot.end();
        matrix_row_iterator itr = s_i.end();
        for ( ; j != j_end; ++j) {
          if ((*j).first != pj) {
            const Coefficient& s_pivot_j = (*j).second;
            // Do nothing if the j-th pivot element is zero.
            if (s_pivot_j != 0) {
              product = s_pivot_j * s_i.get(pj);
              if (product % s_pivot_pj != 0) {
                // Must scale matrix to stay in integer case.
                gcd_assign(gcd, product, s_pivot_pj);
                exact_div_assign(scale_factor, s_pivot_pj, gcd);
                tableau.scale(scale_factor);
                product *= scale_factor;
              }
              PPL_ASSERT(product % s_pivot_pj == 0);
              exact_div_assign(product, product, s_pivot_pj);
              itr = s_i.find_create((*j).first);
              (*itr).second -= product;
              // Now itr has been initialized, use it in next calls to
              // find_create().
              ++j;
              break;
            }
          }
        }
        for ( ; j != j_end; ++j) {
          if ((*j).first != pj) {
            const Coefficient& s_pivot_j = (*j).second;
            // Do nothing if the j-th pivot element is zero.
            if (s_pivot_j != 0) {
              product = s_pivot_j * s_i.get(pj);
              if (product % s_pivot_pj != 0) {
                // Must scale matrix to stay in integer case.
                gcd_assign(gcd, product, s_pivot_pj);
                exact_div_assign(scale_factor, s_pivot_pj, gcd);
                tableau.scale(scale_factor);
                product *= scale_factor;
              }
              PPL_ASSERT(product % s_pivot_pj == 0);
              exact_div_assign(product, product, s_pivot_pj);
              itr = s_i.find_create((*j).first, itr);
              (*itr).second -= product;
            }
          }
        }
      }

      // Compute columns t[*][j] :
      // t[i][j] -= s[i][pj] * t_pivot[j] / s_pivot_pj;
      for (dimension_type i = num_rows; i-- > 0; ) {
        matrix_row_reference_type s_i = tableau.s[i];
        matrix_row_reference_type t_i = tableau.t[i];
        const Coefficient& s_i_pj = s_i.get(pj);
        matrix_const_row_const_iterator j = t_pivot.begin();
        matrix_const_row_const_iterator j_end = t_pivot.end();
        matrix_row_iterator k_end = t_i.end();
        matrix_row_iterator k = k_end;
        for ( ; j != j_end; ++j) {
          const Coefficient& t_pivot_j = (*j).second;
          // Do nothing if the j-th pivot element is zero.
          if (t_pivot_j != 0) {
            product = t_pivot_j * s_i_pj;
            if (product % s_pivot_pj != 0) {
              // Must scale matrix to stay in integer case.
              gcd_assign(gcd, product, s_pivot_pj);
              exact_div_assign(scale_factor, s_pivot_pj, gcd);
              tableau.scale(scale_factor);
              product *= scale_factor;
            }
            PPL_ASSERT(product % s_pivot_pj == 0);
            exact_div_assign(product, product, s_pivot_pj);
            k = t_i.find_create((*j).first);
            (*k).second -= product;

            // Update row sign.
            Row_Sign& sign_i = sign[i];
            switch (sign_i) {
            case ZERO:
              if (product > 0)
                sign_i = NEGATIVE;
              else if (product < 0)
                sign_i = POSITIVE;
              break;
            case POSITIVE:
              if (product > 0)
                sign_i = MIXED;
              break;
            case NEGATIVE:
              if (product < 0)
                sign_i = MIXED;
              break;
            default:
              break;
            }
            // Now k is initialized, so we can use it in the next calls to
            // find_create().
            ++j;
            break;
          }
        }
        for ( ; j!=j_end; ++j) {
          const Coefficient& t_pivot_j = (*j).second;
          // Do nothing if the j-th pivot element is zero.
          if (t_pivot_j != 0) {
            product = t_pivot_j * s_i_pj;
            if (product % s_pivot_pj != 0) {
              // Must scale matrix to stay in integer case.
              gcd_assign(gcd, product, s_pivot_pj);
              exact_div_assign(scale_factor, s_pivot_pj, gcd);
              tableau.scale(scale_factor);
              product *= scale_factor;
            }
            PPL_ASSERT(product % s_pivot_pj == 0);
            exact_div_assign(product, product, s_pivot_pj);
            k = t_i.find_create((*j).first, k);
            (*k).second -= product;

            // Update row sign.
            Row_Sign& sign_i = sign[i];
            switch (sign_i) {
            case ZERO:
              if (product > 0)
                sign_i = NEGATIVE;
              else if (product < 0)
                sign_i = POSITIVE;
              break;
            case POSITIVE:
              if (product > 0)
                sign_i = MIXED;
              break;
            case NEGATIVE:
              if (product < 0)
                sign_i = MIXED;
              break;
            default:
              break;
            }
          }
        }
      }

      // Compute column s[*][pj] : s[i][pj] /= s_pivot_pj;
      // Update column only if pivot coordinate != 1.
      if (s_pivot_pj != pivot_den) {
        for (dimension_type i = num_rows; i-- > 0; ) {
          matrix_row_reference_type s_i = tableau.s[i];
          product = s_i.get(pj) * pivot_den;
          if (product % s_pivot_pj != 0) {
            // As above, perform matrix scaling.
            gcd_assign(gcd, product, s_pivot_pj);
            exact_div_assign(scale_factor, s_pivot_pj, gcd);
            tableau.scale(scale_factor);
            product *= scale_factor;
          }
          PPL_ASSERT(product % s_pivot_pj == 0);
          if (product != 0 || s_i.get(pj) != 0) {
            Coefficient x = Coefficient_zero();
            exact_div_assign(x, product, s_pivot_pj);
            s_i[pj] = x;
          }
        }
      }

      // Pivoting process ended: jump to next iteration.
      continue;
    } // if (first_negative != not_a_dim)


    PPL_ASSERT(first_negative == not_a_dim);
    // If no negative parameter row was found,
    // but a mixed parameter row was found ...
    if (first_mixed != not_a_dim) {
      // Look for a constraint (i_neg):
      //  - having mixed parameter sign;
      //  - having no positive variable coefficient;
      //  - minimizing the score (sum of parameter coefficients).
      dimension_type i_neg = not_a_dim;
      PPL_DIRTY_TEMP_COEFFICIENT(best_score);
      PPL_DIRTY_TEMP_COEFFICIENT(score);
      for (dimension_type i = first_mixed; i < num_rows; ++i) {
        // Mixed parameter sign.
        if (sign[i] != MIXED)
          continue;
        // No positive variable coefficient.
        bool has_positive = false;
        {
          matrix_row_const_reference_type s_i = tableau.s[i];
          matrix_const_row_const_iterator j = s_i.begin();
          matrix_const_row_const_iterator j_end = s_i.end();
          for ( ; j != j_end; ++j)
            if ((*j).second > 0) {
              has_positive = true;
              break;
            }
        }
        if (has_positive)
          continue;
        // Minimize parameter coefficient score,
        // eliminating implicated tautologies (if any).
        matrix_row_const_reference_type t_i = tableau.t[i];
        score = 0;
        {
          matrix_const_row_const_iterator j = t_i.begin();
          matrix_const_row_const_iterator j_end = t_i.end();
          for ( ; j != j_end; ++j)
            score += (*j).second;
        }
        if (i_neg == not_a_dim || score < best_score) {
          i_neg = i;
          best_score = score;
        }
      }

      if (i_neg != not_a_dim) {
#ifdef NOISY_PIP
        std::cerr << "Found row (" << i_neg << ") with mixed parameter sign "
                  << "and negative variable coefficients.\n"
                  << "==> adding tautology.\n";
#endif
        matrix_row_copy_type copy = tableau.t[i_neg];
        copy.normalize();
        context.add_row(copy);
        add_constraint(copy, all_params);
        sign[i_neg] = POSITIVE;
        // Jump to next iteration.
        continue;
      }

      PPL_ASSERT(i_neg == not_a_dim);
      // Heuristically choose "best" (mixed) pivoting row.
      dimension_type best_i = not_a_dim;
      for (dimension_type i = first_mixed; i < num_rows; ++i) {
        if (sign[i] != MIXED)
          continue;
        matrix_row_const_reference_type t_i = tableau.t[i];
        score = 0;
        {
          matrix_const_row_const_iterator j = t_i.begin();
          matrix_const_row_const_iterator j_end = t_i.end();
          for ( ; j != j_end; ++j)
            score += (*j).second;
        }
        if (best_i == not_a_dim || score < best_score) {
          best_score = score;
          best_i = i;
        }
      }

      matrix_row_copy_type t_test(tableau.t[best_i]);
      t_test.normalize();
#ifdef NOISY_PIP
      {
        Linear_Expression expr = Linear_Expression(t_test.get(0));
        dimension_type j = 1;
        for (Variables_Set::const_iterator p = all_params.begin(),
               p_end = all_params.end(); p != p_end; ++p, ++j)
          expr += t_test.get(j) * Variable(*p);
        using namespace IO_Operators;
        std::cerr << "Found mixed parameter sign row: " << best_i << ".\n"
                  << "Solution depends on sign of parameter "
                  << expr << ".\n";
      }
#endif // #ifdef NOISY_PIP

      // Create a solution node for the "true" version of current node.
      PIP_Tree_Node* t_node = new PIP_Solution_Node(*this, No_Constraints());
      // Protect it from exception safety issues via std::auto_ptr.
      std::auto_ptr<PIP_Tree_Node> wrapped_node(t_node);

      // Add parametric constraint to context.
      context.add_row(t_test);
      // Recusively solve true node wrt updated context.
      t_node = t_node->solve(pip, check_feasible_context,
                             context, all_params, space_dim);
      // Resolution may have changed t_node: in case, rewrap it.
      if (t_node != wrapped_node.get()) {
        wrapped_node.release();
        wrapped_node.reset(t_node);
      }

      // Modify *this in place to become the "false" version of current node.
      PIP_Tree_Node* f_node = this;
      // Swap aside constraints and artificial parameters
      // (these will be later restored if needed).
      Constraint_System cs;
      Artificial_Parameter_Sequence aps;
      cs.swap(f_node->constraints_);
      aps.swap(f_node->artificial_parameters);
      // Compute the complement of the constraint used for the "true" node.
      matrix_row_reference_type f_test = context[context.num_rows() - 1];
      complement_assign(f_test, t_test, 1);

      // Recusively solve false node wrt updated context.
      f_node = f_node->solve(pip, check_feasible_context,
                             context, all_params, space_dim);

      // Case analysis on recursive resolution calls outcome.
      if (t_node == 0) {
        if (f_node == 0) {
          // Both t_node and f_node unfeasible.
          return 0;
        }
        else {
          // t_node unfeasible, f_node feasible:
          // restore cs and aps into f_node (i.e., this).
          PPL_ASSERT(f_node == this);
          f_node->constraints_.swap(cs);
          f_node->artificial_parameters.swap(aps);
          // Add f_test to constraints.
          f_node->add_constraint(f_test, all_params);
          return f_node;
        }
      }
      else if (f_node == 0) {
        // t_node feasible, f_node unfeasible:
        // restore cs and aps into t_node.
        t_node->constraints_.swap(cs);
        t_node->artificial_parameters.swap(aps);
        // Add t_test to t_nodes's constraints.
        t_node->add_constraint(t_test, all_params);
        // It is now safe to release previously wrapped t_node pointer
        // and return it to caller.
        return wrapped_node.release();
      }

      // Here both t_node and f_node are feasible:
      // create a new decision node.
      PIP_Tree_Node* parent
        = new PIP_Decision_Node(f_node->get_owner(), f_node, t_node);
      // Previously wrapped 't_node' is now safe: release it
      // and protect new 'parent' node from exception safety issues.
      wrapped_node.release();
      wrapped_node.reset(parent);

      // Add t_test to the constraints of the new decision node.
      parent->add_constraint(t_test, all_params);

      if (!cs.empty()) {
        // If node to be solved had tautologies,
        // store them in a new decision node.
        parent = new PIP_Decision_Node(parent->get_owner(), 0, parent);
        // Previously wrapped 'parent' node is now safe: release it
        // and protect new 'parent' node from exception safety issues.
        wrapped_node.release();
        wrapped_node.reset(parent);
        parent->constraints_.swap(cs);
      }
      parent->artificial_parameters.swap(aps);
      // It is now safe to release previously wrapped decision node
      // and return it to the caller.
      return wrapped_node.release();
    } // if (first_mixed != not_a_dim)


    PPL_ASSERT(first_negative == not_a_dim);
    PPL_ASSERT(first_mixed == not_a_dim);
    // Here all parameters are positive: we have found a continuous
    // solution. If the solution happens to be integer, then it is the
    // solution of the  integer problem. Otherwise, we may need to generate
    // a new cut to try and get back into the integer case.
#ifdef NOISY_PIP
    std::cout << "All parameters are positive.\n";
#endif
    tableau.normalize();

    // Look for any row having non integer parameter coefficients.
    const Coefficient& den = tableau.denominator();
    for (dimension_type k = 0; k < num_vars; ++k) {
      if (basis[k])
        // Basic variable = 0, hence integer.
        continue;
      const dimension_type i = mapping[k];
      matrix_row_const_reference_type t_i = tableau.t[i];
      matrix_const_row_const_iterator j = t_i.begin();
      matrix_const_row_const_iterator j_end = t_i.end();
      for ( ; j != j_end; ++j) {
        if ((*j).second % den != 0)
          goto non_integer;
      }
    }
    // The goto was not taken, the solution is integer.
#ifdef NOISY_PIP
    std::cout << "Solution found for problem in current node.\n";
#endif
    return this;

  non_integer:
    // The solution is non-integer: generate a cut.
    PPL_DIRTY_TEMP_COEFFICIENT(mod);
    dimension_type best_i = not_a_dim;
    dimension_type best_pcount = not_a_dim;

    const PIP_Problem::Control_Parameter_Value cutting_strategy
      = pip.control_parameters[PIP_Problem::CUTTING_STRATEGY];

    if (cutting_strategy == PIP_Problem::CUTTING_STRATEGY_FIRST) {
      // Find the first row with simplest parametric part.
      for (dimension_type k = 0; k < num_vars; ++k) {
        if (basis[k])
          continue;
        const dimension_type i = mapping[k];
        matrix_row_const_reference_type t_i = tableau.t[i];
        // Count the number of non-integer parameter coefficients.
        dimension_type pcount = 0;
        matrix_const_row_const_iterator j = t_i.begin();
        matrix_const_row_const_iterator j_end = t_i.end();
        for ( ; j != j_end; ++j) {
          mod_assign(mod, (*j).second, den);
          if (mod != 0)
            ++pcount;
        }
        if (pcount > 0 && (best_i == not_a_dim || pcount < best_pcount)) {
          best_pcount = pcount;
          best_i = i;
        }
      }
      // Generate cut using 'best_i'.
      generate_cut(best_i, all_params, context, space_dim);
    }
    else {
      assert(cutting_strategy == PIP_Problem::CUTTING_STRATEGY_DEEPEST
             || cutting_strategy == PIP_Problem::CUTTING_STRATEGY_ALL);
      // Find the row with simplest parametric part
      // which will generate the "deepest" cut.
      PPL_DIRTY_TEMP_COEFFICIENT(best_score);
      best_score = 0;
      PPL_DIRTY_TEMP_COEFFICIENT(score);
      PPL_DIRTY_TEMP_COEFFICIENT(s_score);
      std::vector<dimension_type> all_best_is;

      for (dimension_type k = 0; k < num_vars; ++k) {
        if (basis[k])
          continue;
        const dimension_type i = mapping[k];
        // Compute score and pcount.
        score = 0;
        dimension_type pcount = 0;
        matrix_row_const_reference_type t_i = tableau.t[i];
        {
          matrix_const_row_const_iterator j = t_i.begin();
          matrix_const_row_const_iterator j_end = t_i.end();
          for ( ; j != j_end; ++j) {
            mod_assign(mod, (*j).second, den);
            if (mod != 0) {
              score += den;
              score -= mod;
              ++pcount;
            }
          }
        }
        // Compute s_score.
        s_score = 0;
        matrix_row_const_reference_type s_i = tableau.s[i];
        {
          matrix_const_row_const_iterator j = s_i.begin();
          matrix_const_row_const_iterator j_end = s_i.end();
          for ( ; j != j_end; ++j) {
            mod_assign(mod, (*j).second, den);
            s_score += den;
            s_score -= mod;
          }
        }
        // Combine 'score' and 's_score'.
        score *= s_score;
        /*
          Select row i if it is non integer AND
            - no row has been chosen yet; OR
            - it has fewer non-integer parameter coefficients; OR
            - it has the same number of non-integer parameter coefficients,
              but its score is greater.
        */
        if (pcount != 0
            && (best_i == not_a_dim
                || pcount < best_pcount
                || (pcount == best_pcount && score > best_score))) {
          if (pcount < best_pcount)
            all_best_is.clear();
          best_i = i;
          best_pcount = pcount;
          best_score = score;
        }
        if (pcount > 0)
          all_best_is.push_back(i);
      }
      if (cutting_strategy == PIP_Problem::CUTTING_STRATEGY_DEEPEST)
        generate_cut(best_i, all_params, context, space_dim);
      else {
        PPL_ASSERT(cutting_strategy == PIP_Problem::CUTTING_STRATEGY_ALL);
        for (dimension_type k = all_best_is.size(); k-- > 0; )
          generate_cut(all_best_is[k], all_params, context, space_dim);
      }
    } // End of processing for non-integer solutions.

  } // Main loop of the simplex algorithm

  // This point should be unreachable.
  throw std::runtime_error("PPL internal error");
}

void
PIP_Solution_Node::generate_cut(const dimension_type index,
                                Variables_Set& parameters,
                                matrix_type& context,
                                dimension_type& space_dimension) {
  const dimension_type num_rows = tableau.t.num_rows();
  PPL_ASSERT(index < num_rows);
  const dimension_type num_vars = tableau.s.num_columns();
  const dimension_type num_params = tableau.t.num_columns();
  PPL_ASSERT(num_params == 1 + parameters.size());
  const Coefficient& den = tableau.denominator();

  PPL_DIRTY_TEMP_COEFFICIENT(mod);
  PPL_DIRTY_TEMP_COEFFICIENT(coeff);

#ifdef NOISY_PIP
  std::cout << "Row " << index << " contains non-integer coefficients. "
            << "Cut generation required."
            << std::endl;
#endif // #ifdef NOISY_PIP

  // Test if cut to be generated must be parametric or not.
  bool generate_parametric_cut = false;
  {
    // Limiting the scope of reference row_t (may be later invalidated).
    matrix_row_const_reference_type row_t = tableau.t[index];
    matrix_const_row_const_iterator j = row_t.lower_bound(1);
    matrix_const_row_const_iterator j_end = row_t.end();
    for ( ; j != j_end; ++j)
      if ((*j).second % den != 0) {
        generate_parametric_cut = true;
        break;
      }
  }

  // Column index of already existing Artificial_Parameter.
  dimension_type ap_column = not_a_dimension();
  bool reuse_ap = false;

  if (generate_parametric_cut) {
    // Fractional parameter coefficient found: generate parametric cut.
    Linear_Expression expr;

    // Limiting the scope of reference row_t (may be later invalidated).
    {
      matrix_row_const_reference_type row_t = tableau.t[index];
      mod_assign(mod, row_t.get(0), den);
      if (mod != 0) {
        // Optimizing computation: expr += (den - mod);
        expr += den;
        expr -= mod;
      }
      if (!parameters.empty()) {
        // To avoid reallocations of expr.
        add_mul_assign(expr, 0, Variable(*(parameters.rbegin())));
        Variables_Set::const_iterator p_j = parameters.begin();
        matrix_const_row_const_iterator j = row_t.lower_bound(1);
        matrix_const_row_const_iterator j_end = row_t.end();
        dimension_type last_index = 1;
        for ( ; j != j_end; ++j) {
          mod_assign(mod, (*j).second, den);
          if (mod != 0) {
            // Optimizing computation: expr += (den - mod) * Variable(*p_j);
            coeff = den - mod;
            PPL_ASSERT(last_index <= (*j).first);
            std::advance(p_j,(*j).first - last_index);
            last_index = (*j).first;
            add_mul_assign(expr, coeff, Variable(*p_j));
          }
        }
      }
    }
    // Generate new artificial parameter.
    Artificial_Parameter ap(expr, den);

    // Search if the Artificial_Parameter has already been generated.
    ap_column = space_dimension;
    const PIP_Tree_Node* node = this;
    do {
      for (dimension_type j = node->artificial_parameters.size(); j-- > 0; ) {
        --ap_column;
        if (node->artificial_parameters[j] == ap) {
          reuse_ap = true;
          break;
        }
      }
      node = node->parent();
    } while (!reuse_ap && node != 0);

    if (reuse_ap) {
      // We can re-use an existing Artificial_Parameter.
#ifdef NOISY_PIP
      using namespace IO_Operators;
      std::cout << "Re-using parameter " << Variable(ap_column)
                << " = (" << expr << ")/" << den
                << std::endl;
#endif // #ifdef NOISY_PIP
      ap_column = ap_column - num_vars + 1;
    }
    else {
      // Here reuse_ap == false: the Artificial_Parameter does not exist yet.
      // Beware: possible reallocation invalidates row references.
      tableau.t.add_zero_columns(1);
      context.add_zero_columns(1);
      artificial_parameters.push_back(ap);
      parameters.insert(space_dimension);
#ifdef NOISY_PIP
      using namespace IO_Operators;
      std::cout << "Creating new parameter "
                << Variable(space_dimension)
                << " = (" << expr << ")/" << den
                << std::endl;
#endif // #ifdef NOISY_PIP
      ++space_dimension;
      ap_column = num_params;

      // Update current context with constraints on the new parameter.
      const dimension_type ctx_num_rows = context.num_rows();
      context.add_zero_rows(2);
      matrix_row_reference_type ctx1 = context[ctx_num_rows];
      matrix_row_reference_type ctx2 = context[ctx_num_rows+1];
      // Recompute row reference after possible reallocation.
      matrix_row_const_reference_type row_t = tableau.t[index];
      {
        matrix_const_row_const_iterator j = row_t.begin();
        matrix_const_row_const_iterator j_end = row_t.end();
        matrix_row_iterator itr1 = ctx1.end();
        matrix_row_iterator itr2 = ctx2.end();
        for ( ; j != j_end; ++j) {
          mod_assign(mod, (*j).second, den);
          if (mod != 0) {
            const dimension_type j_index = (*j).first;
            itr1 = ctx1.find_create(j_index, den);
            (*itr1).second -= mod;
            itr2 = ctx2.find_create(j_index, (*itr1).second);
            neg_assign((*itr2).second);
            // Now itr1 and itr2 are valid, so we can use them in the next
            // calls to find_create().
            ++j;
            break;
          }
        }
        for ( ; j != j_end; ++j) {
          mod_assign(mod, (*j).second, den);
          if (mod != 0) {
            const dimension_type j_index = (*j).first;
            itr1 = ctx1.find_create(j_index, den, itr1);
            (*itr1).second -= mod;
            itr2 = ctx2.find_create(j_index, (*itr1).second, itr2);
            neg_assign((*itr2).second);
          }
        }
        if (itr1 != ctx1.end()) {
          itr1 = ctx1.find_create(num_params, den, itr1);
          neg_assign((*itr1).second);
          ctx2.find_create(num_params, den, itr2);
        } else {
          itr1 = ctx1.find_create(num_params, den);
          neg_assign((*itr1).second);
          ctx2.find_create(num_params ,den);
        }
      }
      // ctx2[0] += den-1;
      Coefficient& ctx2_0 = ctx2[0];
      ctx2_0 += den;
      --ctx2_0;
#ifdef NOISY_PIP
      {
        using namespace IO_Operators;
        Variables_Set::const_iterator p = parameters.begin();
        Linear_Expression expr1(ctx1.get(0));
        Linear_Expression expr2(ctx2_0);
        for (dimension_type j = 1; j <= num_params; ++j, ++p) {
          expr1 += ctx1.get(j) * Variable(*p);
          expr2 += ctx2.get(j) * Variable(*p);
        }
        std::cout << "Inserting into context: "
                  << Constraint(expr1 >= 0) << " ; "
                  << Constraint(expr2 >= 0) << std::endl;
      }
#endif // #ifdef NOISY_PIP
    }
  }

  // Generate new cut.
  tableau.s.add_zero_rows(1);
  tableau.t.add_zero_rows(1);
  matrix_row_reference_type cut_s = tableau.s[num_rows];
  matrix_row_reference_type cut_t = tableau.t[num_rows];
  // Recompute references after possible reallocation.
  matrix_row_const_reference_type row_s = tableau.s[index];
  matrix_row_const_reference_type row_t = tableau.t[index];
  {
    for (dimension_type j = 0; j < num_vars; ++j) {
      mod_assign(cut_s[j], row_s[j], den);
    }
  }
  {
    matrix_const_row_const_iterator j = row_t.begin();
    matrix_const_row_const_iterator j_end = row_t.end();
    matrix_row_iterator cut_t_itr = cut_t.end();
    for ( ; j != j_end; ++j) {
      mod_assign(mod, (*j).second, den);
      if (mod != 0) {
        cut_t_itr = cut_t.find_create((*j).first, mod);
        (*cut_t_itr).second -= den;
        // Now cut_t_itr is valid, so we'll pass it in the next calls to
        // find_create().
        ++j;
        break;
      }
    }
    for ( ; j!=j_end; ++j) {
      mod_assign(mod, (*j).second, den);
      if (mod != 0) {
        cut_t_itr = cut_t.find_create((*j).first, mod, cut_t_itr);
        (*cut_t_itr).second -= den;
      }
    }
  }
  if (ap_column != not_a_dimension())
    // If we re-use an existing Artificial_Parameter
    cut_t[ap_column] = den;

#ifdef NOISY_PIP
  {
    using namespace IO_Operators;
    Linear_Expression expr;
    dimension_type ti = 1;
    dimension_type si = 0;
    for (dimension_type j = 0; j < space_dimension; ++j) {
      if (parameters.count(j) == 1)
        expr += cut_t.get(ti++) * Variable(j);
      else
        expr += cut_s.get(si++) * Variable(j);
    }
    std::cout << "Adding cut: "
              << Constraint(expr + cut_t.get(0) >= 0)
              << std::endl;
  }
#endif
  var_row.push_back(num_rows + num_vars);
  basis.push_back(false);
  mapping.push_back(num_rows);
  sign.push_back(NEGATIVE);
}


memory_size_type
PIP_Tree_Node::Artificial_Parameter::external_memory_in_bytes() const {
  return Linear_Expression::external_memory_in_bytes()
    + Parma_Polyhedra_Library::external_memory_in_bytes(denom);
}

memory_size_type
PIP_Tree_Node::Artificial_Parameter::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

memory_size_type
PIP_Tree_Node::external_memory_in_bytes() const {
  memory_size_type n = constraints_.external_memory_in_bytes();
  // Adding the external memory for `artificial_parameters'.
  n += artificial_parameters.capacity() * sizeof(Artificial_Parameter);
  for (Artificial_Parameter_Sequence::const_iterator
        ap = art_parameter_begin(),
        ap_end = art_parameter_end(); ap != ap_end; ++ap)
    n += (ap->external_memory_in_bytes());

  return n;
}

memory_size_type
PIP_Decision_Node::external_memory_in_bytes() const {
  memory_size_type n = PIP_Tree_Node::external_memory_in_bytes();
  PPL_ASSERT(true_child != 0);
  n += true_child->total_memory_in_bytes();
  if (false_child)
    n += false_child->total_memory_in_bytes();
  return n;
}

memory_size_type
PIP_Decision_Node::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

memory_size_type
PIP_Solution_Node::Tableau::external_memory_in_bytes() const {
  return Parma_Polyhedra_Library::external_memory_in_bytes(denom)
    + s.external_memory_in_bytes()
    + t.external_memory_in_bytes();
}

memory_size_type
PIP_Solution_Node::external_memory_in_bytes() const {
  memory_size_type n = PIP_Tree_Node::external_memory_in_bytes();
  n += tableau.external_memory_in_bytes();
  // FIXME: size of std::vector<bool> ?
  n += basis.capacity() * sizeof(bool);
  n += sizeof(dimension_type)
    * (mapping.capacity() + var_row.capacity() + var_column.capacity());
  n += sign.capacity() * sizeof(Row_Sign);
  // FIXME: Adding the external memory for `solution'.
  n += solution.capacity() * sizeof(Linear_Expression);
  for (std::vector<Linear_Expression>::const_iterator
         i = solution.begin(), i_end = solution.end(); i != i_end; ++i)
    n += (i->external_memory_in_bytes());

  return n;
}

memory_size_type
PIP_Solution_Node::total_memory_in_bytes() const {
  return sizeof(*this) + external_memory_in_bytes();
}

void
PIP_Tree_Node::indent_and_print(std::ostream& s,
                                const unsigned indent,
                                const char* str) {
  s << std::setw(2*indent) << "" << str;
}

void
PIP_Tree_Node::print(std::ostream& s, unsigned indent) const {
  const dimension_type pip_space_dim = get_owner()->space_dimension();
  const Variables_Set& pip_params = get_owner()->parameter_space_dimensions();

  std::vector<bool> pip_dim_is_param(pip_space_dim);
  for (Variables_Set::const_iterator p = pip_params.begin(),
         p_end = pip_params.end(); p != p_end; ++p)
    pip_dim_is_param[*p] = true;

  dimension_type first_art_dim = pip_space_dim;
  for (const PIP_Tree_Node* node = parent(); node != 0; node = node->parent())
    first_art_dim += node->art_parameter_count();

  print_tree(s, indent, pip_dim_is_param, first_art_dim);
}

void
PIP_Tree_Node::print_tree(std::ostream& s, unsigned indent,
                          const std::vector<bool>& pip_dim_is_param,
                          dimension_type first_art_dim) const {
  used(pip_dim_is_param);

  using namespace IO_Operators;

  // Print artificial parameters.
  for (Artificial_Parameter_Sequence::const_iterator
         api = art_parameter_begin(),
         api_end = art_parameter_end(); api != api_end; ++api) {
    indent_and_print(s, indent, "Parameter ");
    s << Variable(first_art_dim) << " = " << *api << "\n";
    ++first_art_dim;
  }

  // Print constraints, if any.
  if (!constraints_.empty()) {
    indent_and_print(s, indent, "if ");

    Constraint_System::const_iterator ci = constraints_.begin();
    Constraint_System::const_iterator ci_end = constraints_.end();
    PPL_ASSERT(ci != ci_end);
    s << *ci;
    for (++ci; ci != ci_end; ++ci)
      s << " and " << *ci;

    s << " then\n";
  }
}

void
PIP_Decision_Node::print_tree(std::ostream& s, unsigned indent,
                              const std::vector<bool>& pip_dim_is_param,
                              const dimension_type first_art_dim) const {
  // First print info common to decision and solution nodes.
  PIP_Tree_Node::print_tree(s, indent, pip_dim_is_param, first_art_dim);

  // Then print info specific of decision nodes.
  dimension_type child_first_art_dim = first_art_dim + art_parameter_count();

  PPL_ASSERT(true_child != 0);
  true_child->print_tree(s, indent+1, pip_dim_is_param, child_first_art_dim);

  indent_and_print(s, indent, "else\n");

  if (false_child)
    false_child->print_tree(s, indent+1, pip_dim_is_param, child_first_art_dim);
  else
    indent_and_print(s, indent+1, "_|_\n");
}

void
PIP_Solution_Node::print_tree(std::ostream& s, unsigned indent,
                              const std::vector<bool>& pip_dim_is_param,
                              const dimension_type first_art_dim) const {
  // Print info common to decision and solution nodes.
  PIP_Tree_Node::print_tree(s, indent, pip_dim_is_param, first_art_dim);

  // Print info specific of solution nodes:
  // first update solution if needed ...
  update_solution(pip_dim_is_param);
  // ... and then actually print it.
  const bool no_constraints = constraints_.empty();
  indent_and_print(s, indent + (no_constraints ? 0 : 1), "{");
  const dimension_type pip_space_dim = pip_dim_is_param.size();
  for (dimension_type i = 0, num_var = 0; i < pip_space_dim; ++i) {
    if (pip_dim_is_param[i])
      continue;
    if (num_var > 0)
      s << " ; ";
    using namespace IO_Operators;
    s << solution[num_var];
    ++num_var;
  }
  s << "}\n";

  if (!no_constraints) {
    indent_and_print(s, indent, "else\n");
    indent_and_print(s, indent+1, "_|_\n");
  }
}

const Linear_Expression&
PIP_Solution_Node::parametric_values(const Variable var) const {
  const PIP_Problem* pip = get_owner();
  PPL_ASSERT(pip);

  const dimension_type space_dim = pip->space_dimension();
  if (var.space_dimension() > space_dim) {
    std::ostringstream s;
    s << "PPL::PIP_Solution_Node::parametric_values(v):\n"
      << "v.space_dimension() == " << var.space_dimension()
      << " is incompatible with the owning PIP_Problem "
      << " (space dim == " << space_dim << ").";
    throw std::invalid_argument(s.str());
  }

  dimension_type solution_index = var.id();
  const Variables_Set& params = pip->parameter_space_dimensions();
  for (Variables_Set::const_iterator p = params.begin(),
         p_end = params.end(); p != p_end; ++p) {
    const dimension_type param_index = *p;
    if (param_index < var.id())
      --solution_index;
    else if (param_index == var.id())
      throw std::invalid_argument("PPL::PIP_Solution_Node"
                                  "::parametric_values(v):\n"
                                  "v is a problem parameter.");
    else
      break;
  }

  update_solution();
  return solution[solution_index];
}


void
PIP_Solution_Node::update_solution() const {
  // Avoid doing useless work.
  if (solution_valid)
    return;

  const PIP_Problem* pip = get_owner();
  PPL_ASSERT(pip);
  std::vector<bool> pip_dim_is_param(pip->space_dimension());
  const Variables_Set& params = pip->parameter_space_dimensions();
  for (Variables_Set::const_iterator p = params.begin(),
         p_end = params.end(); p != p_end; ++p)
    pip_dim_is_param[*p] = true;

  update_solution(pip_dim_is_param);
}

void
PIP_Solution_Node
::update_solution(const std::vector<bool>& pip_dim_is_param) const {
  // Avoid doing useless work.
  if (solution_valid)
    return;

  // const_cast required so as to refresh the solution cache.
  PIP_Solution_Node& x = const_cast<PIP_Solution_Node&>(*this);

  const dimension_type num_pip_dims = pip_dim_is_param.size();
  const dimension_type num_pip_vars = tableau.s.num_columns();
  const dimension_type num_pip_params = num_pip_dims - num_pip_vars;
  const dimension_type num_all_params = tableau.t.num_columns() - 1;
  const dimension_type num_art_params = num_all_params - num_pip_params;

  if (solution.size() != num_pip_vars)
    x.solution.resize(num_pip_vars);

  // Compute external "names" (i.e., indices) for all parameters.
  std::vector<dimension_type> all_param_names(num_all_params);

  // External indices for problem parameters.
  for (dimension_type i = 0, p_index = 0; i < num_pip_dims; ++i)
    if (pip_dim_is_param[i]) {
      all_param_names[p_index] = i;
      ++p_index;
    }
  // External indices for artificial parameters.
  for (dimension_type i = 0; i < num_art_params; ++i)
    all_param_names[num_pip_params + i] = num_pip_dims + i;


  PPL_DIRTY_TEMP_COEFFICIENT(norm_coeff);
  const Coefficient& den = tableau.denominator();
  for (dimension_type i = num_pip_vars; i-- > 0; ) {
    Linear_Expression& sol_i = x.solution[i];
    sol_i = Linear_Expression(0);
    if (basis[i])
      continue;
    matrix_row_const_reference_type row = tableau.t[mapping[i]];

    // Start from index 1 to skip the inhomogenous term.
    matrix_const_row_const_iterator j = row.lower_bound(1);
    matrix_const_row_const_iterator j_end = row.end();
    for ( ; j != j_end; ++j) {
      const Coefficient& coeff = (*j).second;
      if (coeff == 0)
        continue;
      norm_coeff = coeff / den;
      if (norm_coeff != 0)
        add_mul_assign(sol_i, norm_coeff,
                       Variable(all_param_names[(*j).first - 1]));
    }
    norm_coeff = row.get(0) / den;
    sol_i += norm_coeff;
  }

  // Mark solution as valid.
  x.solution_valid = true;
}

} // namespace Parma_Polyhedra_Library
