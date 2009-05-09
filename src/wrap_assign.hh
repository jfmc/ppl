/* Generic implementation of the wrap_assign() function.
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_wrap_assign_hh
#define PPL_wrap_assign_hh 1

#include "globals.defs.hh"
#include "Coefficient.defs.hh"
#include "Variable.defs.hh"
#include "Constraint_System.defs.hh"
#include <cassert>

namespace Parma_Polyhedra_Library {

namespace Implementation {

struct Wrap_Dim_Translations {
  Variable var;
  Coefficient first_quadrant;
  Coefficient last_quadrant;
  Wrap_Dim_Translations(Variable v,
                        Coefficient_traits::const_reference f,
                        Coefficient_traits::const_reference l)
    : var(v), first_quadrant(f), last_quadrant(l) {
  }
};

typedef std::vector<Wrap_Dim_Translations> Wrap_Translations;

template <typename PSET>
void
wrap_assign_rec(PSET& dest,
                const PSET& src,
                Variables_Set vars,
                Wrap_Translations::iterator first,
                Wrap_Translations::iterator end,
                Bounded_Integer_Type_Width w,
                Coefficient_traits::const_reference min_value,
                Coefficient_traits::const_reference max_value,
                const Constraint_System* pcs,
                unsigned complexity_threshold,
                Coefficient& tmp1,
                Coefficient& tmp2) {
  if (first == end) {
    PSET p(src);
    if (pcs != 0)
      p.refine_with_constraints(*pcs);
    for (Variables_Set::const_iterator i = vars.begin(),
           vars_end = vars.end(); i != vars.end(); ++i) {
      const Variable x = Variable(*i);
      p.refine_with_constraint(min_value <= x);
      p.refine_with_constraint(x <= max_value);
    }
    dest.upper_bound_assign(p);
  }
  else {
    const Wrap_Dim_Translations& wrap_dim_translations = *first;
    const Variable& x = wrap_dim_translations.var;
    const Coefficient& first_quadrant = wrap_dim_translations.first_quadrant;
    const Coefficient& last_quadrant = wrap_dim_translations.last_quadrant;
    Coefficient& quadrant = tmp1;
    Coefficient& shift = tmp2;
    for (quadrant = first_quadrant; quadrant <= last_quadrant; ++quadrant) {
      if (quadrant != 0) {
        mul_2exp_assign(shift, quadrant, w);
        PSET p(src);
        p.affine_image(x, x - shift, 1);
        wrap_assign_rec(dest, p, vars, first+1, end, w, min_value, max_value,
                        pcs, complexity_threshold, tmp1, tmp2);
      }
      else
        wrap_assign_rec(dest, src, vars, first+1, end, w, min_value, max_value,
                        pcs, complexity_threshold, tmp1, tmp2);
    }
  }
}

template <typename PSET>
void
wrap_assign(PSET& pointset,
            const Variables_Set& vars,
            Bounded_Integer_Type_Width w,
            Bounded_Integer_Type_Signedness s,
            Bounded_Integer_Type_Overflow o,
            const Constraint_System* pcs,
            unsigned complexity_threshold,
            bool wrap_individually) {
  const dimension_type space_dim = pointset.space_dimension();
  // Dimension-compatibility check of `*pcs', if any.
  if (pcs != 0 && pcs->space_dimension() != space_dim) {
    std::ostringstream s;
    // FIXME: how can we write the class name of PSET here?
    s << "PPL::...::wrap_assign(..., pcs, ...):" << std::endl
      << "this->space_dimension() == " << space_dim
      << ", pcs->space_dimension() == " << pcs->space_dimension() << ".";
    throw std::invalid_argument(s.str());
  }

  // Wrapping no variable is a no-op.
  if (vars.empty())
    return;

  // Dimension-compatibility check of `vars'.
  if (space_dim < vars.space_dimension()) {
    std::ostringstream s;
    // FIXME: how can we write the class name of PSET here?
    s << "PPL::...::wrap_assign(vs, ...):" << std::endl
      << "this->space_dimension() == " << space_dim
      << ", required space dimension == " << vars.space_dimension() << ".";
    throw std::invalid_argument(s.str());
  }

  // Wrapping an empty polyhedron is a no-op.
  if (pointset.is_empty())
    return;

  // Set `min_value' and `max_value' to the minimum and maximum values
  // a variable of width `w' and signedness `s' can take.
  PPL_DIRTY_TEMP_COEFFICIENT(min_value);
  PPL_DIRTY_TEMP_COEFFICIENT(max_value);
  if (s == UNSIGNED) {
    min_value = 0;
    mul_2exp_assign(max_value, Coefficient_one(), w);
    --max_value;
  }
  else {
    assert(s == SIGNED_2_COMPLEMENT);
    mul_2exp_assign(max_value, Coefficient_one(), w-1);
    neg_assign(min_value, max_value);
    --max_value;
  }

  // If we are wrapping variables collectively, the ranges for the
  // required translations are saved in `translations' instead of being
  // immediately applied.
  Wrap_Translations translations;

  // If we are wrapping variables collectively, dimensions subject
  // to translation are added to this set.
  Variables_Set translated_dimensions;

  // This will contain a lower bound to the number of abstractions
  // to be joined in order to obtain the collective wrapping result.
  // As soon as this exceeds `complexity_threshold', counting will be
  // interrupted and the full range will be the result of wrapping
  // any dimension that is not fully contained in quadrant 0.
  unsigned collective_wrap_complexity = 1;

  // This flag signals that the maximum complexity for collective
  // wrapping as been exceeded.
  bool collective_wrap_too_complex = false;

  if (!wrap_individually) {
    translations.reserve(space_dim);
  }

  // We use `full_range_bounds' to delay conversions whenever
  // this delay does not negatively affect precision.
  Constraint_System full_range_bounds;

  PPL_DIRTY_TEMP_COEFFICIENT(ln);
  PPL_DIRTY_TEMP_COEFFICIENT(ld);
  PPL_DIRTY_TEMP_COEFFICIENT(un);
  PPL_DIRTY_TEMP_COEFFICIENT(ud);

  //using namespace IO_Operators;

  for (Variables_Set::const_iterator i = vars.begin(),
         vars_end = vars.end(); i != vars_end; ++i) {

    const Variable x = Variable(*i);

    bool extremum;

    if (!pointset.minimize(x, ln, ld, extremum)) {
    set_full_range:
      pointset.unconstrain(x);
      full_range_bounds.insert(min_value <= x);
      full_range_bounds.insert(x <= max_value);
      continue;
    }

    if (!pointset.maximize(x, un, ud, extremum))
      goto set_full_range;

    div_assign_r(ln, ln, ld, ROUND_DOWN);
    div_assign_r(un, un, ud, ROUND_DOWN);
    ln -= min_value;
    un -= min_value;
    div_2exp_assign_r(ln, ln, w, ROUND_DOWN);
    div_2exp_assign_r(un, un, w, ROUND_DOWN);
    Coefficient& first_quadrant = ln;
    Coefficient& last_quadrant = un;

    // Special case: this variable does not need wrapping.
    if (first_quadrant == 0 && last_quadrant == 0)
      continue;

    // If overflow is impossible, try not to add useless constraints.
    if (o == OVERFLOW_IMPOSSIBLE) {
      if (first_quadrant < 0)
        full_range_bounds.insert(min_value <= x);
      if (last_quadrant > 0)
        full_range_bounds.insert(x <= max_value);
      continue;
    }

    if (o == OVERFLOW_UNDEFINED || collective_wrap_too_complex)
      goto set_full_range;

    Coefficient& diff = ud;
    diff = last_quadrant - first_quadrant;

    // Please note that the `>=' is intentional here.
    if (diff >= UINT_MAX)
      goto set_full_range;

    unsigned extension;
    assign_r(extension, diff, ROUND_NOT_NEEDED);
    ++extension;
    assert(extension > 0);

    if (extension > complexity_threshold)
      goto set_full_range;

    if (!wrap_individually && !collective_wrap_too_complex) {
      if (collective_wrap_complexity > UINT_MAX / extension)
        collective_wrap_too_complex = true;
      else {
        collective_wrap_complexity *= extension;
        if (collective_wrap_complexity > complexity_threshold)
          collective_wrap_too_complex = true;
      }
      if (collective_wrap_too_complex) {
        // Set all the dimensions in `translations' to full range.
        for (Wrap_Translations::const_iterator i = translations.begin(),
               tend = translations.end(); i != tend; ++i) {
          const Variable& x = i->var;
          pointset.unconstrain(x);
          full_range_bounds.insert(min_value <= x);
          full_range_bounds.insert(x <= max_value);
        }
      }
    }

    if (wrap_individually) {
      Coefficient& quadrant = first_quadrant;
      // Temporary variable holding the shifts to be applied in order
      // to implement the translations.
      Coefficient& shift = ld;
      PSET hull(space_dim, EMPTY);
      for ( ; quadrant <= last_quadrant; ++quadrant) {
        PSET p(pointset);
        if (quadrant != 0) {
          mul_2exp_assign(shift, quadrant, w);
          p.affine_image(x, x - shift, 1);
        }
        if (pcs != 0)
          p.refine_with_constraints(*pcs);
        p.refine_with_constraint(min_value <= x);
        p.refine_with_constraint(x <= max_value);
        hull.upper_bound_assign(p);
      }
      pointset.swap(hull);
    }
    else if (!collective_wrap_too_complex)
      // !wrap_individually.
      translated_dimensions.insert(x);
      translations
        .push_back(Wrap_Dim_Translations(x, first_quadrant, last_quadrant));
  }

  if (!wrap_individually && !translations.empty()) {
    PSET hull(space_dim, EMPTY);
    wrap_assign_rec(hull, pointset, translated_dimensions,
                    translations.begin(), translations.end(),
                    w, min_value, max_value, pcs, complexity_threshold,
                    ln, ld);
    pointset.swap(hull);
  }

  if (pcs != 0)
    pointset.refine_with_constraints(*pcs);
  pointset.refine_with_constraints(full_range_bounds);
}

} // namespace Implementation

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_wrap_assign_hh)
