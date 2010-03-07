/* Utilities for termination analysis: non-inline, non-template functions.
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

#include "termination.defs.hh"

namespace Parma_Polyhedra_Library {

namespace Implementation {

namespace Termination {

template <>
void
assign_all_inequalities_approximation(const C_Polyhedron& ph,
				      Constraint_System& cs) {
  cs = ph.minimized_constraints();
  if (cs.has_equalities() > 0) {
    Constraint_System tmp_cs;
    for (Constraint_System::const_iterator i = cs.begin(),
	   cs_end = cs.end(); i != cs_end; ++i) {
      const Constraint& c = *i;
      if (c.is_equality()) {
	// Insert the two corresponding opposing inequalities.
	tmp_cs.insert(Linear_Expression(c) >= 0);
	tmp_cs.insert(Linear_Expression(c) <= 0);
      }
      else
	// Insert as is.
	tmp_cs.insert(c);
    }
    cs = tmp_cs;
  }
}

/*! \brief
  Fill the constraint system(s) for the application of the
  Mesnard and Serebrenik termination tests.

  \param cs
  The input constraint system, where variables indices are allocated
  as follows:
  - \f$ x'_1, \dots, x'_n \f$ go onto \f$ 0, \dots, n-1 \f$;
  - \f$ x_1, \dots, x_n \f$ go onto \f$ n, \dots, 2n-1 \f$;

  \param cs_out1
  The first output constraint system.

  \param cs_out2
  The second output constraint system, if any: it may be an alias
  for \p cs_out1.

  The allocation of variable indices in the output constraint
  systems \p cs_out1 and \p cs_out2 is as follows:
  - \f$ \mu_1, \dots, \mu_n \f$ go onto \f$ 0, \dots, n-1 \f$;
  - \f$ \mu 0\f $ goes onto \f$ n \f$;
  - \f$ y_1, \dots, y_m \f$ go onto \f$ n+1, \dots, n+m \f$;

  if we use the same constraint system, that is
  <CODE>&cs_out1 == &cs_out2</CODE>, then
  - \f$ z_1, ..., z_m, z_{m+1}, z_{m+2} \f$
    go onto \f$ n+m+1, ..., n+2*m+2 \f$;

  otherwise
  - \f$ z_1, ..., z_m, z_{m+1}, z_{m+2} \f$ go onto \f$ n+1, ..., n+m+2 \f$.
*/
void
fill_constraint_systems_MS(const Constraint_System& cs,
			   const dimension_type n,
			   const dimension_type m,
			   Constraint_System& cs_out1,
			   Constraint_System& cs_out2) {
  dimension_type y_begin = n+1;
  dimension_type z_begin = (&cs_out1 == &cs_out2) ? y_begin + m : y_begin;
  // Make sure linear expressions are not reallocated multiple times.
  Linear_Expression y_le(0*Variable(y_begin + m - 1));
  Linear_Expression z_le(0*Variable(z_begin + m + 2 - 1));
  std::vector<Linear_Expression> y_les(2*n);
  std::vector<Linear_Expression> z_les(2*n + 1);
  dimension_type y = y_begin;
  dimension_type z = z_begin;
  for (Constraint_System::const_iterator i = cs.begin(),
	 cs_end = cs.end(); i != cs_end; ++i) {
    const Constraint& c_i = *i;
    Coefficient_traits::const_reference b_i = c_i.inhomogeneous_term();
    // Note that b_i is to the left ot the relation sign, hence here
    // we have -= and not += just to avoid negating b_i.
    y_le -= b_i*Variable(y);
    cs_out1.insert(Variable(y) >= 0);
    // We have -= and not += for the same reason mentioned above.
    z_le -= b_i*Variable(z);
    cs_out2.insert(Variable(z) >= 0);
    for (dimension_type j = 2*n; j-- > 0; ) {
      Coefficient_traits::const_reference a_i_j = c_i.coefficient(Variable(j));
      y_les[j] += a_i_j*Variable(y);
      z_les[j] += a_i_j*Variable(z);
    }
    ++y;
    ++z;
  }
  z_le += Variable(z);
  z_les[2*n] += Variable(z);
  cs_out2.insert(Variable(z) >= 0);
  ++z;
  z_le -= Variable(z);
  z_les[2*n] -= Variable(z);
  cs_out2.insert(Variable(z) >= 0);
  cs_out1.insert(y_le >= 1);
  cs_out2.insert(z_le >= 0);
  dimension_type j = 2*n;
  while (j-- > n) {
    cs_out1.insert(y_les[j] == Variable(j-n));
    cs_out2.insert(z_les[j] == Variable(j-n));
  }
  ++j;
  while (j-- > 0) {
    cs_out1.insert(y_les[j] == -Variable(j));
    cs_out2.insert(z_les[j] == 0);
  }
  cs_out2.insert(z_les[2*n] == Variable(n));
}

/*! \brief
  Fill the constraint system(s) for the application of the
  Podelski and Rybalchenko termination tests.

  \param cs
  The input constraint system, where variables indices are allocated
  as follows:
  - \f$ x'_1, \dots, x'_n \f$ go onto \f$ 0, \dots, n-1 \f$;
  - \f$ x_1, \dots, x_n \f$ go onto \f$ n, \dots, 2n-1 \f$;

  \param cs_out
  The output constraint system, where variables indices are allocated
  as follows:
  - \f$ \mu_1, \dots, \mu_n \f$ go onto \f$ 0, \dots, n-1 \f$;
  - \f$ \mu 0\f $ goes onto \f$ n \f$;
  - \f$ y_1, \dots, y_m \f$ go onto \f$ n+1, \dots, n+m \f$;
  - \f$ z_1, ..., z_m, z_{m+1}, z_{m+2} \f$
    go onto \f$ n+m+1, ..., n+2*m+2 \f$.

  The improved Podelski-Rybalchenko method described in the paper
  is based on a loop encoding of the form
  \f[
    \begin{pmatrix}
      \mat{A}_B  & \mat{0}    \\
      \mat{A}_C  & \mat{A}'_C
    \end{pmatrix}
    \begin{pmatrix}
     \vect{x} \\ \vect{x}'
    \end{pmatrix}
    \leq
    \begin{pmatrix}
     \vect{b}_B \\ \vect{b}_C
    \end{pmatrix},
  \f]
  where \f$ \mat{A}_B \in \Qset^r_n\f$, \f$ \mat{A}_C \in \Qset^s_n\f$,
  \f$ \mat{A}'_C \in \Qset^s_n\f$, \f$ \vect{b}_B \in \Qset^r\f$,
  \f$ \vect{b}_C \in \Qset^s\f$.
  The corresponding system is:
  \f[
    \begin{aligned}
      (\vect{v}_1-\vect{v}_2)^\transpose \mat{A}_B
        - \vect{v}_3^\transpose \mat{A}_C
          &= \vect{0}^\transpose, \\
      \vect{v}_2^\transpose \mat{A}_B
        + \vect{v}_3^\transpose (\mat{A}_C+\mat{A}_C')
          &= \vect{0}^\transpose, \\
      \vect{v}_2 \vect{b}_B + \vect{v}_3 \vect{b}_C
          &< 0.
    \end{aligned}
  \f]

  In contrast, our encoding is of the form
  \f[
    \begin{pmatrix}
      \mat{0}    & \mat{E}_B \\
      \mat{E}'_C & \mat{E}_C
    \end{pmatrix}
    \begin{pmatrix}
     \vect{x}' \\ \vect{x}
    \end{pmatrix}
    +
    \begin{pmatrix}
     \vect{d}_B \\ \vect{d}_C
    \end{pmatrix}
    \geq
    \mat{0},
  \f]
  where \f$ {E}_B = -{A}_B \f$, \f$ {E}_C = -{A}_C \f$,
  \f$ {E}'_C = -{A}'_C \f$, \f$ \vect{d}_B = \vect{b}_B \f$
  and \f$ \vect{d}_B = \vect{b}_B \f$.
  The corresponding system is:
  \f[
    \begin{aligned}
      (\vect{v}_1-\vect{v}_2)^\transpose \mat{A}_B
        - \vect{v}_3^\transpose \mat{A}_C
          &= \vect{0}^\transpose, \\
      \vect{v}_2^\transpose \mat{A}_B
        + \vect{v}_3^\transpose (\mat{A}_C+\mat{A}_C')
          &= \vect{0}^\transpose, \\
      \vect{v}_2 \vect{b}_B + \vect{v}_3 \vect{b}_C
          &< 0.
    \end{aligned}
  \f]

% parts.
% Loop is encoded as (A|A') * (x|x') <= b and the system is:
%   L1 * A' = 0, (L1-L2) * A = 0, L2 * (A+A') = 0, L2 * b < 0.
% Here we reuse as much as possible from the SVG-MS method, so our loop
% comes in encoded as (C'|C) * (x'|x) + d >= 0
% where the primed part comes first due to an implementation choice.
% We then have A' = -C', A = -C, b = d and the system becomes:
%   L1 * C' = 0, (L1-L2) * C = 0, L2 * (C+C') = 0, L2 * d < 0.
% Note the change in the last inequality.
% But, instead of looking for L1 and L2, we look for N1=-L1 and N2=-L2.
% So the final system becomes
%   N1 * C' = 0, (N1-N2) * C = 0, N2 * (C+C') = 0, N2 * d > 0.
% Note the change (again) in the last inequality.
% In addition to that, nonnegativity constraints on L1 and L2 are now
% expressed as nonpositivity constraints on N1 and N2.

% The ranking functions are expressed by L2 * A' * x, which
% becomes L2 * (-C') * x, which becomes N2 * C' * x.

% Variables have numbers between 0 and N-1 included, i.e., we have
% N duplicate (primed) variables numbered 0, ..., N-1 and N original
% (unprimed) variables numbered N, ..., 2*N-1.

*/
void
fill_constraint_systems_PR(const Constraint_System& cs,
			   const dimension_type n,
			   const dimension_type m,
			   Constraint_System& cs_out) {
  // Determine the partitioning of the m rows into the r rows
  // of A_B and the s rows of A_C|A'_C (see the paper).
  std::deque<bool> in_A_B(m, true);
  dimension_type r = m;
  for (Constraint_System::const_iterator i = cs.begin(),
	 cs_end = cs.end(); i != cs_end; ++i) {
    const Constraint& c_i = *i;
    for (dimension_type j = 2*n; j-- > n; )
      if (c_i.coefficient(Variable(j)) != 0) {
	in_A_B[j] = false;
	--r;
	break;
      }
  }
  dimension_type s = m - r;
}


} // namespace Termination

} // namespace Implementation

} // namespace Parma_Polyhedra_Library
