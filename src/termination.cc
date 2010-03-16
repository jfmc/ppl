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

void
shift_unprimed_variables(Constraint_System& cs) {
  const dimension_type cs_space_dim = cs.space_dimension();
  Constraint_System cs_shifted;
  for (Constraint_System::const_iterator i = cs.begin(),
	 cs_end = cs.end(); i != cs_end; ++i) {
    const Constraint& c_i = *i;
    Linear_Expression le_i_shifted;
    for (dimension_type j = cs_space_dim; j-- > 0; ) {
      Coefficient_traits::const_reference a_i_j
	= c_i.coefficient(Variable(j));
      if (a_i_j != 0)
	le_i_shifted += a_i_j*Variable(cs_space_dim + j);
    }
    cs_shifted.insert(le_i_shifted >= c_i.inhomogeneous_term());
  }
  cs.swap(cs_shifted);
}

/*! \brief
  Fill the constraint system(s) for the application of the
  Mesnard and Serebrenik improved termination tests.

  \param cs
  The input constraint system, where variables indices are allocated
  as follows:
  - \f$ x'_1, \ldots, x'_n \f$ go onto space dimensions
    \f$ 0, \ldots, n-1 \f$,
  - \f$ x_1, \ldots, x_n \f$ go onto space dimensions
    \f$ n, \ldots, 2n-1 \f$,
  .
  The system does not contain any equality.

  \param n
  The number of loop-relevant variables in the analyzed loop.

  \param m
  The number of inequalities in \p cs.

  \param cs_out1
  The first output constraint system.

  \param cs_out2
  The second output constraint system, if any: it may be an alias
  for \p cs_out1.

  The allocation of variable indices in the output constraint
  systems \p cs_out1 and \p cs_out2 is as follows:
  - \f$ \mu_1, \ldots, \mu_n \f$ go onto space dimensions
    \f$ 0, \ldots, n-1 \f$;
  - \f$ \mu_0\f$ goes onto space dimension \f$ n \f$;
  - \f$ y_1, \ldots, y_m \f$ go onto space dimensions
    \f$ n+1, \ldots, n+m \f$;
  .
  if we use the same constraint system, that is
  <CODE>&cs_out1 == &cs_out2</CODE>, then
  - \f$ z_1, ..., z_m, z_{m+1}, z_{m+2} \f$ go onto space dimensions
    \f$ n+m+1, ..., n+2*m+2 \f$;
  .
  otherwise
  - \f$ z_1, ..., z_m, z_{m+1}, z_{m+2} \f$ go onto space dimensions
    \f$ n+1, ..., n+m+2 \f$.
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
  Podelski and Rybalchenko improved termination tests.

  \param cs_before
  The input constraint system describing the state <EM>before</EM>
  the execution of the loop body, where variables indices are allocated
  as follows:
  - \f$ x_1, \ldots, x_n \f$ go onto space dimensions
    \f$ 0, \ldots, n-1 \f$.
  .
  The system does not contain any equality.

  \param cs_after
  The input constraint system describing the state <EM>after</EM>
  the execution of the loop body, where variables indices are allocated
  as follows:
  - \f$ x'_1, \ldots, x'_n \f$ go onto space dimensions
    \f$ 0, \ldots, n-1 \f$,
  - \f$ x_1, \ldots, x_n \f$ go onto space dimensions
    \f$ n, \ldots, 2n-1 \f$.
  .
  The system does not contain any equality.

  \param cs_out
  The output constraint system, where variables indices are allocated
  as follows:
  - \f$ u_3 \f$ goes onto space dimensions \f$ 0, \ldots, s-1 \f$;
  - \f$ u_2 \f$ goes onto space dimensions \f$ s, \ldots, s+r-1 \f$;
  - \f$ u_1 \f$ goes onto space dimensions \f$ s+r, \ldots, s+2r-1 \f$.

  The improved Podelski-Rybalchenko method described in the paper
  is based on a loop encoding of the form
  \f[
    \begin{pmatrix}
      A_B  & \vect{0}    \\
      A_C  & A'_C
    \end{pmatrix}
    \begin{pmatrix}
     \vect{x} \\ \vect{x}'
    \end{pmatrix}
    \leq
    \begin{pmatrix}
     \vect{b}_B \\ \vect{b}_C
    \end{pmatrix},
  \f]
  where
  \f$ A_B \in \Qset^{r \times n} \f$,
  \f$ A_C \in \Qset^{s \times n} \f$,
  \f$ A'_C \in \Qset^{s \times n} \f$,
  \f$ \vect{b}_B \in \Qset^r \f$,
  \f$ \vect{b}_C \in \Qset^s \f$.
  The corresponding system is:
  \f[
    \begin{aligned}
      (\vect{v}_1-\vect{v}_2)^\transpose A_B
        - \vect{v}_3^\transpose A_C
          &= \vect{0}^\transpose, \\
      \vect{v}_2^\transpose A_B
        + \vect{v}_3^\transpose (A_C+A_C')
          &= \vect{0}^\transpose, \\
      \vect{v}_2 \vect{b}_B + \vect{v}_3 \vect{b}_C
          &< 0,
    \end{aligned}
  \f]
  where \f$ \vect{v}_1 \in \Qset_+^r \f$, \f$ \vect{v}_2 \in \Qset_+^r \f$,
  \f$ \vect{v}_3 \in \Qset_+^s \f$.
  The space of ranking functions is then spanned by
  \f$ \vect{v}_3^\transpose A_C' \vect x \f$.

  In contrast, our encoding is of the form
  \f[
    \begin{pmatrix}
      \vect{0}    & E_B \\
      E'_C & E_C
    \end{pmatrix}
    \begin{pmatrix}
     \vect{x}' \\ \vect{x}
    \end{pmatrix}
    +
    \begin{pmatrix}
     \vect{d}_B \\ \vect{d}_C
    \end{pmatrix}
    \geq
    \vect{0},
  \f]
  where \f$ {E}_B = -{A}_B \f$, \f$ {E}_C = -{A}_C \f$,
  \f$ {E}'_C = -{A}'_C \f$, \f$ \vect{d}_B = \vect{b}_B \f$
  and \f$ \vect{d}_C = \vect{b}_C \f$.
  The corresponding system is:
  \f[
    \begin{aligned}
      (\vect{u}_1-\vect{u}_2)^\transpose E_B
        - \vect{u}_3^\transpose E_C
          &= \vect{0}^\transpose, \\
      \vect{u}_2^\transpose E_B
        + \vect{u}_3^\transpose (E_C+E_C')
          &= \vect{0}^\transpose, \\
      \vect{u}_2 \vect{d}_B + \vect{u}_3 \vect{d}_C
          &> 0,
    \end{aligned}
  \f]
  where \f$ \vect{u}_1 \in \Qset_-^r \f$, \f$ \vect{u}_2 \in \Qset_-^r \f$,
  \f$ \vect{u}_3 \in \Qset_-^s \f$.
  The space of ranking functions is then spanned by
  \f$ \vect{u}_3^\transpose E_C' \vect x \f$.
*/
void
fill_constraint_system_PR(const Constraint_System& cs_before,
			  const Constraint_System& cs_after,
			  Constraint_System& cs_out,
			  Linear_Expression& le_out) {
  const dimension_type n = cs_before.space_dimension();
  const dimension_type r = distance(cs_before.begin(), cs_before.end());
  const dimension_type s = distance(cs_after.begin(), cs_after.end());
  const dimension_type m = r + s;

  std::vector<Linear_Expression> les_eq(2*n);

  dimension_type row_index = 0;
  for (Constraint_System::const_iterator i = cs_before.begin(),
	 cs_before_end = cs_before.end();
       i != cs_before_end;
       ++i, ++row_index) {
    const Constraint& c_i = *i;
    for (dimension_type j = n; j-- > 0; ) {
      Coefficient_traits::const_reference k = c_i.coefficient(Variable(j));
      les_eq[j] += k*Variable(m + row_index);
      les_eq[j] -= k*Variable(s + row_index);
      les_eq[j + n] += k*Variable(s + row_index);
    }
    le_out += c_i.inhomogeneous_term()*Variable(s + row_index);
  }


  row_index = 0;
  for (Constraint_System::const_iterator i = cs_after.begin(),
	 cs_after_end = cs_after.end();
       i != cs_after_end;
       ++i, ++row_index) {
    const Constraint& c_i = *i;
    for (dimension_type j = n; j-- > 0; ) {
      PPL_DIRTY_TEMP_COEFFICIENT(k);
      k = c_i.coefficient(Variable(j + n));
      les_eq[j] -= k*Variable(row_index);
      k += c_i.coefficient(Variable(j));
      les_eq[j + n] += k*Variable(row_index);
    }
    le_out += c_i.inhomogeneous_term()*Variable(row_index);
  }

  // FIXME: iterate backwards once the debuggin phase is over.
  //for (dimension_type j = 2*n; j-- > 0; )
  for (dimension_type j = 0; j < 2*n; ++j)
    cs_out.insert(les_eq[j] == 0);
}

bool
termination_test_MS(const Constraint_System& cs) {
  const dimension_type n = cs.space_dimension()/2;
  const dimension_type m = distance(cs.begin(), cs.end());

#if PRINT_DEBUG_INFO
  Variable::output_function_type* p_default_output_function
    = Variable::get_output_function();
  Variable::set_output_function(output_function_MS);

  output_function_MS_n = n;
  output_function_MS_m = m;

  std::cout << "*** cs ***" << std::endl;
  output_function_MS_which = 0;
  using namespace IO_Operators;
  std::cout << cs << std::endl;
#endif

  Constraint_System cs_mip;
  fill_constraint_systems_MS(cs, n, m, cs_mip, cs_mip);

#if PRINT_DEBUG_INFO
  std::cout << "*** cs_mip ***" << std::endl;
  output_function_MS_which = 3;
  using namespace IO_Operators;
  std::cout << cs_mip << std::endl;

  Variable::set_output_function(p_default_output_function);
#endif

  MIP_Problem mip = MIP_Problem(cs_mip.space_dimension(), cs_mip);

  return mip.is_satisfiable();
}

bool
one_affine_ranking_function_MS(const Constraint_System& cs, Generator& mu) {
  const dimension_type n = cs.space_dimension()/2;
  const dimension_type m = distance(cs.begin(), cs.end());

#if PRINT_DEBUG_INFO
  Variable::output_function_type* p_default_output_function
    = Variable::get_output_function();
  Variable::set_output_function(output_function_MS);

  output_function_MS_n = n;
  output_function_MS_m = m;

  std::cout << "*** cs ***" << std::endl;
  output_function_MS_which = 0;
  using namespace IO_Operators;
  std::cout << cs << std::endl;
#endif

  Constraint_System cs_mip;
  fill_constraint_systems_MS(cs, n, m, cs_mip, cs_mip);

#if PRINT_DEBUG_INFO
  std::cout << "*** cs_mip ***" << std::endl;
  output_function_MS_which = 3;
  using namespace IO_Operators;
  std::cout << cs_mip << std::endl;

  Variable::set_output_function(p_default_output_function);
#endif

  MIP_Problem mip = MIP_Problem(cs_mip.space_dimension(), cs_mip);

  if (mip.is_satisfiable()) {
    Generator fp = mip.feasible_point();
    assert(fp.is_point());
    Linear_Expression le;
    for (dimension_type i = n+1; i-- > 0; ) {
      Variable vi(i);
      le += vi*fp.coefficient(vi);
    }
    mu = point(le, fp.divisor());
    return true;
  }
  else
    return false;
}

void
all_affine_ranking_functions_MS(const Constraint_System& cs,
				C_Polyhedron& mu_space) {
  const dimension_type n = cs.space_dimension()/2;
  const dimension_type m = distance(cs.begin(), cs.end());

#if PRINT_DEBUG_INFO
  Variable::output_function_type* p_default_output_function
    = Variable::get_output_function();
  Variable::set_output_function(output_function_MS);

  output_function_MS_n = n;
  output_function_MS_m = m;

  std::cout << "*** cs ***" << std::endl;
  output_function_MS_which = 0;
  using namespace IO_Operators;
  std::cout << cs << std::endl;
#endif

  Constraint_System cs_out1;
  Constraint_System cs_out2;
  fill_constraint_systems_MS(cs, n, m, cs_out1, cs_out2);

#if PRINT_DEBUG_INFO
  std::cout << "*** cs_out1 ***" << std::endl;
  output_function_MS_which = 1;
  using namespace IO_Operators;
  std::cout << cs_out1 << std::endl;

  std::cout << "*** cs_out2 ***" << std::endl;
  output_function_MS_which = 2;
  using namespace IO_Operators;
  std::cout << cs_out2 << std::endl;
#endif

  C_Polyhedron ph1(cs_out1);
  C_Polyhedron ph2(cs_out2);
  ph1.remove_higher_space_dimensions(n);
  ph1.add_space_dimensions_and_embed(1);
  ph2.remove_higher_space_dimensions(n+1);

#if PRINT_DEBUG_INFO
  std::cout << "*** ph1 projected ***" << std::endl;
  output_function_MS_which = 4;
  using namespace IO_Operators;
  std::cout << ph1.minimized_constraints() << std::endl;

  std::cout << "*** ph2 projected ***" << std::endl;
  std::cout << ph2.minimized_constraints() << std::endl;
#endif

  ph1.intersection_assign(ph2);

#if PRINT_DEBUG_INFO
  std::cout << "*** intersection ***" << std::endl;
  using namespace IO_Operators;
  std::cout << ph1.minimized_constraints() << std::endl;

  Variable::set_output_function(p_default_output_function);
#endif

  mu_space.swap(ph1);
}

bool
termination_test_PR(const Constraint_System& cs_before,
		    const Constraint_System& cs_after) {
  Constraint_System cs_mip;
  Linear_Expression le_obj;
  fill_constraint_system_PR(cs_before, cs_after, cs_mip, le_obj);

#if PRINT_DEBUG_INFO
  Variable::output_function_type* p_default_output_function
    = Variable::get_output_function();
  Variable::set_output_function(output_function_PR);

  output_function_PR_r = distance(cs_before.begin(), cs_before.end());
  output_function_PR_s = distance(cs_after.begin(), cs_after.end());

  std::cout << "*** cs_mip ***" << std::endl;
  using namespace IO_Operators;
  std::cout << cs_mip << std::endl;
  std::cout << "*** le_obj ***" << std::endl;
  std::cout << le_obj << std::endl;

  Variable::set_output_function(p_default_output_function);
#endif

  MIP_Problem mip = MIP_Problem(cs_mip.space_dimension(), cs_mip,
				le_obj, MAXIMIZATION);
  switch (mip.solve()) {
  case UNFEASIBLE_MIP_PROBLEM:
    return false;
  case UNBOUNDED_MIP_PROBLEM:
    return true;
  case OPTIMIZED_MIP_PROBLEM:
    {
      PPL_DIRTY_TEMP_COEFFICIENT(num);
      PPL_DIRTY_TEMP_COEFFICIENT(den);
      mip.optimal_value(num, den);
      assert(den > 0);
      return num > 0;
    }
  }

  // This point should be unreachable.
  throw std::runtime_error("PPL internal error");
}

} // namespace Termination

} // namespace Implementation

} // namespace Parma_Polyhedra_Library
