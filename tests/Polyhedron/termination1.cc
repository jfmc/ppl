/* Test the termination analysis facilities of the PPL.
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

#include "ppl_test.hh"

namespace Parma_Polyhedra_Library {

namespace {

template <typename PSET>
void
assign_all_inequalities_approximation(const PSET& pset,
				      Constraint_System& cs) {
  cs = pset.minimized_constraints();
  if (cs.has_strict_inequalities() || cs.has_equalities() > 0) {
    Constraint_System tmp_cs;
    for (Constraint_System::const_iterator i = cs.begin(),
	   cs_end = cs.end(); i != cs_end; ++i) {
      const Constraint& c = *i;
      if (c.is_equality()) {
	// Insert the two corresponding opposing inequalities.
	tmp_cs.insert(Linear_Expression(c) >= 0);
	tmp_cs.insert(Linear_Expression(c) <= 0);
      }
      else if (c.is_strict_inequality())
	// Insert the non-strict approximation.
	tmp_cs.insert(Linear_Expression(c) >= 0);
      else
	// Insert as is.
	tmp_cs.insert(c);
    }
    cs = tmp_cs;
  }
}

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

template <typename T>
void
assign_all_inequalities_approximation(const BD_Shape<T>& bds,
				      Constraint_System& cs) {
  cs = bds.minimized_constraints();
}

template <typename T>
void
assign_all_inequalities_approximation(const Octagonal_Shape<T>& ocs,
				      Constraint_System& cs) {
  cs = ocs.minimized_constraints();
}

#define PRINT_DEBUG_INFO 1

#if PRINT_DEBUG_INFO
dimension_type output_function_MS_n;
dimension_type output_function_MS_m;
/* Encodes which object are we printing:

   0 means input constraint system;
   1 means first output constraint system;
   2 means second output constraint system;
   3 means only output constraint system
     (i.e., when first and second are the same);
   4 means mu space.
*/
int output_function_MS_which = -1;

/*
  Debuggin output function.  See the documentation of
  fill_constraint_systems_MS() for the allocation of variable indices.
*/
void
output_function_MS(std::ostream& s, const Variable& v) {
  dimension_type id = v.id();
  switch (output_function_MS_which) {
  case 0:
    if (id < output_function_MS_n)
      s << "x'" << id + 1;
    else if (id < 2*output_function_MS_n)
      s << "x" << id - output_function_MS_n + 1;
    else
      s << "WHAT?";
    break;
  case 1:
    if (id < output_function_MS_n)
      s << "mu" << id + 1;
    else if (id == output_function_MS_n)
      s << "WHAT?";
    else if (id <= output_function_MS_n + output_function_MS_m)
      s << "y" << id - output_function_MS_n;
    else
      s << "WHAT?";
    break;
  case 2:
  case 4:
    if (id < output_function_MS_n)
      s << "mu" << id + 1;
    else if (id == output_function_MS_n)
      s << "mu0";
    else if (output_function_MS_which == 2
	     && id <= output_function_MS_n + output_function_MS_m + 2)
      s << "z" << id - output_function_MS_n;
    else
      s << "WHAT?";
    break;
  case 3:
    if (id < output_function_MS_n)
      s << "mu" << id + 1;
    else if (id == output_function_MS_n)
      s << "mu0";
    else if (id <= output_function_MS_n + output_function_MS_m)
      s << "y" << id - output_function_MS_n;
    else if (id <= output_function_MS_n + 2*output_function_MS_m + 2)
      s << "z" << id - (output_function_MS_n + output_function_MS_m);
    else
      s << "WHAT?";
    break;
  default:
    abort();
    break;
  }
}
#endif

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
#if 0
    cs_out1.insert(y_les[j] == -Variable(j-n));
    cs_out2.insert(z_les[j] == 0);
#else
    cs_out1.insert(y_les[j] == Variable(j-n));
    cs_out2.insert(z_les[j] == Variable(j-n));
#endif
  }
  ++j;
  while (j-- > 0) {
#if 0
    cs_out1.insert(y_les[j] == Variable(j));
    cs_out2.insert(z_les[j] == Variable(j));
#else
    cs_out1.insert(y_les[j] == -Variable(j));
    cs_out2.insert(z_les[j] == 0);
#endif
  }
  cs_out2.insert(z_les[2*n] == Variable(n));
}

template <typename PSET>
void
prepare_input_MS_PR(const PSET& pset, Constraint_System& cs,
		    dimension_type& n, dimension_type& m,
		    const char* function) {
  dimension_type space_dim = pset.space_dimension();
  if (space_dim % 2 != 0) {
    std::ostringstream s;
    s << "PPL" << function << "(pset, ...):\n"
      << "pset.space_dimension() == " << space_dim
      << " is odd.";
    throw std::invalid_argument(s.str());
  }

  n = space_dim/2;
  assign_all_inequalities_approximation(pset, cs);
  m = std::distance(cs.begin(), cs.end());

#if PRINT_DEBUG_INFO
  output_function_MS_n = n;
  output_function_MS_m = m;
  Variable::output_function_type* p_default_output_function
    = Variable::get_output_function();
  Variable::set_output_function(output_function_MS);

  std::cout << "*** cs ***" << endl;
  output_function_MS_which = 0;
  using namespace IO_Operators;
  std::cout << cs << endl;

  Variable::set_output_function(p_default_output_function);
#endif
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
  where \f$ \mat{A}_B \in \Qset^r_n\f$ , \f$ \mat{A}_C \in \Qset^s_n\f$ ,
  \f$ \mat{A}'_C \in \Qset^s_n\f$ , \f$ \vect{b}_B \in \Qset^r\f$ ,
  \f$ \vect{b}_C \in \Qset^s\f$ .

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

} // namespace

template <typename PSET>
void
all_affine_ranking_functions_MS(const PSET& pset, C_Polyhedron& mu_space) {
  Constraint_System cs;
  dimension_type n;
  dimension_type m;
  prepare_input_MS_PR(pset, cs, n, m, "all_affine_ranking_functions_MS");

  Constraint_System cs1;
  Constraint_System cs2;
  fill_constraint_systems_MS(cs, n, m, cs1, cs2);

#if PRINT_DEBUG_INFO
  Variable::output_function_type* p_default_output_function
    = Variable::get_output_function();
  Variable::set_output_function(output_function_MS);

  std::cout << "*** cs1 ***" << endl;
  output_function_MS_which = 1;
  using namespace IO_Operators;
  std::cout << cs1 << endl;

  std::cout << "*** cs2 ***" << endl;
  output_function_MS_which = 2;
  using namespace IO_Operators;
  std::cout << cs2 << endl;
#endif

  C_Polyhedron ph1(cs1);
  C_Polyhedron ph2(cs2);
  ph1.remove_higher_space_dimensions(n);
  ph1.add_space_dimensions_and_embed(1);
  ph2.remove_higher_space_dimensions(n+1);

#if PRINT_DEBUG_INFO
  std::cout << "*** ph1 projected ***" << endl;
  output_function_MS_which = 4;
  using namespace IO_Operators;
  std::cout << ph1.minimized_constraints() << endl;

  std::cout << "*** ph2 projected ***" << endl;
  std::cout << ph2.minimized_constraints() << endl;
#endif

  ph1.intersection_assign(ph2);

#if PRINT_DEBUG_INFO
  std::cout << "*** intersection ***" << endl;
  using namespace IO_Operators;
  std::cout << ph1.minimized_constraints() << endl;
  Variable::set_output_function(p_default_output_function);
#endif

  mu_space.swap(ph1);
}

template <typename PSET>
bool
one_affine_ranking_function_MS(const PSET& pset, Generator& mu) {
  Constraint_System cs;
  dimension_type n;
  dimension_type m;
  prepare_input_MS_PR(pset, cs, n, m, "one_affine_ranking_function_MS");

  Constraint_System cs_mip;
  fill_constraint_systems_MS(cs, n, m, cs_mip, cs_mip);

#if PRINT_DEBUG_INFO
  Variable::output_function_type* p_default_output_function
    = Variable::get_output_function();
  Variable::set_output_function(output_function_MS);

  std::cout << "*** cs_mip ***" << endl;
  output_function_MS_which = 3;
  using namespace IO_Operators;
  std::cout << cs_mip << endl;
  Variable::set_output_function(p_default_output_function);
#endif

  MIP_Problem mip = MIP_Problem(cs_mip.space_dimension(), cs_mip);
  if (mip.is_satisfiable()) {
    mu = mip.feasible_point();
    return true;
  }
  else
    return false;
}

template <typename PSET>
bool
termination_test_MS(const PSET& pset) {
  Constraint_System cs;
  dimension_type n;
  dimension_type m;
  prepare_input_MS_PR(pset, cs, n, m, "termination_test_MS");

  Constraint_System cs_mip;
  fill_constraint_systems_MS(cs, n, m, cs_mip, cs_mip);

#if PRINT_DEBUG_INFO
  Variable::output_function_type* p_default_output_function
    = Variable::get_output_function();
  Variable::set_output_function(output_function_MS);

  std::cout << "*** cs_mip ***" << endl;
  output_function_MS_which = 3;
  using namespace IO_Operators;
  std::cout << cs_mip << endl;
  Variable::set_output_function(p_default_output_function);
#endif

  MIP_Problem mip = MIP_Problem(cs_mip.space_dimension(), cs_mip);
  return mip.is_satisfiable();
}

template <typename PSET>
bool
termination_test_PR(const PSET& pset) {
  Constraint_System cs;
  dimension_type n;
  dimension_type m;
  prepare_input_MS_PR(pset, cs, n, m, "termination_test_PR");

  Constraint_System cs_mip;
  fill_constraint_systems_PR(cs, n, m, cs_mip);

#if PRINT_DEBUG_INFO
  Variable::output_function_type* p_default_output_function
    = Variable::get_output_function();
  Variable::set_output_function(output_function_MS);

  std::cout << "*** cs_mip ***" << endl;
  output_function_MS_which = 3;
  using namespace IO_Operators;
  std::cout << cs_mip << endl;
  Variable::set_output_function(p_default_output_function);
#endif

  MIP_Problem mip = MIP_Problem(cs_mip.space_dimension(), cs_mip);
  return mip.is_satisfiable();
}

} // namespace Parma_Polyhedra_Library

namespace {

bool
test01() {
  Variable xp1(0);
  Variable xp2(1);
  Variable x1(2);
  Variable x2(3);
  C_Polyhedron ph(4);
  ph.add_constraint(x1 >= 2);
  ph.add_constraint(2*xp1 + 1 >= x1);
  ph.add_constraint(2*xp1 <= x1);
  ph.add_constraint(xp2 + 1 == x2);

  C_Polyhedron mu_space;
  all_affine_ranking_functions_MS(ph, mu_space);

  print_constraints(mu_space, "*** mu_space ***");

  Variable mu1(0);
  Variable mu2(1);
  Variable mu0(2);
  C_Polyhedron known_result(3);
  known_result.add_constraint(mu1 - mu2 >= 1);
  known_result.add_constraint(mu2 >= 0);
  known_result.add_constraint(2*mu0 + mu1 + 2*mu2 >= 0);

  print_constraints(known_result, "*** known_result ***");

  if (known_result.contains(mu_space))
    std::cout << "->" << endl;
  if (mu_space.contains(known_result))
    std::cout << "<-" << endl;
  return known_result == mu_space;
}

bool
test02() {
  Variable xp1(0);
  Variable xp2(1);
  Variable x1(2);
  Variable x2(3);
  C_Polyhedron ph(4);
  ph.add_constraint(x1 >= 2);
  ph.add_constraint(2*xp1 + 1 >= x1);
  ph.add_constraint(2*xp1 <= x1);
  ph.add_constraint(xp2 + 1 == x2);

  return termination_test_MS(ph);
}

bool
test03() {
  BD_Shape<int> bds(2);
  return termination_test_MS(bds);
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  //DO_TEST(test03);
END_MAIN
