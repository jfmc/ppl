/* Test the LP_Problem class.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

namespace Parma_Polyhedra_Library {

class ILP_Problem {
private:
  LP_Problem lp_problem;
  Variables_Set i_variables;
  bool have_provisional_optimum;
  mpq_class provisional_optimum_value;
  Generator provisional_optimum_point;
  mpq_class tmp_rational;

public:
  ILP_Problem()
    : lp_problem(),
      i_variables(),
      have_provisional_optimum(false),
      provisional_optimum_value(),
      provisional_optimum_point(point()),
      tmp_rational() {
  }

  explicit
  ILP_Problem(const Constraint_System& cs,
	      const Variables_Set& ivs = Variables_Set(),
	      const Linear_Expression& obj = Linear_Expression::zero(),
	      Optimization_Mode mode = MAXIMIZATION)
    : lp_problem(cs, obj, mode),
      i_variables(ivs),
      have_provisional_optimum(false),
      provisional_optimum_value(),
      provisional_optimum_point(point()),
      tmp_rational() {
  }

  //! Returns the maximum space dimension a LP_Problem can handle.
  static dimension_type max_space_dimension() {
    return LP_Problem::max_space_dimension();
  }

  //! Returns the space dimension of the current LP problem.
  dimension_type space_dimension() const {
    return lp_problem.space_dimension();
  }

  #if 0 // FIXME: properly implement the following to avoid a copy.
  //! Returns the constraints defining the current feasible region.
  const Constraint_System& constraints() const;
#else
  //! Returns the constraints defining the current feasible region.
  Constraint_System constraints() const {
    return lp_problem.constraints();
  }
#endif

  //! Returns the current objective function.
  const Linear_Expression& objective_function() const {
    return lp_problem.objective_function();
  }

  //! Returns the current optimization mode.
  Optimization_Mode optimization_mode() const;

  //! Returns the current objective function.
  const Variables_Set& integer_variables() const {
    return i_variables;
  }

  //! Resets \p *this to be equal to the trivial LP problem.
  void clear() {
    lp_problem.clear();
  }

  /*! \brief
    Adds a copy of constraint \p c to the current LP problem,
    increasing the number of space dimensions if needed.

    \exception std::invalid_argument
    Thrown if the constraint \p c is a strict inequality.
  */
  void add_constraint(const Constraint& c) {
    lp_problem.add_constraint(c);
  }

  /*! \brief
    Adds a copy of the constraints in \p cs to the current LP problem,
    increasing the number of space dimensions if needed.

    \exception std::invalid_argument
    Thrown if the constraint system \p cs contains any strict inequality.
  */
  void add_constraints(const Constraint_System& cs) {
    lp_problem.add_constraints(cs);
  }

  //! Sets the objective function to \p obj.
  /*!
    \exception std::invalid_argument
    Thrown if the space dimension of \p obj is strictly greater than
    the space dimension of \p *this.
  */
  void set_objective_function(const Linear_Expression& obj) {
    lp_problem.set_objective_function(obj);
  }

  //! Sets the optimization mode to \p mode.
  void set_optimization_mode(Optimization_Mode mode) {
    lp_problem.set_optimization_mode(mode);
  }

  //! Checks satisfiability of \p *this.
  //! Optimizes the current LP problem using the primal simplex algorithm.
  /*!
    \return
    An LP_Problem_Status flag indicating the outcome of the optimization
    attempt (unfeasible, unbounded or optimized problem).
  */
  LP_Problem_Status solve() {
    return solve(lp_problem);
  }

  /*!
    \return
    <CODE>true</CODE> if and only if the LP problem is satisfiable.
  */
  bool is_satisfiable() const {
    // FIXME: can be implemented more efficiently.
    return const_cast<ILP_Problem&>(*this).solve() != UNFEASIBLE_LP_PROBLEM;
  }

private:
  LP_Problem_Status solve(const LP_Problem& lp) {
    LP_Problem_Status lp_status = lp.solve();
    if (lp_status == UNFEASIBLE_LP_PROBLEM)
      return UNFEASIBLE_LP_PROBLEM;

    Generator p = point();
    TEMP_INTEGER(tmp_coeff1);
    TEMP_INTEGER(tmp_coeff2);

    if (lp_status == UNBOUNDED_LP_PROBLEM)
      p = lp.feasible_point();
    else {
      assert(lp_status == OPTIMIZED_LP_PROBLEM);
      p = lp.optimizing_point();
      lp.evaluate_objective_function(p, tmp_coeff1, tmp_coeff2);
      assign_r(tmp_rational.get_num(), tmp_coeff1, ROUND_NOT_NEEDED);
      assign_r(tmp_rational.get_den(), tmp_coeff2, ROUND_NOT_NEEDED);
      tmp_rational.canonicalize();
      if (have_provisional_optimum
	  && ((lp_problem.optimization_mode() == MAXIMIZATION
	       && tmp_rational <= provisional_optimum_value)
	      || tmp_rational >= provisional_optimum_value))
	// Abandon this path.
	return lp_status;
    }

    if (p.divisor() == Coefficient_one()) {
      // All the coordinates of `point' are integer.
      if (lp_status == UNBOUNDED_LP_PROBLEM)
	return lp_status;

      if (!have_provisional_optimum
	  || (lp_problem.optimization_mode() == MAXIMIZATION
	      && tmp_rational > provisional_optimum_value)
	  || tmp_rational < provisional_optimum_value) {
	provisional_optimum_value = tmp_rational;
	provisional_optimum_point = p;
	have_provisional_optimum = true;
      }
      return lp_status;
    }

    dimension_type nonint_dim;
    dimension_type space_dim = space_dimension();
    const Coefficient& p_divisor = p.divisor();
    // FIXME: we need a divisibility test for Coefficient.
    TEMP_INTEGER(gcd);
    for (nonint_dim = 0; nonint_dim < space_dim; ++nonint_dim) {
      gcd_assign(gcd, p.coefficient(Variable(nonint_dim)), p_divisor);
      if (gcd != p_divisor)
	break;
    }
    assert(nonint_dim < space_dim);

    assign_r(tmp_rational.get_num(), p.coefficient(Variable(nonint_dim)),
	     ROUND_NOT_NEEDED);
    assign_r(tmp_rational.get_den(), p_divisor,
	     ROUND_NOT_NEEDED);
    tmp_rational.canonicalize();
    assign_r(tmp_coeff1, tmp_rational, ROUND_DOWN);
    assign_r(tmp_coeff2, tmp_rational, ROUND_UP);
    LP_Problem lp_aux = lp;
    lp_aux.add_constraint(Variable(nonint_dim) <= tmp_coeff1);
    solve(lp_aux);
    // TODO: change this when we be able to remove constraints.
    lp_aux = lp;
    lp_aux.add_constraint(Variable(nonint_dim) >= tmp_coeff2);
    solve(lp_aux);
    return lp_status;
  }

public:
  /*! \brief
    Sets \p num and \p den so that \f$\frac{num}{den}\f$ is the result
    of evaluating the objective function on \p evaluating_point.

    \param evaluating_point
    The point on which the objective function will be evaluated.

    \param num
    On exit will contain the numerator of the evaluated value.

    \param den
    On exit will contain the denominator of the evaluated value.

    \exception std::invalid_argument
    Thrown if \p *this and \p evaluating_point are dimension-incompatible
    or if the generator \p evaluating_point is not a point.
  */
  void evaluate_objective_function(const Generator& evaluating_point,
				   Coefficient& num,
				   Coefficient& den) const;

  //! Returns a feasible point for \p *this, if it exists.
  /*!
    \exception std::domain_error
    Thrown if the LP problem is not satisfiable.
  */
  const Generator& feasible_point() const;

  //! Returns an optimal point for \p *this, if it exists.
  /*!
    \exception std::domain_error
    Thrown if \p *this doesn't not have an optimizing point, i.e.,
    if the LP problem is unbounded or not satisfiable.
  */
  const Generator& optimizing_point() const {
    return provisional_optimum_point;
  }
};

} // namespace Parma_Polyhedra_Library

using namespace Parma_Polyhedra_Library;

bool
test01() {
  Variable A(0);
  Variable B(1);
  Variable C(2);

  // Cost function
  Linear_Expression cost(-2*A - 3*B - 4*C);

  // Feasible region.
  Constraint_System cs;
  cs.insert(A + B <= 1);
  cs.insert(A + C <= 1);
  cs.insert(B + C <= 1);
  Variables_Set ivs;
  for (dimension_type i = A.id(); i <= C.id(); ++i) {
    ivs.insert(Variable(i));
    cs.insert(Variable(i) >= 0);
    cs.insert(Variable(i) <= 1);
  }

  ILP_Problem ilp(cs, ivs, cost, MINIMIZATION);

  ilp.solve();

  Generator pg = ilp.optimizing_point();
  nout << "Optimizing point = ";
  using namespace Parma_Polyhedra_Library::IO_Operators;
  nout << pg << endl;
  return true;
}

bool
test02() {
  Variable A(0);
  Variable B(1);
  Constraint_System cs;
  cs.insert(-2*A - B >= -5);
  cs.insert(4*A -4*B >= -5);
  cs.insert(A >= 0);
  cs.insert(B >= 0);

  Variables_Set ivs;
  for (dimension_type i = A.id(); i <= B.id(); ++i)
    ivs.insert(Variable(i));

  Linear_Expression cost(A - 2*B);

  ILP_Problem ilp(cs, ivs, cost, MAXIMIZATION);
  ilp.solve();

  Generator pg = ilp.optimizing_point();
  nout << "Optimizing point = ";
  using namespace Parma_Polyhedra_Library::IO_Operators;
  nout << pg << endl;

  ilp.set_optimization_mode(MINIMIZATION);
  ilp.solve();

  pg = ilp.optimizing_point();
  nout << "Optimizing point = ";
  using namespace Parma_Polyhedra_Library::IO_Operators;
  nout << pg << endl;
  return true;
}

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
END_MAIN
