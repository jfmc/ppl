/* ConSys class implementation: primal_simplex().
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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
#include "Constraint_System.defs.hh"
#include "Row.defs.hh"
#include "Matrix.defs.hh"
#include "Linear_Row.defs.hh"
#include "Linear_System.defs.hh"
#include "Generator.defs.hh"
#include "Linear_Expression.defs.hh"
#include "globals.defs.hh"
#include <cstddef>
#include <iostream>
#include <string>
#include <stdexcept>
#include <sstream>
#include <map>
#include <deque>

namespace PPL = Parma_Polyhedra_Library;
using namespace PPL;

namespace {

//! Normalizes the modulo of coefficients of a row so that they are 
//  mutually prime.
/*!

 \param x  The row to be normalized
 
 Computes the Greatest Common Divisor (GCD) among the elements of
 the row and normalizes them by the GCD itself.
*/
void
normalize_row(Row& x) {
  // Compute the GCD of all the coefficients into gcd.
  TEMP_INTEGER(gcd);
  gcd = 0;
  const dimension_type sz = x.size();
  for (dimension_type i = sz; i-- > 0; ) {
    const Coefficient& x_i = x[i];
    if (x_i != 0)
      gcd_assign(gcd, x_i);
  }
  if (gcd > 1)
    // Divide the coefficients by the GCD.
    for (dimension_type i = sz; i-- > 0; )
      exact_div_assign(x[i], gcd);
}



//! Linearly combines \p x with \p y so that <CODE>*this[k]</CODE> is 0.
/*!
  
\param x  The Row that will be combined with \p y object;
\param y  The Row that will be combined with \p x object;
\param k  The position of \p *this that have to be \f$0\f$.

Computes a linear combination of \p x and \p y having
the element of index \p k equal to \f$0\f$. Then it assigns
the resulting Linear_Row to \p x and normalizes it.
*/

void linear_combine(Row& x, const Row& y, const dimension_type k) {
  assert(x.size() == y.size());
  assert(y[k] != 0 && x[k] != 0);
  // Let g be the GCD between `x[k]' and `y[k]'.
  // For each i the following computes
  //   x[i] = x[i]*y[k]/g - y[i]*x[k]/g.
  TEMP_INTEGER(normalized_x_k);
  TEMP_INTEGER(normalized_y_k);
  normalize2(x[k], y[k], normalized_x_k, normalized_y_k);
  for (dimension_type i = x.size(); i-- > 0; )
    if (i != k) {
      Coefficient& x_i = x[i];
      x_i *= normalized_y_k;
      sub_mul_assign(x_i, y[i], normalized_x_k);
    }
  x[k] = 0;
  normalize_row(x);
} 

//! Inserts a row in a matrix.
//! This has the same effects of Linear_System.insert()
//! The Matrix grows if the Row has more columns than the Matrix ones.
/*!
  
\param matrix The Matrix in which will be inserted the Row;
\param row    The Row that will be inserted with in the \p x Matrix;
  
*/
void
insert_row_in_matrix(Matrix& matrix, const Row& row) {
  dimension_type row_size = row.size();
  dimension_type matrix_num_columns = matrix.num_columns();
  
  if (matrix_num_columns < row_size)
    matrix.add_zero_columns(row_size - matrix_num_columns);
  
  matrix.add_zero_rows(1, Row::Flags());
  dimension_type matrix_num_rows = matrix.num_rows();
  
  for (dimension_type i = 0; i < row_size; ++i) 
    matrix[matrix_num_rows - 1][i] = row[i];
}

//! Append \p k elements to \p r, each with value \p n.
void
add_elements_to_row(Row& r, const dimension_type k, const Coefficient& n) {
  dimension_type r_size = r.size();
  dimension_type s_size = r_size + k;
  Row s(r, s_size, s_size);
  for (dimension_type i = r_size; i < s_size; ++i) 
    s[i] = n;  
  r.swap(s);
}

//! \brief
//! Creates a copy of a column of a matrix.
/*| 
  \param tableau   The matrix that contains the column to be extracted.
  \param index     The index of the column to be extracted.
  \param column    A vector where will be stored the column.
*/

inline void 
copy_column(const Matrix& tableau,
	    const dimension_type index,
	    std::vector<Coefficient>& column) {
  dimension_type tableau_num_rows = tableau.num_rows();
  // Used to avoid memory reallocation during column.push_back().
  column.reserve(tableau_num_rows);
  for (dimension_type i = 0; i < tableau_num_rows; ++i)
    column.push_back(tableau[i][index]);
}

// See pag 42-43 of Papadimitriou. 

//! \brief
//! Swaps two variables in base during the simplex algorithm, doing the 
//! right linear combinations.
/*!
  \param tableau       The matrix representing the constraints
                       of the LP problem.
  \param expressions   The matrix representing the cost function/s
                       of the LP problem.			
  \param inside_var    The index of the column of the matrix representing
                       the variable that has to be inside the base.
  \param outside_row   The index of the row that has to be outside the base.
*/

void
swap_base(Matrix& tableau,
	  Matrix& expressions,
	  const dimension_type inside_var,
	  const dimension_type outside_row) {
  dimension_type tableau_num_rows = tableau.num_rows();
  
  // First we combine the constraints.
  for (dimension_type i = tableau_num_rows; i-- > 0; )
    if (i != outside_row && tableau[i][inside_var] != 0)
      linear_combine(tableau[i],tableau[outside_row], inside_var);
  
  // Now we combine the cost function.  
  if (expressions[0][inside_var] != 0)
    linear_combine(expressions[0], tableau[outside_row], inside_var);
  
}

// See pag. 47 of Papadimitriou.

//! \brief
//!  Checks for the optimality condition of the LP problem.
//!  If this condition is not satisfied, chooses the variable to
//!  put in the base. Implemented with anti-cycling rule.
/*!
  \return              <CODE>true</CODE> if and if only the algorithm finds
                       the optimum condition.
  \param expressions   The matrix that contains the cost function/s.
  \param index_in      The index of the matrix representing the variable that
                       has to go in base, if needed.
*/

inline bool
check_optimality(const Matrix& expressions,
		 dimension_type& index_in) {  
  // If all the coefficients of the cost function are greater or equal
  // than 0, we have reached the optimality condition.
  // The last coefficient represents the sign, so we don't have
  // to check this one. If the optimality condition is not satisfied,
  // we have to choose the variable with the smallest index.
  dimension_type last_column_index = expressions.num_columns()-1;
  
  // We have to remember the "sign" of the cost function:
  // we will use 'sign_value' to get this info. We need this to check the 
  // optimality condition.
  bool sign_value = expressions[0][last_column_index] > 0;
  for (dimension_type i = 1; i < last_column_index; ++i)
    if (sign_value ? expressions[0][i] > 0 : expressions[0][i] < 0) {
      index_in = i;
      return false;
    }
  return true;
}

// See pag. 47 + 50 of Papadimitriou.  

//! \brief
//! Chooses the variable to put out of the base of the LP problem.
//! Implemented with anti-cycling rules.
/*!
  \return                   An integer representing the row that refers to the
                            variable that has to go out of the base.
  \param tableau            The matrix that has to be checked.
  \param index_will_be_in   The index of the matrix that represents the
                            variable that has to go out from the base.
  \param base               The array that stores the variables in base.
  \param unbounded          Bool that will be set to <CODE>true<\CODE>
                            if and only if the LP problem is unbounded.
*/

inline int
choose_out_var(const Matrix& tableau,  
	       const int index_will_be_in,
	       std::vector<dimension_type>& base,
	       bool& unbounded){  
  // To choose the variable that has to go out from the base, we must 
  // look for the row such that row[index_will_be_in]/row[inhomogeneous_term]
  // is the smallest, but > 0, in all the rows of the tableau.
  
  // To compute the variable that has to go out from the base, we simply need
  // these 2 columns: inhomogeneous terms, variable that has to get in the
  // base(previously computed).
  
  std::vector<Coefficient> inhomogeneous;
  copy_column(tableau, 0, inhomogeneous);
  
  // This one will contain the row indexes of the tableau that will be
  // considered to compute the variable that has to go out from the base.
  std::vector<dimension_type> candidate_constraints;
  
  // We must suppose at this moment that the solution to the LP is unbounded,
  // but if we will find a row such that tableau[index_will_be_in][i] > 0  
  // && tableau[i][base[i]] > 0 or (since we don't store the constraints in a 
  // special way) tableau[index_will_be_in][i] < 0 && tableau[i][base[i]] < 0
  // we will be sure that the solution is not unbounded.
  unbounded = true;
  dimension_type tableau_num_rows = tableau.num_rows();
  
  // We want to insert in candidate_constraints the indexes of the rows such
  // that tableau[index_will_be_in][i] > 0 && tableau[i][base[i]] > 0
  // or tableau[index_will_be_in][i] < 0 && tableau[i][base[i]] < 0.
  for (dimension_type i = tableau_num_rows; i-- > 0; ) {
    int sgn_a = sgn(tableau[i][index_will_be_in]);
    if (sgn_a != 0 && sgn_a == sgn(tableau[i][base[i]])) {
      candidate_constraints.push_back(i);
      unbounded = false;
    }
  }
 
  // If the problem is unbounded we have finished: in this case the return 
  // value is not important, the calling function will look to the "unbounded"
  // variable first.
  if (unbounded)
    return 0;
  
  // Now we are sure that we can compute the out_variable_row index.
  // So we start computing the lcm of the var_in_column rows in the candidate
  // constraints.
  dimension_type candidate_constraints_size = candidate_constraints.size();
  Coefficient lcm = 1;
  
  // We are not interested about the sign of 'lcm'. We will work with 
  // an absolute value later.
  for (dimension_type i = candidate_constraints_size; i-- > 0; )
    lcm_assign(lcm, tableau[candidate_constraints[i]][index_will_be_in]);
  
  // We proceed computing the right representation of inhomogeneous terms.
  for (dimension_type i = candidate_constraints_size; i-- > 0; ) {  
    dimension_type index = candidate_constraints[i];
    inhomogeneous[index] *= abs(lcm / tableau[index][index_will_be_in]);
  }
  // On exit this will contain the column index of tableau of the variable
  // that has to go out from the base. At the begin we suppose that the first
  // row indexed by candidate_constraints.
  dimension_type out_var_row = candidate_constraints[0];  
  
  // This is used to store the value of the inhomogeneous term of the row
  // that express the variable that has to go out from the base.
  Coefficient var_value = abs(inhomogeneous[candidate_constraints[0]]);
  
  // Now we proceed comparing the other rows indexed by candidate_constraints
  // with the first one.
  for (dimension_type i = 0; i < candidate_constraints_size; ++i) {
    dimension_type checked_index = candidate_constraints[i]; {
      if (abs(inhomogeneous[checked_index]) < var_value){
	var_value = abs(inhomogeneous[checked_index]); 
	out_var_row = checked_index;
      }
      // But if we find a variable with the same value of the inhomogeneous
      // term of the last one chosen, but its index is smaller, we choose
      // that one. (Anti-cycling Rule)
      else
	if (abs(inhomogeneous[checked_index]) == var_value
	    && base[checked_index] < base[out_var_row])
	  out_var_row = checked_index;
    }
  }
  
  // Now we can set the "base" vector with the new computed base.
  base[out_var_row] = index_will_be_in;
  return out_var_row;
}

// See pag 49 of Papadimitriou.

//! \brief
//! The simplex algorithm.
/*! 
  \return            <CODE>true</CODE> if and if only the algorithm computes
                     a solution. It`s a point of the ConSys.
  \param tableau     The matrix containing the constraints of the LP problem.
  \param expressions The matrix that contains the cost function-s to be
                     maximized/minimized. 
  \param base        The vector containing the elements in the base
                     of the LP problem.

To solve the LP problem compute_simplex must receive the problem in this way:

1) Linear Expressions:
k + x_1 + x_2 + ... + x_n + s (with s > 0)
where k is the inhomogeneous term, x_i are the coefficients of the variables
and s is a special variable that stands for the sign of the cost function.
In this way is possible to know if the cost function was reversed during 
the linear combinations. Reasoning in a "PPL way" we can think that this 
row stands for k + x_1 + x_2 + s = 0, so -s = k + x_1 + x_2 + ...  + x_n.
We use a matrix instead of a row because in this way is very simple to have
the cost function of the second_phase. The old objective cost function, if
compute_simplex is called by first_phase, is stored in the second row.

2) Constraints:
These are the standard "PPL rows", so there should not be special 
problems to understand what's in this matrix.

We must assure that the two matrices have the same space dimension!
*/

bool
compute_simplex(Matrix& tableau,
		Matrix& expressions,
		std::vector<dimension_type>& base){ 
  while (true) {
    // In this one will be stored the variable that will go in base
    // if and only if the optimality condition is not satisfied.
    dimension_type index_will_be_in;
    if (check_optimality(expressions, index_will_be_in)){
      // If we are in the 1st phase, we have to express the old cost function
      // terms of the computed base.
      if (expressions.num_rows() == 2){
	for (dimension_type i = tableau.num_rows(); i-- > 0; ) 
	  if (expressions[1][base[i]] != 0)
	    linear_combine(expressions[1],tableau[i], base[i]);
      }
      return true;
    }
    
    // We have to choose the row the contains the variable to get out
    // from the base.
    bool unbounded = false;
    dimension_type out_var_row;
    out_var_row = choose_out_var(tableau, index_will_be_in, base, unbounded);
    
    // During the search of the variable that has to get out from the base,
    // we can know if the problem is unbounded or not.
    if (unbounded) 
      return false;
    
    // We have not reached the optimality or unbounded condition.
    // So we have to reach another vertex of the Polyhedron.
    swap_base(tableau, expressions, index_will_be_in, out_var_row);
  }
}
// See pag 28  Papadimitriou.

//! \brief
//! Adds the slack variables to satisfy the standard form of a LP problem,
//! inserts the "sign" to the cost functions, and makes the
//! necessary swaps to express the problem with the 1st phase base.
/*!
  \param tableau   The matrix containing the LP problem.
  \param obj       The row containing the original cost function.
  \param base      The vector that stores the variables in the base of
                   the LP problem.
*/
void   
prepare_for_1st_ph_simplex(Matrix& tableau,
			   Row& old_obj,
			   Row& new_obj,
			   std::vector<dimension_type>& base) {
  
  // We negate the row if tableau[i][0] <= 0 to get the inhomogeneous term > 0.
  // This simplifies the insertion of the slack variables: the value of the 
  // slack variable of every constraint will be 1.
  for (dimension_type i = tableau.num_rows(); i-- > 0 ; ) 
    if (tableau[i][0] > 0)
      for (dimension_type j = tableau.num_columns(); j-- > 0; )
	negate(tableau[i][j]);
  // Insertion of the slack variables.
  for (dimension_type i = 0; i < tableau.num_rows(); ++i) {
    tableau.add_zero_columns(1);
    tableau[i][tableau.num_columns()-1] = 1;
    base[i] = tableau.num_columns()-1;
  }
  
  // We add the extra-coefficient (sign) for the cost function to the tableau.
  // This is done to keep track of the possible sign`s inversion.
  // We have to resize the old and the new cost function row and add the
  // extra-coefficient also to the old cost function.

  // Here we set the 1st phase cost function: if the first slack variable has 
  // index n+1 and the space dimension is n+m, the new cost function is:
  // k = - x_n+1 - x_n+2 - ... - x_n+m.  
  add_elements_to_row(new_obj, tableau.num_rows(), -1);

  // We have also to resize the old cost function.
  add_elements_to_row(old_obj, tableau.num_rows(), 0);
 
  // Extra variable insertion (sign).
  add_elements_to_row(new_obj, 1, 1);
  add_elements_to_row(old_obj, 1, 1);
  
  // We have to add another column to the tableau to allow linear combinations:
  // the rows must have the same space dimension.
  tableau.add_zero_columns(1);
  
  // Now we express the problem in terms of the variable in base.
  for (dimension_type i = tableau.num_rows(); i-- > 0; ) 
   linear_combine(new_obj, tableau[i], base[i]);              
  
}

// See pag 55-56 Papadimitriou.

//! \brief
//! Deletes the non necessary slack variables from the matrix
//! and makes the last necessary swaps to let start the 2nd phase.
/*!
  \param tableau      The matrix containing the constraints of the LP problem.
  \param expressions  The matrix containing the cost functions.
  \param base         The array that stores the variables in base of
                      a LP problem.
*/

void erase_slacks(Matrix& tableau,
		  Matrix& expressions,
		  std::vector<dimension_type>& base) {
  dimension_type tableau_n_col = tableau.num_columns();
  dimension_type tableau_last_index = tableau.num_columns()-1;
  dimension_type tableau_n_rows = tableau.num_rows();
  dimension_type first_slack_index = tableau_n_col - tableau_n_rows -1;
  std::vector<dimension_type> redundant_rows;
  
  // Step 1: We try to put out from the base all the remaining slack variables:
  for (dimension_type i = tableau_n_rows; i-- > 0; ){
    bool transformed = false;
    if (base[i] >= first_slack_index) {
      // We search for an element != 0, this will go in base.
      for (dimension_type j = first_slack_index; j-- > 1; ) {
	if (tableau[i][j] != 0) { 
	  swap_base(tableau, expressions, j, i);
	  // So we adjust the base.
	  base[i] = j;
	  transformed = true;
	  break;
	}
      }
      // If we reach this point of the code, we did not successfully find 
      // an original variable to put in base, this means that this constraint
      // is redundant: we have to delete this one from the tableau.
      if (!transformed)
	redundant_rows.push_back(i);
    }
  }

  // Step 2: Creation the new constraints tableau and the new expression 
  //         tableau; adjust base vector.
  
  // We set the dimensions of the new constraints_tableau...
  dimension_type new_tableau_columns = first_slack_index + 1;
 
  // This one will be the new matrix that will represent the 2nd phase problem.
  Matrix new_constraint_tableau(0, 0);
 
  // ...and here the ones of the expressions tableau, but this time we will
  // have only one cost function.  
  Matrix new_expression_tableau(1, new_tableau_columns);
 
  dimension_type new_tableau_last_index = new_tableau_columns - 1; 
  
  // This one will represent the new base.
  std::vector<dimension_type> new_base;
  
  // Now we can fill the new matrices, without the redundant constraints.
  for (dimension_type i = 0; i < tableau_n_rows; ++i)
    // We will insert the constraint if and if only is not redundant.
    if (find(redundant_rows.begin(), redundant_rows.end(), i) ==
	redundant_rows.end()){

      // We will create a new row: this will be inserted in the matrix.
      Row new_row(new_tableau_columns, Row::Flags());
      
      // We start copying the row from the beginning (inhomogeneous term)
      // until the last original variable index. 
      for(dimension_type j = 0; j < first_slack_index; ++j)
	new_row[j] = tableau[i][j];
      
      // Last step, we must copy the last element of the row, be we know
      // is a zero! (it's the special variable used to know the sign of the
      // cost function.
      new_row[new_tableau_last_index] = 0;
      assert(tableau[i][tableau_last_index] == 0);
      
      // Insertion of the row in the new matrix.
      insert_row_in_matrix(new_constraint_tableau, new_row);
      
      // We have to set also the base value.
      new_base.push_back(base[i]);
    }
  
  // We can set the new expression tableau.
  for(dimension_type j = 0; j < first_slack_index; ++j)
    new_expression_tableau[0][j] = expressions[1][j];
  
  // Last step, we must copy the last element of the row.
  new_expression_tableau[0][new_tableau_last_index] =
    expressions[1][tableau_last_index];
  
  // Well done, let's start swapping the old tableau
  // and the base with the new ones!
  expressions.swap(new_expression_tableau);
  tableau.swap(new_constraint_tableau);
  base.swap(new_base);
}

// See pag 55 of Papadimitriou.

//! \brief
//! First phase of the simplex algorithm. Computes a feasible base and, if 
//! possible, solves also the second phase.
/*!
  \return         <CODE>true</CODE> if and if only the problem has a feasible
                  base.
  \param tableau  The matrix containing the constraints of the LP problem, 
                  given in the "compute_tableau" way.
  \param base     The vector that stores the variables in the base of a LP 
                  problem.

   To do this, will be inserted the necessary slack variables, the signs to 
   the cost functions and the new problem will be expressed by the slack 
   variables that will be in base at first.
*/ 
bool 
first_phase(Matrix& tableau,
	    Row& old_obj_function,
	    std::vector<dimension_type>& base) {
  // This will contain the new cost function of the 1st phase problem.
  Row new_obj_function(old_obj_function);
  
  // We need to clean this row setting all the values to 0.
  for (dimension_type i = new_obj_function.size(); i-- > 0; )  
    new_obj_function[i] = 0;
  
  // Adds the necessary slack variables to get the 1st phase problem.
  prepare_for_1st_ph_simplex(tableau, old_obj_function,
			     new_obj_function, base);
    
  // This code will build a matrix where will be stored only
  // the constraints of the LP problem.
  Matrix tableau_constraints(0,0);
  for (dimension_type i = 0; i < tableau.num_rows(); ++i)
   insert_row_in_matrix(tableau_constraints, tableau[i]);              
  
  // Here we will build a matrix that will store the new cost function in the
  // first row, and the old cost function in the second.
  Matrix tableau_expressions(0,0);
  insert_row_in_matrix(tableau_expressions, new_obj_function); 
  insert_row_in_matrix(tableau_expressions, old_obj_function);
  
  // Now we can start solving the first phase problem.
  bool ok = compute_simplex(tableau_constraints, tableau_expressions, base); 
  
  // If the first phase problem succeeds and the optimum value is zero,
  // we proceed erasing the slack variables we introduced and solve the
  // problem with the base computed by the 1st phase.
  
  if (ok && tableau_expressions[0][0] == 0) {
    erase_slacks(tableau_constraints, tableau_expressions, base);
    bool result = compute_simplex(tableau_constraints, tableau_expressions, 
				 base);
    tableau.swap(tableau_constraints); 
    return result;
  } 
  else  // No feasible base found.
    return false;
}

//! \brief
//! Assigns to \p tableau a simplex tableau representing the problem
//! given by the constraints in \p cs and the cost function \p expr,
//! inserting to \p map the informations that are required to go
//! back to the original problem.
/*!
  \return               <CODE>false</CODE> if the problem is trivially empty,
                        in which case the tableau is not built;
                        <CODE>true</CODE> otherwise.
  \param cs             A matrix containing the constraints of the problem.
  \param cost_function  The cost function of the problem.
  \param tableau        A matrix where to store the resulting tableau.
  \param map            Contains all the pairs (i, j) such that Variable(i)
                        (that was not found to be constrained in sign)
                        has been split into two nonnegative variables.
                        The "positive" one is represented again by Variable(i),
	                and the "negative" one is represented by Variable(j).
*/
bool
compute_tableau(const Linear_System& cs,
		Row& cost_function,
		Matrix& tableau,
		std::map<dimension_type, dimension_type>& map) {
  assert(tableau.num_rows() == 0);
  // In the zero-dimensional case there is little to do:
  // the maximum/minimum is simply the inhomogeneous term of the expression.
  if (cs.space_dimension() == 0)
    return false;
  
  typedef std::map<dimension_type, dimension_type>::value_type map_value_type;
  dimension_type cs_num_rows = cs.num_rows();
  dimension_type cs_num_columns = cs.num_columns();
  dimension_type cs_space_dim = cs.space_dimension();
  
  // Phase 1:
  // determine variables that are constrained to be nonnegative,
  // detect nonnegativity constraints that will not be part of the
  // tableau and count the number of inequalities.
  
  // On exit, `nonnegative_variable[j]' will be true if and only if
  // Variable(j) is bound to be nonnegative in `cs'.
  std::deque<bool> nonnegative_variable(cs_space_dim, false);
  
  // On exit, `trivially_true_constraint[j]' will be true if and only if
  // the row of index 'j' is trivially true: it will not be inserted in
  // the tableau.
  std::deque<bool> trivially_true_constraint(cs_num_rows, false);

  // On exit, this will hold the number of nonnegative variables.
  dimension_type num_nonnegative = 0;
  
  // On exit, `nonnegativity_constraint[i]' will be true if and only if
  // the row of index `i' in `cs' contains a constraint that only
  // expresses the non-negativity of one variable.
  std::deque<bool> nonnegativity_constraint(cs_num_rows, false);
  dimension_type nonnegativity_constraint_number = 0;
  
  // On exit, `num_inequalities_left' will contain the number of
  // inequalities that convey more than the non-negativity of one
  // variables.  For each one of these inequalities we will have to
  // introduce a slack variable.
  dimension_type num_inequalities_left = 0;
  
  // Process each row of the `cs' matrix.
  for (dimension_type i = cs.num_rows(); i-- > 0; ) { 
    const Linear_Row& cs_i = cs[i];
    bool found_nonzero_coeff = false;   
    dimension_type nonzero_coeff_column_index = 0;
    for (dimension_type j = cs_num_columns; j-- > 1; ) {
      if (cs_i[j] != 0)
	if (found_nonzero_coeff) {
	  // Now we check if we have to insert a slack variable
	  // for this constraint.
	  if (! cs_i.is_line_or_equality())
	    ++num_inequalities_left;
	  goto skip;
	}
	else {
	  nonzero_coeff_column_index = j;
	  found_nonzero_coeff = true;
	}
    }
    if (!found_nonzero_coeff) {  
      // All coefficients were 0.  Either the constraint is trivially
      // true (like 1 >= 0), in which case it must be ignored, or it
      // is trivially false (like -1 >= 0), in which case there is no
      // solution and we return false to the caller.
      
      if ((cs_i.is_ray_or_point_or_inequality() && cs_i[0] >= 0)
	  || (cs_i.is_line_or_equality() && cs_i[0] == 0)) {
	// The constraint is trivially true.
	trivially_true_constraint[i] = true;
	goto skip;
      }
      else
	// The constraint is trivially false: the problem is thus empty.
	return false;
    }
    // We can now check if the variable represented by 
    // non_zero_coeff_column_index can be split or not
    // and if a constraint is non-negativity constraint.
    else {
      /* 
	 
      We have the following cases:
      1) The variable will be split and the constraint will 
         be inserted in the tableau.
      2) The variable will not be split and the constraint will be 
         inserted in the tableau.
      3) The variable will not be split and the constraint will not 
         be inserted in the tableau.
	    
	            
	 These are the 12 possible combinations we can have: 
               a |  b | constraint type   | case
      where a represents the coefficient of the variable of the constraint, 
      b represents the inhomogeneous term.
	    
      1)       >0 | >0 |         >=        |   1
      2)       >0 | =0 |         >=        |   3
      3)       <0 | >0 |         >=        |   1
      4)       <0 | =0 |         >=        |   1
      5)       >0 | >0 |          =        |   1
      6)       >0 | =0 |          =        |   2
      7)       <0 | >0 |          =        |   2
      8)       <0 | =0 |          =        |   2
      9)       >0 | <0 |         >=        |   2
      10)      >0 | <0 |         >=        |   1
      11)      <0 | <0 |          =        |   2
      12)      <0 | <0 |          =        |   1
       
      The next lines will cover these 3 cases.
      */     
    
      // We now look for the signs non_zero_coeff_column_index and the
      // inhomogeneous term.
      
      int sgn_a = sgn(cs_i[nonzero_coeff_column_index]);
      int sgn_b = sgn(cs_i[0]);
     
      // The variable index is not equal to the column index.
      dimension_type nonzero_var_index = nonzero_coeff_column_index - 1;
 
      // Case 1 (1)(5)(10)(12): 
      // If this case succeeds, we have to insert the constraint 
      // to the problem, splitting the variable represented by
      // nonzero_coeff_column_index.
      if (sgn_a == sgn_b) {
	if (!cs_i.is_line_or_equality()){
	  ++num_inequalities_left;
	}
	else { 
	}
      }
      
      // Case 2: (6)(7)(8)(9)(11) 
      // In this case, we find a non_negative variable, but the
      // constraint must be inserted in our new LP problem.
      else if (cs_i.is_line_or_equality() || sgn_b < 0){
      
	// Before increasing num_nonnegative, we have to check if this
	// coefficient was already successfully checked.
	if (!nonnegative_variable[nonzero_var_index]) {
	  nonnegative_variable[nonzero_var_index] = true;
	  ++num_nonnegative;
	}
	if (!cs_i.is_line_or_equality())
	  ++num_inequalities_left;
      }
      
      // Case 3: (2)
      // In this case we find a non_negative_variable expressed by
      // a nonnegativity constraint. We will not insert this constraint 
      // in the tableau.
      else if (sgn_a > 0) {
	
	// Before increasing num_nonnegative, we have to check if this
	// coefficient was already successfully checked.
	if (!nonnegative_variable[nonzero_var_index]) {
	  nonnegative_variable[nonzero_var_index] = true;
	  ++num_nonnegative;
	}
	nonnegativity_constraint[i] = true;
	++nonnegativity_constraint_number;
      }
      
      // Case 1: (3)(4)
      // Last case, we have to split the variable and insert the constraint.
      // So we have nothing to do.(We have surely an inequality constraint now)
      else 
	++num_inequalities_left;
    }
  skip: ;
    
  }
  // Now we can fill the map.
  int j = nonnegative_variable.size();
  for (dimension_type i = 0; i < nonnegative_variable.size(); ++i) {
    if (!nonnegative_variable[i]) {
      map.insert(map_value_type(i, j));
      ++j;
    }
  }
  // Phase 2: 
  // set the dimension for the tableau and insert the (possibly transformed)
  // cost function as the first row.

  // This will be the new size of the rows that will be inserted in the 
  // tableau. The size is computed in this way:
  dimension_type new_row_size = 2*cs_num_columns - num_nonnegative +
    num_inequalities_left - 1; 
   
  // We have to resize the cost function to 'new_row_size'.
  Row resized_cost_function(cost_function, new_row_size, new_row_size);
  cost_function.swap(resized_cost_function);
  
  // Phase 3:
  // insert all the (possibly transformed) constraints that are not
  // nonnegativity constraints. The transformation includes both
  // the variable splitting (for variables that are unconstrained
  // in sign) and the addition of slack variables (for inequalities
  // in the original problem).
  
  // This is the index in the matrix where we will begin to work
  // with the slack variables.
  dimension_type slack_pos_index = new_row_size - num_inequalities_left; 
  
  // We must set the new size of the rows of tableau (at the moment is a 0x0).
  tableau.add_zero_columns(new_row_size);
  
  // Insertion of the constraints the tableau.
  for(dimension_type i = 0; i < cs.num_rows(); ++i) {
    // We are going to insert only constraints that are not trivially true
    // (5 > 3) and that are not nonnegativity_constraints (X > 0).
    if (!nonnegativity_constraint[i] && !trivially_true_constraint[i]){
      insert_row_in_matrix(tableau, cs[i]);
      // Here we also add a slack variable to the constraint, if we need it.
      if (!cs[i].is_line_or_equality()) {
	tableau[tableau.num_rows() - 1][slack_pos_index] = -1;
	++slack_pos_index;
      } 
    }
  }
 
  dimension_type tableau_num_rows = tableau.num_rows();
  // Last step: we proceed splitting variables in the tableau.
  std::map<dimension_type, dimension_type>::iterator map_itr = map.begin();
  while (map_itr != map.end()) {
    for (dimension_type i = tableau_num_rows; i-- > 0; ) 
      tableau[i][(map_itr->second) +1] = -tableau[i][(map_itr->first) + 1];
    cost_function[(map_itr->second) +1] = -cost_function[(map_itr->first) + 1];
    ++map_itr;
  }
  // If the computed tableau is empty the problem is unbounded or the origin
  // is a point of maximum/minimum.
  if (tableau.num_rows() == 0) 
    return false;
  
  assert(tableau.num_columns() != 0);
  return true;
}

//! \brief
//  FIXME: This comment sucks!
//  Checks if a variable is in base and assigns to 'row' the row index of which
//  is base.
/*!
  \return              <CODE>true</CODE> if and if only a is in base
  \param base          The base of the LP problem.
  \param var_to_check  The variable that has to be checked if is in base.
  \param row           Here will be written the index of the
                       row of which var_to_check is base.
*/ 
bool
is_in_base(const std::vector<dimension_type>& base,
	   const dimension_type var_to_check,
	   dimension_type& row ) {
  for (dimension_type i = base.size(); i-- > 0; )
    if (base[i] == var_to_check) {
      row = i;
      return true;
    }
  return false;
}

//! \brief
//! Computes the generator that will be given on exit of primal_simplex, if
//! the problem has an optimality point.
/*!
  \return                   The computed generator.                 
  \param tableau            A matrix containing the constraints 
                            of the solved problem.
  \param base               The base of the LP problem
  \param map                Contains all the pairs (i, j) such that Variable(i)
                            (that was not found to be constrained in sign)
                            has been split into two nonnegative variables.
                            The "positive" one is represented again by 
                            Variable(i), and the "negative" one is represented
                            by Variable(j).
  \param original_space_dim The original space dimension of the LP problem.
 
*/

Generator
compute_generator(const Matrix& tableau,
		  const std::vector<dimension_type>& base,
		  std::map<dimension_type, dimension_type>& map,
		  const dimension_type original_space_dim) {
  // We will store in num[] and in den[] the numerators and
  // the denominators of every variable of the original problem.
  Coefficient num[original_space_dim];
  Coefficient den[original_space_dim];
  dimension_type row = 0;

  // We start to compute num[] and den[].
  for (dimension_type i = original_space_dim; i-- > 0; ) {
    // If the variable was not split, we have simply to check the 
    // value of that variable in the computed matrix.
    if(map.find(i) == map.end()) {
       
      // If the variable is in base we look at its value.
      if (is_in_base(base, i+1, row)) {
	num[i]= -tableau[row][0];
	den[i]= tableau[row][base[row]];
      }
       
      // If is not in base, its value is surely 0/1.
      else {
	num[i] = 0;
	den[i] = 1;
      }
    }
     
    // If the original variable was split, we have to find the its value.
    // The value of the original variable is expressed by the difference
    // between the original variable and the second one we introduced 
    // (the index of this one in the tableau is known by the map).
    // map[i] + 1 is the index in the tableau that express the location
    // of the extra variable that we have introduced.
    else {
      Coefficient split_num[2];
      Coefficient split_den[2];
    
      for (dimension_type j = 0; j < 2; ++j){
	// Like before, we he have to check if the variable is in base.
	if (is_in_base(base, j == 0 ? i+1 : map[i]+1, row)) {
	  split_num[j] = - tableau[row][0];
	  split_den[j] =  tableau[row][base[row]];
	}
	else {
	  split_num[j] = 0;
	  split_den[j] = 1;
	}
      }
      // We compute the lcm to compute subsequently the difference
      // between the 2 variables.
      Coefficient lcm;
      lcm_assign(lcm, split_den[0], split_den[1]);
      split_num[0] *= lcm/split_den[0];
      split_num[1] *= lcm/split_den[1];
      num[i] = split_num[0] - split_num[1];
      den[i] = lcm;
      
      // If the numerator is 0, is better to keep the denominator to 1,
      // this to increase the speed of the computation of the lcm.
      if (num[i] == 0)
	den[i] = 1;
    }
  }

  // Before computing the generator, we need to have all the denominators > 0.
  // In this way we get the (a/b) rational representation, with b > 0 and 
  // a free in sign.
  for (dimension_type i = original_space_dim; i-- > 0; )
    if (den[i] < 0 ){
      negate(num[i]);
      negate(den[i]);
    }
  
  // To compute the generator we have to get the common denominator
  // of all the values stored in denominators[].
  Coefficient lcm = 1;
  for (dimension_type i = original_space_dim; i-- > 0; ) 
    lcm_assign(lcm, den[i]);
  
  // So we multiply the numerators.
  for (dimension_type i = original_space_dim; i-- > 0; )
   num[i] *= lcm/den[i];
  
  // At last we can build our generator.
  Linear_Expression my_generator;
  for (dimension_type i = original_space_dim; i-- > 0; )
    my_generator += Variable(i)*num[i];
  
  // We proceed returning g.
  Generator g = point(my_generator, lcm);
  return g;
}
}//namespace

bool
PPL::Constraint_System::primal_simplex(const Linear_Expression& expression,
				       const bool maximize,
				       Coefficient& ext_n, Coefficient& ext_d,
				       const Generator** const pppoint) const {
  // FIXME: putting this declaration at the beginning of the file
  // does not work.  A GCC bug?
  using namespace Parma_Polyhedra_Library::IO_Operators;

  // Strict inequality constraints are not supported.
  if (topology() == NOT_NECESSARILY_CLOSED)
    throw std::invalid_argument("PPL::ConSys::primal_simplex(): "
				"strict inequality constraints "
				"are not supported.");

  dimension_type space_dim = space_dimension();
  dimension_type expr_space_dim = expression.space_dimension();
  
  // Make sure the dimension of the expression to maximize is not greater
  // than the dimension of the constraint system.
  if (space_dim < expr_space_dim) {
    std::ostringstream s;
    s << "PPL::ConSys::primal_simplex():" << std::endl
      << "this->space_dimension() == " << space_dim
      //      << ", " << expression << "->space_dimension() == "
      << expr_space_dim << ".";
    throw std::invalid_argument(s.str());
  }

  // This will be processed by compute_tableau.
  Matrix tableau(0,0);
 
  // We will work with a copy of the cost function.
  Linear_Expression cost_function = expression;
 
  // We start computing the tableau.
  std::map<dimension_type,dimension_type> map;
  
  // To compute minimization, we have just to negate the coefficients of cost
  // function.
  if (!maximize) 
    for (dimension_type i = cost_function.size(); i-- > 0; ) 
      negate(cost_function[i]);
  
  if (!compute_tableau(*this, cost_function, tableau, map)) {
    // The tableau was not successfully built: there are 2 cases to analyze.
    // If a coefficient of variable of the cost function is > 0 the problem
    // is unbounded.
    for (dimension_type i = cost_function.size(); i-- > 1; )
      if (cost_function[i] > 0)
	return false;
    // Else a feasible solution to our LP problem is the origin. 
    ext_n = cost_function.inhomogeneous_term();
    ext_d = 1;
    Generator g = point();
    Generator* gen = new Generator(g);
    if (pppoint != 0) 
      *pppoint = gen;
    return true;
  }
   
   
  // At this moment we can call only first_phase() to solve our LP problem
  // since we don't have a feasible base.
  std::vector<dimension_type> base(tableau.num_rows()); 
  bool return_value = first_phase(tableau, cost_function, base);
  
  if (!return_value) 
    return false;
  
  else {
    Generator g = compute_generator(tableau, base, map, space_dim);
    // To use pppoint we need dynamic memory allocation.
    // FIXME: Here we have a memory leak! 
    Generator* gen = new Generator(g);
    if(pppoint != 0) 
      *pppoint = gen;    
    
    // Here we recompute the value of the cost function substituting the
    // value of the Generator g in the cost function.
    Coefficient max = expression.inhomogeneous_term();
    for (dimension_type i = g.space_dimension(); i-- > 0; )  
      max += g.coefficient(Variable(i))*expression.coefficient(Variable(i));
    
    // We want numerator and denominator to be coprime.
    Coefficient gcd = 0;
    Coefficient g_divisor = g.divisor();
    gcd_assign(gcd, max, g_divisor);
    max /= gcd;
    g_divisor /= gcd;
    ext_n = max;
    ext_d = g_divisor;
    
    // We check our computed generator.
    assert(this->satisfies_all_constraints(g));  
    return true; 
  }
}
