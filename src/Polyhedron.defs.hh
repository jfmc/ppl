/* Polyhedron class declaration.
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

#ifndef _Polyhedron_defs_hh
#define _Polyhedron_defs_hh 1

#include "Variable.defs.hh"
#include "LinExpression.defs.hh"
#include "ConSys.defs.hh"
#include "GenSys.defs.hh"
#include "SatMatrix.defs.hh"
#include "Status.defs.hh"
#include "Polyhedron.types.hh"
#include <vector>

namespace Parma_Polyhedra_Library {
  // Put them in the namespace here to declare them friend later.

  //! @name Polyhedron. 
  //@{
  //! Returns <CODE>true</CODE> if and only if
  //! \p x and \p y are the same polyhedron.
  bool operator ==(const Polyhedron& x, const Polyhedron& y);
  //! Returns <CODE>true</CODE> if and only if
  //! \p x and \p y are different polyhedra.
  bool operator !=(const Polyhedron& x, const Polyhedron& y);
  //! Returns <CODE>true</CODE> if and only if
  //! \p x is strictly contained in \p y.
  bool operator <(const Polyhedron& x, const Polyhedron& y);
  //! Returns <CODE>true</CODE> if and only if
  //! \p x strictly contains \p y.
  bool operator >(const Polyhedron& x, const Polyhedron& y);
  //! Returns <CODE>true</CODE> if and only if
  //! \p x is contained in \p y.
  bool operator <=(const Polyhedron& x, const Polyhedron& y);
  //! Returns <CODE>true</CODE> if and only if
  //!  \p x contains \p y.
  bool operator >=(const Polyhedron& x, const Polyhedron& y);
  //! Output operator.
  std::ostream& operator <<(std::ostream& s, const Polyhedron& p);
  //! Input operator.
  std::istream& operator >>(std::istream& s, Polyhedron& p);
  //@}
}

//! A convex polyhedron.
/*!
  An object of the class Polyhedron represents a convex polyhedron
  in the space \f$\mathbb{R}^n\f$.

  A polyhedron can be specified as either a finite system of constraints
  or a finite set of generators (see Minkowski's theorem in definition.dox).
  So, we have the possibility to obtain a system from the 
  other. In fact, if we have the system of constaints we can obtain 
  the set of generators from this and vice versa.
  These systems can contain some redundant members: in this case we say 
  that they are not in the minimal form.
 
  \par Example 1
  The following code builds a square in \f$\mathbb{R}^2\f$ starting from
  the system of constraints:
  \code 
  Variable x(0);
  Variable y(1);
  ConSys cs;
  cs.insert(x >= 0);
  cs.insert(x <= 3);
  cs.insert(y >= 0);
  cs.insert(y <= 3);
  Polyhedron ph(cs);
  \endcode
  The following code builds the same polyhedron starting from the
  set of generators:
  \code
  Variable x(0);
  Variable y(1);
  GenSys gs;
  gs.insert(0 * x + 0 * y /= 1);
  gs.insert(0 * x + 3 * y /= 1);
  gs.insert(3 * x + 0 * y /= 1);
  gs.insert(3 * x + 3 * y /= 1);
  Polyhedron ph(gs);
  \endcode

  \par Example 2
  The following code builds an half-strip in \f$\mathbb{R}^2\f$ 
  starting from the system of constraints:
  \code
  Variable x(0);
  Variable y(1);
  ConSys cs;
  cs.insert(x >= 0);
  cs.insert(x - y <= 0);
  cs.insert(x - y + 1 >= 0);
  Polyhedron ph(cs);
  \endcode
  The following code builds the same polyhedron starting from the
  set of generators:
  \code 
  Variable x(0);
  Variable y(1);
  GenSys gs;
  gs.insert(0 * x + 0 * y /= 1);
  gs.insert(0 * x + y /= 1);
  gs.insert(1 ^ x - y);
  Polyhedron ph(gs);
  \endcode

  \par Example 3
  The following code builds the half-plane in \f$\mathbb{R}^2\f$
  starting from the constraints:
  \code
  Variable x(0);
  Variable y(1);
  Polyhedron ph;
  ph.insert(y >= 0);
  \endcode
  The following code builds the same polyhedron starting from 
  the generators:
  \code
  Variable x(0);
  Variable y(1);
  Polyhedron ph;
  ph.insert(0 * x + 0 * y /= 1);
  ph.insert(1 ^ 0 * x + y);
  ph.insert(1 | x + 0 * y);
  \endcode
  In this last case, we can note an important thing: even if this 
  polyhedron has no real vertex, we must add one, because otherwise
  the polyhedron is considered empty.

  \par Example 4
  The following code shows the use of the function
  <CODE>add_dimensions_and_embed</CODE>:
  \code
  Variable x(0);
  ConSys cs;
  cs.insert(x == 2);
  Polyhedron ph(cs);
  ph.add_dimensions_and_embed(1);
  \endcode
  The starting polyhedron is a point whose abscissa is equal to \f$2\f$
  in \f$\mathbb{R}\f$. The resulting polyhedron in \f$\mathbb{R}^2\f$ 
  is a line parallel to the axis \f$y\f$ and its intersection with the
  axis \f$x\f$ is the point with the abscissa equal to \f$2\f$.
  
  \par Example 5
  The following code shows the use of the function
  <CODE>add_dimensions_and_project</CODE>:
  \code
  Variable x(0);
  ConSys cs;
  cs.insert(x == 2);
  Polyhedron ph(cs);
  ph.add_dimensions_and_poject(1);
  \endcode
  The starting polyhedron is the same of the example for the function
  <CODE>add_dimensions_and_embed</CODE>. The resulting polyhedron
  is a point with the abscissa equal to \f$2\f$ and the ordinate
  equal to \f$0\f$.

  \par Example 6
  The following code shows the use of the function
  <CODE>assign_variable</CODE>:
  \code
  Variable x(0);
  Variable y(1);
  GenSys gs;
  gs.insert(0 * x + 0 * y /= 1);
  gs.insert(0 * x + 3 * y /= 1);
  gs.insert(3 * x + 0 * y /= 1);
  gs.insert(3 * x + 3 * y /= 1);
  Polyhedron ph(gs); 
  Integer d = 1;
  LinExpression coeff = x + 0*y + 4;
  ph.assign_variable(x, coeff, d);
  \endcode
  In this example the starting polyhedron is a square in \f$\mathbb{R}^2\f$, 
  \p var is the variable \f$x\f$, the affine_expression is \f$x+4\f$,
  the resulting polyhedron is the same square translated towards right.
  Moreover, if the affine transformation for the same variable is \f$x+y\f$
  \code
  Integer d = 1;
  LinExpression coeff = x + y;
  \endcode
  the resulting polyhedron is a parallelogram with the height equal to
  the side of the square and the oblique sides parallel to the line 
  \f$x-y\f$.
  Instead, if we do not use an invertible transformation for the same
  variable, for example \f$y\f$:
  \code
  Integer d = 1;
  LinExpression coeff = 0*x + y;
  the resulting polyhedron is the diagonal of the square.
  
  \par Example 7
  The following code shows the use of the function
  <CODE>substitue_variable</CODE>:
  \code
  Variable x(0);
  Variable y(1);
  ConSys cs;
  cs.insert(x >= 0);
  cs.insert(x <= 3);
  cs.insert(y >= 0);
  cs.insert(y <= 3);
  Polyhedron ph(cs);
  Integer d = 1;
  LinExpression coeff = x + 0*y + 4;
  ph.substitute_variable(x, coeff, d);
  \endcode
  In this example the starting polyhedron, \p var and the affine 
  expression are the same of the previous example, while the resulting
  polyhedron is again the same square but it is translated towards
  left.
  Moreover, if the affine transformation for the same variable is \f$x+y\f$
  \code
  Integer d = 1;
  LinExpression coeff = x + y;
  \endcode
  the resulting polyhedron is a parallelogram with the height equal to
  the side of the square and the oblique sides parallel to the line 
  \f$x+y\f$.
  Instead, if we do not use an invertible transformation for the same
  variable, for example \f$y\f$:
  \code
  Integer d = 1;
  LinExpression coeff = 0*x + y;
  \endcode
  the resulting polyhedron is a line that corresponds to the axis \f$y\f$.
*/

class Parma_Polyhedra_Library::Polyhedron {
public:
  //! Kinds of degenerate polyhedron.
  enum Degenerate_Kind {
    //! The full polyhedron in \f$\Rset^0\f$, i.e., a singleton.
    ZERO_DIMENSIONAL,
    //! The empty polyhedron, i.e., the empty set.
    EMPTY
  };

  //! Builds the zero-dimensional, universe polyhedron, if \p kind is
  //! <CODE>ZERO_DIMENSIONAL</CODE> (the default);
  //! otherwise (i.e., if \p kind is <CODE>EMPTY</CODE>)
  //! builds an empty polyhedron.
  Polyhedron(Degenerate_Kind kind = ZERO_DIMENSIONAL);
  //! Ordinary copy-constructor.
  Polyhedron(const Polyhedron& y);
  //! Builds the universe polyhedron of dimension \p num_dimensions.
  explicit Polyhedron(size_t num_dimensions);
  //! Builds a polyhedron from a system of constraints.
  Polyhedron(ConSys& cs);
  //! Builds a polyhedron from a system of generators.
  Polyhedron(GenSys& gs);
  // Destructor
  ~Polyhedron();

  //! The assignment operator.
  Polyhedron& operator =(const Polyhedron& y);

  //! Returns the dimension of the polyhedron.
  size_t num_dimensions() const;
  //! Intersects \p *this with polyhedron \p y and
  //! assigns the result to \p *this.
  void intersection_assign(const Polyhedron& y);
  //! Assigns the convex hull of \p *this \f$\cup\f$ \p y to \p *this.
  void convex_hull_assign(const Polyhedron& y);
  //! Assigns the convex hull of \p *this \f$\cup\f$ \p y to \p *this,
  //! without minimizing the result.
  void convex_hull_assign_lazy(const Polyhedron& y);

  //! Returns the relation between the generators of \p *this 
  //! and the constraint \p con.
  GenSys_Con_Rel poly_satisfies_constraint(const Constraint& con);
  //! Tests the inclusion of a generator in a polyhedron. 
  bool includes(const Generator& gen);

  //! Computes the widening between \p *this and \p y and 
  //! assigns the result to \p *this.
  void widening_assign(const Polyhedron& y);
  //! Limits the widening between \p *this and \p y by \p constraints
  //! and assigns the result to \p *this.
  bool limited_widening_assign(const Polyhedron& y, ConSys& constraints);

  //! Returns the system of constraints.
  const ConSys& constraints() const;
  //! Returns the system of generators.
  const GenSys& generators() const;

  //! Insert a new constraints \p c into the system of constraints.
  void insert(const Constraint& c);

  //! Insert a new generator \p g into the set of generators.
  void insert(const Generator& g);

  //! Assigns an affine expression to the specified variable.
  void assign_variable(const Variable& var, 
		       const LinExpression& coefficient,
		       Integer& denominator);
  //! Substitute an affine expression to the specified variable.
  void substitute_variable(const Variable& var,
			   const LinExpression& coefficient,
			   Integer& denominator);

#ifndef NDEBUG
  //! Checks if a polyhedron is allowed.
  bool OK(bool check_satisfiable = true) const;
  // Temporary debug constructor.
  Polyhedron(size_t nblines, size_t nbrays, size_t nbeq);
#endif

private:
  //! Minimizes generators and constraints.
  void minimize() const;
  //! Updates constraints starting from generators and minimizes them.
  void update_constraints() const;
  //! Updates generators starting from constraints and minimizes them.
  bool update_generators() const;
  //! Updates \p sat_c using the updated constraints and generators.
  void update_sat_c() const;
  //! Updates \p sat_g using the updated constraints and generators.
  void update_sat_g() const;
  //! Sorts the matrix of constraints keeping \p sat_g consistent.
  void obtain_sorted_constraints();
  //! Sorts the matrix of generators keeping \p sat_c consistent.
  void obtain_sorted_generators();
  //! Sorts the matrix of constraints and makes \p sat_c consistent.
  void obtain_sorted_constraints_with_sat_c();
  //! Sorts the matrix of generators and makes \p sat_g consistent.
  void obtain_sorted_generators_with_sat_g();

public:
  // Adds new dimensions and embeds the old polyhedron in the new space. 
  void add_dimensions_and_embed(size_t add_dim);
  // Adds new dimensions to the polyhedron 
  // and does not embed it in the new space.
  void add_dimensions_and_project(size_t add_dim);
  // Removes the specified dimensions.
  void remove_dimensions(const std::vector<unsigned int>& to_be_removed);
  //! Adds given constraints to the polyhedron and compute a new polyhedron.
  bool add_constraints(ConSys& constraints_to_add);
  //! Adds given constraints to the polyhedron without minimizing. 
  void add_constraints_lazy(ConSys& constraints_to_add);
  //! Adds given generators to the existing ones.
  void add_generators(GenSys& generators_to_add);
  //! Returns <CODE>true</CODE> if and only if the polyhedron is empty.
  bool check_empty() const;
  //! Returns <CODE>true</CODE> if \p *this is a universe polyhedron.
  bool check_universe() const;

  friend bool Parma_Polyhedra_Library::operator <=(const Polyhedron& x,
						   const Polyhedron& y);

  //! Raw read operator.
  friend std::ostream&
  Parma_Polyhedra_Library::operator <<(std::ostream& s, const Polyhedron& p);

  //! Raw write operator.
  friend std::istream&
  Parma_Polyhedra_Library::operator >>(std::istream& s, Polyhedron& p);

  //! Swaps \p *this with polyhedron \p y.
  void swap(Polyhedron& y);

private:
  //! The system of constraints.
  ConSys con_sys;
  //! The system of generators.
  GenSys gen_sys;
  //! The saturation matrix having constraints on its columns.
  SatMatrix sat_c;
  //! The saturation matrix having generators on its columns.
  SatMatrix sat_g;
  //! The status flags to keep track of the polyhedron's internal state.
  Status status;

public:
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is an empty polyhedron.
  bool is_empty() const;
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is a zero-dimensional polyhedron.
  bool is_zero_dim() const;

private:
  /*! @name Private Verifiers 
    Verify if individual flags are set.
  */
  //@{
  bool constraints_are_up_to_date() const;
  bool generators_are_up_to_date() const;
  bool constraints_are_minimized() const;
  bool generators_are_minimized() const;
  bool sat_c_is_up_to_date() const;
  bool sat_g_is_up_to_date() const;
  //@}

   
  /*! @name State flag setters.
    Set only the specified flags.
  */
  //@{
  void set_zero_dim();
  void set_empty();
  void set_constraints_up_to_date();
  void set_generators_up_to_date();
  void set_constraints_minimized();
  void set_generators_minimized();
  void set_sat_c_up_to_date();
  void set_sat_g_up_to_date();
  //@}

  /*! @name State flag cleaners.
    Clear only the specified flag.
  */
  //@{
  void clear_empty();
  void clear_constraints_up_to_date();
  void clear_generators_up_to_date();
  void clear_constraints_minimized();
  void clear_generators_minimized();
  void clear_sat_c_up_to_date();
  void clear_sat_g_up_to_date();
//@}

  //! Adds new dimensions to the given matrices.
  static void add_dimensions(Matrix& mat1,
                             Matrix& mat2,
                             SatMatrix& sat1,
                             SatMatrix& sat2,
			     size_t add_dim);

  //! Performs the conversion from constraints to generators and vice versa.
  static size_t conversion(Matrix& entry,
			   size_t start,
			   Matrix& result,
			   SatMatrix& sat,
			   size_t num_lines_or_equalities);

  //! Uses Gauss' elimination method to simplify the result of 
  //! <CODE>conversion()</CODE>.
  static int simplify(Matrix& mat, SatMatrix& sat);

  //! Builds and simplifies constraints from generators (or vice versa).
  static bool minimize(bool con_to_ray, Matrix& source, Matrix& dest,
		       SatMatrix& sat);

  //! Adds given constraints and builds minimized corresponding generators
  //! or vice versa.
  static bool add_and_minimize(bool con_to_ray,
			       Matrix& source1, Matrix& dest, SatMatrix& sat,
			       const Matrix& source2);
};

namespace std {
  //! Specialize std::swap to use the faster Polyhedron::swap.
  void swap(Parma_Polyhedra_Library::Polyhedron& x,
	    Parma_Polyhedra_Library::Polyhedron& y);
}

#if !OUTLINE
#include "Polyhedron.inlines.hh"
#endif

#endif
