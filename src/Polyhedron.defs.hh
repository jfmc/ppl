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
#include <set>

namespace Parma_Polyhedra_Library {

  //! @name Non-friend operators on objects of the class Polyhedron.
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
  //!  \p x contains \p y.
  bool operator >=(const Polyhedron& x, const Polyhedron& y);
  //@}

  // Put them in the namespace here to declare them friend later.
  bool operator <=(const Polyhedron& x, const Polyhedron& y);
  std::ostream& operator <<(std::ostream& s, const Polyhedron& p);
  std::istream& operator >>(std::istream& s, Polyhedron& p);
}

//! A convex polyhedron.
/*!
    An object of the class Polyhedron represents a convex polyhedron
    in the space \f$\Rset^n\f$. 

    A polyhedron can be specified as either a finite system of constraints
    or a finite system of generators (see Minkowski's theorem in
    the Introduction).
    So, it is possible to obtain one system from the
    other. That is, if we know the system of constraints, we can obtain
    from this the system of generators that define the same polyhedron
    and vice versa.
    These systems can contain some redundant members: in this case we say
    that they are not in the minimal form.

    \par
    In all the examples it is assumed that variables
    <CODE>x</CODE> and <CODE>y</CODE> are defined (where they are
    used) as follows:
    \code
  Variable x(0);
  Variable y(1);
    \endcode

    \par Example 1
    The following code builds a polyhedron corresponding to
    a square in \f$\Rset^2\f$, given as a system of constraints:
    \code
  ConSys cs;
  cs.insert(x >= 0);
  cs.insert(x <= 3);
  cs.insert(y >= 0);
  cs.insert(y <= 3);
  Polyhedron ph(cs);
    \endcode
    The following code builds the same polyhedron as above,
    but starting from a system of generators specifying
    the four vertices of the square:
    \code
  GenSys gs;
  gs.insert(vertex(0*x + 0*y));
  gs.insert(vertex(0*x + 3*y));
  gs.insert(vertex(3*x + 0*y));
  gs.insert(vertex(3*x + 3*y));
  Polyhedron ph(gs);
    \endcode

    \par Example 2
    The following code builds an unbounded polyhedron
    corresponding to a half-strip in \f$\Rset^2\f$,
    given as a system of constraints:
    \code
  ConSys cs;
  cs.insert(x >= 0);
  cs.insert(x - y <= 0);
  cs.insert(x - y + 1 >= 0);
  Polyhedron ph(cs);
    \endcode
    The following code builds the same polyhedron as above,
    but starting from the system of generators specifying
    the two vertices of the polyhedron and one ray:
    \code
  GenSys gs;
  gs.insert(vertex(0*x + 0*y));
  gs.insert(vertex(0*x + y));
  gs.insert(ray(x - y));
  Polyhedron ph(gs);
    \endcode

    \par Example 3
    The following code builds the polyhedron corresponding to
    an half-plane in \f$\Rset^2\f$,
    by adding a single constraint to the universe polyhedron:
    \code
  Polyhedron ph;
  ph.insert(y >= 0);
    \endcode
    The following code builds the same polyhedron as above,
    but starting from a system of generators specifying a vertex,
    a ray and a line.
    \code
  Polyhedron ph;
  ph.insert(vertex(0*x + 0*y));
  ph.insert(ray(0*x + y));
  ph.insert(line(x + 0*y));
    \endcode
    In this last case, it is important to note that: even if this
    polyhedron has no real vertex, we must add one, because otherwise
    the polyhedron is considered empty.

    \par Example 4
    The following code shows the use of the function
    <CODE>add_dimensions_and_embed</CODE>:
    \code
  Polyhedron ph;
  ph.insert(x == 2);
  ph.add_dimensions_and_embed(1);
    \endcode
    We start with the universe polyhedron in the 0-dimensional space.
    Then we add a single equality constraint,
    thus obtaining the polyhedron corresponding to the singleton set
    \f$\{ 2 \} \sseq \Rset\f$.
    After the last line of code, the resulting polyhedron is
    \f[
      \bigl\{\,
        (2, x_1)^\transpose \in \Rset^2
      \bigm|
        x_1 \in \Rset
      \,\bigr\}.
    \f]

    \par Example 5
    The following code shows the use of the function
    <CODE>add_dimensions_and_project</CODE>:
    \code
  Polyhedron ph;
  ph.insert(x == 2);
  ph.add_dimensions_and_poject(1);
    \endcode
    The first two lines of code are the same as in Example 4 for
    <CODE>add_dimensions_and_embed</CODE>.
    After the last line of code, the resulting polyhedron is
    the singleton set
    \f$\bigl\{ (2, 0)^\transpose \bigr\} \sseq \Rset^2\f$.

    \par Example 6
    The following code shows the use of the function
    <CODE>assign_variable</CODE>:
    \code
  Polyhedron ph;
  ph.insert(vertex(0*x + 0*y));
  ph.insert(vertex(0*x + 3*y));
  ph.insert(vertex(3*x + 0*y));
  ph.insert(vertex(3*x + 3*y));
  LinExpression coeff = x + 0*y + 4;
  ph.assign_variable(x, coeff);
    \endcode
    In this example the starting polyhedron is a square in \f$\Rset^2\f$,
    the considered variable is \f$x\f$ and the affine expression is \f$x+4\f$.
    The resulting polyhedron is the same square translated towards right.
    Moreover, if the affine transformation for the same variable \p x
    is \f$x+y\f$:
    \code
  LinExpression coeff = x + y;
    \endcode
    the resulting polyhedron is a parallelogram with the height equal to
    the side of the square and the oblique sides parallel to the line
    \f$x-y\f$.
    Instead, if we do not use an invertible transformation for the same
    variable; for example, the affine expression \f$y\f$:
    \code
  LinExpression coeff = 0*x + y;
    \endcode
    the resulting polyhedron is a diagonal of the square.

    \par Example 7
    The following code shows the use of the function
    <CODE>substitute_variable</CODE>:
    \code
  Polyhedron ph;
  ph.insert(x >= 0);
  ph.insert(x <= 3);
  ph.insert(y >= 0);
  ph.insert(y <= 3);
  LinExpression coeff = x + 0*y + 4;
  ph.substitute_variable(x, coeff);
    \endcode
    In this example the starting polyhedron, \p var and the affine
    expression and the denominator are the same as in Example 6,
    while the resulting
    polyhedron is again the same square but translated towards
    left.
    Moreover, if the affine transformation for \p x is \f$x+y\f$
    \code
  LinExpression coeff = x + y;
    \endcode
    the resulting polyhedron is a parallelogram with the height equal to
    the side of the square and the oblique sides parallel to the line
    \f$x+y\f$.
    Instead, if we do not use an invertible transformation for the same
    variable \p x, for example, the affine expression \f$y\f$:
    \code
  LinExpression coeff = 0*x + y;
    \endcode
    the resulting polyhedron is a line that corresponds to the \f$y\f$ axis.
*/

class Parma_Polyhedra_Library::Polyhedron {
public:
  //! Kinds of degenerate polyhedra.
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
  //! \exception std::invalid_argument thrown if \p *this and \p y
  //!                                  have different dimension.
  void intersection_assign(const Polyhedron& y);
  //! Assigns the convex hull of \p *this \f$\cup\f$ \p y to \p *this.
  //! \exception std::invalid_argument thrown if \p *this and \p y
  //!                                  have different dimension.
  void convex_hull_assign(const Polyhedron& y);
  //! Assigns the convex hull of \p *this \f$\cup\f$ \p y to \p *this,
  //! without minimizing the result.
  //! \exception std::invalid_argument thrown if \p *this and \p y
  //!                                  have different dimension.
  void convex_hull_assign_lazy(const Polyhedron& y);

  //! Returns the relation between the generators of \p *this
  //! and the constraint \p c.
  //! \exception std::invalid_argument thrown if\p *this and constraint 
  //!                                  \p c have different dimension.
  GenSys_Con_Rel satisfies(const Constraint& c);
  //! Tests the inclusion of the generator \p g in the 
  //! polyhedron \p *this.
  //! \exception std::invalid_argument thrown if \p *this and constraint 
  //!                                  \p g have different dimension.
  bool includes(const Generator& g);

  //! Computes the widening between \p *this and \p y and
  //! assigns the result to \p *this.
  //! \param y           The polyhedron that <EM>must</EM>
  //!                    be contained in \p *this.
  //! \exception std::invalid_argument thrown if \p *this and \p y 
  //!                                  have different dimension.
  void widening_assign(const Polyhedron& y);
  //! Limits the widening between \p *this and \p y by \p cs
  //! and assigns the result to \p *this.
  //! \param y                 The polyhedron that <EM>must</EM>
  //!                          be contained in \p *this.
  //! \param cs                The system of constraints that limits
  //!                          the widened polyhedron.
  //! \return       <CODE>true</CODE> if the resulting polyhedron is not
  //!               empty <CODE>false</CODE> otherwise.
  //! \exception std::invalid_argument thrown if \p *this, \p y and 
  //!                                  \p cs have different dimension.
  bool limited_widening_assign(const Polyhedron& y, ConSys& cs);

  //! Returns the system of constraints.
  //! \exception std::invalid_argument thrown if constraints of \p *this 
  //!                                  can not be obtained (\p *this is 
  //!                                  empty or zero-dimensional).
  const ConSys& constraints() const;
  //! Returns the system of generators.
  //! \exception std::invalid_argument thrown if generators of \p *this 
  //!                                  can not be obtained (\p *this is 
  //!                                  empty or zero-dimensional).
  const GenSys& generators() const;

  //! Inserts a new constraint \p c into the system of constraints
  //! of \p *this.
  void insert(const Constraint& c);

  //! Inserts a new generator \p g into the system of generators
  //! of \p *this.
  void insert(const Generator& g);

  //! Assigns an affine expression to the specified variable.
  //! \param v             The variable to which the affine
  //!                      expression is assigned.
  //! \param expr          The numerator of the affine expression.
  //! \param denominator   The denominator of the affine expression
  //!                      (optional argument with default value 1.)
  //! \exception std::invalid_argument thrown if \p denominator is zero or
  //!                                  if \p expr and \p *this have 
  //!                                  different dimension or if
  //!                                  \p v is not a variable of the
  //!                                  polyhedron.
  void assign_variable(const Variable& v,
		       const LinExpression& expr,
		       const Integer& denominator = 1);
  //! Substitutes an affine expression for the specified variable.
  //! \param v             The variable to which the affine expression 
  //!                      is substituted.
  //! \param expr          The numerator of the affine expression.
  //! \param denominator   The denominator of the affine expression
  //!                      (optional argument with default value 1.)
  //! \exception std::invalid_argument thrown if \p denominator is zero or
  //!                                  if \p expr and \p *this have 
  //!                                  different dimension or if
  //!                                  \p v is not a variable of the
  //!                                  polyhedron.
  void substitute_variable(const Variable& v,
			   const LinExpression& expr,
			   const Integer& denominator = 1);

  //! Checks if all the invariants are satisfied.
  //! \param check_not_empty    <CODE>true</CODE> if it must be checked
  //!                           whether the system of constraint is
  //!                           satisfiable.
  //! \return       <CODE>true</CODE> if the polyhedron satisfies
  //!               all the invariants stated in the PPL,
  //!               <CODE>false</CODE> otherwise.
  bool OK(bool check_not_empty = true) const;

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
  //! Adds new dimensions and embeds the old polyhedron in the new space.
  //! \param dim      The number of dimensions to add.
  void add_dimensions_and_embed(size_t dim);
  //! Adds new dimensions to the polyhedron
  //! and does not embed it in the new space.
  //! \param dim      The number of dimensions to add.
  void add_dimensions_and_project(size_t dim);
  //! Removes the specified dimensions.
  //! \param to_be_removed The set of variables to remove.
  void remove_dimensions(const std::set<Variable>& to_be_removed);
  //! Adds the specified constraints and computes a new polyhedron.
  //! \param  cs            The constraints that will be added to the
  //!                       current system of constraints.
  //! \return               <CODE>false</CODE> if the resulting
  //!                       polyhedron is empty.
  //! \exception std::invalid_argument thrown if \p *this and \p cs
  //!                                  have different dimension.
  bool add_constraints(ConSys& cs);
  //! Adds the specified constraints without minimizing.
  //! \param  cs             The constraints that will be added to the
  //!                        current system of constraints.
  //! \exception std::invalid_argument thrown if \p *this and \p cs 
  //!                                  have different dimension.
  void add_constraints_lazy(ConSys& cs);
  //! Adds the specified generators.
  //! \param  gs          The generators that will be added to the
  //!                     current system of generators.
  //! \exception std::invalid_argument thrown if \p *this and 
  //!                                  \p gs have different dimension
  void add_generators(GenSys& gs);
  //! Returns <CODE>true</CODE> if and only if the polyhedron is empty.
  bool check_empty() const;
  //! Returns <CODE>true</CODE> if \p *this is a universe polyhedron.
  bool check_universe() const;

  //! Returns <CODE>true</CODE> if and only if
  //! polyhedron \p x is contained in polyhedron \p y.
  friend bool Parma_Polyhedra_Library::operator <=(const Polyhedron& x,
						   const Polyhedron& y);

  //! Output operator.
  friend std::ostream&
  Parma_Polyhedra_Library::operator <<(std::ostream& s, const Polyhedron& p);

  //! Input operator.
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
  //! Specialize std::swap.
  void swap(Parma_Polyhedra_Library::Polyhedron& x,
	    Parma_Polyhedra_Library::Polyhedron& y);
}

#if !OUTLINE
#include "Polyhedron.inlines.hh"
#endif

#endif
