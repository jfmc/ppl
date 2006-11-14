/* Polyhedron Java class declaration and implementation.
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

package ppl_java;


//! A closed convex polyhedron.
/*! \ingroup PPL_Java_interface
  An object of the class Polyhedron represents a convex polyhedron
  in the vector space \f$\Rset^n\f$.
*/
public class Polyhedron extends PPL_Object {

    /*! \brief
      Returns <CODE>true</CODE> if and only if \p this is
      an empty polyhedron.
    */
    public native boolean is_empty();

    /*! \brief
      Returns <CODE>true</CODE> if and only if \p this
      is a universe polyhedron.
    */
    public native boolean is_universe();


    /*! \brief
      Returns <CODE>true</CODE> if and only if
      \p x and \p y are the same polyhedron.
    */
    public native boolean equals(Polyhedron p);

    /*! \brief
      Returns <CODE>true</CODE> if and only if \p this
      is a topologically closed subset of the vector space.
    */
    public native boolean is_topologically_closed();

    /*! \brief
      Returns <CODE>true</CODE> if and only if \p this
      is a bounded polyhedron.
    */
    public native boolean is_bounded();

    //! Returns <CODE>true</CODE> if and only if \p this and \p y are disjoint.
    /*!
      \exception RuntimeErrorException
      Thrown if \p x and \p y are topology-incompatible or
      dimension-incompatible.
    */
    public native boolean is_disjoint_from(Polyhedron y);

    /*! \brief
      Returns <CODE>true</CODE> if and only if \p expr is
      bounded from above in \p this.

      \exception RuntimeErrorException
      Thrown if \p expr and \p this are dimension-incompatible.
    */

    public native boolean bounds_from_above(Linear_Expression expr);
    /*! \brief
      Returns <CODE>true</CODE> if and only if \p expr is
      bounded from below in \p this.

      \exception RuntimeErrorException
      Thrown if \p expr and \p this are dimension-incompatible.
    */

    public native boolean bounds_from_below(Linear_Expression expr);
    //! Returns <CODE>true</CODE> if and only if \p this contains \p y.
    /*!
      \exception RuntimeErrorException
      Thrown if \p this and \p y are topology-incompatible or
      dimension-incompatible.
    */

    public native boolean contains(Polyhedron p);

    /*! \brief
      Returns <CODE>true</CODE> if and only if \p this
      strictly contains \p y.

      \exception RuntimeErrorException
      Thrown if \p this and \p y are topology-incompatible or
      dimension-incompatible.
    */
    public native boolean strictly_contains(Polyhedron p);

    /*! \brief
      Adds an object \p c that represents a constraint to the system of
      constraints of \p this (without minimizing the result).

      \exception RuntimeErrorException
      Thrown if \p this and constraint \p c are topology-incompatible
      or dimension-incompatible.
    */
    public native void add_constraint(Constraint c);

    /*! \brief
      Adds an object \p c that represents a constraint to the system of
      constraints of \p this, minimizing the result

      \return
      <CODE>false</CODE> if and only if the result is empty.

      \exception RuntimeErrorException
      Thrown if \p this and constraint \p c are topology-incompatible
      or dimension-incompatible.
    */
    public native boolean add_constraint_and_minimize(Constraint c);

    /*! \brief
      Adds an object \p g that represents a generator to the system of
      generators of \p this (without minimizing the result).

      \exception RuntimeErrorException
      Thrown if \p this and generator \p g are topology-incompatible or
      dimension-incompatible, or if \p this is an empty polyhedron and
      \p g is not a point.
    */
    public native void add_generator(Generator g);

    /*! \brief
       Adds an object \p g that represents a generator to the system of
       generators of \p this, minimizing the result.

      \return
      <CODE>false</CODE> if and only if the result is empty.


      \exception RuntimeErrorException
      Thrown if \p this and generator \p g are topology-incompatible or
      dimension-incompatible, or if \p this is an empty polyhedron and
      \p g is not a point.
    */
    public native boolean add_generator_and_minimize(Generator g);

    /*! \brief
      Adds an object \p cs that represents a constraint system to the system
      of constraints of \p this (without minimizing the result).

      \param cs
      Contains a representation of the constraints that will be added to the
      system of constraints of \p this.

      \exception RuntimeErrorException
      Thrown if \p this and \p cs are topology-incompatible or
      dimension-incompatible.
    */
    public native void add_constraints(Constraint_System cs);

    /*! \brief
       Adds an object \p cs that represents a constraint system to the system
       of constraints of \p this, minimizing the result.

      \return
      <CODE>false</CODE> if and only if the result is empty.

      \param cs
      Contains a representation of the constraints that will be added to the
      system of constraints of \p this.

      \exception RuntimeErrorException
      Thrown if \p this and \p cs are topology-incompatible or
      dimension-incompatible.
    */
    public native boolean add_constraints_and_minimize(Constraint_System cs);

    /*! \brief
      Adds a an object \p gs that represents a generator system in to the
      system of generators of \p this (without minimizing the result).

      \param gs
      Contains a representation of the generators that will be added to the
      system of generators of \p this.

      \exception RuntimeErrorException
      Thrown if \p this and \p gs are topology-incompatible or
      dimension-incompatible, or if \p this is empty and the system of
      generators \p gs is not empty, but has no points.
    */
    public native void add_generators(Generator_System gs);

   /*! \brief
      Adds a an object \p gs that represents a generator system to the system
      of generators of \p this, minimizing the result.

      \return
      <CODE>false</CODE> if and only if the result is empty.

      \param gs
      Contains a representation of the generators that will be added to the
      system of generators of \p this.

      \exception RuntimeErrorException
      Thrown if \p this and \p gs are topology-incompatible or
      dimension-incompatible, or if \p this is empty and the the system
      of generators \p gs is not empty, but has no points.
    */
    public native boolean add_generators_and_minimize(Generator_System gs);

    /*! \brief
      Assigns to \p this the intersection of \p this and \p y.
      The result is not guaranteed to be minimized.

      \exception RuntimeErrorException
      Thrown if \p this and \p y are topology-incompatible or
      dimension-incompatible.
    */
    public native void intersection_assign(Polyhedron p);

    /*! \brief
      Assigns to \p this the intersection of \p this and \p y,
      minimizing the result.

      \return
      <CODE>false</CODE> if and only if the result is empty.

      \exception RuntimeErrorException
      Thrown if \p this and \p y are topology-incompatible or
      dimension-incompatible.
    */
    public native boolean intersection_assign_and_minimize(Polyhedron p);

    /*! \brief
      Assigns to \p this the poly-hull of \p this and \p y.
      The result is not guaranteed to be minimized.

      \exception RuntimeErrorException
      Thrown if \p this and \p y are topology-incompatible or
      dimension-incompatible.
    */
    public native void poly_hull_assign(Polyhedron p);

    /*! \brief
      Assigns to \p this the poly-hull of \p this and \p y,
      minimizing the result.

      \return
      <CODE>false</CODE> if and only if the result is empty.

      \exception RuntimeErrorException
      Thrown if \p this and \p y are topology-incompatible or
      dimension-incompatible.
    */
    public native boolean poly_hull_assign_and_minimize(Polyhedron p);

    /*! \brief
      Assigns to \p this
      the \ref Convex_Polyhedral_Difference "poly-difference"
      of \p this and \p y. The result is not guaranteed to be minimized.

      \exception RuntimeErrorException
      Thrown if \p this and \p y are topology-incompatible or
      dimension-incompatible.
    */
    public native void poly_difference_assign(Polyhedron p);

    //! Same as poly_difference_assign(y).
    public native void difference_assign(Polyhedron p);

    //! Assigns to \p this its topological closure.
    public native void topological_closure_assign();

    /*! \brief
      Returns <CODE>true</CODE> if and only if \p this
      contains at least one integer point.
    */
    public native boolean contains_integer_point();

    //! Returns <CODE>true</CODE> if and only if \p this is discrete.
    public native boolean is_discrete();

    /*! \brief
      Assigns to \p this the result of computing the
      \ref Time_Elapse_Operator "time-elapse" between \p this and \p y.

      \exception RuntimeErrorException
      Thrown if \p this and \p y are topology-incompatible or
      dimension-incompatible.
    */
    public native void time_elapse_assign(Polyhedron p);

    /*! \brief
      Assigns to \p this the
      \ref Single_Update_Affine_Functions "affine image"
      of \p this under the function mapping variable \p var to the
      affine expression specified by \p expr and \p denominator.

      \param var
      The variable to which the affine expression is assigned;

      \param expr
      An object that represents the numerator of the affine expression;

      \param denominator
      The denominator of the affine expression.

      \exception RuntimeErrorException
      Thrown if \p denominator is zero or if \p expr and \p this are
      dimension-incompatible or if \p var is not a space dimension of
      \p this.

      \if Include_Implementation_Details

      When considering the generators of a polyhedron, the
      affine transformation
      \f[
      \frac{\sum_{i=0}^{n-1} a_i x_i + b}{\mathrm{denominator}}
      \f]
      is assigned to \p var where \p expr is
      \f$\sum_{i=0}^{n-1} a_i x_i + b\f$
      (\f$b\f$ is the inhomogeneous term).

      If constraints are up-to-date, it uses the specialized function
      affine_preimage() (for the system of constraints)
      and inverse transformation to reach the same result.
      To obtain the inverse transformation we use the following observation.

      Observation:
      -# The affine transformation is invertible if the coefficient
      of \p var in this transformation (i.e., \f$a_\mathrm{var}\f$)
      is different from zero.
      -# If the transformation is invertible, then we can write
      \f[
      \mathrm{denominator} * {x'}_\mathrm{var}
      = \sum_{i = 0}^{n - 1} a_i x_i + b
      = a_\mathrm{var} x_\mathrm{var}
      + \sum_{i \neq var} a_i x_i + b,
      \f]
      so that the inverse transformation is
      \f[
      a_\mathrm{var} x_\mathrm{var}
      = \mathrm{denominator} * {x'}_\mathrm{var}
      - \sum_{i \neq j} a_i x_i - b.
      \f]

      Then, if the transformation is invertible, all the entities that
      were up-to-date remain up-to-date. Otherwise only generators remain
      up-to-date.

      In other words, if \f$R\f$ is a \f$m_1 \times n\f$ matrix representing
      the rays of the polyhedron, \f$V\f$ is a \f$m_2 \times n\f$
      matrix representing the points of the polyhedron and
      \f[
      P = \bigl\{\,
      \vect{x} = (x_0, \ldots, x_{n-1})^\mathrm{T}
      \bigm|
      \vect{x} = \vect{\lambda} R + \vect{\mu} V,
      \vect{\lambda} \in \Rset^{m_1}_+,
      \vect{\mu} \in \Rset^{m_2}_+,
      \sum_{i = 0}^{m_2 - 1} \mu_i = 1
      \,\bigr\}
      \f]
      and \f$T\f$ is the affine transformation to apply to \f$P\f$, then
      the resulting polyhedron is
      \f[
      P' = \bigl\{\,
      (x_0, \ldots, T(x_0, \ldots, x_{n-1}),
      \ldots, x_{n-1})^\mathrm{T}
      \bigm|
      (x_0, \ldots, x_{n-1})^\mathrm{T} \in P
      \,\bigr\}.
      \f]

      Affine transformations are, for example:
      - translations
      - rotations
      - symmetries.
      \endif
    */
    public native void affine_image(Variable var, Linear_Expression expr,
				    Coefficient denominator);
    /*! \brief
      Assigns to \p this the
      \ref Single_Update_Affine_Functions "affine preimage"
      of \p this under the function mapping variable \p var to the
      affine expression specified by \p expr and \p denominator.

      \param var
      The variable to which the affine expression is substituted;

      \param expr
      An object that represents the numerator of the affine expression;

      \param denominator
      The denominator of the affine expression.

      \exception RuntimeErrorException
      Thrown if \p denominator is zero or if \p expr and \p this are
      dimension-incompatible or if \p var is not a space dimension of \p this.

      \if Include_Implementation_Details

      When considering constraints of a polyhedron, the affine transformation
      \f[
      \frac{\sum_{i=0}^{n-1} a_i x_i + b}{denominator},
      \f]
      is assigned to \p var where \p expr is
      \f$\sum_{i=0}^{n-1} a_i x_i + b\f$
      (\f$b\f$ is the inhomogeneous term).

      If generators are up-to-date, then the specialized function
      affine_image() is used (for the system of generators)
      and inverse transformation to reach the same result.
      To obtain the inverse transformation, we use the following observation.

      Observation:
      -# The affine transformation is invertible if the coefficient
      of \p var in this transformation (i.e. \f$a_\mathrm{var}\f$)
      is different from zero.
      -# If the transformation is invertible, then we can write
      \f[
      \mathrm{denominator} * {x'}_\mathrm{var}
      = \sum_{i = 0}^{n - 1} a_i x_i + b
      = a_\mathrm{var} x_\mathrm{var}
      + \sum_{i \neq \mathrm{var}} a_i x_i + b,
      \f],
      the inverse transformation is
      \f[
      a_\mathrm{var} x_\mathrm{var}
      = \mathrm{denominator} * {x'}_\mathrm{var}
      - \sum_{i \neq j} a_i x_i - b.
      \f].

      Then, if the transformation is invertible, all the entities that
      were up-to-date remain up-to-date. Otherwise only constraints remain
      up-to-date.

      In other words, if \f$A\f$ is a \f$m \times n\f$ matrix representing
      the constraints of the polyhedron, \f$T\f$ is the affine transformation
      to apply to \f$P\f$ and
      \f[
      P = \bigl\{\,
      \vect{x} = (x_0, \ldots, x_{n-1})^\mathrm{T}
      \bigm|
      A\vect{x} \geq \vect{0}
      \,\bigr\}.
      \f]
      The resulting polyhedron is
      \f[
      P' = \bigl\{\,
      \vect{x} = (x_0, \ldots, x_{n-1}))^\mathrm{T}
      \bigm|
      A'\vect{x} \geq \vect{0}
      \,\bigr\},
      \f]
      where \f$A'\f$ is defined as follows:
      \f[
      {a'}_{ij}
      = \begin{cases}
      a_{ij} * \mathrm{denominator} + a_{i\mathrm{var}}*\mathrm{expr}[j]
      \quad \mathrm{for } j \neq \mathrm{var}; \\
      \mathrm{expr}[\mathrm{var}] * a_{i\mathrm{var}},
      \quad \text{for } j = \mathrm{var}.
      \end{cases}
      \f]
      \endif
    */
    public native void affine_preimage(Variable var, Linear_Expression expr,
				       Coefficient denominator);

    /*! \brief
      Assigns to \p this the image of \p this with respect to the
      \ref Generalized_Affine_Relations "generalized affine relation"
      \f$\mathrm{var}' \relsym \frac{\mathrm{expr}}{\mathrm{denominator}}\f$,
      where \f$\mathord{\relsym}\f$ is the relation symbol encoded
      by \p relsym.

      \param var
      The left hand side variable of the generalized affine relation;

      \param relsym
      The relation symbol;

      \param expr
      An object that represents the numerator of the right hand side affine
      expression;

      \param denominator
      The denominator of the right hand side affine expression.

      \exception RuntimeErrorException
      Thrown if \p denominator is zero or if \p expr and \p this are
      dimension-incompatible or if \p var is not a space dimension of \p this
      or if \p this is a C_Polyhedron and \p relsym is a strict
      relation symbol.
    */
    public native void generalized_affine_image(Variable var,
						Relation_Symbol relsym,
						Linear_Expression expr,
						Coefficient denominator);

    /*! \brief
      Assigns to \p this the preimage of \p this with respect to the
      \ref Generalized_Affine_Relations "generalized affine relation"
      \f$\mathrm{var}' \relsym \frac{\mathrm{expr}}{\mathrm{denominator}}\f$,
      where \f$\mathord{\relsym}\f$ is the relation symbol encoded
      by \p relsym.

      \param var
      An object that represents the left hand side variable of the generalized
      affine relation;

      \param relsym
      The relation symbol;

      \param expr
      An object that represents the numerator of the right hand side affine
      expression;

      \param denominator
      The denominator of the right hand side affine expression.

      \exception RuntimeErrorException
      Thrown if \p denominator is zero or if \p expr and \p this are
      dimension-incompatible or if \p var is not a space dimension of \p this
      or if \p this is a C_Polyhedron and \p relsym is a strict
      relation symbol.
    */
    public native void generalized_affine_preimage(Variable var,
						   Relation_Symbol relsym,
						   Linear_Expression expr,
						   Coefficient denominator);

    /*! \brief
      Assigns to \p this the image of \p this with respect to the
      \ref Generalized_Affine_Relations "generalized affine relation"
      \f$\mathrm{lhs}' \relsym \mathrm{rhs}\f$, where
      \f$\mathord{\relsym}\f$ is the relation symbol encoded by \p relsym.

      \param lhs
      An object that represents the left hand side affine expression;

      \param relsym
      The relation symbol;

      \param rhs
      An object that represents the right hand side affine expression.

      \exception RuntimeErrorException
      Thrown if \p this is dimension-incompatible with \p lhs or \p rhs
      or if \p this is a C_Polyhedron and \p relsym is a strict
      relation symbol.
    */
    public native void generalized_affine_image(Linear_Expression lhs,
						Relation_Symbol relsym,
						Linear_Expression rhs);

    /*! \brief
      Assigns to \p this the preimage of \p this with respect to the
      \ref Generalized_Affine_Relations "generalized affine relation"
      \f$\mathrm{lhs}' \relsym \mathrm{rhs}\f$, where
      \f$\mathord{\relsym}\f$ is the relation symbol encoded by \p relsym.

      \param lhs
      An object that represents the left hand side affine expression;

      \param relsym
      The relation symbol;

      \param rhs
      An object that represents the right hand side affine expression.

      \exception RuntimeErrorException
      Thrown if \p this is dimension-incompatible with \p lhs or \p rhs
      or if \p this is a C_Polyhedron and \p relsym is a strict
      relation symbol.
    */
    public native void generalized_affine_preimage(Linear_Expression lhs,
						   Relation_Symbol relsym,
						   Linear_Expression rhs);

    /*!
      \brief
      Assigns to \p this the image of \p this with respect to the
      \ref Single_Update_Bounded_Affine_Relations "bounded affine relation"
      \f$\frac{\mathrm{lb\_expr}}{\mathrm{denominator}}
      \leq \mathrm{var}'
      \leq \frac{\mathrm{ub\_expr}}{\mathrm{denominator}}\f$.

      \param var
      The variable updated by the affine relation;

      \param lb_expr
      An object that represents the numerator of the lower bounding
      affine expression;

      \param ub_expr
      An object that represents the numerator of the upper bounding affine
      expression;

      \param denominator
      The (common) denominator for the lower and upper bounding
      affine expressions.

      \exception RuntimeErrorException
      Thrown if \p denominator is zero or if \p lb_expr (resp., \p ub_expr)
      and \p this are dimension-incompatible or if \p var is not a space
      dimension of \p this.
    */
    public native void bounded_affine_image(Variable var,
					    Linear_Expression lb_expr,
					    Linear_Expression ub_expr,
					    Coefficient denominator);
    /*!
      \brief
      Assigns to \p this the preimage of \p this with respect to the
      \ref Single_Update_Bounded_Affine_Relations "bounded affine relation"
      \f$\frac{\mathrm{lb\_expr}}{\mathrm{denominator}}
      \leq \mathrm{var}'
      \leq \frac{\mathrm{ub\_expr}}{\mathrm{denominator}}\f$.

      \param var
      The variable updated by the affine relation;

      \param lb_expr
      An object that represents the numerator of the lower bounding affine
      expression;

      \param ub_expr
      An object that represents the numerator of the upper bounding
      affine expression;

      \param denominator
      The (common) denominator for the lower and upper bounding
      affine expressions.

      \exception RuntimeErrorException
      Thrown if \p denominator is zero or if \p lb_expr (resp., \p ub_expr)
      and \p this are dimension-incompatible or if \p var is not a space
      dimension of \p this.
    */
    public native void bounded_affine_preimage(Variable var,
					       Linear_Expression lb_expr,
					       Linear_Expression ub_expr,
					       Coefficient denominator);

    //@} // Space Dimension Preserving Member Functions that May Modify [...]

    //! \name Member Functions that May Modify the Dimension of the Vector Space
    //@{

    /*! \brief
      Adds \p m new space dimensions and embeds the old polyhedron
      in the new vector space.

      \param m
      The number of dimensions to add.

      \exception std::length_error
      Thrown if adding \p m new space dimensions would cause the
      vector space to exceed dimension <CODE>max_space_dimension()</CODE>.

      The new space dimensions will be those having the highest indexes
      in the new polyhedron, which is characterized by a system
      of constraints in which the variables running through
      the new dimensions are not constrained.
      For instance, when starting from the polyhedron \f$\cP \sseq \Rset^2\f$
      and adding a third space dimension, the result will be the polyhedron
      \f[
      \bigl\{\,
      (x, y, z)^\transpose \in \Rset^3
      \bigm|
      (x, y)^\transpose \in \cP
      \,\bigr\}.
      \f]
    */
    public native void add_space_dimensions_and_embed(long m);

    /*! \brief
      Adds \p m new space dimensions to the polyhedron
      and does not embed it in the new vector space.

      \param m
      The number of space dimensions to add.

      \exception std::length_error
      Thrown if adding \p m new space dimensions would cause the
      vector space to exceed dimension <CODE>max_space_dimension()</CODE>.

      The new space dimensions will be those having the highest indexes
      in the new polyhedron, which is characterized by a system
      of constraints in which the variables running through
      the new dimensions are all constrained to be equal to 0.
      For instance, when starting from the polyhedron \f$\cP \sseq \Rset^2\f$
      and adding a third space dimension, the result will be the polyhedron
      \f[
      \bigl\{\,
      (x, y, 0)^\transpose \in \Rset^3
      \bigm|
      (x, y)^\transpose \in \cP
      \,\bigr\}.
      \f]
    */

    public native void add_space_dimensions_and_project(long m);
    /*! \brief
      Assigns to \p this the \ref Concatenating_Polyhedra "concatenation"
      of \p this and \p y, taken in this order.

      \exception RuntimeErrorException
      Thrown if \p this and \p y are topology-incompatible.

      \exception std::length_error
      Thrown if the concatenation would cause the vector space
      to exceed dimension <CODE>max_space_dimension()</CODE>.
    */
    public native void concatenate_assign(Polyhedron p);

    //! Removes all the specified dimensions from the vector space.
    /*!
      \param to_be_removed
      The set of Variable objects corresponding to the space dimensions
      to be removed.

      \exception RuntimeErrorException
      Thrown if \p this is dimension-incompatible with one of the
      Variable objects contained in \p to_be_removed.
    */
    public native void remove_space_dimensions(Variables_Set to_be_removed);

    /*! \brief
      Removes the higher dimensions of the vector space so that
      the resulting space will have dimension \p new_dimension.

      \exception RuntimeErrorException
      Thrown if \p new_dimensions is greater than the space dimension of
      \p this.
    */
    public native void remove_higher_space_dimensions(long
						      new_dimension);

    //! Creates \p m copies of the space dimension corresponding to \p var.
    /*!
      \param var
      The variable corresponding to the space dimension to be replicated;

      \param m
      The number of replicas to be created.

      \exception RuntimeErrorException
      Thrown if \p var does not correspond to a dimension of the vector space.

      \exception std::length_error
      Thrown if adding \p m new space dimensions would cause the
      vector space to exceed dimension <CODE>max_space_dimension()</CODE>.

      If \p this has space dimension \f$n\f$, with \f$n > 0\f$,
      and <CODE>var</CODE> has space dimension \f$k \leq n\f$,
      then the \f$k\f$-th space dimension is
      \ref expand_space_dimension "expanded" to \p m new space dimensions
      \f$n\f$, \f$n+1\f$, \f$\dots\f$, \f$n+m-1\f$.
    */
    public native void expand_space_dimension(Variable var, long m);

    //! Folds the space dimensions in \p to_be_folded into \p var.
    /*!
      \param to_be_folded
      The set of Variable objects corresponding to the space dimensions
      to be folded;

      \param var
      The variable corresponding to the space dimension that is the
      destination of the folding operation.

      \exception RuntimeErrorException
      Thrown if \p this is dimension-incompatible with \p var or with
      one of the Variable objects contained in \p to_be_folded.
      Also thrown if \p var is contained in \p to_be_folded.

      If \p this has space dimension \f$n\f$, with \f$n > 0\f$,
      <CODE>var</CODE> has space dimension \f$k \leq n\f$,
      \p to_be_folded is a set of variables whose maximum space dimension
      is also less than or equal to \f$n\f$, and \p var is not a member
      of \p to_be_folded, then the space dimensions corresponding to
      variables in \p to_be_folded are \ref fold_space_dimensions "folded"
      into the \f$k\f$-th space dimension.
    */
    public native void fold_space_dimensions(Variables_Set to_be_folded,
					     Variable var);

    /*! \brief
      Returns <CODE>true</CODE> if and only if \p this is not empty
      and \p expr is bounded from above in \p this, in which case
      the supremum value is computed.

      \param expr
      The linear expression to be maximized subject to \p this;

      \param sup_n
      The numerator of the supremum value;

      \param sup_d
      The denominator of the supremum value;

      \param maximum
      <CODE>true</CODE> if and only if the supremum is also the maximum value.

      \exception RuntimeErrorException
      Thrown if \p expr and \p this are dimension-incompatible.

      If \p this is empty or \p expr is not bounded from above,
      <CODE>false</CODE> is returned and \p sup_n, \p sup_d
      and \p maximum are left untouched.
    */
    public native boolean maximize(Linear_Expression expr,
				   Coefficient sup_n, Coefficient sup_d,
				   By_Reference<Boolean> maximum);

    /*! \brief
      Returns <CODE>true</CODE> if and only if \p this is not empty
      and \p expr is bounded from above in \p this, in which case
      the supremum value and a point where \p expr reaches it are computed.

      \param expr
      The linear expression to be maximized subject to \p this;

      \param sup_n
      The numerator of the supremum value;

      \param sup_d
      The denominator of the supremum value;

      \param maximum
      <CODE>true</CODE> if and only if the supremum is also the maximum value;

      \param point
      When maximization succeeds, will be assigned the point or
      closure point where \p expr reaches its supremum value.

      \exception RuntimeErrorException
      Thrown if \p expr and \p this are dimension-incompatible.

      If \p this is empty or \p expr is not bounded from above,
      <CODE>false</CODE> is returned and \p sup_n, \p sup_d, \p maximum
      and \p point are left untouched.
    */
    public native boolean maximize(Linear_Expression expr,
				   Coefficient sup_n, Coefficient sup_d,
				   By_Reference<Boolean> maximum,
				   Generator point);

    /*! \brief
      Returns <CODE>true</CODE> if and only if \p this is not empty
      and \p expr is bounded from below in \p this, in which case
      the infimum value is computed.

      \param expr
      The linear expression to be minimized subject to \p this;

      \param inf_n
      The numerator of the infimum value;

      \param inf_d
      The denominator of the infimum value;

      \param minimum
      <CODE>true</CODE> if and only if the infimum is also the minimum value.

      \exception RuntimeErrorException
      Thrown if \p expr and \p this are dimension-incompatible.

      If \p this is empty or \p expr is not bounded from below,
      <CODE>false</CODE> is returned and \p inf_n, \p inf_d
      and \p minimum are left untouched.
    */
    public native boolean minimize(Linear_Expression expr,
				   Coefficient inf_n, Coefficient inf_d,
				   By_Reference<Boolean> minimum);

    /*! \brief
      Returns <CODE>true</CODE> if and only if \p this is not empty
      and \p expr is bounded from below in \p this, in which case
      the infimum value and a point where \p expr reaches it are computed.

      \param expr
      The linear expression to be minimized subject to \p this;

      \param inf_n
      The numerator of the infimum value;

      \param inf_d
      The denominator of the infimum value;

      \param minimum
      <CODE>true</CODE> if and only if the infimum is also the minimum value;

      \param point
      When minimization succeeds, will be assigned a point or
      closure point where \p expr reaches its infimum value.

      \exception RuntimeErrorException
      Thrown if \p expr and \p this are dimension-incompatible.

      If \p this is empty or \p expr is not bounded from below,
      <CODE>false</CODE> is returned and \p inf_n, \p inf_d, \p minimum
      and \p point are left untouched.
    */
    public native boolean minimize(Linear_Expression expr,
				   Coefficient inf_n, Coefficient inf_d,
				   By_Reference<Boolean> minimum,
				   Generator point);

    /*! \brief
      Assigns to \p this the result of computing the
      \ref BHRZ03_widening "BHRZ03-widening" between \p this and \p y.

      \param y
      A polyhedron that <EM>must</EM> be contained in \p this;

      \param tp
      An optional pointer to an unsigned variable storing the number of
      available tokens (to be used when applying the
      \ref Widening_with_Tokens "widening with tokens" delay technique).

      \exception RuntimeErrorException
      Thrown if \p this and \p y are topology-incompatible or
      dimension-incompatible.
    */
    public native void BHRZ03_widening_assign(Polyhedron y, By_Reference<Integer> tp);

    /*! \brief
      Improves the result of the \ref BHRZ03_widening "BHRZ03-widening"
      computation by also enforcing those raints in \p cs that are
      satisfied by all the points of \p this.

      \param y
      A polyhedron that <EM>must</EM> be contained in \p this;

      \param cs
      The system of raints used to improve the widened polyhedron;

      \param tp
      An optional pointer to an unsigned variable storing the number of
      available tokens (to be used when applying the
      \ref Widening_with_Tokens "widening with tokens" delay technique).

      \exception RuntimeErrorException
      Thrown if \p this, \p y and \p cs are topology-incompatible or
      dimension-incompatible.
    */
    public native
	void limited_BHRZ03_extrapolation_assign(Polyhedron y,
						 Constraint_System cs,
						 By_Reference<Integer> tp);

    /*! \brief
      Improves the result of the \ref BHRZ03_widening "BHRZ03-widening"
      computation by also enforcing those constraints in \p cs that are
      satisfied by all the points of \p this, plus all the constraints
      of the form \f$\pm x \leq r\f$ and \f$\pm x < r\f$, with
      \f$r \in \Qset\f$, that are satisfied by all the points of \p this.

      \param y
      A polyhedron that <EM>must</EM> be contained in \p this;

      \param cs
      The system of constraints used to improve the widened polyhedron;

      \param tp
      An optional pointer to an unsigned variable storing the number of
      available tokens (to be used when applying the
      \ref Widening_with_Tokens "widening with tokens" delay technique).

      \exception RuntimeErrorException
      Thrown if \p this, \p y and \p cs are topology-incompatible or
      dimension-incompatible.
    */
    public native
	void bounded_BHRZ03_extrapolation_assign(Polyhedron y,
						 Constraint_System cs,
						 By_Reference<Integer> tp);
    /*! \brief
      Assigns to \p this the result of computing the
      \ref H79_widening "H79-widening" between \p this and \p y.

      \param y
      A polyhedron that <EM>must</EM> be contained in \p this;

      \param tp
      An optional pointer to an unsigned variable storing the number of
      available tokens (to be used when applying the
      \ref Widening_with_Tokens "widening with tokens" delay technique).

      \exception RuntimeErrorException
      Thrown if \p this and \p y are topology-incompatible or
      dimension-incompatible.
    */
    public native void H79_widening_assign(Polyhedron y, By_Reference<Integer> tp);

    //! Same as H79_widening_assign(y, tp).
    public native void widening_assign(Polyhedron y, By_Reference<Integer> tp );

    /*! \brief
      Improves the result of the \ref H79_widening "H79-widening"
      computation by also enforcing those constraints in \p cs that are
      satisfied by all the points of \p this.

      \param y
      A polyhedron that <EM>must</EM> be contained in \p this;

      \param cs
      The system of constraints used to improve the widened polyhedron;

      \param tp
      An optional pointer to an unsigned variable storing the number of
      available tokens (to be used when applying the
      \ref Widening_with_Tokens "widening with tokens" delay technique).

      \exception RuntimeErrorException
      Thrown if \p this, \p y and \p cs are topology-incompatible or
      dimension-incompatible.
    */
    public native void limited_H79_extrapolation_assign(Polyhedron y,
							Constraint_System cs,
							By_Reference<Integer> tp);

    /*! \brief
      Improves the result of the \ref H79_widening "H79-widening"
      computation by also enforcing those constraints in \p cs that are
      satisfied by all the points of \p this, plus all the constraints
      of the form \f$\pm x \leq r\f$ and \f$\pm x < r\f$, with
      \f$r \in \Qset\f$, that are satisfied by all the points of \p this.

      \param y
      A polyhedron that <EM>must</EM> be contained in \p this;

      \param cs
      The system of constraints used to improve the widened polyhedron;

      \param tp
      An optional pointer to an unsigned variable storing the number of
      available tokens (to be used when applying the
      \ref Widening_with_Tokens "widening with tokens" delay technique).

      \exception RuntimeErrorException
      Thrown if \p this, \p y and \p cs are topology-incompatible or
      dimension-incompatible.
    */
    public native void bounded_H79_extrapolation_assign(Polyhedron y,
							Constraint_System cs,
							By_Reference<Integer> tp);
    //! Domain compatibility method.
    public native void add_grid_generator(Grid_Generator g);

    //! Returns <CODE>true</CODE> if \p this is empty else <CODE>false</CODE>.
    public native boolean add_grid_generator_and_minimize(Grid_Generator g);

    /*! \brief
      Adds a copy of congruence \p cg to the system of congruences of \p
      this (without minimizing the result).

      \exception RuntimeErrorException
      Thrown if \p this and congruence \p cg are topology-incompatible
      or dimension-incompatible.
    */
    public native void add_congruence(Congruence cg);

    /*! \brief
      Adds to \p this constraints equivalent to the congruences in \p
      cgs (without minimizing the result).

      \param cgs
      Contains the congruences that will be added to the system of
      constraints of \p this.

      \exception RuntimeErrorException
      Thrown if \p this and \p cgs are topology-incompatible or
      dimension-incompatible.
    */
    public native void add_congruences(Congruence_System cgs);

    //! Returns the dimension of the vector space enclosing \p this.
    public native long space_dimension();

    /*! \brief
      Returns \f$0\f$, if \p this is empty; otherwise, returns the
      \ref Affine_Independence_and_Affine_Dimension "affine dimension"
      of \p this.
    */
    public native long affine_dimension();

    //! Returns the system of constraints.
    public native Constraint_System constraints();

    //! Returns the system of constraints, with no redundant constraint.
    public native Constraint_System minimized_constraints();

    //! Returns the system of generators.
    public native Generator_System generators();

    //! Returns the system of generators, with no redundant generator.
    public native Generator_System minimized_generators();

    //! Returns a system of congruences created from the constraints.
    public native Congruence_System congruences();

    /*! \brief
      Returns a system of congruences created from the minimized
      constraints.
    */
    public native Congruence_System minimized_congruences();

    //! Returns a universe system of grid generators.
    public native Grid_Generator_System grid_generators();

    //! Returns a universe system of grid generators.
    public native Grid_Generator_System minimized_grid_generators();

    /*! \brief
      Returns the relations holding between the polyhedron \p this
      and the object representing constraint \p c.

      \exception RuntimeErrorException
      Thrown if \p this and constraint \p c are dimension-incompatible.
    */
    public native Poly_Con_Relation relation_with(Constraint c);

    /*! \brief
      Returns the relations holding between the polyhedron \p this
      and the object representing the generator \p g.

      \exception RuntimeErrorException
      Thrown if \p this and generator \p g are dimension-incompatible.
    */
    public native Poly_Gen_Relation relation_with(Generator g);

}
