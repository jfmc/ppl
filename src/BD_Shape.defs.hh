/* BD_Shape class implementation (non inline functions).
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_BD_Shape_defs_hh
#define PPL_BD_Shape_defs_hh 1

#include "BD_Shape.types.hh"
#include "DB_Matrix.defs.hh"
#include "DB_Row.defs.hh"
#include "Poly_Con_Relation.defs.hh"
#include "Poly_Gen_Relation.defs.hh"
#include "Polyhedron.types.hh"
#include "globals.defs.hh"
#include <vector>
#include <cstddef>
#include <climits>
#include <iosfwd>


namespace Parma_Polyhedra_Library {

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::BD_Shape
  Writes a textual representation of \p bd on \p s:
  <CODE>false</CODE> is written if \p bd is an empty system of 
  bounded differences;
  <CODE>true</CODE> is written if \p bd is an universe system of
  bounded differences;
  a system of constraints defining \p bd is written otherwise,
  all constraints separated by ", ".
*/
template <typename T>
std::ostream& 
operator<<(std::ostream& s, const BD_Shape<T>& c);

} // namespace IO_Operators

//! \brief
//! Returns <CODE>true</CODE> if and only if
//! \p x and \p y represent the same polyhedron.
/*!
  \relates BD_Shape
  Note that \p x and \p y may be dimension-incompatible
  systems of bounded differences: in this case, the value <CODE>false</CODE> 
  is returned.
*/
template <typename T>
bool operator==(const BD_Shape<T>& x, const BD_Shape<T>& y);
  
//! \brief
//! Returns <CODE>true</CODE> if and only if
//! \p x and \p y aren't the same polyhedron.
/*!
 \relates BD_Shape
  Note that \p x and \p y may be dimension-incompatible
  systems of bounded differences: in this case, the value <CODE>true</CODE> 
  is returned.
*/
template <typename T>
bool operator!=(const BD_Shape<T>& x, const BD_Shape<T>& y);

} // namespace Parma_Polyhedra_Library

namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::BD_Shape */
template <typename T>
void swap(Parma_Polyhedra_Library::BD_Shape<T>& x,
	  Parma_Polyhedra_Library::BD_Shape<T>& y);

} // namespace std

//! \brief
//! A class representing a restricted kind of convex polyhedra called
//! <EM>bounded differences</EM> (bdiffs).
/*!
  The templatic class BD_Shape<T> allow the efficient representation of
  a restricted kind of <EM>closed</EM> convex polyhedra called
  <EM>bounded differences</EM>.  The name comes from fact that the
  closed affine half-spaces that characterize the polyhedron can be
  expressed by constraints of the form
  \f[
    ax_i - bx_j \leq c
  \f]  
  Where \f$a, b \in \{0, 1\}\f$ and \f$c\f$ belongs to some family of
  extended numbers that is provided by the template argument \p T.
  This family of extended numbers must provide representation for
   \f$ -\infty \f$, \f$ 0 \f$, \f$ +\infty \f$ and for <EM>nan</EM>, not 
  a number, since this arises as the ''result'' of undefined sums like 
  \f$ +\infty + (-\infty) \f$, and of course, must be closed with 
  respect to the operations specified below.
  
  The class T must provide the following methods:
  
  \code
    T()
  \endcode
  is the default constructor: no assumption is made on the particular
  object constructed, provided <CODE>T().OK()</CODE> gives <CODE>true</CODE>
  (see below).
  
  \code
    T(int y)
  \endcode
  constructs the object that best approximates \p y, from below, 
  if \f$ y < 0 \f$ and from above, if \f$ y > 0 \f$ .

  \code
    ~T()
  \endcode
  is the destructor.
    
  \code
    T(const T& y)
  \endcode
  is the ordinary copy constructor.
    
  \code
   static const T& plus_infinity()
  \endcode
  returns a representation for \f$ +\infty \f$.

  \code
    bool is_plus_infinity() const 
  \endcode  
  returns <CODE>true</CODE> if and only if \p *this
  represents \f$ +\infty \f$.
  
  \code
    bool is_nan() const      
  \endcode
  returns <CODE>true</CODE> if and only if \p *this
  represents the ``not a number'' value.
  
  \code
    bool is_negative() const
  \endcode
  returns <CODE>true</CODE> if and only if \p *this is negative.
  
  \code
    bool is_nonnegative() const
  \endcode
  returns <CODE>true</CODE> if and only if \p *this is non-negative.

  \code
    bool is_zero() const 
  \endcode
  returns <CODE>true</CODE> if and only if \p *this is zero.
  
  \code
    bool OK() const 
  \endcode
  returns <CODE>true</CODE> if and only if \p *this satisfied all its invariants.

  \code
    T& operator=(const T&) 
  \endcode
  is the ordinary assignment operator.
  
  \code
    void numer_denom(Coefficient& num, Coefficient& den) const
  \endcode
  sets \p num and \p den to numerator and denominator 
  of \p *this, where \p *this must be a finite value.

  \code
    std::ostream& operator<<(std::ostream& s, const T& x)
  \endcode
  writes a textual representation of \p x to \p s.
  
  \code  
    std::istream& operator>>(std::istream& s, T& x)
  \endcode
  reads a textual representation for \p x from \p s.

  \code
    bool operator==(const T& x, const T& y)
  \endcode
  returns <CODE>true</CODE> if and only if \p x is equal to \p y.
 
  \code
    bool operator!=(const T& x, const T& y)
  \endcode
  returns <CODE>true</CODE> if and only if \p x and \p y are different.
  
  \code
    bool operator>(const T& x, const T& y)
  \endcode
  returns <CODE>true</CODE> if and only if \p x is greater than \p y.  

  \code
    bool operator>=(const T& x, const T& y)
  \endcode
  returns <CODE>true</CODE> if and only if \p x is greater than
  or equal to \p y.  

  \code
    bool operator<(const T& x, const T& y)
  \endcode
  returns <CODE>true</CODE> if and only if \p x is less than \p y.

  \code
    bool operator<=(const T& x, const T& y)
  \endcode
  returns <CODE>true</CODE> if and only if \p x is less than 
  or equal to \p y.
  
  \code
    T add_round_up(const T& x, const T& y)
  \endcode
  returns an approximation from above of the sum of \p x and \p y. 
  
  \code
    T add_round_down(const T& x, const T& y)
  \endcode
  returns an approximation from below of the sum of \p x and \p y. 

  \code
    T negate_round_down(const T& x)
  \endcode
  returns an approximation from below of the opposite of \p x . 

  \code
    T negate_round_up(const T& x)
  \endcode
  returns an approximation from above of the opposite of \p x . 

  \code
  template <>
    T div_round_up<T>(const Coefficient& x, const Coefficient& y)
  \endcode
  returns an approximation from above of the division of \p x by \p y.
  
  Now we specify the approximations, applied by methods that add
  constraints to <EM>bounded differences</EM>. 
  If the constraint is in the form \f$ ax_i - bx_j \leq c \f$
  we have the following approximation:
          \f[
              ax_i - bx_j \leq c\uparrow
          \f]
  otherwise if the constraint is in the form \f$ ax_i - bx_j = c \f$
  we have the following approximations:
          \f[
            \begin{cases}
              ax_i - bx_j = c, & \text{if } c = c\uparrow
              = c\downarrow; \\
              ax_i - bx_j \geq c\downarrow \text{ and } ax_i - bx_j 
              \leq c\uparrow,  & \text{otherwise}. 
            \end{cases} 
          \f]
  with \f$a, b \in \{0, 1\}\f$, where \f$\uparrow\f$ is used to indicate
  an approximation from above and \f$\downarrow\f$ 
  is used to indicate an approximations from below.
  Moreover the constraints actually inserted depend on the expressive 
  power of T.

  \par
  In all the examples it is assumed the class T is defined as above 
  and that variables <CODE>x</CODE>, <CODE>y</CODE> and <CODE>z</CODE>  
  are defined (where they are used) as follows:
  \code
    Variable x(0);
    Variable y(1);
    Variable z(2);
  \endcode

  \par Example 1
  The following code builds a <EM>bounded differences</EM> corresponding 
  to a cube in \f$\Rset^3\f$, given as a system of constraints:
  \code
    Constraint_System cs;
    cs.insert(x >= 0);
    cs.insert(x <= 1);
    cs.insert(y >= 0);
    cs.insert(y <= 1);
    cs.insert(z >= 0);
    cs.insert(z <= 1);
    BD_Shape<T> bd(cs);
  \endcode
  Remark that only constraints of the form of 
  <EM>bounded differences</EM> are inserted.
  The following code builds the same bdiffs as above,
  in fact the only inserted constraints must be of the form
  \f[
    ax_i + bx_j \leq c
  \f]  
  or
  \f[
    ax_i + bx_j = c
  \f]
  with \f$a, b \in \{-1, 0, 1\}\f$ and \f$c\f$ belongs to class T,
  the others are ignored (in this example the constraints 7, 8, 9
  are ignored):
  \code
    Constraint_System cs;
    cs.insert(x >= 0);
    cs.insert(x <= 1);
    cs.insert(y >= 0);
    cs.insert(y <= 1);
    cs.insert(z >= 0);
    cs.insert(z <= 1);          
    cs.insert(x + y <= 0);      // 7
    cs.insert(x - z + x >= 0);  // 8
    cs.insert(3*z - y <= 1);    // 9
    BD_Shape<T> bd(cs);
  \endcode

  \par Example 2
  The following code shows the use of the function
  <CODE>CH78_widening_assign</CODE>:
  \code
    BD_Shape<T> bd1(2);
    bd1.add_constraint(x <= 0);
    bd1.add_constraint(y >= 0);
    bd1.add_constraint(x - y <= 0);

    BD_Shape<T> bd2(2);
    bd2.add_constraint(x <= -1);
    bd2.add_constraint(y >= 0);
    bd2.add_constraint(x + y <= 0);

    bd1.CH78_widening_assign(bd2);
  \endcode
  In this example the starting bdiffs \p bd1 is the fourth quadrant
  and \p bd2 is an half-plane in \f$\Rset^2\f$. The resulting bdiffs
  is the half-plane \f$y >= 0\f$.

  \par Example 3
  The following code shows the use of the function
  <CODE>CC76_extrapolation_assign</CODE>:
  \code
    BD_Shape<T> bd1(2);
    bd1.add_constraint(x <= 0);
    bd1.add_constraint(y >= 0);
    bd1.add_constraint(x - y <= 0);

    BD_Shape<T> bd2(2);
    bd2.add_constraint(x <= -1);
    bd2.add_constraint(y >= 0);
    bd2.add_constraint(x - y <= 0);
    
    bd1.CC76_extrapolation_assign(bd2);
  \endcode
  In this example the starting bdiffs \p bd1 is the fourth quadrant
  and \p bd2 is an half-plane in \f$\Rset^2\f$. The resulting bdiffs 
  is still \p bd1.

  \par Example 4
  The following code shows the use of the function
  <CODE>CC76_narrowing_assign</CODE>:
  \code
    BD_Shape<T> bd1(2);
    BD_Shape<T> bd2(2);

    Constraint_System cs;
    cs.insert(x >= 0);
    bd2.add_constraints(cs2);

    bd1.CC76_narrowing_assign(bd2);
  \endcode
  In this example the starting bdiffs \p bd1 is universe
  and \p bd2 is non-negative half-lines. The resulting bdiffs 
  is the same \p bd2.
*/

template <typename T>
class Parma_Polyhedra_Library::BD_Shape {   
public:
  //! \brief
  //! Returns the maximum space dimension that a system 
  //! of bounded differences can handle.
  static dimension_type max_space_dimension();

  //! \brief
  //! The type upon which bounded differences are built, that is,
  //! the type of their inhomogeneous terms.
  typedef T base_type;

  //! Builds a universe or empty system of bounded differences.
  /*!
    \param num_dimensions   The number of dimensions of the vector
                            space enclosing the system of bounded differences.

    \param kind             Specifies whether the universe or the empty
                            system of bounded differences has to be built.
  */
  explicit BD_Shape(dimension_type num_dimensions = 0,
		    Polyhedron::Degenerate_Kind kind = Polyhedron::UNIVERSE);

  //! Ordinary copy-constructor.
  BD_Shape(const BD_Shape& x);

  //! Builds a BDS from the system of constraints \p cs.
  /*!
    The system of bounded differences inherits the space dimension of \p cs.
    \param cs       A system of constraints: constraints that are not in
                    \ref bounded_difference_form "bounded differences form"
                    are ignored (even though they may have contributed
                    to the space dimension).
    \exception std::invalid_argument thrown if the system of constraints \p cs
                                     contains strict inequalities.
  */
  BD_Shape(const Constraint_System& cs);

  //! Builds a BDS from the system of generators \p gs.
  /*!
    Builds the smallest BDS containing the polyhedron defined by \p gs.
    The built BDS inherits its space dimension from the space dimension
    of \p gs.
  */
  BD_Shape(const Generator_System& gs);

  //! Builds a BDS from the polyhedron \p ph.
  /*!
    Builds a BDS containing \p ph using algorithms whose complexity
    does not exceed the one specified by \p complexity.  If
    \p complexity is \p ANY_COMPLEXITY, then the BDS built is the
    smallest one containing \p ph.
  */
  BD_Shape(const Polyhedron& ph, Complexity_Class complexity = ANY_COMPLEXITY);

  //! \brief
  //! The assignment operator
  //! (\p *this and \p y can be dimension-incompatible).
  BD_Shape& operator=(const BD_Shape& y);

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this is
  //! an empty system of bounded differences.
  bool is_empty() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this
  //! is an universe system of bounded differences.
  bool is_universe() const;

  //! Returns <CODE>true</CODE> if and only if \p *this contains \p y.
  /* 
    exception std::invalid_argument thrown if \p *this and \p y
                                     are dimension-incompatible.
  */
  bool contains(const BD_Shape& y) const;

  //! \brief
  //! Returns the relations holding between the BD_Shape \p *this
  //! and the constraint \p c.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and constraint \p c are dimension-incompatible
                                       or if \p c is a strict inequality
				       or if \p c is not a bounded difference.
  */
  Poly_Con_Relation relation_with(const Constraint& c) const;

  //! \brief
  //! Returns the relations holding between the BD_Shape \p *this
  //! and the generator \p g.
  /*!
    \exception std::invalid_argument
    Thrown if \p *this and generator \p g are dimension-incompatible.
  */
  Poly_Gen_Relation relation_with(const Generator& g) const;

  //! \name Space-Dimension Preserving Member Functions that May Modify the BD_Shape
  //@{

  //! \brief
  //! Adds a copy of constraint \p c to the system of constraints
  //! of \p *this.
  /*!
    \exception std::invalid_argument thrown if \p *this and constraint \p c
                                     are dimension-incompatible,
				     or if \p c is a strict inequality.
  */
  void add_constraint(const Constraint& c);

  //! \brief
  //! Adds a copy of constraint \p c to the system of constraints
  //! of \p *this.
  /*!
    \return    <CODE>false</CODE> if and only if the result is empty.
    \exception std::invalid_argument thrown if \p *this and constraint \p c
                                     are dimension-incompatible,
				     or if \p c is a strict inequality.
  */
  bool add_constraint_and_minimize(const Constraint& c);

  //! \brief
  //! Adds the constraints in \p cs to the system of constraints
  //! of \p *this.
  /*!
    \param  cs              The constraints that will be added to the
                            current system of constraints.
    \exception std::invalid_argument thrown if \p *this and \p cs
                                     are dimension-incompatible,
				     or if there is in \p cs a
				     strict inequality.
  */
  void add_constraints(const Constraint_System& cs);

  //! \brief
  //! Adds the constraints in \p cs to the system of constraints
  //! of \p *this.
  /*!
    \return    <CODE>false</CODE> if and only if the result is empty.
    \param     cs         The constraints that will be added to the
                          current system of constraints.
    \exception std::invalid_argument thrown if \p *this and \p cs
                                     are dimension-incompatible,
				     or if there is in \p cs a
				     strict inequality.
  */
  bool add_constraints_and_minimize(const Constraint_System& cs);

  //! \brief
  //! Assigns to \p *this the intersection of \p *this and \p y.
  /*!
    \exception std::invalid_argument thrown if \p *this and \p y
                                     are dimension-incompatible.
  */
  void intersection_assign(const BD_Shape& y);

  //! \brief
  //! Assigns to \p *this the intersection of \p *this and \p y.
  /*!
    \return    <CODE>false</CODE> if and only if the result is empty.
    \exception std::invalid_argument thrown if \p *this and \p y
                                     are dimension-incompatible.
  */
  bool intersection_assign_and_minimize(const BD_Shape& y);

  //! \brief
  //! Assigns to \p *this the smallest BD_Shape that contains the convex 
  //! union of \p *this and \p y.
  /*!
    \exception std::invalid_argument thrown if \p *this and \p y
                                     are dimension-incompatible.
  */
  void poly_hull_assign(const BD_Shape& y);

  //! \brief
  //! Assigns to \p *this the smallest BD_Shape that contains the convex 
  //! union of \p *this and \p y.
  /*!
    \return    <CODE>false</CODE> if and only if the result is empty.
    \exception std::invalid_argument thrown if \p *this and \p y
                                     are dimension-incompatible.
  */
  bool poly_hull_assign_and_minimize(const BD_Shape& y);

  //! \brief
  //! Assigns to \p *this the \ref poly_difference "poly-difference" of
  //! \p *this and \p y.
  /*!
    \exception std::invalid_argument thrown if \p *this and \p y
                                     are dimension-incompatible.
  */
  void poly_difference_assign(const BD_Shape& y);

  //! \brief
  //! Assigns to \p *this the \ref affine_transformation "affine image"
  //! of \p *this under the function mapping variable \p var into the
  //! affine expression specified by \p expr and \p denominator.
  /*!
    \param var           The variable to which the affine
                         expression is assigned.
    \param expr          The numerator of the affine expression.
    \param denominator   The denominator of the affine expression
                         
    \exception std::invalid_argument thrown if \p denominator is zero
                                     or if \p expr and \p *this
                                     are dimension-incompatible
                                     or if \p var is not a dimension
                                     of \p *this or if \p expr is in
                                     two or plus variables
				     or if the value of coefficient 
				     of the single variable in \p expr 
				     is different from value of 
				     \p denominator.

    Note that, only the linear expressions of the form
    \f[
      ax_i + c
    \f]  
    with  \f$a \in \{0,\f$ denominator \f$ \}\f$ and \f$c\f$ belong to class T 
    are accepted.
  */
  void affine_image(Variable var,
		    const Linear_Expression& expr,
		    const Coefficient& denominator = Coefficient_one());

  //! \brief
  //! Assigns to \p *this the \ref affine_transformation "affine preimage"
  //! of \p *this under the function mapping variable \p var into the
  //! affine expression specified by \p expr and \p denominator.
  /*!
    \param var           The variable to which the affine expression
                         is substituted.
    \param expr          The numerator of the affine expression.
    \param denominator   The denominator of the affine expression.
    \exception std::invalid_argument thrown if \p denominator is zero
                                     or if \p expr and \p *this
                                     are dimension-incompatible
                                     or if \p var is not a dimension
                                     of \p *this or if \p expr is in 
                                     two or plus variables
				     or if the value of coefficient 
				     of the single variable in \p expr 
				     is different from value of 
				     \p denominator.
  
    Note that, only the linear expressions of the form
    \f[
      ax_i + c
    \f]  
    with  \f$a \in \{0,\f$ denominator \f$ \}\f$ and \f$c\f$ belong to class T 
    are accepted.
  */
  void affine_preimage(Variable var,
		       const Linear_Expression& expr,
		       const Coefficient& denominator = Coefficient_one());

  //! \brief
  //! Assigns to \p *this the image of \p *this with respect to the
  //! \ref generalized_image "generalized affine transfer function"
  //! \f$\mathrm{var}' \relsym \frac{\mathrm{expr}}{\mathrm{denominator}}\f$,
  //! where \f$\mathord{\relsym}\f$ is the relation symbol encoded
  //! by \p relsym.
  /*!
    \param var           The left hand side variable of
                         the generalized affine transfer function.
    \param relsym        The relation symbol.
    \param expr          The numerator of the right hand side
                         affine expression.
    \param denominator   The denominator of the right hand side affine
                         expression.
    \exception std::invalid_argument thrown if \p denominator is zero
                                     or if \p expr and \p *this
                                     are dimension-incompatible
                                     or if \p var is not a dimension
                                     of \p *this or if \p relsym is 
                                     a strict relation symbol
                                     or if \p expr is in two or plus
				     variables or if the value of 
                                     coefficient of the single variable 
                                     in \p expr is different from value of 
				     \p denominator.

    Note that, only the linear expressions of the form
    \f[
      ax_i + c
    \f]  
    with  \f$a \in \{0,\f$ denominator \f$ \}\f$ and \f$c\f$ belong to class T 
    are accepted.
  */

  void generalized_affine_image(Variable var,
				const Relation_Symbol relsym,
				const Linear_Expression& expr,
				const Coefficient& denominator = Coefficient_one());

  //! \brief
  //! Assigns to \p *this the image of \p *this with respect to the
  //! \ref generalized_image "generalized affine transfer function"
  //! \f$\mathrm{lhs}' \relsym \mathrm{rhs}\f$, where
  //! \f$\mathord{\relsym}\f$ is the relation symbol encoded by \p relsym.
  /*!
    \param lhs           The left hand side affine expression.
    \param relsym        The relation symbol.
    \param rhs           The right hand side affine expression.
    \exception std::invalid_argument thrown if \p *this is
                                     dimension-incompatible with
                                     \p lhs or \p rhs
				     or if \p relsym is a strict relation
				     symbol
                                     or if \p lhs and \p rhs are in two
                                     or plus variables, or both 
                                     are constants. 
                    
  */
  void generalized_affine_image(const Linear_Expression& lhs,
				const Relation_Symbol relsym,
				const Linear_Expression& rhs);

  //! \brief
  //! Assigns to \p *this the result of computing the
  //! \ref time_elapse "time-elapse" between \p *this and \p y.
  /*!
    \exception std::invalid_argument thrown if \p *this and \p y
                                     are dimension-incompatible.
  */
  void time_elapse_assign(const BD_Shape& y);

  //! \brief
  //! Assigns to \p *this the result of computing the 
  //! \ref CC76_extrapolation "CC76-extrapolation" between \p *this and \p y.
  //! The computation is not guarantee to be terminated.
  /*!
    \param y                 A BDS that <EM>must</EM>
                             be contained in \p *this.
    \exception std::invalid_argument thrown if \p *this and \p y
                                     are dimension-incompatible.

    \note This operator is an <EM>extrapolation</EM> and not a
          <EM>widening</EM>, since it not provide a convergence
          guarantee for fixpoint iterations.  Use
          CH78_widening_assign(const BD_Shape&) if such a guarantee is
          required.
  */
  void CC76_extrapolation_assign(const BD_Shape& y);  

  //! \brief
  //! Assigns to \p *this the result of computing the
  //! \ref CC76_widening "CC76-widening" between \p *this and \p y.
  /*!
    \param y                 A BDS that <EM>must</EM>
                             be contained in \p *this.
    \param first             An iterator that points to the first
                             stop-point.
    \param last		     An iterator that points one past the last
                             stop-point.	     
    \exception std::invalid_argument thrown if \p *this and \p y
                                            are dimension-incompatible.

    \note This operator is an <EM>extrapolation</EM> and not a
          <EM>widening</EM>, since it not provide a convergence
          guarantee for fixpoint iterations.  Use
          CH78_widening_assign(const BD_Shape&) if such a guarantee is
          required.
  */
  template <typename Iterator>
  void CC76_extrapolation_assign(const BD_Shape& y,
				 Iterator first, Iterator last);

  //! \brief
  //! Assigns to \p *this the result of computing the 
  //! \ref CH78_widening "CH78-widening"
  //! of \p *this and \p y.
  /*!
    \exception std::invalid_argument thrown if \p *this and \p y
                                     are dimension-incompatible.
  */ 
  void CH78_widening_assign(const BD_Shape& y); 
  
  //! \brief
  //! Improves the result of the \ref CH78_widening "CH78-widening"
  //! computation by also enforcing those constraints in \p cs that are
  //! satisfied by all the points of \p *this.
  /*!
    \param y                 A system of bounded differences that 
                             <EM>must</EM> be contained in \p *this.
    \param cs                The system of constraints used to improve
                             the widened system of bounded differences.
    \param tp                An optional pointer to an unsigned variable
                             storing the number of available tokens
                             (to be used when applying the
			     \ref widening_with_tokens "widening with tokens"
			     delay technique).
    \exception std::invalid_argument thrown if \p *this, \p y and \p cs
                             are dimension-incompatible or if there is 
                             in \p cs a strict inequality.
  */
  void limited_CH78_extrapolation_assign(const BD_Shape& y,
					 const Constraint_System& cs,
					 unsigned* tp = 0);
    
  //! \brief
  //! Restores from \p y, the constraints of \p *this, lost by
  //! \ref CC76_extrapolation "CC76-extrapolation" applications. 
  /*!
    \param y                 A system of bounded differences that 
                             <EM>must</EM> be contained in \p *this.
    \exception std::invalid_argument thrown if \p *this and \p y
                                     are dimension-incompatible.
  */ 
  void CC76_narrowing_assign(const BD_Shape& y);

  //! \brief
  //! Improves the result of the \ref CC76_extrapolation "CC76-extrapolation"
  //! computation by also enforcing those constraints in \p cs that are
  //! satisfied by all the points of \p *this.
  /*!
    \param y                 A system of bounded differences that 
                             <EM>must</EM> be contained in \p *this.
    \param cs                The system of constraints used to improve
                             the widened system of bounded differences.
    \param tp                An optional pointer to an unsigned variable
                             storing the number of available tokens
			     (to be used when applying the
			     \ref widening_with_tokens "widening with tokens"
			     delay technique).
    \exception std::invalid_argument thrown if \p *this, \p y and \p cs
                             are dimension-incompatible or if there is in 
                             \p cs a strict inequality.
  */
  void limited_CC76_extrapolation_assign(const BD_Shape& y,
					 const Constraint_System& cs,
					 unsigned* tp = 0);
  
  //! \brief
  //! Assigns to \p *this the result of computing the
  //! \ref H79_widening "H79-widening" between \p *this and \p y.
  /*!
    \param y           A system of bounded differences that <EM>must</EM>
                       be contained in \p *this.
    \exception std::invalid_argument thrown if \p *this and \p y
                       are dimension-incompatible.
  */
  void H79_widening_assign(const BD_Shape& y);  
 
  //! \brief
  //! Improves the result of the \ref H79_widening "H79-widening"
  //! computation by also enforcing those constraints in \p cs that are
  //! satisfied by all the points of \p *this.
  /*!
    \param y                 A system of bounded differences that <EM>must</EM>
                             be contained in \p *this.
    \param cs                The system of constraints used to improve
                             the widened system of bounded differences.
    \param tp                An optional pointer to an unsigned variable
                             storing the number of available tokens
			     (to be used when applying the
			     \ref widening_with_tokens "widening with tokens"
			     delay technique).
    \exception std::invalid_argument thrown if \p *this, \p y and \p cs
                             are dimension-incompatible.
  */
  void limited_H79_extrapolation_assign(const BD_Shape& y,
					const Constraint_System& cs,
					unsigned* tp = 0);

  //@} Space-Dimension Preserving Member Functions that May Modify [...]

  //! Returns the system of constraints.
  Constraint_System constraints() const;

  //! \name Member Functions that May Modify the Dimension of the Vector Space
  //@{

  //! \brief
  //! Adds \p m new dimensions and embeds the old system of bounded
  //! differences into the new space.
  /*!
    \param m      The number of dimensions to add.

    The new dimensions will be those having the highest indexes
    in the new system of bounded differences, which is characterized 
    by a system of constraints in which the variables running through
    the new dimensions are not constrained.
    For instance, when starting from the system of bounded differences
    \f$\cB \sseq \Rset^2\f$ and adding a third dimension, 
    the result will be the system of bounded differences
    \f[
      \bigl\{\,
        (x, y, z)^\transpose \in \Rset^3
      \bigm|
        (x, y)^\transpose \in \cB
      \,\bigr\}.
    \f]
  */
  void add_space_dimensions_and_embed(dimension_type m);

  //! \brief
  //! Adds \p m new dimensions to the system of bounded differences
  //! and does not embed it in the new space.
  /*!
    \param m      The number of dimensions to add.

    The new dimensions will be those having the highest indexes
    in the new system of bounded differences, which is characterized 
    by a system of constraints in which the variables running through
    the new dimensions are all constrained to be equal to 0.
    For instance, when starting from the system of bounded differences
    \f$\cB \sseq \Rset^2\f$ and adding a third dimension, 
    the result will be the system of bounded differences
    \f[
      \bigl\{\,
        (x, y, 0)^\transpose \in \Rset^3
      \bigm|
        (x, y)^\transpose \in \cB
      \,\bigr\}.
    \f]
  */
  void add_space_dimensions_and_project(dimension_type m);

  //! \brief
  //! Seeing a system of bounded differences as a set of tuples (its points), 
  //! assigns to \p *this all the tuples that can be obtained by concatenating,
  //! in the order given, a tuple of \p *this with a tuple of \p y.
  /*!
    Let \f$B \sseq \Rset^n\f$ and \f$D \sseq \Rset^m\f$ be the systems 
    of bounded differences represented, on entry, by \p *this and 
    \p y, respectively.
    Upon successful completion, \p *this will represent the system 
    of bounded differences
    \f$R \sseq \Rset^{n+m}\f$ such that
    \f[
      R
        \defeq
          \Bigl\{\,
            (x_1, \ldots, x_n, y_1, \ldots, y_m)^\transpose
          \Bigm|
            (x_1, \ldots, x_n)^\transpose \in B,
            (y_1, \ldots, y_m)^\transpose \in D
          \,\Bigl\}.
    \f]
    Another way of seeing it is as follows: first increases the space
    dimension of \p *this by adding \p y.space_dimension() new
    dimensions; then adds to the system of constraints of \p *this a
    renamed-apart version of the constraints of \p y.
  */
  void concatenate_assign(const BD_Shape& y);

  //! Removes all the specified dimensions.
  /*!
    \param to_be_removed  The set of Variable objects corresponding
                          to the dimensions to be removed.
    \exception std::invalid_argument thrown if \p *this is
                                     dimension-incompatible with one
				     of the Variable objects contained
				     in \p to_be_removed.
  */
  void remove_space_dimensions(const Variables_Set& to_be_removed);

  //! \brief
  //! Removes the higher dimensions so that the resulting space
  //! will have dimension \p new_dimension.
  /*!
    \exception std::invalid_argument thrown if \p new_dimensions is greater
                                     than the space dimension of \p *this.
  */
  void remove_higher_space_dimensions(dimension_type new_dimension);

  //! \brief
  //! Remaps the dimensions of the vector space
  //! according to a \ref map_space_dimensions "partial function".
  /*!
    \param pfunc   The partial function specifying
                   the destiny of each dimension.

    The template class PartialFunction must provide the following
    methods.
    \code
      bool has_empty_codomain() const
    \endcode
    returns <CODE>true</CODE> if and only if the represented partial
    function has an empty co-domain (i.e., it is always undefined).
    The <CODE>has_empty_codomain()</CODE> method will always be called
    before the methods below.  However, if
    <CODE>has_empty_codomain()</CODE> returns <CODE>true</CODE>, none
    of the functions below will be called.
    \code
      dimension_type max_in_codomain() const
    \endcode
    returns the maximum value that belongs to the co-domain
    of the partial function.
    \code
      bool maps(dimension_type i, dimension_type& j) const
    \endcode
    Let \f$f\f$ be the represented function and \f$k\f$ be the value
    of \p i.  If \f$f\f$ is defined in \f$k\f$, then \f$f(k)\f$ is
    assigned to \p j and <CODE>true</CODE> is returned.
    If \f$f\f$ is undefined in \f$k\f$, then <CODE>false</CODE> is
    returned.

    The result is undefined if \p pfunc does not encode a partial
    function with the properties described in the
    \ref map_space_dimensions "specification of the mapping operator".
  */
  template <typename PartialFunction>
  void map_space_dimensions(const PartialFunction& pfunc);

  //@} // Member Functions that May Modify the Dimension of the Vector Space

  //! \name Miscellaneous Member Functions
  //@{

  //! Destructor.
  ~BD_Shape();

  //! \brief
  //! Swaps \p *this with system of bounded differences \p y
  //! (\p *this and \p y can be dimension-incompatible).
  void swap(BD_Shape& y);

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! \brief
  //! Writes to \p s an ASCII representation of the internal
  //! representation of \p *this.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  void ascii_dump(std::ostream& s) const;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! \brief
  //! Loads from \p s an ASCII representation (as produced by \ref
  //! ascii_dump) and sets \p *this accordingly.  Returns <CODE>true</CODE>
  //! if successful, <CODE>false</CODE> otherwise.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  bool ascii_load(std::istream& s);

  //@} // Miscellaneous Member Functions

  friend bool Parma_Polyhedra_Library::operator==<T>(const BD_Shape<T>& x, 
						     const BD_Shape<T>& y);

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this satisfied all its invariants.
  bool OK() const;

private:

  //! The matrix that represents the system of bounded differences.
  DB_Matrix<T> dbm;

#define PPL_IN_BD_Shape_CLASS
#include "BDS_Status.idefs.hh"
#undef PPL_IN_BD_Shape_CLASS

  //! \brief  
  //! The status flags to keep track of the internal state,
  //! flags can be \p zero, \p zero-dim-univ or \p closed.
  Status status;

  //! \brief
  //! Returns <CODE>true</CODE> if the system of bounded differences
  //! is known to be empty.
  /*!
    The return value <CODE>false</CODE> does not necessarily
    implies that \p *this is non-empty.
  */
  bool marked_empty()const;

  //! \brief
  //! Returns <CODE>true</CODE> if the system of bounded differences
  //! is known to be closed.
  /*!
    The return value <CODE>false</CODE> does not necessarily
    implies that \p *this is non-closed.
  */
  bool marked_transitively_closed()const;

  //! Turns \p *this into an empty system of bounded differences.
  void set_empty();

  //! \brief
  //! Turns \p *this into an zero-dimensional universe 
  //! system of bounded differences.
  void set_zero_dim_univ();

 //! Adds to \p *this all implicit constraints and computes the tighter ones.
  void closure_assign() const;

  //! Remove the redundant constraints.
  void transitive_reduction_assign() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this
  //! is a reduced system of bounded differences.
  bool is_transitively_reduced() const;

  //! Initializes \p *this without constraints.
  void init();

#if !defined(__GNUC__) || __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ > 3)
  friend std::ostream&
  Parma_Polyhedra_Library::IO_Operators::operator<<<>(std::ostream& s,
						      const BD_Shape<T>& c);
#else
  // This is too lax than wanted.
  template <typename S>
  friend std::ostream&
  Parma_Polyhedra_Library::IO_Operators::operator<<(std::ostream& s,
						    const BD_Shape<S>& c);
#endif

  //! \name Exception Throwers
  //@{
  void throw_dimension_incompatible(const char* method,
				    const BD_Shape& x) const;

  void throw_dimension_incompatible(const char* method,
				    dimension_type required_dim) const;
  
  void throw_dimension_incompatible(const char* method,
				    const Constraint& c) const;

  void throw_dimension_incompatible(const char* method,
				    const Generator& g) const;

  void throw_dimension_incompatible(const char* method,
				    const char* name_row,
				    const Linear_Expression& y) const;     

  void throw_constraint_incompatible(const char* method) const;

  void throw_expression_too_complex(const char* method,
				    const Linear_Expression& e) const;

  void throw_generic(const char* method, const char* reason) const;
  //@} // Exception Throwers  
 };


#include "BDS_Status.inlines.hh"
#include "BD_Shape.inlines.hh"

#endif // !defined(PPL_BD_Shape_defs_hh)
