/* Linear_Row class declaration.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2011 BUGSENG srl (http://bugseng.com)

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

#ifndef PPL_Linear_Row_defs_hh
#define PPL_Linear_Row_defs_hh 1

#include "Linear_Row.types.hh"
#include "globals.defs.hh"
#include "Linear_Expression.defs.hh"
#include "Constraint.types.hh"
#include "Generator.types.hh"

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! The base class for linear expressions, constraints and generators.
/*! \ingroup PPL_CXX_interface
  The class Linear_Row allows us to build objects of the form
  \f$[b, a_0, \ldots, a_{d-1}]\f$,
  i.e., a finite sequence of coefficients.
  The flag \f$t \in \{ \mathrm{c}, \mathrm{nnc} \}\f$ represents
  the <EM>topology</EM> and
  the flag \f$k \in \{\mathord{=}, \mathord{\geq} \}\f$ represents
  the <EM>kind</EM> of the Linear_Row object.
  Note that, even though all the four possible combinations of topology
  and kind values will result in a legal Linear_Row::Flags object, some
  of these pose additional constraints on the values of the Linear_Row's
  coefficients.

  When \f$t = c\f$, we have the following cases
  (\f$d\f$ is the dimension of the vector space):
    - \f$[b, a_0, \ldots, a_{d-1}]_{(c,=)}\f$
      represents the equality constraint
      \f$\sum_{i=0}^{d-1} a_i x_i + b = 0\f$.
    - \f$[b, a_0, \ldots, a_{d-1}]_{(c,\geq)}\f$
      represents the non-strict inequality constraint
      \f$\sum_{i=0}^{d-1} a_i x_i + b \geq 0\f$.
    - \f$[0, a_0, \ldots, a_{d-1}]_{(c,=)}\f$
      represents the line of direction
      \f$\vect{l} = (a_0, \ldots, a_{d-1})^\transpose\f$.
    - \f$[0, a_0, \ldots, a_{d-1}]_{(c,\geq)}\f$
      represents the ray of direction
      \f$\vect{r} = (a_0, \ldots, a_{d-1})^\transpose\f$.
    - \f$[b, a_0, \ldots, a_{d-1}]_{(c,\geq)}\f$, with \f$b > 0\f$,
      represents the point
      \f$\vect{p} = (\frac{a_0}{b}, \ldots, \frac{a_{d-1}}{b})^\transpose\f$.

  When \f$t = \mathrm{nnc}\f$, the last coefficient of the Linear_Row is
  associated to the slack variable \f$\epsilon\f$, so that we have the
  following cases (\f$d\f$ is again the dimension of the vector space,
  but this time we have \f$d+2\f$ coefficients):
    - \f$[b, a_0, \ldots, a_{d-1}, 0]_{(\mathrm{nnc},=)}\f$
      represents the equality constraint
      \f$\sum_{i=0}^{d-1} a_i x_i + b = 0\f$.
    - \f$[b, a_0, \ldots, a_{d-1}, 0]_{(\mathrm{nnc},\geq)}\f$
      represents the non-strict inequality constraint
      \f$\sum_{i=0}^{d-1} a_i x_i + b \geq 0\f$.
    - \f$[b, a_0, \ldots, a_{d-1}, e]_{(\mathrm{nnc},\geq)}\f$,
      with \f$e < 0\f$, represents the strict inequality constraint
      \f$\sum_{i=0}^{d-1} a_i x_i + b > 0\f$.
    - \f$[0, a_0, \ldots, a_{d-1}, 0]_{(\mathrm{nnc},=)}\f$
      represents the line of direction
      \f$\vect{l} = (a_0, \ldots, a_{d-1})^\transpose\f$.
    - \f$[0, a_0, \ldots, a_{d-1}, 0]_{(\mathrm{nnc},\geq)}\f$
      represents the ray of direction
      \f$\vect{r} = (a_0, \ldots, a_{d-1})^\transpose\f$.
    - \f$[b, a_0, \ldots, a_{d-1}, e]_{(\mathrm{nnc},\geq)}\f$,
      with \f$b > 0\f$ and \f$e > 0\f$, represents the point
      \f$\vect{p} = (\frac{a_0}{b}, \ldots, \frac{a_{d-1}}{b})^\transpose\f$.
    - \f$[b, a_0, \ldots, a_{d-1}, 0]_{(\mathrm{nnc},\geq)}\f$,
      with \f$b > 0\f$, represents the closure point
      \f$\vect{c} = (\frac{a_0}{b}, \ldots, \frac{a_{d-1}}{b})^\transpose\f$.

  So, a Linear_Row can be both a constraint and a generator: it can be an
  equality, a strict or non-strict inequality, a line, a ray, a point
  or a closure point.

  The inhomogeneous term of a constraint can be zero or different from zero.

  Points and closure points must have a positive inhomogeneous term
  (which is used as a common divisor for all the other coefficients),
  lines and rays must have the inhomogeneous term equal to zero.
  If needed, the coefficients of points and closure points are negated
  at creation time so that they satisfy this invariant.
  The invariant is maintained because, when combining a point or closure
  point with another generator, we only consider positive combinations.

  The \f$\epsilon\f$ coefficient, when present, is negative for strict
  inequality constraints, positive for points and equal to zero in all
  the other cases.
  Note that the above description corresponds to the end-user, high-level
  view of a Linear_Row object. In the implementation, to allow for code reuse,
  it is sometimes useful to regard an \f$\mathrm{nnc}\f$-object on
  the vector space \f$\Rset^d\f$ as if it was a \f$\mathrm{c}\f$-object
  on the vector space \f$\Rset^{d+1}\f$, therefore interpreting the slack
  variable \f$\epsilon\f$ as an ordinary dimension of the vector space.

  A Linear_Row object implementing a Linear_Expression is always of the form
  \f$[0, a_0, \ldots, a_{d-1}]_{(c,=)}\f$, which represents the
  linear expression \f$\sum_{i=0}^{d-1} a_i x_i\f$.
*/
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)

class Parma_Polyhedra_Library::Linear_Row : public Linear_Expression {
public:
  //! Pre-constructs a row: construction must be completed by construct().
  Linear_Row();

  //! Tight constructor: resizing will require reallocation.
  Linear_Row(dimension_type sz);

  //! Sizing constructor with capacity.
  Linear_Row(dimension_type sz, dimension_type capacity);

  //! Ordinary copy constructor.
  Linear_Row(const Linear_Row& y);

  //! Copy constructor with specified size.
  /*!
    It is assumed that \p size is greater than or equal to \p y size.
  */
  Linear_Row(const Linear_Row& y, dimension_type size);

  //! Copy constructor with specified size and capacity.
  /*!
    It is assumed that \p sz is greater than or equal to the size of \p y
    and, of course, that \p sz is less than or equal to \p capacity.
  */
  Linear_Row(const Linear_Row& y, dimension_type sz, dimension_type capacity);

  //! Destructor.
  ~Linear_Row();
};

#include "Linear_Row.inlines.hh"

#endif // !defined(PPL_Linear_Row_defs_hh)
