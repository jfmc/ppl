/* GenSys class declaration.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef _GenSys_defs_hh
#define _GenSys_defs_hh 1

#include "LinExpression.defs.hh"
#include "GenSys.types.hh"
#include "Matrix.defs.hh"
#include "Generator.types.hh"
#include "Constraint.types.hh"
#include "Poly_Con_Relation.defs.hh"
#include <cstddef>
#include <vector>

//! A system of generators.
/*!
    An object of the class GenSys is a system of generators,
    i.e., a multiset of objects of the class Generator
    (lines, rays and vertices).
    When inserting generators in a system, dimensions are automatically
    adjusted so that all the generators in the system are defined
    on the same vector space.
    A system of generators which is meant to define a non-empty polyhedron
    must include at least one vertex, even if the polyhedron has
    no ``proper'' vertices: the reason is that lines and rays need
    a supporting point (they only specify directions).

    \par
     In all the examples it is assumed that variables
    <CODE>x</CODE> and <CODE>y</CODE> are defined as follows:
    \code
  Variable x(0);
  Variable y(1);
    \endcode

    \par Example 1
    The following code defines the line having the same direction
    as the \f$x\f$ axis (i.e., the first Cartesian axis)
    in \f$\Rset^2\f$:
    \code
  GenSys gs;
  gs.insert(line(x + 0*y));
    \endcode
    As said above, this system of generators corresponds to
    an empty polyhedron, because the line has no supporting point.
    To define a system of generators indeed corresponding to
    the \f$x\f$ axis, one can add the following code which
    inserts the origin of the space as a vertex:
    \code
  gs.insert(vertex(0*x + 0*y));
    \endcode
    Since dimensions are automatically adjusted, the following
    code obtains the same effect:
    \code
  gs.insert(vertex(0*x));
    \endcode
    In contrast, if we had added the following code, we would have
    defined a line parallel to the \f$x\f$ axis and including
    the point \f$(0, 1)^\transpose \in \Rset^2\f$.
    \code
  gs.insert(vertex(0*x + 1*y));
    \endcode

    \par Example 2
    The following code builds a ray having the same direction as
    the positive part of the \f$x\f$ axis in \f$\Rset^2\f$:
    \code
  GenSys gs;
  gs.insert(ray(x + 0*y));
    \endcode
    To define a system of generators indeed corresponding to the set
    \f[
      \bigl\{\,
        (x, 0)^\transpose \in \Rset^2
      \bigm|
        x \geq 0
      \,\bigr\},
    \f]
    one just has to add the origin:
    \code
  gs.insert(vertex(0*x + 0*y));
    \endcode

    \par Example 3
    The following code builds a system of generators having four vertices
    and corresponding to a square in \f$\Rset^2\f$
    (the same as Example 1 for the system of constraints):
    \code
  GenSys gs;
  gs.insert(vertex(0*x + 0*y));
  gs.insert(vertex(0*x + 3*y));
  gs.insert(vertex(3*x + 0*y));
  gs.insert(vertex(3*x + 3*y));
    \endcode

    \par Example 4
    The following code builds a system of generators having two vertices
    and a ray, corresponding to a half-strip in \f$\Rset^2\f$
    (the same as Example 2 for the system of constraints):
    \code
  GenSys gs;
  gs.insert(vertex(0*x + 0*y));
  gs.insert(vertex(0*x + 1*y));
  gs.insert(ray(x - y));
    \endcode

    \note
    After inserting a multiset of generators in a generator system,
    there are no guarantees that an <EM>exact</EM> copy of them
    can be retrieved:
    in general, only an <EM>equivalent</EM> generator system
    will be available, where original generators may have been
    reordered, removed (if they are duplicate or redundant), etc.
*/
class Parma_Polyhedra_Library::GenSys : PPL_HIDDEN Matrix {
public:
  //! Default constructor: builds an empty system of generators.
  GenSys();
  //! Builds the singleton system containing only generator \p g.
  GenSys(const Generator& g);
  //! Ordinary copy-constructor.
  GenSys(const GenSys& gs);
  //! Destructor.
  virtual ~GenSys();

  //! Assignment operator.
  GenSys& operator =(const GenSys& y);

  //! Returns the dimension of the vector space enclosing \p *this.
  size_t space_dimension() const;

  //! Inserts a copy of the generator \p g into \p *this,
  //! increasing the number of dimensions if needed.
  void insert(const Generator& g);

  //! Returns the singleton system containing only
  //! Generator::zero_dim_vertex().
  static const GenSys& zero_dim_univ();

  /*!
      A const_iterator is used to provide read-only access
      to each generator contained in an object of GenSys.

      \par Example
      The following code prints the system of generators
      of the polyhedron <CODE>ph</CODE>:
      \code
  const GenSys gs = ph.generators();
  GenSys::const_iterator iend = gs.end();
  for (GenSys::const_iterator i = gs.begin(); i != iend; ++i)
    cout << *i << endl;
      \endcode
      The same effect can be obtained more concisely by using
      more features of the STL:
      \code
  const GenSys gs = ph.generators();
  copy(gs.begin(), gs.end(), ostream_iterator<Generator>(cout, "\n"));
      \endcode
  */
  class const_iterator
    : public std::iterator<std::forward_iterator_tag,
				Generator,
				void,
				const Generator*,
                                const Generator&> {

  private:
    Matrix::const_iterator i;

    //! Copy-constructor.
    const_iterator(const Matrix::const_iterator& iter);

    friend class GenSys;

  public:
    //! Default constructor.
    const_iterator();
    //! Ordinary copy-constructor.
    const_iterator(const const_iterator& y);
    //! Destructor.
    virtual ~const_iterator();
    //! Assignment operator.
    const_iterator& operator =(const const_iterator& y);
    //! Dereference operator.
    const Generator& operator *() const;
    //! Indirect member selector.
    const Generator* operator ->() const;
    //! Prefix increment operator.
    const_iterator& operator ++();
    //! Postfix increment operator.
    const_iterator operator ++(int);
    //! Returns <CODE>true</CODE> if and only if
    //! \p *this and \p y are identical.
    bool operator ==(const const_iterator& y) const;
    //! Returns <CODE>true</CODE> if and only if
    //! \p *this and \p y are different.
    bool operator !=(const const_iterator& y) const;
  };

  //! Returns the const_iterator pointing to the first generator,
  //! if \p *this is not empty;
  //! otherwise, returns the past-the-end const_iterator.
  const_iterator begin() const;
  //! Returns the past-the-end const_iterator.
  const_iterator end() const;

PPL_INTERNAL:
  //! Constructor: it builds a system of \p num_rows rays/vertices
  //! on a \p num_columns - 1 dimensional space.
  GenSys(size_t num_rows, size_t num_columns);

  //! Returns the \p k- th generator of the system.
  Generator& operator [](size_t k);
  //! Returns a constant reference to the \p k- th generator of the system.
  const Generator& operator [](size_t k) const;

  //! Returns the relation holding between the generator system
  //! and the constraint \p c.
  Parma_Polyhedra_Library::Poly_Con_Relation
  relation_with(const Constraint& c) const;

  //! Assigns to a given variable an affine expression.
  void affine_image(size_t v,
		    const LinExpression& expr,
		    const Integer& denominator);
  //! Returns the number of lines of the system.
  size_t num_lines() const;
  //! Returns the number of rays of the system.
  size_t num_rays() const;

  //! Removes all the invalid lines and rays (i.e., those with all
  //! the homogeneous terms set to zero).
  void remove_invalid_lines_and_rays();

  //! Checks if all the invariants are satisfied.
  bool OK() const;

private:
  //! Input operator.
  void get(std::istream& s);
  //! Output operator.
  void print(std::ostream& s) const;
};

namespace std {

//! Specialize std::swap.
void swap(Parma_Polyhedra_Library::GenSys& x,
	  Parma_Polyhedra_Library::GenSys& y);

} // namespace std

#include "GenSys.inlines.hh"

#endif
