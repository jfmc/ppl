/* GenSys class declaration.
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

#ifndef _GenSys_defs_hh
#define _GenSys_defs_hh 1

#include "LinExpression.defs.hh"
#include "GenSys.types.hh"
#include "Matrix.defs.hh"
#include "Generator.types.hh"
#include "Constraint.types.hh"
#include <cstddef>
#include <vector>

namespace Parma_Polyhedra_Library {
  //! Describes possible relations between a system of
  //! generators and a given constraint. 
  enum GenSys_Con_Rel {
    //! No generators satisfy the given constraint.  
    NONE_SATISFIES,
    //! All generators satisfy the given constraint but they do not
    //! belong to the hyper-plane defined by it.  
    ALL_SATISFY,
    //! All generators saturate the given constraint (i.e.
    //! all generators belong to the hyper-plane defined by 
    //! the constraint).  
    ALL_SATURATE,
    //! Some generators satisfy the given constraint (i.e. one or 
    //! more generatordo not satisfy).  
    SOME_SATISFY
  };
}

//! A system of generators.
/*!
    An object of the class GenSys is a system of generators,
    i.e. a container of objects of the class Generator
    (lines, rays and vertices).
    It is necessary that inside the set of generators there is
    at least a vertex, because this indicates for a line one of its 
    points, and for a ray the point from which it starts.
    
    \par
     In all the examples it is assumed that variables
    <CODE>x</CODE> and <CODE>y</CODE> are defined as follows:
    \code
  Variable x(0);
  Variable y(1);
    \endcode

    \par Example 1
    The following code builds the axis \f$x\f$ in \f$\Rset^2\f$:
    \code
    GenSys gs;
    gs.insert(0 * x + 0 * y /= 1);
    gs.insert(1 | x + 0 * y);
    \endcode
    Instead, the following code builds a line parallel to the axis
    \f$x\f$ in \f$\Rset^2\f$: 
    \code
    GenSys gs;
    gs.insert(0 * x + y /= 1);
    gs.insert(1 | x + 0 * y);
    \endcode
    If we use the following code:
    \code
    GenSys gs;
    gs.insert(1 | x + 0 * y);
    \endcode
    this set of generators does not represent anything.

    \par Example 2
    The following code builds a ray that corresponds to the positive
    part of the axis \f$x\f$ in \f$\Rset^2\f$:
    \code
    GenSys gs;
    gs.insert(0 * x + 0 * y /= 1);
    gs.insert(1 ^ x + 0 * y);
    \endcode
    Instead,the following code builds a ray parallel to the previuos:
    \code
    GenSys gs;
    gs.insert(0 * x + y /= 1);
    gs.insert(1 ^ x + 0 * y);
    \endcode
    If we use the following code:
    \code
    GenSys gs;
    gs.insert(1 ^ x + 0 * y);
    \endcode
    this set of generators does not represent anything.


    \par Example 3
    The following code builds a square in \f$\Rset^2\f$ 
    (the same of the first example for the system of constraints):
    \code
    GenSys gs;
    gs.insert(0 * x + 0 * y /= 1);
    gs.insert(0 * x + 3 * y /= 1);
    gs.insert(3 * x + 0 * y /= 1);
    gs.insert(3 * x + 3 * y /= 1);
    \endcode

    \par Example 4
    The following code builds an half-strip in \f$\Rset^2\f$:
    (the same of the second example for the system of constraints): 
    \code 
    GenSys gs;
    gs.insert(0 * x + 0 * y /= 1);
    gs.insert(0 * x + y /= 1);
    gs.insert(1 ^ x - y);
    \endcode 
*/
class Parma_Polyhedra_Library::GenSys : public Matrix {
public:
  //! Default constructor: builds an empty system of generators.
  GenSys();
  //! Ordinary copy-constructor.
  GenSys(const GenSys& gs);
  //! Destructor.
  virtual ~GenSys();

  //! Inserts a copy of the generator \p g into \p *this.
  void insert(const Generator& g);

  /*!
      A const_iterator is used to provide read-only access
      to each generator contained in an object of GenSys.

      \par Example
      The following code prints the set of generators
      of the polyhedron <CODE>ph</CODE>:
      \code
      const GenSys gs = ph.generators();
      GenSys::const_iterator iend = gs.end();
      for (GenSys::const_iterator i = gs.begin(); i != iend; ++i)
        cout << *i << endl;
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

  //! Returns the const_iterator pointing to the first generator.
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
  
  //! Checks if the given constraint is satisfied by all generators
  //! in the system.
  GenSys_Con_Rel satisfy_constraint(const Constraint& c) const;
  
  //! Assigns to a given variable an affine expression.
  void assign_variable(size_t var,
		       const LinExpression& expr,
		       Integer& denominator);
  //! Returns the number of lines of the system.
  size_t num_lines() const;
  //! Returns the number of rays of the system.
  size_t num_rays() const;

  //! Checks if all the invariants are satisfied.
  bool OK() const;

private:
  //! Input operator.
  void get(std::istream& s);
  //! Output operator.
  void print(std::ostream& s) const;
};

#if !OUTLINE
#include "GenSys.inlines.hh"
#endif

#endif
