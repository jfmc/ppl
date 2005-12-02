/* Grid_Generator_System class declaration.
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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Grid_Generator_System_defs_hh
#define PPL_Grid_Generator_System_defs_hh 1

// FIX
#include "Grid_Generator_System.types.hh"
#include "Generator_System.defs.hh"
#include "Grid_Generator.types.hh"
#include "Polyhedron.types.hh"
//#include "Grid.types.hh"
#include <iosfwd>

namespace Parma_Polyhedra_Library {

// FIX --

namespace IO_Operators {

//! Output operator.
/*!
  \relates Parma_Polyhedra_Library::Grid_Generator_System
  Writes <CODE>false</CODE> if \p gs is empty.  Otherwise, writes on
  \p s the generators of \p gs, all in one row and separated by ", ".
*/
std::ostream& operator<<(std::ostream& s, const Grid_Generator_System& gs);

} // namespace IO_Operators

// Put it in the namespace here to declare it friend later.
/*! \relates Polyhedron */
bool operator==(const Polyhedron& x, const Polyhedron& y);

} // namespace Parma_Polyhedra_Library


namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Grid_Generator_System */
void swap(Parma_Polyhedra_Library::Grid_Generator_System& x,
	  Parma_Polyhedra_Library::Grid_Generator_System& y);

} // namespace std

//--FIX

//FIX
//! A system of grid generators.
/*!
    An object of the class Grid_Generator_System is a system of generators,
    i.e., a multiset of objects of the class Generator
    (lines, rays, points and closure points).
    When inserting generators in a system, space dimensions are automatically
    adjusted so that all the generators in the system are defined
    on the same vector space.
    A system of generators which is meant to define a non-empty
    polyhedron must include at least one point: the reason is that
    lines, rays and closure points need a supporting point
    (lines and rays only specify directions while closure points only
    specify points in the topological closure of the NNC polyhedron).

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
  Grid_Generator_System gs;
  gs.insert(line(x + 0*y));
    \endcode
    As said above, this system of generators corresponds to
    an empty polyhedron, because the line has no supporting point.
    To define a system of generators that does correspond to
    the \f$x\f$ axis, we can add the following code which
    inserts the origin of the space as a point:
    \code
  gs.insert(point(0*x + 0*y));
    \endcode
    Since space dimensions are automatically adjusted, the following
    code obtains the same effect:
    \code
  gs.insert(point(0*x));
    \endcode
    In contrast, if we had added the following code, we would have
    defined a line parallel to the \f$x\f$ axis through
    the point \f$(0, 1)^\transpose \in \Rset^2\f$.
    \code
  gs.insert(point(0*x + 1*y));
    \endcode

    \par Example 2
    The following code builds a ray having the same direction as
    the positive part of the \f$x\f$ axis in \f$\Rset^2\f$:
    \code
  Grid_Generator_System gs;
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
  gs.insert(point(0*x + 0*y));
    \endcode

    \par Example 3
    The following code builds a system of generators having four points
    and corresponding to a square in \f$\Rset^2\f$
    (the same as Example 1 for the system of constraints):
    \code
  Grid_Generator_System gs;
  gs.insert(point(0*x + 0*y));
  gs.insert(point(0*x + 3*y));
  gs.insert(point(3*x + 0*y));
  gs.insert(point(3*x + 3*y));
    \endcode

    \par Example 4
    By using closure points, we can define the \e kernel
    (i.e., the largest open set included in a given set)
    of the square defined in the previous example.
    Note that a supporting point is needed and, for that purpose,
    any inner point could be considered.
    \code
  Grid_Generator_System gs;
  gs.insert(point(x + y));
  gs.insert(closure_point(0*x + 0*y));
  gs.insert(closure_point(0*x + 3*y));
  gs.insert(closure_point(3*x + 0*y));
  gs.insert(closure_point(3*x + 3*y));
    \endcode

    \par Example 5
    The following code builds a system of generators having two points
    and a ray, corresponding to a half-strip in \f$\Rset^2\f$
    (the same as Example 2 for the system of constraints):
    \code
  Grid_Generator_System gs;
  gs.insert(point(0*x + 0*y));
  gs.insert(point(0*x + 1*y));
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
class Parma_Polyhedra_Library::Grid_Generator_System : private Generator_System {
public:
  //! Default constructor: builds an empty system of generators.
  Grid_Generator_System();

  //! Builds the singleton system containing only generator \p g.
  explicit Grid_Generator_System(const Generator& g);

  //! Builds the singleton system containing only generator \p g.
  explicit Grid_Generator_System(const Grid_Generator& g);

  //! \brief
  //! Removes all the generators from the generator system
  //! and sets its space dimension to 0.
  void clear();

  //! \brief
  //! Inserts in \p *this a copy of the generator \p g, increasing the
  //! number of space dimensions if needed.
  void insert(const Grid_Generator& g);

  // FIX this could go if grid_point... made compulsory
  //! \brief
  //! Inserts in \p *this a copy of the polyhedron generator \p g,
  //! increasing the number of space dimensions if needed.
  void insert(const Generator& g);

  //! Adds a copy of the given Grid_Generator to the system.
  void add_row(const Grid_Generator& g);

  //! An iterator over a system of grid generators
  /*!
      A const_iterator is used to provide read-only access
      to each generator contained in an object of Grid_Generator_System.

      \par Example
      The following code prints the system of generators
      of the grid <CODE>gr</CODE>:
      \code
  const Grid_Generator_System& gs = gr.generators();
  for (Grid_Generator_System::const_iterator i = gs.begin(),
         gs_end = gs.end(); i != gs_end; ++i)
    cout << *i << endl;
      \endcode
      The same effect can be obtained more concisely by using
      more features of the STL:
      \code
  const Generator_System& gs = gr.generators();
  copy(gs.begin(), gs.end(), ostream_iterator<Grid_Generator>(cout, "\n"));
      \endcode
  */
  class const_iterator : private Generator_System::const_iterator {
  public:
    //! Default constructor.
    const_iterator();

    //! Ordinary copy-constructor.
    const_iterator(const const_iterator& y);

    //! Destructor.
    ~const_iterator();

    //! Assignment operator.
    const_iterator& operator=(const const_iterator& y);

    //! Dereference operator.
    const Grid_Generator& operator*() const;

    //! Indirect member selector.
    const Grid_Generator* operator->() const;

    //! Prefix increment operator.
    const_iterator& operator++();

    //! Postfix increment operator.
    const_iterator operator++(int);

    //! \brief
    //! Returns <CODE>true</CODE> if and only if
    //! \p *this and \p y are identical.
    bool operator==(const const_iterator& y) const;

    //! \brief
    //! Returns <CODE>true</CODE> if and only if
    //! \p *this and \p y are different.
    bool operator!=(const const_iterator& y) const;

  private:
    friend class Grid_Generator_System;

    //! Copy-constructor from Generator_System::const_iterator.
    const_iterator(const Generator_System::const_iterator& y);
  };

  //! \brief
  //! Returns the const_iterator pointing to the first generator,
  //! if \p *this is not empty;
  //! otherwise, returns the past-the-end const_iterator.
  const_iterator begin() const;

  //! Returns the past-the-end const_iterator.
  const_iterator end() const;

  //! Swaps \p *this with \p y.
  void swap(Grid_Generator_System& y);

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  //! Returns the total size in bytes of the memory occupied by \p *this.
  memory_size_type total_memory_in_bytes() const;

  //! Assigns to a given variable an affine expression.
  /*!
    \param v
    Index of the column to which the affine transformation is assigned;

    \param expr
    The numerator of the affine transformation:
    \f$\sum_{i = 0}^{n - 1} a_i x_i + b\f$;

    \param denominator
    The denominator of the affine transformation;

    We want to allow affine transformations (see the Introduction) having
    any rational coefficients. Since the coefficients of the
    constraints are integers we must also provide an integer \p denominator
    that will be used as denominator of the affine transformation.
    The denominator is required to be a positive integer.

    The affine transformation assigns to each element of \p v -th
    column the follow expression:
    \f[
      \frac{\sum_{i = 0}^{n - 1} a_i x_i + b}
           {\mathrm{denominator}}.
    \f]

    \p expr is a constant parameter and unaltered by this computation.
  */
  void affine_image(dimension_type v,
		    const Linear_Expression& expr,
		    Coefficient_traits::const_reference denominator,
		    bool grid = true);

  //! Returns the number of rows of the system.
  dimension_type num_rows() const;

  //! Returns the number of rays in the system.
  dimension_type num_rays() const;

  //! \brief
  //! Returns <CODE>true</CODE> if and only if \p *this contains one
  //! or more points.
  bool has_points() const;

  //! Returns the \p k- th generator of the system.
  Grid_Generator& operator[](dimension_type k);

  //! Returns a constant reference to the \p k- th generator of the system.
  const Grid_Generator& operator[](dimension_type k) const;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! \brief
  //! Writes to \p s an ASCII representation of the internal
  //! representation of \p *this.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  void ascii_dump(std::ostream& s) const;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! \brief
  //! Writes to std::cerr an ASCII representation of the internal
  //! representation of \p *this.
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  void ascii_dump() const;

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  //! \brief
  //! Loads from \p s an ASCII representation (as produced by
  //! \ref ascii_dump) and sets \p *this accordingly.
  //! Returns <CODE>true</CODE> if successful, <CODE>false</CODE> otherwise.
  /*!
    Resizes the matrix of generators using the numbers of rows and columns
    read from \p s, then initializes the coordinates of each generator
    and its type reading the contents from \p s.
  */
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
  bool ascii_load(std::istream& s);

  //! Checks if all the invariants are satisfied.
  /*!
    Returns <CODE>true</CODE> if and only if \p *this is a valid
    Linear_System and each row in the system is a valid Grid_Generator.
  */
  bool OK() const;

private:
  friend class Parma_Polyhedra_Library::Grid;

  //! Builds an empty system of generators having the specified topology.
  explicit Grid_Generator_System(Topology topol);

  //! Sets the sortedness flag of the system to \p b.
  void set_sorted(bool b);

  //! \brief
  //! Adds \p n rows and \p m columns of zeroes to the matrix,
  //! initializing the added rows as in the universe generator system.
  /*!
    \param n
    The number of rows to be added: must be strictly positive.

    \param m
    The number of columns to be added: must be strictly positive.

    \param row_flags
    Flags for the newly added rows.

    FIX
    Turns the \f$r \times c\f$ matrix \f$M\f$ into
    the \f$(r+n) \times (c+m)\f$ matrix
    \f$\bigl({M \atop 0}{0 \atop 0}\bigr)\f$.
    The matrix is expanded avoiding reallocation whenever possible.
  */
  void add_universe_rows_and_columns(dimension_type dims);

  //! A local version of Linear_System::insert.
  void linear_system_insert(const Linear_Row& r);
};

// Grid_Generator_System.inlines.hh is not included here on purpose.

#endif // !defined(PPL_Grid_Generator_System_defs_hh)
