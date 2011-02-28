/* Constraint_System class declaration.
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

#ifndef PPL_Constraint_System_defs_hh
#define PPL_Constraint_System_defs_hh 1

#include "Constraint_System.types.hh"

#include "Linear_System.defs.hh"
#include "Constraint.defs.hh"

#include "Linear_Expression.types.hh"
#include "Generator.types.hh"
#include "Constraint.types.hh"
#include "Congruence_System.types.hh"
#include <iterator>
#include <iosfwd>

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

//! Output operator.
/*!
  \relates Parma_Polyhedra_Library::Constraint_System
  Writes <CODE>true</CODE> if \p cs is empty.  Otherwise, writes on
  \p s the constraints of \p cs, all in one row and separated by ", ".
*/
std::ostream& operator<<(std::ostream& s, const Constraint_System& cs);

} // namespace IO_Operators

// TODO: Consider removing this.
#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \p x and \p y are identical.
/*! \relates Constraint_System */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
bool operator==(const Constraint_System& x, const Constraint_System& y);

// TODO: Consider removing this.
#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Returns <CODE>true</CODE> if and only if \p x and \p y are different.
/*! \relates Constraint_System */
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
bool operator!=(const Constraint_System& x, const Constraint_System& y);

} // namespace Parma_Polyhedra_Library


namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Constraint_System */
void swap(Parma_Polyhedra_Library::Constraint_System& x,
	  Parma_Polyhedra_Library::Constraint_System& y);

} // namespace std

//! A system of constraints.
/*! \ingroup PPL_CXX_interface
    An object of the class Constraint_System is a system of constraints,
    i.e., a multiset of objects of the class Constraint.
    When inserting constraints in a system, space dimensions are
    automatically adjusted so that all the constraints in the system
    are defined on the same vector space.

    \par
    In all the examples it is assumed that variables
    <CODE>x</CODE> and <CODE>y</CODE> are defined as follows:
    \code
  Variable x(0);
  Variable y(1);
    \endcode

    \par Example 1
    The following code builds a system of constraints corresponding to
    a square in \f$\Rset^2\f$:
    \code
  Constraint_System cs;
  cs.insert(x >= 0);
  cs.insert(x <= 3);
  cs.insert(y >= 0);
  cs.insert(y <= 3);
    \endcode
    Note that:
    the constraint system is created with space dimension zero;
    the first and third constraint insertions increase the space
    dimension to \f$1\f$ and \f$2\f$, respectively.

    \par Example 2
    By adding four strict inequalities to the constraint system
    of the previous example, we can remove just the four
    vertices from the square defined above.
    \code
  cs.insert(x + y > 0);
  cs.insert(x + y < 6);
  cs.insert(x - y < 3);
  cs.insert(y - x < 3);
    \endcode

    \par Example 3
    The following code builds a system of constraints corresponding to
    a half-strip in \f$\Rset^2\f$:
    \code
  Constraint_System cs;
  cs.insert(x >= 0);
  cs.insert(x - y <= 0);
  cs.insert(x - y + 1 >= 0);
    \endcode

    \note
    After inserting a multiset of constraints in a constraint system,
    there are no guarantees that an <EM>exact</EM> copy of them
    can be retrieved:
    in general, only an <EM>equivalent</EM> constraint system
    will be available, where original constraints may have been
    reordered, removed (if they are trivial, duplicate or
    implied by other constraints), linearly combined, etc.
*/
class Parma_Polyhedra_Library::Constraint_System {
public:
  typedef Constraint row_type;

  //! Default constructor: builds an empty system of constraints.
  Constraint_System();

  //! Builds the singleton system containing only constraint \p c.
  explicit Constraint_System(const Constraint& c);

  //! Builds a system containing copies of any equalities in \p cgs.
  explicit Constraint_System(const Congruence_System& cgs);

  //! Ordinary copy constructor.
  Constraint_System(const Constraint_System& cs);

  //! Builds an empty system of constraints having the specified topology.
  explicit Constraint_System(Topology topol);

  //! Destructor.
  ~Constraint_System();

  //! Assignment operator.
  Constraint_System& operator=(const Constraint_System& y);

  //! Returns the maximum space dimension a Constraint_System can handle.
  static dimension_type max_space_dimension();

  //! Returns the dimension of the vector space enclosing \p *this.
  dimension_type space_dimension() const;

  //! Sets the space dimension of the rows in the system to \p space_dim .
  void set_space_dimension(dimension_type space_dim);

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this
    contains one or more equality constraints.
  */
  bool has_equalities() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this
    contains one or more strict inequality constraints.
  */
  bool has_strict_inequalities() const;

  // TODO: Consider making this private.
  //! Returns the number of equality constraints.
  dimension_type num_equalities() const;

  // TODO: Consider making this private.
  //! Returns the number of inequality constraints.
  dimension_type num_inequalities() const;

  // TODO: Consider making this private.
  /*! \brief
    Applies Gaussian elimination and back-substitution so as
    to provide a partial simplification of the system of constraints.

    It is assumed that the system has no pending constraints.
  */
  void simplify();

  // TODO: Consider making this private.
  /*! \brief
    Adjusts \p *this so that it matches the topology and
    the number of space dimensions given as parameters
    (adding or removing columns if needed).
    Returns <CODE>false</CODE> if and only if \p topol is
    equal to <CODE>NECESSARILY_CLOSED</CODE> and \p *this
    contains strict inequalities.
  */
  bool adjust_topology_and_space_dimension(Topology topol,
                                           dimension_type num_dimensions);

  // TODO: Consider making this private.
  //! Returns a constant reference to the \p k- th constraint of the system.
  const Constraint& operator[](dimension_type k) const;

  // TODO: Consider making this private.
  //! Returns <CODE>true</CODE> if \p g satisfies all the constraints.
  bool satisfies_all_constraints(const Generator& g) const;

  // TODO: Consider making this private.
  //! Substitutes a given column of coefficients by a given affine expression.
  /*!
    \param v
    Index of the column to which the affine transformation is substituted.

    \param expr
    The numerator of the affine transformation:
    \f$\sum_{i = 0}^{n - 1} a_i x_i + b\f$;

    \param denominator
    The denominator of the affine transformation.

    We want to allow affine transformations
    (see Section \ref Images_and_Preimages_of_Affine_Transfer_Relations)
    having any rational coefficients. Since the coefficients of the
    constraints are integers we must also provide an integer \p
    denominator that will be used as denominator of the affine
    transformation.
    The denominator is required to be a positive integer.

    The affine transformation substitutes the matrix of constraints
    by a new matrix whose elements \f${a'}_{ij}\f$ are built from
    the old one \f$a_{ij}\f$ as follows:
    \f[
      {a'}_{ij} =
        \begin{cases}
          a_{ij} * \mathrm{denominator} + a_{iv} * \mathrm{expr}[j]
            \quad \text{for } j \neq v; \\
          \mathrm{expr}[v] * a_{iv}
            \quad \text{for } j = v.
        \end{cases}
    \f]

    \p expr is a constant parameter and unaltered by this computation.
  */
  void affine_preimage(dimension_type v,
                       const Linear_Expression& expr,
                       Coefficient_traits::const_reference denominator);

  // TODO: Consider making this private.
  /*! \brief
    Inserts in \p *this a copy of the constraint \p c,
    increasing the number of space dimensions if needed.
    It is a pending constraint.
  */
  void insert_pending(const Constraint& c);

  // TODO: Consider making this private.
  //! Adds low-level constraints to the constraint system.
  void add_low_level_constraints();

  /*! \brief
    Removes all the constraints from the constraint system
    and sets its space dimension to 0.
  */
  void clear();

  /*! \brief
    Inserts in \p *this a copy of the constraint \p c,
    increasing the number of space dimensions if needed.
  */
  void insert(const Constraint& c);

  // TODO: Consider removing this, or making it private.
  //! Returns the system topology.
  Topology topology() const;

  // TODO: Consider removing this, or making it private.
  dimension_type num_rows() const;

  // TODO: Consider removing this, or making it private.
  //! Swaps the i-th and the j-th rows.
  void swap_rows(dimension_type i, dimension_type j);

  // TODO: Consider removing this, or making it private.
  /*! \brief
    Returns <CODE>true</CODE> if and only if
    the system topology is <CODE>NECESSARILY_CLOSED</CODE>.
  */
  bool is_necessarily_closed() const;

  // TODO: Consider removing this, or making it private.
  //! Returns the number of rows that are in the pending part of the system.
  dimension_type num_pending_rows() const;

  // TODO: Consider removing this, or making it private.
  //! Returns the index of the first pending row.
  dimension_type first_pending_row() const;

  // TODO: Consider removing this, or making it private.
  //! Returns the value of the sortedness flag.
  bool is_sorted() const;

  // TODO: Check if this should be removed.
  dimension_type num_columns() const;

  // TODO: Consider removing this, or making it private.
  //! Sets the index to indicate that the system has no pending rows.
  void unset_pending_rows();

  // TODO: Consider removing this, or making it private.
  //! Sets the index of the first pending row to \p i.
  void set_index_first_pending_row(dimension_type i);

  // TODO: Consider removing this, or making it private.
  //! Sets the sortedness flag of the system to \p b.
  void set_sorted(bool b);

  // TODO: Consider removing this, or making it private.
  //! Makes the system shrink by removing its \p n trailing rows.
  void remove_trailing_rows(dimension_type n);

  // TODO: Consider making this private.
  //! Removes all the specified dimensions from the constraint system.
  /*!
    The space dimension of the variable with the highest space
    dimension in \p vars must be at most the space dimension
    of \p this.
  */
  void remove_space_dimensions(const Variables_Set& vars);

  // TODO: Consider making this private.
  //! Permutes the space dimensions of the matrix.
  /*
    \param cycle
    A vector representing a cycle of the permutation according to which the
    columns must be rearranged.

    The \p cycle vector represents a cycle of a permutation of space
    dimensions.
    For example, the permutation
    \f$ \{ x_1 \mapsto x_2, x_2 \mapsto x_3, x_3 \mapsto x_1 \}\f$ can be
    represented by the vector containing \f$ x_1, x_2, x_3 \f$.
  */
  void permute_space_dimensions(const std::vector<Variable>& cycle);

  //! Swaps the coefficients of the variables \p v1 and \p v2 .
  void swap_space_dimensions(Variable v1, Variable v2);

  // TODO: Consider removing this, or making it private.
  bool has_no_rows() const;

  // TODO: Consider removing this, or making it private.
  //! Strongly normalizes the system.
  void strong_normalize();

  // TODO: Consider removing this, or making it private.
  /*! \brief
    Sorts the non-pending rows (in growing order) and eliminates
    duplicated ones.
  */
  void sort_rows();

  // TODO: Consider removing this, or making it private.
  /*! \brief
    Adds the given row to the pending part of the system, stealing its
    contents and automatically resizing the system or the row, if needed.
  */
  void insert_pending_recycled(Constraint& r);

  // TODO: Consider removing this, or making it private.
  //! Adds the rows of `y' to the pending part of `*this', stealing them from
  //! `y'.
  void insert_pending_recycled(Constraint_System& r);

  // TODO: Consider removing this, or making it private.
  /*! \brief
    Adds \p r to the system, stealing its contents and
    automatically resizing the system or the row, if needed.
  */
  void insert_recycled(Constraint& r);

  // TODO: Consider removing this, or making it private.
  //! Adds to \p *this a the rows of `y', stealing them from `y'.
  /*!
    It is assumed that \p *this has no pending rows.
  */
  void insert_recycled(Constraint_System& r);

  // TODO: Consider removing this, or making it private.
  //! Adds a copy of the rows of `y' to the pending part of `*this'.
  void insert_pending(const Constraint_System& r);

  // TODO: Consider removing this, or making it private.
  /*! \brief
    Assigns to \p *this the result of merging its rows with
    those of \p y, obtaining a sorted system.

    Duplicated rows will occur only once in the result.
    On entry, both systems are assumed to be sorted and have
    no pending rows.
  */
  void merge_rows_assign(const Constraint_System& y);

  // TODO: Consider removing this, or making it private.
  //! Adds to \p *this a copy of  the rows of \p y.
  /*!
    It is assumed that \p *this has no pending rows.
  */
  void insert(const Constraint_System& y);

  // TODO: Consider removing this, or making it private.
  //! Sets the system topology to <CODE>NECESSARILY_CLOSED</CODE>.
  void set_necessarily_closed();

  // TODO: Consider removing this, or making it private.
  //! Sets the system topology to <CODE>NOT_NECESSARILY_CLOSED</CODE>.
  void set_not_necessarily_closed();

  // TODO: Consider removing this, or making it private.
  //! Swaps \p row with the last row and then removes that row from the
  //! system.
  void release_row(Constraint& row);

  // TODO: Consider removing this, or making it private.
  //! Swaps the vector of rows with \p v. \p v must be empty.
  void release_rows(Swapping_Vector<Constraint>& v);

  // TODO: Consider removing this, or making it private.
  //! Swaps the vector of rows with \p v. \p *this must have no rows.
  void take_ownership_of_rows(Swapping_Vector<Constraint>& v);

  // TODO: Consider removing this, or making it private.
  //! Resizes the system without worrying about the old contents.
  /*!
    \param new_n_rows
    The number of rows of the resized system;

    \param new_n_columns
    The number of columns of the resized system.

    The system is expanded to the specified dimensions avoiding
    reallocation whenever possible.
    The contents of the original system is lost.
  */
  void resize_no_copy(dimension_type new_n_rows, dimension_type new_n_columns);

  // TODO: Consider removing this, or making it private.
  //! Minimizes the subsystem of equations contained in \p *this.
  /*!
    This method works only on the equalities of the system:
    the system is required to be partially sorted, so that
    all the equalities are grouped at its top; it is assumed that
    the number of equalities is exactly \p n_lines_or_equalities.
    The method finds a minimal system for the equalities and
    returns its rank, i.e., the number of linearly independent equalities.
    The result is an upper triangular subsystem of equalities:
    for each equality, the pivot is chosen starting from
    the right-most columns.
  */
  dimension_type gauss(dimension_type n_lines_or_equalities);

  // TODO: Consider removing this, or making it private.
  /*! \brief
    Back-substitutes the coefficients to reduce
    the complexity of the system.

    Takes an upper triangular system having \p n_lines_or_equalities rows.
    For each row, starting from the one having the minimum number of
    coefficients different from zero, computes the expression of an element
    as a function of the remaining ones and then substitutes this expression
    in all the other rows.
  */
  void back_substitute(dimension_type n_lines_or_equalities);

  // TODO: Consider removing this, or making it private.
  //! Full assignment operator: pending rows are copied as pending.
  void assign_with_pending(const Constraint_System& y);

  // TODO: Consider removing this, or making it private.
  /*! \brief
    Sorts the pending rows and eliminates those that also occur
    in the non-pending part of the system.
  */
  void sort_pending_and_remove_duplicates();

  // TODO: Consider removing this, or making it private.
  /*! \brief
    Sorts the system, removing duplicates, keeping the saturation
    matrix consistent.

    \param sat
    Bit matrix with rows corresponding to the rows of \p *this.
  */
  void sort_and_remove_with_sat(Bit_Matrix& sat);

  // TODO: Consider removing this, or making it private.
  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this is sorted,
    without checking for duplicates.
  */
  bool check_sorted() const;

  // TODO: Consider removing this, or making it private.
  /*! \brief
    Returns the number of rows in the system
    that represent either lines or equalities.
  */
  dimension_type num_lines_or_equalities() const;

  // TODO: Consider removing this, or making it private.
  //! Adds \p n rows and columns to the system.
  /*!
    \param n
    The number of rows and columns to be added: must be strictly positive.

    Turns the system \f$M \in \Rset^r \times \Rset^c\f$ into
    the system \f$N \in \Rset^{r+n} \times \Rset^{c+n}\f$
    such that
    \f$N = \bigl(\genfrac{}{}{0pt}{}{0}{M}\genfrac{}{}{0pt}{}{J}{o}\bigr)\f$,
    where \f$J\f$ is the specular image
    of the \f$n \times n\f$ identity matrix.
  */
  void add_universe_rows_and_columns(dimension_type n);

  //! Initializes the class.
  static void initialize();

  //! Finalizes the class.
  static void finalize();

  /*! \brief
    Returns the singleton system containing only Constraint::zero_dim_false().
  */
  static const Constraint_System& zero_dim_empty();

  typedef Constraint_System_const_iterator const_iterator;

  //! Returns <CODE>true</CODE> if and only if \p *this has no constraints.
  bool empty() const;

  /*! \brief
    Returns the const_iterator pointing to the first constraint,
    if \p *this is not empty;
    otherwise, returns the past-the-end const_iterator.
  */
  const_iterator begin() const;

  //! Returns the past-the-end const_iterator.
  const_iterator end() const;

  // FIXME: This is useless: it always returns true.
  //! Checks if all the invariants are satisfied.
  bool OK() const;

  PPL_OUTPUT_DECLARATIONS

  /*! \brief
    Loads from \p s an ASCII representation (as produced by
    ascii_dump(std::ostream&) const) and sets \p *this accordingly.
    Returns <CODE>true</CODE> if successful, <CODE>false</CODE> otherwise.
  */
  bool ascii_load(std::istream& s);

  //! Returns the total size in bytes of the memory occupied by \p *this.
  memory_size_type total_memory_in_bytes() const;

  //! Returns the size in bytes of the memory managed by \p *this.
  memory_size_type external_memory_in_bytes() const;

  //! Swaps \p *this with \p y.
  void swap(Constraint_System& y);

private:
  Linear_System<Constraint> sys;

  /*! \brief
    Holds (between class initialization and finalization) a pointer to
    the singleton system containing only Constraint::zero_dim_false().
  */
  static const Constraint_System* zero_dim_empty_p;

  friend class Constraint_System_const_iterator;

  friend bool operator==(const Constraint_System& x,
                         const Constraint_System& y);

  /*! \brief
    Builds a system of \p n_rows constraints on a \p n_columns - 1
    dimensional space (including the \f$\epsilon\f$ dimension, if
    \p topol is <CODE>NOT_NECESSARILY_CLOSED</CODE>).
  */
  Constraint_System(Topology topol,
		    dimension_type n_rows, dimension_type n_columns);
};

//! An iterator over a system of constraints.
/*! \ingroup PPL_CXX_interface
  A const_iterator is used to provide read-only access
  to each constraint contained in a Constraint_System object.

  \par Example
  The following code prints the system of constraints
  defining the polyhedron <CODE>ph</CODE>:
  \code
const Constraint_System& cs = ph.constraints();
for (Constraint_System::const_iterator i = cs.begin(),
        cs_end = cs.end(); i != cs_end; ++i)
  cout << *i << endl;
  \endcode
*/
// NOTE: This is not an inner class of Constraint_System, so Constraint can
// declare that this class is his friend without including this file (the
// .types.hh file suffices).
class Parma_Polyhedra_Library::Constraint_System_const_iterator
  : public std::iterator<std::forward_iterator_tag,
        Constraint,
        ptrdiff_t,
        const Constraint*,
        const Constraint&> {
public:
  //! Default constructor.
  Constraint_System_const_iterator();

  //! Ordinary copy constructor.
  Constraint_System_const_iterator(const Constraint_System_const_iterator& y);

  //! Destructor.
  ~Constraint_System_const_iterator();

  //! Assignment operator.
  Constraint_System_const_iterator& operator=(const Constraint_System_const_iterator& y);

  //! Dereference operator.
  const Constraint& operator*() const;

  //! Indirect member selector.
  const Constraint* operator->() const;

  //! Prefix increment operator.
  Constraint_System_const_iterator& operator++();

  //! Postfix increment operator.
  Constraint_System_const_iterator operator++(int);

  /*! \brief
    Returns <CODE>true</CODE> if and only if
    \p *this and \p y are identical.
  */
  bool operator==(const Constraint_System_const_iterator& y) const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if
    \p *this and \p y are different.
  */
  bool operator!=(const Constraint_System_const_iterator& y) const;

private:
  friend class Constraint_System;

  //! The const iterator over the matrix of constraints.
  Linear_System<Constraint>::const_iterator i;

  //! A const pointer to the matrix of constraints.
  const Linear_System<Constraint>* csp;

  //! Constructor.
  Constraint_System_const_iterator(const Linear_System<Constraint>::const_iterator& iter,
      const Constraint_System& csys);

  //! \p *this skips to the next non-trivial constraint.
  void skip_forward();
};

// Constraint_System.inlines.hh is not included here on purpose.

#endif // !defined(PPL_Constraint_System_defs_hh)
