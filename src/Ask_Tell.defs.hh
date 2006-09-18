/* Ask_Tell class declaration.
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

#ifndef PPL_Ask_Tell_defs_hh
#define PPL_Ask_Tell_defs_hh

#include "Ask_Tell.types.hh"
// #include "Constraint_System.types.hh"
// #include "Constraint.types.hh"
// #include "Variable.defs.hh"
// #include "globals.defs.hh"
#include <iosfwd>
#include <list>

namespace Parma_Polyhedra_Library {

//! Returns <CODE>true</CODE> if and only if \p x and \p y are equivalent.
/*! \relates Ask_Tell */
template <typename D>
bool
operator==(const Ask_Tell<D>& x, const Ask_Tell<D>& y);

//! Returns <CODE>true</CODE> if and only if \p x and \p y are not equivalent.
/*! \relates Ask_Tell */
template <typename D>
bool
operator!=(const Ask_Tell<D>& x, const Ask_Tell<D>& y);

namespace IO_Operators {

//! Output operator.
/*! \relates Parma_Polyhedra_Library::Ask_Tell */
template <typename D>
std::ostream&
operator<<(std::ostream&, const Ask_Tell<D>&);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

//! A pair of <EM>ask</EM> and <EM>tell</EM> descriptions.
template <typename D>
class Parma_Polyhedra_Library::Ask_Tell_Pair {
public:
  //! Pair constructor.
  Ask_Tell_Pair(const D& ask, const D& tell);

  //! Const accessor to the <EM>ask</EM> component.
  const D& ask() const;

  //! Non-const accessor to the <EM>ask</EM> component.
  D& ask();

  //! Const accessor to the <EM>ask</EM> component.
  const D& tell() const;

  //! Non-const accessor to the <EM>tell</EM> component.
  D& tell();

  /*! \brief
    Returns <CODE>true</CODE> if \p *this definitely entails \p y.
    Returns <CODE>false</CODE> if \p *this may not entail \p y
    (i.e., if \p *this does not entail \p y or if entailment could
    not be decided).
  */
  bool definitely_entails(const Ask_Tell_Pair& y) const;

private:
  //! The <EM>ask</EM> component.
  D a;

  //! The <EM>tell</EM> component.
  D t;
};

//! The ask and tell construction on constraint systems.
/*!
  This class offers a generic implementation of <EM>ask-and-tell
  constraint systems</EM> as defined in \ref Bag98 "[Bag98]".
*/
template <typename D>
class Parma_Polyhedra_Library::Ask_Tell {
public:
  //! \name Constructors and Destructor
  //@{

  /*! \brief
    Default constructor: builds the top of the ask-and-tell constraint
    system (i.e., the empty system).
  */
  Ask_Tell();

  //! Copy constructor.
  Ask_Tell(const Ask_Tell& y);

  /*! \brief
    If \p p is not top, builds an ask-and-tell system containing only \p p.
    Builds the empty system otherwise.
  */
  explicit Ask_Tell(const Ask_Tell_Pair<D>& p);

  /*! \brief
    If \p ask and \p tell do not constitute a top pair, builds an
    ask-and-tell system containing only that pair.
    Builds the empty system otherwise.
  */
  Ask_Tell(const D& ask, const D& tell);

  //! Destructor.
  ~Ask_Tell();

  //@} // Constructors and Destructor

  //! \name Member Functions that Do Not Modify the Ask_Tell Element
  //@{

  /*! \brief
    Returns <CODE>true</CODE> if \p *this definitely entails \p y.
    Returns <CODE>false</CODE> if \p *this may not entail \p y
    (i.e., if \p *this does not entail \p y or if entailment could
    not be decided).
  */
  bool definitely_entails(const Ask_Tell& y) const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this is the top
    element of the ask-and-tell constraint system (i.e., it
    represents the universe).
  */
  bool is_top() const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this is the bottom
    element of the ask-and-tell constraint system (i.e., it
    represents the empty set).
  */
  bool is_bottom() const;

  /*! \brief
    Returns a lower bound to the total size in bytes of the memory
    occupied by \p *this.
  */
  memory_size_type total_memory_in_bytes() const;

  /*! \brief
    Returns a lower bound to the size in bytes of the memory
    managed by \p *this.
  */
  memory_size_type external_memory_in_bytes() const;

  //! Checks if all the invariants are satisfied.
  bool OK() const;

  //@} // Member Functions that Do Not Modify the Ask_Tell Element

  //! \name Member Functions that May Modify the Ask_Tell Element
  //@{

  /*! \brief
    The assignment operator.
    (\p *this and \p y can be dimension-incompatible.)
  */
  Ask_Tell& operator=(const Ask_Tell& y);

  //! Swaps \p *this with \p y.
  void swap(Ask_Tell& y);

  //! Assigns to \p *this an upper bound of \p *this and \p y.
  void upper_bound_assign(const Ask_Tell& y);

  //! Assigns to \p *this the meet of \p *this and \p y.
  void meet_assign(const Ask_Tell& y);

  //@} // Member Functions that May Modify the Ask_Tell element

  Ask_Tell& bottom();

  //! Assigns to \p *this the concatenation of \p *this and \p y.
  /*!
    Seeing an ask-and-tell agent as a set of tuples, this method
    assigns to \p *this all the tuples that can be obtained by
    concatenating, in the order given, a tuple of \p *this with
    a tuple of \p y.
  */
  void concatenate_assign(const Ask_Tell& y);


  template <typename Partial_Function>
  void map_space_dimensions(const Partial_Function& pfunc);

private:
  //! An ask-tell agent is implemented as a sequence of ask-tell pairs
  /*!
    The particular sequence employed must support efficient deletion
    in any position and efficient back insertion.
  */
  typedef std::list<Ask_Tell_Pair<D> > Sequence;

  //! The sequence container holding powerset's elements.
  Sequence sequence;

public:
  typedef typename Sequence::size_type size_type;

  size_type size() const;

  typedef typename Sequence::iterator iterator;
  typedef typename Sequence::const_iterator const_iterator;
  typedef typename Sequence::reverse_iterator reverse_iterator;
  typedef typename Sequence::const_reverse_iterator const_reverse_iterator;
  typedef typename Sequence::value_type value_type;

  iterator begin();
  const_iterator begin() const;

  iterator end();
  const_iterator end() const;

  reverse_iterator rbegin();
  const_reverse_iterator rbegin() const;

  reverse_iterator rend();
  const_reverse_iterator rend() const;

  //! \name Member Functions for the Direct Manipulation of Pairs
  //@{

  //! Adds to \p *this the pair constituted by \p ask and \p tell.
  Ask_Tell& add_pair(const D& ask, const D& tell);

  /*! \brief
    Drops the pair in \p *this pointed to by \p position, returning
    an iterator to the pair following \p position.
  */
  iterator drop_pair(iterator position);

  //! Drops all the pairs from \p first to \p last (excluded).
  void drop_pairs(iterator first, iterator last);

  //! Drops all the pairs, making \p *this an empty powerset.
  void clear();

  //@} // Member Functions for the Direct Manipulation of Pairs

//protected:
  iterator erase(iterator first, iterator last) {
    return sequence.erase(first, last);
  }
  iterator erase(iterator position) {
    return sequence.erase(position);
  }

protected:
  void pair_insert(const D& a, const D& t);
  void pair_insert_good(const D& a, const D& t);

  bool reduce();
  bool deduce();
  bool absorb();
  void engine();

  bool probe(const D& tellv, const D& askv) const;
};


namespace std {

//! Specializes <CODE>std::swap</CODE>.
/*! \relates Parma_Polyhedra_Library::Ask_Tell */
template <typename D>
void swap(Parma_Polyhedra_Library::Ask_Tell<D>& x,
	  Parma_Polyhedra_Library::Ask_Tell<D>& y);

} // namespace std

#include "Ask_Tell.inlines.hh"
#include "Ask_Tell.templates.hh"

#endif // !defined(PPL_Ask_Tell_defs_hh)
