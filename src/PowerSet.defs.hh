/* PowerSet class declaration.
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

#ifndef _PowerSet_defs_hh
#define _PowerSet_defs_hh

#include "PowerSet.types.hh"
#include "LCompare.defs.hh"
#include <iosfwd>
#include <list>

namespace Parma_Polyhedra_Library {

template <class CS>
PowerSet<CS>
operator+(const PowerSet<CS>&, const PowerSet<CS>&);

template <class CS>
PowerSet<CS>
operator*(const PowerSet<CS>&, const PowerSet<CS>&);

template <class CS>
CS
project(const PowerSet<CS>&);

template <class CS>
bool
operator==(const PowerSet<CS>&, const PowerSet<CS>&);

template <class CS>
int
lcompare(const PowerSet<CS>&, const PowerSet<CS>&);

template <class CS>
std::ostream&
operator<<(std::ostream&, const PowerSet<CS>&);

} // namespace Parma_Polyhedra_Library


//! The powerset construction on constraint systems.
template <class CS>
class Parma_Polyhedra_Library::PowerSet {
public:
  //! Creates an empty (bottom) PowerSet.
  PowerSet();

  //! Injects \p y into \p *this.
  PowerSet& inject(const CS& y);

  //! Assigns to \p *this an upper bound of \p *this and \p y.
  void upper_bound_assign(const PowerSet& y);

  //! Assigns to \p *this the meet of \p *this and \p y.
  void meet_assign(const PowerSet& y);

  //! Returns <CODE>true</CODE> if \p *this definitely entails \p y.
  //! Returns <CODE>false</CODE> if \p *this may not entail \p y
  //! (i.e., if \p *this does not entail \p y or if entailment could
  //! not be decided).
  bool definitely_entails(const PowerSet& y) const;

  inline bool is_top() const;
  inline bool is_bottom() const;

  friend CS project<>(const PowerSet& x);

  friend int lcompare<>(const PowerSet& x, const PowerSet& y);

  friend std::ostream& operator <<<>(std::ostream& s, const PowerSet& x);

private:
  typedef std::list<CS> Sequence;
  typedef typename Sequence::const_reference const_reference;

public:
  typedef typename Sequence::iterator iterator;
  typedef typename Sequence::const_iterator const_iterator;
  typedef typename Sequence::value_type value_type;

  size_t size() const;

  iterator begin();
  const_iterator begin() const;

  iterator end();
  const_iterator end() const;


  Sequence sequence;

  void omega_reduction();
};

#include "PowerSet.inlines.hh"

#endif // _PowerSet_defs_hh
