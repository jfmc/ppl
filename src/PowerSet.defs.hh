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
PowerSet<CS>
operator<<(const PowerSet<CS>&, Variable);

template <class CS>
PowerSet<CS>
hide(const PowerSet<CS>&, Variable);

template <class CS>
CS
project(const PowerSet<CS>&);

template <class CS>
bool
operator==(const PowerSet<CS>&, const PowerSet<CS>&);

template <class CS>
bool
entails(const PowerSet<CS>&, const PowerSet<CS>&);

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

  //! Assign to \p *this an upper bound of \p y and \p *this itself.
  void upper_bound_assign(const PowerSet& y);

  inline PowerSet& operator *= (const PowerSet& y);
  inline PowerSet& operator <<= (const Variable n);
  inline PowerSet& hide_assign(const Variable n);

  inline bool is_top() const;
  inline bool is_bottom() const;

  friend PowerSet operator +<>(const PowerSet& x, const PowerSet& y);
  friend PowerSet operator *<>(const PowerSet& x, const PowerSet& y);
  friend PowerSet operator <<<>(const PowerSet& x, Variable n);
  friend PowerSet hide<>(const PowerSet& x, Variable n);

  friend CS project<>(const PowerSet& x);

  friend bool operator ==<>(const PowerSet& x, const PowerSet& y);

  friend bool entails<>(const PowerSet& x, const PowerSet& y);

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
