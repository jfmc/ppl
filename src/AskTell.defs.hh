/* AskTell class declaration.
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

#ifndef _AskTell_defs_hh
#define _AskTell_defs_hh

#include "AskTell.types.hh"
#include "LCompare.defs.hh"
#include <map>
#include <iosfwd>

namespace Parma_Polyhedra_Library {

template <typename CS>
bool
entails(const CS& ax, const CS& tx, const CS& ay, const CS& ty) {
  if(!entails(ay, ax))
    return false;
  else if (ax == ay)
    return entails(tx, ty);            // Optional
  else if (entails(tx, ty))
    return true;
  else if (entails(tx, ay))
    return false;               // Optional
  else
    return entails(tx*ay, ty);
}

template <typename CS>
AskTell<CS>
operator+(const AskTell<CS>&, const AskTell<CS>&);

template <typename CS>
AskTell<CS>
operator*(const AskTell<CS>&, const AskTell<CS>&);

template <typename CS>
AskTell<CS>
operator<<(const AskTell<CS>&, Variable);

template <typename CS>
AskTell<CS>
hide(const AskTell<CS>&, Variable);

template <typename CS>
CS
project(const AskTell<CS>&);

template <typename CS>
bool
operator==(const AskTell<CS>&, const AskTell<CS>&);

template <typename CS>
bool
entails(const AskTell<CS>&, const AskTell<CS>&);

template <typename CS>
int
lcompare(const AskTell<CS>&, const AskTell<CS>&);

template <typename CS>
std::ostream&
operator <<(std::ostream&, const AskTell<CS>&);

}

//! The ask and tell construction on constraint systems.
template <typename CS>
class Parma_Polyhedra_Library::AskTell
  : public std::map<CS, CS, LCompare<CS> > {
protected:
  typedef LCompare<CS> Less;
  typedef std::map<CS, CS, Less> Base;
  void pair_insert(const CS& a, const CS& t);
  void pair_insert_good(const CS& a, const CS& t);
//  inline const_iterator pair_insert(const_iterator& p, AskTellPair<CS>& pair);

  bool reduce();
  bool deduce();
  bool absorb();
  void engine();

  bool probe(const CS& tellv, const CS& askv) const;

public:
  AskTell& inject(const CS& askv, const CS& tellv);

  AskTell& bottom();

  AskTell& operator += (const AskTell& y);
  AskTell& operator *= (const AskTell& y);
  AskTell& operator <<= (const Variable n);
  AskTell& hide_assign(const Variable n);

  bool is_top() const;
  bool is_bottom() const;

  friend AskTell operator +<>(const AskTell<CS>& x,
			      const AskTell<CS>& y);
  friend AskTell operator *<>(const AskTell& x, const AskTell& y);
  friend AskTell operator <<<>(const AskTell& x, Variable n);
  friend AskTell hide<>(const AskTell& x, Variable n);

  friend CS project<>(const AskTell& x);

  friend bool operator ==<>(const AskTell& x, const AskTell& y);

  friend bool entails<>(const AskTell& x, const AskTell& y);

  friend int lcompare<>(const AskTell& x, const AskTell& y);

  std::ostream& print(std::ostream &s);

  friend std::ostream& operator <<<>(std::ostream& s, const AskTell& x);

public:

  class iterator : public Base::iterator {
    typedef typename Base::iterator Base;
  public:
    iterator() { }
    iterator(const Base& x) : Base(x) { }
    const CS& ask() const { return Base::operator*().first; }
    CS& tell() const { return Base::operator*().second; }
  };
  class const_iterator : public Base::const_iterator {
    typedef typename Base::const_iterator Base;
  public:
    const_iterator() { }
    const_iterator(const Base& x) : Base(x) { }
    const CS& ask() const { return Base::operator*().first; }
    const CS& tell() const { return Base::operator*().second; }
  };
  iterator begin() {
    return iterator(Base::begin());
  }
  iterator end() {
    return iterator(Base::end());
  }
  const_iterator begin() const {
    return const_iterator(Base::begin());
  }
  const_iterator end() const {
    return const_iterator(Base::end());
  }
};

#include "AskTell.inlines.hh"

#endif // _AskTell_defs_hh
