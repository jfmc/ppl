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
AskTell<CS>
operator+(const AskTell<CS>&, const AskTell<CS>&);

template <typename CS>
AskTell<CS>
operator*(const AskTell<CS>&, const AskTell<CS>&);

template <typename CS>
CS
project(const AskTell<CS>&);

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

  void upper_bound_assign(const AskTell& y);

  void meet_assign(const AskTell& y);

  bool definitely_entails(const AskTell& y) const;

  bool is_top() const;
  bool is_bottom() const;

  friend AskTell operator +<>(const AskTell<CS>& x,
			      const AskTell<CS>& y);
  friend AskTell operator *<>(const AskTell& x, const AskTell& y);

  friend CS project<>(const AskTell& x);

  friend int lcompare<>(const AskTell& x, const AskTell& y);

  std::ostream& print(std::ostream &s);

  friend std::ostream& operator <<<>(std::ostream& s, const AskTell& x);

public:

  class iterator : public Base::iterator {
  private:
    typedef typename Base::iterator Base;

  public:
    iterator();
    iterator(const Base& x);
    const CS& ask() const;
    const CS& tell() const;
  };

  class const_iterator : public Base::const_iterator {
  private:
    typedef typename Base::const_iterator Base;

  public:
    const_iterator();
    const_iterator(const Base& x);
    const CS& ask() const;
    const CS& tell() const;
  };

  iterator begin();
  iterator end();
  const_iterator begin() const;
  const_iterator end() const;
};

#include "AskTell.inlines.hh"

#endif // !defined(_AskTell_defs_hh)
