/* Determinate class declaration.
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

#ifndef _Determinate_defs_hh
#define _Determinate_defs_hh

#include "Determinate.types.hh"
#include <iosfwd>
#include <cassert>

namespace Parma_Polyhedra_Library {

template <typename PH>
class Determinate;

template <typename PH>
bool
lcompare(const Determinate<PH>& x, const Determinate<PH>& y);

template <typename PH>
Determinate<PH>
operator+(const Determinate<PH>& x, const Determinate<PH>& y);

template <typename PH>
Determinate<PH>
operator*(const Determinate<PH>& x, const Determinate<PH>& y);

template <typename PH>
std::ostream&
operator<<(std::ostream&, const Determinate<PH>&);

} // namespace Parma_Polyhedra_Library

//! Wrap a polyhedron class into a determinate constraint system interface.
template <typename PH>
class Parma_Polyhedra_Library::Determinate {
public:
  Determinate(const PH& p);
  Determinate(const Determinate& y);
  ~Determinate();

  Determinate& operator=(const Determinate& y);

  void mutate();

  void upper_bound_assign(const Determinate& y);

  void meet_assign(const Determinate& y);

  bool definitely_entails(const Determinate& y) const;

  Determinate& operator <<= (unsigned int n);
  Determinate& hide_assign(unsigned int n);

  inline bool is_top() const;
  inline bool is_bottom() const;

  friend bool lcompare<>(const Determinate& x, const Determinate& y);
  friend Determinate operator +<>(const Determinate& x, const Determinate& y);
  friend Determinate operator *<>(const Determinate& x, const Determinate& y);
  friend std::ostream& operator<<<>(std::ostream& s, const Determinate& x);

  //! \brief
  //! Intersects \p *this with (a copy of) constraint \p c.
  /*!
    \exception std::invalid_argument thrown if \p *this and constraint \p c
                                     are topology-incompatible
                                     or dimension-incompatible.
  */
  void add_constraint(const Constraint& c);

  //! Intersects \p *this with the constraints in \p cs.
  /*!
    \param  cs             The constraints to intersect with.
                           This parameter is not declared
                           <CODE>const</CODE> because  it can be modified.
    \exception std::invalid_argument thrown if \p *this and \p cs
                                     are topology-incompatible
                                     or dimension-incompatible.
  */
  void add_constraints(ConSys& cs);

  //! \brief
  //! First increases the space dimension of \p *this by adding
  //! \p cs.space_dimension() new dimensions;
  //! then intersects \p *this with a renamed-apart version of
  //! the constraints in \p cs.
  /*!
    \param  cs             Specifies the constraints to be added.
                           This parameter is not declared <CODE>const</CODE>
                           because it can be modified.
    \exception std::invalid_argument thrown if \p *this and \p cs
                                     are topology-incompatible.
  */
  void add_dimensions_and_constraints(ConSys& cs);

  //! \brief
  //! Adds \p dim new dimensions and embeds the old polyhedron
  //! into the new space.
  void add_dimensions_and_embed(size_t dim);

  //! \brief
  //! Adds \p dim new dimensions to the polyhedron
  //! and does not embed it in the new space.
  void add_dimensions_and_project(size_t dim);

  //! \brief
  //! Removes all the specified dimensions.
  /*!
    \param to_be_removed  The set of Variable objects corresponding
                          to the dimensions to be removed.
    \exception std::invalid_argument thrown if \p *this is
                                     dimension-incompatible with one
				     of the Variable objects contained
				     in \p to_be_removed.
  */
  void remove_dimensions(const std::set<Variable>& to_be_removed);

  //! \brief
  //! Removes the higher dimensions so that the resulting space
  //! will have dimension \p new_dimension.
  /*!
    \exception std::invalid_argument thrown if \p new_dimensions is greater
                                     than the space dimension of \p *this.
  */
  void remove_higher_dimensions(size_t new_dimension);

private:
  class Rep {
  private:
    //! Count the number of references:
    //! -   0: leaked, \p pph is non-const;
    //! -   1: one reference, \p pph is non-const;
    //! - > 1: more than one reference, \p pph is const.
    mutable unsigned long references;

    //! Private and unimplemented.
    Rep& operator=(const Rep& y);
    Rep(const Rep& y);
    Rep();

  public:
    //! A polyhedron.
    PH ph;

    //! True if and only if this representation is currently shared.
    bool is_shared() const {
      return references > 1;
    }

    //! Register a new reference.
    void new_reference() const {
      ++references;
    }

    //! Unregister a reference and return true if the representation
    //! has become unreferenced.
    bool del_reference() const {
      return --references == 0;
    }

    Rep(const PH& p)
      : references(0), ph(p) {
    }

    //! Destructor.
    ~Rep() {
      assert(references == 0);
    }

  };

  Rep* prep;

  void new_reference() const {
    if (prep)
      prep->new_reference();
  }

  bool del_reference() const {
    if (prep)
      return prep->del_reference();
    else
      return false;
  }

  Determinate();
};

#include "Determinate.inlines.hh"

#endif // !defined(_Determinate_defs_hh)
