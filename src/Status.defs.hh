/* Status class declaration.
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

#ifndef _Status_defs_hh
#define _Status_defs_hh 1

#include "Status.types.hh"
#include <iosfwd>

namespace Parma_Polyhedra_Library {
  // Put them in the namespace here to declare them friends later.
  Status operator &(const Status& x, const Status& y);
  Status operator |(const Status& x, const Status& y);
}

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*!
  This class implements conjunctions of assertions about a polyhedron.
  The assertions supported are:
  - <EM>zero-dim</EM>: the polyhedron is the zero-dimensional singleton
    \f$\Rset^0 = \{\cdot\}\f$;
  - <EM>empty</EM>: the polyhedron is the empty set;
  - <EM>constraints up-to-date</EM>: the polyhedron is correctly
    characterized by the attached set of constraints;
  - <EM>generators up-to-date</EM>: the polyhedron is correctly
    characterized by the attached set of generators;
  - <EM>constraints minimized</EM>: the set of constraints attached
    to the polyhedron is minimized;
  - <EM>generators minimized</EM>: the set of generators attached
    to the polyhedron is minimized.
  - <EM>constraints' saturation matrix up-to-date</EM>: the attached
    saturation matrix having rows indexed by generators and columns 
    indexed by constraints correctly expresses the saturation
    relation between the attached constraints and generators;
  - <EM>generators' saturation matrix up-to-date</EM>: the attached
    saturation matrix having rows indexed by constraints and 
    columns indexed by generators correctly expresses the saturation
    relation between the attached constraints and generators;

  Not all the conjunctions of these elementary assertions constitute
  a legal Status.  In fact:
  - <EM>zero-dim</EM> excludes any other assertion;
  - <EM>empty</EM>: excludes any other assertion;
  - <EM>constraints minimized</EM> implies <EM>constraints up-to-date</EM>;
  - <EM>generators minimized</EM> implies <EM>generators up-to-date</EM>;
  - <EM>constraints' saturation matrix up-to-date</EM> implies both
    <EM>constraints up-to-date</EM> and <EM>generators up-to-date</EM>;
  - <EM>generators' saturation matrix up-to-date</EM> implies both
    <EM>constraints up-to-date</EM> and <EM>generators up-to-date</EM>.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS

class Parma_Polyhedra_Library::Status {
public:
  //! By default Status is the <EM>zero-dim</EM> assertion.
  Status();

  //! Intersection: yields the assertions that are in \p x <EM>and</EM> \p y.
  friend Status Parma_Polyhedra_Library::operator &(const Status& x,
						    const Status& y);
  //! Union: yields the assertions that are in \p x <EM>or</EM> \p y.
  friend Status Parma_Polyhedra_Library::operator |(const Status& x,
						    const Status& y);

  //! @name Test, remove or add an individual assertion from the conjunction.
  //@{
  bool test_zero_dim() const;
  void reset_zero_dim();
  void set_zero_dim();

  bool test_empty() const;
  void reset_empty();
  void set_empty();

  bool test_c_up_to_date() const;
  void reset_c_up_to_date();
  void set_c_up_to_date();

  bool test_g_up_to_date() const;
  void reset_g_up_to_date();
  void set_g_up_to_date();

  bool test_c_minimized() const;
  void reset_c_minimized();
  void set_c_minimized();

  bool test_g_minimized() const;
  void reset_g_minimized();
  void set_g_minimized();

  bool test_sat_c_up_to_date() const;
  void reset_sat_c_up_to_date();
  void set_sat_c_up_to_date();

  bool test_sat_g_up_to_date() const;
  void reset_sat_g_up_to_date();
  void set_sat_g_up_to_date();
  //@}

  //! Check whether \p *this is a legal status.
  bool OK() const;

private:
  //! Status is implemented by means of a finite bitset.
  typedef unsigned int status_t;

  //! @name Bitmasks for the individual assertions.
  //@{
  static const status_t ZERO_DIM =             0U;
  static const status_t EMPTY =            1U << 0;
  static const status_t C_UP_TO_DATE =     1U << 1;
  static const status_t G_UP_TO_DATE =     1U << 2;
  static const status_t C_MINIMIZED  =     1U << 3;
  static const status_t G_MINIMIZED  =     1U << 4;
  static const status_t SAT_C_UP_TO_DATE = 1U << 5;
  static const status_t SAT_G_UP_TO_DATE = 1U << 6;
  //@}

  //! This holds the current bitset.
  status_t flags;

  //! Construct from a bitmask.
  Status(status_t mask);
  //! Check whether <EM>all</EM> bits in \p mask are set.
  bool test_all(status_t mask) const;
  //! Check whether <EM>at least one</EM> bit in \p mask is set.
  bool test_any(status_t mask) const;
  //! Set the bits in \p mask.
  void set(status_t mask);
  //! Reset the bits in \p mask.
  void reset(status_t mask);
};

namespace Parma_Polyhedra_Library {
  //! @name Input/output.
  //@{
  std::ostream& operator <<(std::ostream& s, const Status& u);
  std::istream& operator >>(std::istream& s, Status& u);
  //@}
}

#if !OUTLINE
#include "Status.inlines.hh"
#endif

#endif
