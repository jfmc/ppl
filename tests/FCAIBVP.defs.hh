/* FCAIBVP class declaration.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_FCAIBVP_defs_hh
#define PPL_FCAIBVP_defs_hh 1

#include "FCAIBVP.types.hh"
#include "ppl.hh"

namespace Parma_Polyhedra_Library {

bool
operator==(const FCAIBVP& x, const FCAIBVP& y);

bool
operator!=(const FCAIBVP& x, const FCAIBVP& y);

namespace IO_Operators {

std::ostream&
operator<<(std::ostream& s, const FCAIBVP& x);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

/*! \brief
  A class for representing Finite Conjunctions of Attribute
  Independent Boolean Variable Properties.
*/
class Parma_Polyhedra_Library::FCAIBVP {
private:
  typedef std::set<dimension_type> Set;

  Set set;

public:
  FCAIBVP();

  explicit FCAIBVP(const Variable& x);

  explicit FCAIBVP(const Variables_Set& y);

  FCAIBVP(const FCAIBVP& y, unsigned offset);

  memory_size_type total_memory_in_bytes() const;

  memory_size_type external_memory_in_bytes() const;

  bool is_top() const;

  bool is_bottom() const;

  bool definitely_entails(const FCAIBVP& y) const;

  void upper_bound_assign(const FCAIBVP& y);

  void difference_assign(const FCAIBVP& y);

  void meet_assign(const FCAIBVP& y);

  void weakening_assign(const FCAIBVP& y);

  static bool has_nontrivial_weakening();

  bool OK() const;

  friend std::ostream&
  Parma_Polyhedra_Library::IO_Operators::operator<<(std::ostream& s,
						    const FCAIBVP& x);
};

#include "FCAIBVP.inlines.hh"

#endif // !defined(PPL_FCAIBVP_defs_hh)
