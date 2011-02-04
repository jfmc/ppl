/* Row_Flags class declaration.
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

#ifndef PPL_Row_Flags_defs_hh
#define PPL_Row_Flags_defs_hh 1

#include "Row_Flags.types.hh"

#include "globals.defs.hh"

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \brief
  Wrapper class to represent a set of flags with bits in a native
  unsigned integral type.
  \ingroup PPL_CXX_interface
*/
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
class Parma_Polyhedra_Library::Row_Flags {
public:
  //! Constructs an object with all the flags unset.
  Row_Flags();

  //! Returns <CODE>true</CODE> if and only if \p *this and \p y are equal.
  bool operator==(const Row_Flags& y) const;

  /*! \brief
    Returns <CODE>true</CODE> if and only if \p *this and \p y
    are different.
  */
  bool operator!=(const Row_Flags& y) const;

  PPL_OUTPUT_DECLARATIONS

  //! Uses the ASCII Flags representation from \p s to recreate *this.
  /*!
    Returns <CODE>true</CODE> if successful, <CODE>false</CODE>
    otherwise.  The ASCII representation is as output by
    \ref Parma_Polyhedra_Library::Row_Flags::ascii_dump.
  */
  bool ascii_load(std::istream& s);

protected:
  //! A native integral type holding the bits that encode the flags.
  typedef unsigned int base_type;

  //! Index of the first bit derived classes can use.
  static const unsigned first_free_bit = 0;

  //! Total number of bits that can be stored.
  static const unsigned num_bits = std::numeric_limits<base_type>::digits;

  //! Constructs an object with flags set as in \p n.
  explicit Row_Flags(base_type n);

  //! Returns the integer encoding \p *this.
  base_type get_bits() const;

  //! Sets the bits in \p mask.
  void set_bits(base_type mask);

  //! Resets the bits in \p mask.
  void reset_bits(base_type mask);

  /*! \brief
    Returns <CODE>true</CODE> if and only if all the bits
    in \p mask are set.
  */
  bool test_bits(base_type mask) const;

private:
  //! The integer encoding \p *this.
  base_type bits;
};


#include "Row_Flags.inlines.hh"

#endif // !defined(PPL_Row_Flags_defs_hh)
