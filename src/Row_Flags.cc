/* Row_Flags class implementation (non-inline functions).
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

#include <ppl-config.h>

#include "Row_Flags.defs.hh"
#include <iostream>
#include <iomanip>

namespace PPL = Parma_Polyhedra_Library;

void
PPL::Row_Flags::ascii_dump(std::ostream& s) const {
  s << "0x";
  std::istream::fmtflags f = s.setf(std::istream::hex);
  std::streamsize sz = s.width(2*sizeof(Row_Flags::base_type));
  std::ostream::char_type ch = s.fill('0');
  s << bits;
  s.fill(ch);
  s.width(sz);
  s.flags(f);
}

PPL_OUTPUT_DEFINITIONS_ASCII_ONLY(Row_Flags)

bool
PPL::Row_Flags::ascii_load(std::istream& s) {
  std::string str;
  std::streamsize sz = s.width(2);
  if (!(s >> str) || str != "0x")
    return false;
  s.width(sz);
  std::istream::fmtflags f = s.setf(std::istream::hex);
  bool r = s >> bits;
  s.flags(f);
  return r;
}
