/* Ask_Tell class implementation: non-inline template functions.
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

#ifndef PPL_Ask_Tell_templates_hh
#define PPL_Ask_Tell_templates_hh 1

#include <iostream>

namespace Parma_Polyhedra_Library {

template <typename D>
bool
Ask_Tell<D>::OK() const {
  for (typename Ask_Tell<D>::const_iterator i = begin(),
	 send = end(); i != send; ++i) {
    const Ask_Tell_Pair<D>& p = *i;
    if (!p.ask().OK())
      return false;
    if (!p.tell().OK())
      return false;
    if (p.ask().definitely_entails(p.tell())) {
#ifndef NDEBUG
      std::cerr << "Illegal agent in ask-and-tell: "
		<< p.ask() << " -> " << p.tell()
		<< std::endl;
#endif
      return false;
    }
  }
  return true;
}

namespace IO_Operators {

template <typename D>
std::ostream&
operator<<(std::ostream& s, const Ask_Tell<D>& x) {
  if (x.is_top())
    s << "true";
  else if (x.is_bottom())
    s << "false";
  else
    for (typename Ask_Tell<D>::const_iterator xi = x.begin(),
	   x_end = x.end(); xi != x_end; ++xi)
      s << "(" << xi->ask() << " -> " << xi->tell() << ")";
  return s;
}

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Ask_Tell_templates_hh)
