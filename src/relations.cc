/* Relation enumerations implementation (non-inline functions).
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

#include <config.h>

#include "relations.hh"


namespace PPL = Parma_Polyhedra_Library;

std::ostream&
PPL::operator <<(std::ostream& s, Relation_Poly_Con r) {
  const char* p = 0;
  switch (r) {
  case IS_DISJOINT:
    p = "IS_DISJOINT";
    break;
  case STRICTLY_INTERSECTS:
    p = "STRICTLY_INTERSECTS";
    break;
  case IS_INCLUDED:
    p = "IS_INCLUDED";
    break;
  case SATURATES:
    p = "SATURATES";
    break;
  }
  assert(p != 0);
  s << p;
  return s;
}


std::ostream&
PPL::operator <<(std::ostream& s, Relation_Poly_Gen r) {
  const char* p = 0;
  switch (r) {
  case SUBSUMES:
    p = "SUBSUMES";
    break;
  case DOES_NOT_SUBSUME:
    p = "DOES_NOT_SUBSUME";
    break;
  }
  assert(p != 0);
  s << p;
  return s;
}
