/* Fake declarations to test the validity of the arguments of the
   --enabled-instantiations option defined in configure.ac.
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include <stdint.h>

namespace Parma_Polyhedra_Library {

class Polyhedron {
public:
  static bool valid_instantiation() {
    return true;
  }
  static bool valid_Polyhedra_Powerset_argument() {
    return true;
  }
};

template <typename T>
bool
valid_BD_Shape_argument(void);

template <>
bool
valid_BD_Shape_argument<char>() {
  return true;
}

template <>
bool
valid_BD_Shape_argument<int>() {
  return true;
}

template <>
bool
valid_BD_Shape_argument<int8_t>() {
  return true;
}

template <typename T>
class BD_Shape {
public:
  static bool valid_instantiation() {
    return valid_BD_Shape_argument<T>();
  }
};

template <typename PH>
class Polyhedra_Powerset {
public:
  static bool valid_instantiation() {
    return PH::valid_Polyhedra_Powerset_argument();
  }
};

} // namespace Parma_Polyhedra_Library
