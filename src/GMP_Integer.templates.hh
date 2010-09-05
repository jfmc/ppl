/* GMP_Integer class implementation: non-inline template functions.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_GMP_Integer_templates_hh
#define PPL_GMP_Integer_templates_hh 1

#include <boost/serialization/split_free.hpp>
#include <boost/serialization/string.hpp>

template<class Archive>
void save(Archive & ar, const Parma_Polyhedra_Library::GMP_Integer & n,
         const unsigned int /* version */) {
 std::string s = n.get_str(16);
 ar & s;
}

template<class Archive>
void load(Archive & ar, Parma_Polyhedra_Library::GMP_Integer & n,
         const unsigned int /* version */) {
 std::string s;
 ar & s;
 n.set_str(s.c_str(), 16);
}

template<class Archive>
void serialize(Archive &ar, Parma_Polyhedra_Library::GMP_Integer & n,
               const unsigned int version){
  boost::serialization::split_free(ar, n, version);
}


#endif // !defined(PPL_GMP_Integer_templates_hh)
