/* Random_Number_Generator class implementation: inline functions.
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

#ifndef PPL_Random_Number_Generator_inlines_hh
#define PPL_Random_Number_Generator_inlines_hh 1

namespace Parma_Polyhedra_Library {

namespace {

template <typename T>
class Random_Number_Generator_Aux {
public:
  Random_Number_Generator_Aux(unsigned int max_bits) {
    if (std::numeric_limits<T>::is_bounded) {
      assign(zmin, std::numeric_limits<T>::min(), ROUND_IGNORE);
      assign(zrange, std::numeric_limits<T>::max(), ROUND_IGNORE);
      zrange -= zmin;
      ++zrange;
    }
    else if (std::numeric_limits<T>::is_signed) {
      zmin = 1;
      zmin <<= (max_bits - 1);
      zmin = -zmin;
    }
    else {
      assign(zmin, std::numeric_limits<T>::min(), ROUND_IGNORE);
    }
  }
  mpz_class zmin;
  mpz_class zrange;
};

} // namespace

inline
Random_Number_Generator::Random_Number_Generator()
  : rand(gmp_randinit_default), max_bits(512) {
  // Seed the random number generator.
  rand.seed((unsigned long) time(0));
}

template <typename T>
inline void
Random_Number_Generator::get(T& x, unsigned int info) {
  static Random_Number_Generator_Aux<T> aux(max_bits);
  mpz_class n;
  if (std::numeric_limits<T>::is_bounded) {
    n = rand.get_z_range(aux.zrange);
  }
  else {
    n = rand.get_z_bits(max_bits);
  }
  n += aux.zmin;
  assign(x, n, ROUND_IGNORE);
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Random_Number_Generator_inlines_hh)
