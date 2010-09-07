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
  // Increasing this number the stack memory usage is incresed, but the
  // new[]/delete[] overhead is reduced.
  const size_t local_buffer_size = 100;
  const __mpz_struct* mpz = n.get_mpz_t();
  const size_t buffer_size = 1 + (mpz_sizeinbase(mpz, 2) - 1) / (CHAR_BIT*sizeof(mp_limb_t));
  size_t count;
  if (buffer_size <= local_buffer_size) {
    mp_limb_t buffer[local_buffer_size];
    mpz_export(buffer, &count, 1, sizeof(mp_limb_t), 0, 0, mpz);
    PPL_ASSERT(count == buffer_size || (count == 0 && buffer_size == 1));
    ar & count;
    bool negative = (mpz_sgn(mpz) < 0);
    ar & negative;
    for (size_t i = 0; i < count; ++i)
      ar & buffer[i];
  } else {
    mp_limb_t* buffer = new mp_limb_t[buffer_size];
    mpz_export(buffer, &count, 1, sizeof(mp_limb_t), 0, 0, mpz);
    PPL_ASSERT(count == buffer_size || (count == 0 && buffer_size == 1));
    ar & count;
    bool negative = (mpz_sgn(mpz) < 0);
    ar & negative;
    for (size_t i = 0; i < count; ++i)
      ar & buffer[i];
    delete [] buffer;
  }
}

template<class Archive>
void load(Archive & ar, Parma_Polyhedra_Library::GMP_Integer & n,
         const unsigned int /* version */) {
  // Increasing this number the stack memory usage increases, but the
  // new[]/delete[] overhead decreases.
  const size_t local_buffer_size = 100;
  size_t count;
  ar & count;
  bool negative;
  ar & negative;
  if (count <= local_buffer_size) {
    mp_limb_t buffer[local_buffer_size];
    for (size_t i = 0; i < count; ++i)
      ar & buffer[i];
    __mpz_struct* mpz = n.get_mpz_t();
    mpz_import(mpz, count, 1, sizeof(mp_limb_t), 0, 0, buffer);
  } else {
    mp_limb_t* buffer = new mp_limb_t[count];
    for (size_t i = 0; i < count; ++i)
      ar & buffer[i];
    __mpz_struct* mpz = n.get_mpz_t();
    mpz_import(mpz, count, 1, sizeof(mp_limb_t), 0, 0, buffer);
    delete [] buffer;
  }
  if (negative)
    Parma_Polyhedra_Library::neg_assign(n);
}

template<class Archive>
void serialize(Archive &ar, Parma_Polyhedra_Library::GMP_Integer & n,
               const unsigned int version){
  boost::serialization::split_free(ar, n, version);
}


#endif // !defined(PPL_GMP_Integer_templates_hh)
