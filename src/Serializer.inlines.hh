/* Serializer class implementation (inline public functions).
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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ */

#ifndef PPL_Serializer_inlines_hh
#define PPL_Serializer_inlines_hh 1

namespace Parma_Polyhedra_Library {

inline
Serializer::Serializer()
  : open_mode(closed), compress_mode(none),
    n_buffer(0), i_buffer(0), n_minibuffer(0),
    i_minibuffer(0), buffer(), minibuffer(), buffer_mpz_t(),
    little_mpz_t(0), order_mpz_t(1), endian_mpz_t(0),
    nail_mpz_t(0), size_mpz_t(sizeof(unsigned long)),
    numb_mpz_t(8*sizeof(unsigned long)),
    different_endian(false), compression_level("wb"), path(),
    file(), gzfile(0), bzfile(0), zerror(0) {
#if ENDIAN_SOLVER
  int test = 1;
  host_endian = (*reinterpret_cast<unsigned char*>(&test)) ? little : big;
#endif
}

inline
Serializer::Serializer(const std::string& name, openmode mode)
  : open_mode(mode), compress_mode(none),
    n_buffer(0), i_buffer(0), n_minibuffer(0),
    i_minibuffer(0), buffer(), minibuffer(), buffer_mpz_t(),
    little_mpz_t(0), order_mpz_t(1), endian_mpz_t(0),
    nail_mpz_t(0), size_mpz_t(sizeof(unsigned long)),
    numb_mpz_t(8*sizeof(unsigned long)),
    different_endian(false), compression_level("wb"), path(name),
    file(), gzfile(0), bzfile(0), zerror(0) {
#if ENDIAN_SOLVER
  int test = 1;
  host_endian = (*reinterpret_cast<unsigned char*>(&test)) ? little : big;
#endif
}

inline void
Serializer::compression_path_assign() {
  std::string extension(path.end()-3,path.end());
  if(extension == ".gz")
    compress_mode = zlib;
  else {
    extension.assign(path.end()-4,path.end());
    if(extension == ".bz2")
      compress_mode = bzlib;
  }
#if HAVE_ZLIB_H
#else
  if(compress_mode == zlib) {
    compress_mode = none;
    path.assign(path.begin(),path.end()-3);
    printf("\nYou haven't zlib.");
    printf("\nThe name of file will be:\n");
    printf("%s.\n",path.c_str());
    return;
  }
#endif
#if HAVE_BZLIB_H
#else
  if(compress_mode == bzlib) {
    compress_mode = none;
    path.assign(path.begin(),path.end()-4);
    printf("\nYou haven't bzlib.");
    printf("\nThe name of file will be:\n");
    printf("%s.\n",path.c_str());
    return;
  }
#endif
}

inline void
Serializer::compression_level_assign() throw(std::runtime_error) {
  switch(open_mode) {
  case in: return;
  case out: return;
  case out1: break;
  case out2:
    compression_level += "2";
    break;
  case out3:
    compression_level += "3";
    break;
  case out4: 
    compression_level += "4";
    break;;
  case out5: 
    compression_level += "5";
    break;;
  case out6: 
    compression_level += "6";
    break;;
  case out7: 
    compression_level += "7";
    break;;
  case out8: 
    compression_level += "8";
    break;;
  case out9: 
    compression_level += "9";
    break;
  case closed: throw std::runtime_error("PPL::Serializer::open() not valid open mode.");
  }
  open_mode = out;
}

template <typename T>
inline void
Serializer::put(T t) {
  for(unsigned i = 0; i < sizeof(T); ++i) {
    buffer[i_buffer++] = static_cast<unsigned char>(t);
    t >>= 8;
  }
}

template <typename A, typename B>
inline void
Serializer::get(A& t, const std::vector<B>& buf, unsigned& i_buf) {
  t = 0;
  A tmp = 0;
  for(unsigned i = 0; i < sizeof(A); ++i, ++i_buf) {
    tmp = buf[i_buf];
    tmp <<= 8*i;
    t += tmp;
  }
}

template <typename T>
inline void
Serializer::change_endian(T& t) {
  unsigned char a, j = sizeof(T) - 1,
    i = 0, sizeof_t = sizeof(T);
  T tmp = t;
  t = 0;
  for(;i < sizeof_t; ++i, --j) {
    a = static_cast<unsigned char>((tmp >> i*8) & 255);
    t += static_cast<T>(a << j*8);
  }
}

inline void
Serializer::get_file_endian() {
  unsigned char endian_file;
  low_level_deserialize(endian_file);
  if( (endian_file == 255) && (host_endian == little) )
    different_endian = true;
  else
    different_endian = false;
}

inline void
Serializer::put_file_endian() {
  if( host_endian == big )
    low_level_serialize(static_cast<unsigned char>(255));
  else
    low_level_serialize(static_cast<unsigned char>(0));
}

inline void
Serializer::serialize(bool b) {
  low_level_serialize(b);
}

inline void
Serializer::deserialize(bool& b) {
  low_level_deserialize(b);
}

inline void
Serializer::serialize(dimension_type dim) {
  low_level_serialize(dim);
}

inline void
Serializer::deserialize(dimension_type& dim) {
  low_level_deserialize(dim);
}

inline void
Serializer::serialize(const Topology& topol) {
  low_level_serialize(static_cast<char>(topol));
}

inline void
Serializer::deserialize(Topology& topol) {
  char a;
  low_level_deserialize(a);
  topol = Topology(a);
}

inline void
Serializer::serialize(const Polyhedron::Status& status) {
  low_level_serialize(status.flags);
  // check for well-formedness.
  assert(status.OK());
}

inline void
Serializer::deserialize(Polyhedron::Status& status) {
  low_level_deserialize(status.flags);
  // check for well-formedness.
  assert(status.OK());
}

inline void
Serializer::serialize(const Coefficient& z) {
  serialize(z.get_mpz_t());
}

inline void
Serializer::deserialize(Coefficient& z) {
  deserialize(z.get_mpz_t());
}

inline void
Serializer::serialize(const Row::Flags& t) {
  low_level_serialize(t.bits);
}

inline void
Serializer::deserialize(Row::Flags& t) {
  low_level_deserialize(t.bits);
}

inline void
Serializer::serialize(const Bit_Row& row) {
  serialize(row.vec);
  // check for well-formedness.
  assert(row.OK());
}

inline void
Serializer::deserialize(Bit_Row& row) {
  deserialize(row.vec);
  // check for well-formedness.
  assert(row.OK());
}

inline void
Serializer::serialize(const Generator_System& gs) {
#if ENDIAN_SOLVER
  put_file_endian();
#endif
  serialize_matrix(gs);
  // check for well-formedness.
  assert(gs.OK());
}

inline void
Serializer::deserialize(Generator_System& gs) {
#if ENDIAN_SOLVER
  get_file_endian();
#endif
  deserialize_matrix(gs);
  // check for well-formedness.
  assert(gs.OK());
}

inline void
Serializer::serialize(const Constraint_System& cs) {
#if ENDIAN_SOLVER
  put_file_endian();
#endif
  serialize_matrix(cs);
  // check for well-formedness.
  assert(cs.OK());
}

inline void
Serializer::deserialize(Constraint_System& cs) {
#if ENDIAN_SOLVER
  get_file_endian();
#endif
  deserialize_matrix(cs);
  // check for well-formedness.
  assert(cs.OK());
}

inline void
Serializer::open(const std::string& name, openmode mode) {
  path = name;
  open_mode = mode;
  open();
}

} // namespace Parma_Polyhedra_Library
#endif// !defined(PPL_Serializer_inlines_hh)
