/* Serializer class implementation (non-inline public functions).
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
site: http://www.cs.unipr.it/ppl/ . */

#include <config.h>
#include "Serializer.defs.hh"
namespace PPL = Parma_Polyhedra_Library;

void
PPL::Serializer::open() throw(std::ios_base::failure, std::runtime_error) {
  compression_path_assign();
  if(compress_mode == zlib || compress_mode == bzlib) {
    buffer.resize(1024);
    minibuffer.resize(10);
    compression_level_assign();
  }
  switch(open_mode) {
  case in: {
    switch(compress_mode) {
    case none: {
      file.open(path.c_str(), std::ios_base::in | std::ios_base::binary);
      if(!file.good())
	throw std::ios_base::failure("std::fstream::open() failure.");
      break;
    }
    case zlib: {
      gzfile = gzopen(path.c_str(),"rb");
      if(gzfile == NULL)
	throw std::runtime_error("zlib::gzopen() failure.");
      n_buffer = i_buffer = 0;
      break;
    }
    case bzlib: {
      bzfile = BZ2_bzopen(path.c_str(),"rb");
      if(bzfile == NULL)
	throw std::runtime_error("bzlib::BZ2_bzopen() failure.");
      n_buffer = i_buffer = 0;
      break;
    }}
    break;
  }
  case out: {
    switch(compress_mode) {
    case none: {
      file.open(path.c_str(), std::ios_base::out | std::ios_base::binary);
      if(!file.good())
	throw std::ios_base::failure("std::fstream::open() failure.");
      break;
    }
    case zlib: {
      gzfile = gzopen(path.c_str(), compression_level.c_str());
      if(gzfile == NULL)
	throw std::runtime_error("zlib::gzopen() failure.");
      n_buffer = i_buffer = 0;
      break;
    }
    case bzlib: {
      bzfile = BZ2_bzopen(path.c_str(), compression_level.c_str());
      if(bzfile == NULL)
	throw std::runtime_error("bzlib::BZ2_bzopen() failure.");
      n_buffer = i_buffer = 0;
      break;
    }}
    break;
  }
  default: throw std::runtime_error("PPL::Serializer::open() not valid open_mode.");
  }
}

void
PPL::Serializer::close() throw(std::ios_base::failure, std::runtime_error) {
  switch(compress_mode) {
  case none: {
    file.close();
    if(!file.good())
      throw std::ios_base::failure("std::fstream::close() failure.");
    break;
  }
  case zlib: {
    if(open_mode == out && i_buffer > 0) {
      zerror = gzwrite(gzfile, &buffer.front(), i_buffer);
      if(zerror == 0)
	throw std::runtime_error("zlib::gzwrite() failure.");
    }
    zerror = gzclose(gzfile);
    if(zerror != Z_OK)
      throw std::runtime_error("zlib::gzclose() failure.");
    n_buffer = i_buffer = 0;
    break;
    }
  case bzlib: {
    if(open_mode == out && i_buffer > 0) {
      zerror = BZ2_bzwrite(bzfile, &buffer.front(), i_buffer);
      if(zerror == 0)
	throw std::runtime_error("bzlib::BZ2_bzwrite() failure.");
    }
    BZ2_bzclose(bzfile);
    n_buffer = i_buffer = 0;
    break;
  }}
  open_mode = closed;
#if ENDIAN_SOLVER
  different_endian = false;
#endif
}

template <typename T> void
PPL::Serializer::low_level_serialize(T t)
  throw(std::ios_base::failure, std::runtime_error) {
  switch(compress_mode) {
  case none: {
    file.write(reinterpret_cast<char*>(&t), sizeof(T));
    if(!file.good())
      throw std::ios_base::failure("std::fstream::write() failure.");
    break;
  }
  case zlib: {
    if(i_buffer + sizeof(T) > buffer.size()) {
      // write buffer to gzfile.
      zerror = gzwrite(gzfile, &buffer.front(), i_buffer);
      if(zerror == 0)
	throw std::runtime_error("zlib::gzwrite() failure.");
      i_buffer = 0;
    }
    // put t in buffer.
    put(t);
    break;
  }
  case bzlib: {
    if(i_buffer + sizeof(T) > buffer.size()) {
      // write buffer to bzfile.
      zerror = BZ2_bzwrite(bzfile, &buffer.front(), i_buffer);
      if(zerror == 0)
	throw std::runtime_error("bzlib::BZ2_bzwrite() failure.");
      i_buffer = 0;
    }
    // put t in buffer.
    put(t);
    break;
  }}
}

template <typename T> void
PPL::Serializer::low_level_deserialize(T& t)
  throw(std::ios_base::failure, std::runtime_error) {
  if(compress_mode == none) {
    file.read(reinterpret_cast<char*>(&t), sizeof(T));
    if(!file.good())
      throw std::ios_base::failure("std::fstream::read() failure.");
  }
  else {
    bool incomplete_type = false;
    // if buffer is empty
    if((n_buffer == 0) && (i_buffer == 0)) {
      if(compress_mode == zlib)
	zerror = n_buffer = gzread(gzfile, &buffer.front(), buffer.size());
      if(compress_mode == bzlib)
	zerror = n_buffer = BZ2_bzread(bzfile, &buffer.front(), buffer.size());
      if(zerror == -1)
	throw std::runtime_error("zlib-bzlib read() failure.");
    }
    // if t isn't complete.
    if((n_buffer - i_buffer) < sizeof(T)) {
      incomplete_type = true;
      i_minibuffer = n_minibuffer = 0;
      while(i_buffer < n_buffer)
	minibuffer[n_minibuffer++] = buffer[i_buffer++];
      // read buffer from file.
      if(compress_mode == zlib)
	zerror = n_buffer = gzread(gzfile, &buffer.front(), buffer.size());
      if(compress_mode == bzlib)
	zerror = n_buffer = BZ2_bzread(bzfile, &buffer.front(), buffer.size());
      if(zerror == -1)
	throw std::runtime_error("zlib-bzlib read() failure.");
      i_buffer = 0;
      unsigned interval = sizeof(T) - n_minibuffer;
      while(i_buffer < interval)
	minibuffer[n_minibuffer++] = buffer[i_buffer++];
      if(n_minibuffer != sizeof(T))
	throw std::runtime_error("PPL::Serializer::low_level_deserialize() failure.");
    }
    // get t from buffer if flag is false, from minibuffer otherwise
    if(incomplete_type)
      get(t, minibuffer, i_minibuffer);
    else
      get(t, buffer, i_buffer);
  }
#if ENDIAN_SOLVER
  if(different_endian)
    change_endian(t);
#endif
}

void
PPL::Serializer::serialize(const mpz_t z) {
  /*
    Check the value of mpz_t `z' to save space on file.
    If the value is between -127 and +126 then will be
    serialize an unsigned char plus 127, else will be
    serialize an unsigned char: 254 if the sign of
    mpz_t `z' is negative or 255 if the sign of mpz_t
    `z' is positive, follow by dimension of exported
    mpz_t `z' and the modulus of mpz_t.
  */

  // 255 = std::numeric_limits<unsigned char>::max()
  // 127 = std::numeric_limits<signed char>::max()
  // -127 = std::numeric_limits<signed char>::min() + 1
  // 126 = std::numeric_limits<signed char>::max() - 1
  little_mpz_t = 255;
  if((mpz_cmp_si(z, -127) >= 0) &&
     (mpz_cmp_si(z, 126) < 0)) {
    little_mpz_t = mpz_get_si(z) + 127;
    low_level_serialize(little_mpz_t);
    return;
  }
  if(mpz_sgn(z) < 0)
    --little_mpz_t;
  low_level_serialize(little_mpz_t);
  size_t written;
  // exporting the modulus of `z' into `buffer_mpz_t'.
  size_t count = (mpz_sizeinbase(z, 2) +
		  numb_mpz_t - 1) / numb_mpz_t;
  if(count > buffer_mpz_t.size())
    buffer_mpz_t.resize(count);
  mpz_export(&buffer_mpz_t.front(), &written, order_mpz_t,
	     size_mpz_t, endian_mpz_t, nail_mpz_t, z);
  // serialize `count'.
  count = abs(count);
  low_level_serialize(count);
  // serialize modulus.
  for(size_t i = 0; i < count; ++i)
    low_level_serialize(buffer_mpz_t[i]);
}

void
PPL::Serializer::deserialize(mpz_t z) {
  // 254 = std::numeric_limits<unsigned char>::max() - 1
  // 253 = std::numeric_limits<unsigned char>::max() - 2
  // 127 = abs(std::numeric_limits<signed char>::min() + 1)

  low_level_deserialize(little_mpz_t);
  if(little_mpz_t < 253) {
    mpz_init_set_si(z, (little_mpz_t - 127));
    return;
  }
  size_t count;
  low_level_deserialize(count);
  if(count > buffer_mpz_t.size())
    buffer_mpz_t.resize(count);
  for(size_t i = 0; i < count; ++i)
    low_level_deserialize(buffer_mpz_t[i]);
  // import `z' from `buffer_mpz_t'.
  mpz_t tmp;
  mpz_init(tmp);
  mpz_import(tmp, count, order_mpz_t,
	     size_mpz_t, endian_mpz_t,
	     nail_mpz_t, &buffer_mpz_t.front());
  // adjust sign if needed.
  if(little_mpz_t == 254)
    mpz_neg(tmp, tmp);
  mpz_swap(z, tmp);
}

void
PPL::Serializer::serialize_matrix(const Linear_System& m) {
  size_t num_rows = m.num_rows();
  size_t row_size = m.row_size;
  serialize(m.row_topology);
  serialize(row_size);
  serialize(m.row_capacity);
  serialize(m.index_first_pending);
  serialize(m.sorted);
  serialize(num_rows);
  for (size_t i = 0; i < num_rows; ++i) {
    const Linear_Row& r = m[i];
    assert(r.impl->size_ == row_size);
    serialize(r.flags());
    for (size_t j = 0; j < row_size; ++j)
      serialize(r[j]);
  }
  // check for well-formedness.
  assert(m.OK());
}

void
PPL::Serializer::deserialize_matrix(Linear_System& m) {
  Linear_System tmp(NECESSARILY_CLOSED);
  deserialize(tmp.row_topology);
  deserialize(tmp.row_size);
  deserialize(tmp.row_capacity);
  deserialize(tmp.index_first_pending);
  deserialize(tmp.sorted);
  size_t num_rows;
  deserialize(num_rows);
  // construct the rows with given topology, size and capacity.
  tmp.rows.resize(num_rows);
  size_t size = tmp.row_size;
  size_t capacity = tmp.row_capacity;
  for (dimension_type i = 0; i < num_rows; ++i) {
    Row& r = tmp[i];
    Row::Flags row_flags;
    deserialize(row_flags);
    r.construct(size, capacity, row_flags);
    // deserialize the coefficients.
    for (size_t j = 0; j < size; ++j)
      deserialize(r[j]);
  }
  // check for well-formedness.
  assert(tmp.OK());
  std::swap(m, tmp);
}

void
PPL::Serializer::serialize(const Polyhedron& ph) {
#if ENDIAN_SOLVER
  put_file_endian();
#endif
  serialize(ph.status);
  if(ph.status.test_empty()) {
    if(!ph.status.test_zero_dim_univ())
      serialize(ph.space_dim);
    // check for well-formedness.
    assert(ph.OK());
    return;
  }
  serialize(ph.space_dim);
  if(ph.status.test_c_up_to_date())
    serialize(ph.con_sys);
  if(ph.status.test_g_up_to_date())
    serialize(ph.gen_sys);
  if(ph.status.test_sat_c_up_to_date())
     serialize(ph.sat_c);
  if(ph.status.test_sat_g_up_to_date())
    serialize(ph.sat_g);
  // check for well-formedness.
  assert(ph.OK());
}

void
PPL::Serializer::deserialize(Polyhedron& ph) {
#if ENDIAN_SOLVER
  get_file_endian();
#endif
  deserialize(ph.status);
  if( ph.status.test_empty() ) {
    if(  !ph.status.test_zero_dim_univ() )
      deserialize(ph.space_dim);
    // check for well-formedness.
    assert(ph.OK());
    return;
  }
  deserialize(ph.space_dim);
  if( ph.status.test_c_up_to_date() )
    deserialize(ph.con_sys);
  if( ph.status.test_g_up_to_date() )
    deserialize(ph.gen_sys);
  if( ph.status.test_sat_c_up_to_date() )
    deserialize(ph.sat_c);
   if( ph.status.test_sat_g_up_to_date() )
     deserialize(ph.sat_g);
  // check for well-formedness.
  assert(ph.OK());
}

void
PPL::Serializer::serialize(const Bit_Matrix& sat) {
#if ENDIAN_SOLVER
  put_file_endian();
#endif
  size_t num_rows = sat.num_rows();
  serialize(num_rows);
  serialize(sat.row_size);
  for (size_t i = 0; i < num_rows; ++i)
    serialize(sat[i]);
  // check for well-formedness.
  assert(sat.OK());
}

void
PPL::Serializer::deserialize(Bit_Matrix& sat) {
#if ENDIAN_SOLVER
  get_file_endian();
#endif
  size_t num_rows;
  deserialize(num_rows);
  size_t num_columns;
  deserialize(num_columns);
  Bit_Matrix tmp(num_rows, num_columns);
  for(size_t i = 0; i < num_rows; ++i)
    deserialize(tmp[i]);
  // check for well-formedness.
  assert(tmp.OK());
  std::swap(sat, tmp);
}

void
PPL::IO_Operators::operator<<(Serializer& s, const Polyhedron& ph) {
  s.open(s.path,out);
  s.serialize(ph);
  s.close();
}

void
PPL::IO_Operators::operator<<(Serializer& s, const Generator_System& gs) {
  s.open(s.path,out);
  s.serialize(gs);
  s.close();
}

void
PPL::IO_Operators::operator<<(Serializer& s, const Constraint_System& cs) {
  s.open(s.path,out);
  s.serialize(cs);
  s.close();
}

void
PPL::IO_Operators::operator<<(Serializer& s, const Bit_Matrix& sat_mat) {
  s.open(s.path,out);
  s.serialize(sat_mat);
  s.close();
}

void
PPL::IO_Operators::operator>>(Serializer& s, Polyhedron& ph) {
  s.open(s.path,in);
  s.deserialize(ph);
  s.close();
}

void
PPL::IO_Operators::operator>>(Serializer& s, Generator_System& gs) {
  s.open(s.path,in);
  s.deserialize(gs);
  s.close();
}

void
PPL::IO_Operators::operator>>(Serializer& s, Constraint_System& cs) {
  s.open(s.path,in);
  s.deserialize(cs);
  s.close();
}

void
PPL::IO_Operators::operator>>(Serializer& s, Bit_Matrix& bit_mat) {
   s.open(s.path, in);
   s.deserialize(bit_mat);
   s.close();
}
