/* stdiobuf class declaration.
   Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_stdiobuf_defs_hh
#define PPL_stdiobuf_defs_hh 1

#include "stdiobuf.types.hh"
#include <cstdio>
#include <streambuf>

class Parma_Polyhedra_Library::stdiobuf
  : public std::basic_streambuf<char, std::char_traits<char> > {
public:
  //! Constructor.
  stdiobuf(FILE* file);

protected:
  virtual int_type uflow();

  virtual int_type underflow();

  virtual std::streamsize xsgetn(char_type* s, std::streamsize n);

  virtual int_type pbackfail(int_type c = traits_type::eof());

  virtual std::streamsize xsputn(const char_type* s, std::streamsize n);

  virtual int_type overflow(int_type c);

  virtual int sync();

private:
  typedef char char_type;
  typedef std::char_traits<char_type> traits_type;
  typedef traits_type::int_type int_type;
  FILE* fp;
  int_type ungetc_buf;
};

#include "stdiobuf.inlines.hh"

#endif // !defined(PPL_stdiobuf_defs_hh)
