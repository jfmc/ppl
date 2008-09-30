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
private:
  typedef char char_type;
  typedef std::char_traits<char_type> traits_type;
  typedef traits_type::int_type int_type;
  FILE* fp;
  int_type ungetc_buf;

protected:
  virtual int_type uflow() {
    ungetc_buf = getc(fp);
    return ungetc_buf;
  }
  virtual int_type underflow() {
    int_type c = getc(fp);
    return ungetc(c, fp);
  }
  virtual std::streamsize xsgetn(char_type* s, std::streamsize n) {
    std::streamsize r = fread(s, 1, n, fp);
    if (s > 0)
      ungetc_buf = traits_type::to_int_type(s[r - 1]);
    else
      ungetc_buf = traits_type::eof();
    return r;
  }
  virtual int_type pbackfail(int_type c = traits_type::eof()) {
    const int_type eof = traits_type::eof();
    int_type u = traits_type::eq_int_type(c, eof) ? ungetc_buf : c;
    ungetc_buf = eof;
    return traits_type::eq_int_type(u, eof) ? eof : ungetc(u, fp);
  }

  virtual std::streamsize xsputn(const char_type* s, std::streamsize n) {
    return fwrite(s, 1, n, fp);
  }
  virtual int_type overflow(int_type c = traits_type::eof()) {
    const int_type eof = traits_type::eof();
    if (traits_type::eq_int_type(c, eof))
      return fflush(fp) ? eof : traits_type::not_eof(c);
    else
      return putc(c, fp);
  }
  virtual int sync() {
    return fflush(fp);
  }
public:
  stdiobuf(FILE* file)
    : fp(file), ungetc_buf(traits_type::eof()) {
  }
};

#include "stdiobuf.inlines.hh"

#endif // !defined(PPL_stdiobuf_defs_hh)
