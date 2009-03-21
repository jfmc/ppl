/* Implementation of pretty print helpers: inline functions.
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_pretty_print_inlines_hh
#define PPL_pretty_print_inlines_hh 1

#include <sstream>
#include <cstdio>
// CHECKME: is this needed?
// #include <memory.h>

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

inline
Write_To_Stream::Write_To_Stream(std::ostream& str)
  : os(str) {
}

inline size_t
Write_To_Stream::write(const char* buf, size_t size) {
  os.write(buf, size);
  return os ? size : 0;
}

inline size_t
write_to_stdio(void* data, const char* buf, size_t size) {
  FILE* fp = (FILE*) data;
  return fwrite(buf, 1, size, fp);
}

inline size_t
write_to_count(void*, const char*, size_t size) {
  return size;
}

template <typename T>
inline size_t
pretty_print(const T& o, Write_Function& wfunc,
             unsigned indent_depth,
             unsigned preferred_first_line_length,
             unsigned preferred_line_length) {
  std::ostringstream s;
  s << o;
  return wrap(wfunc, s.str().c_str(),
              indent_depth,
              preferred_first_line_length,
              preferred_line_length);
}

template <typename T>
inline size_t
c_pretty_print(const T& o, write_function wfunc, void* data,
               unsigned indent_depth,
               unsigned preferred_first_line_length,
               unsigned preferred_line_length) {
  Write_Function_C_Wrapper w(wfunc, data);
  return pretty_print(o, w,
                      indent_depth,
                      preferred_first_line_length,
		      preferred_line_length);
}

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_pretty_print_inlines_hh)
