/* c_streambuf_format class declaration.
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

#ifndef PPL_c_streambuf_format_defs_hh
#define PPL_c_streambuf_format_defs_hh 1

#include <iostream>
#include "c_streambuf_format.types.hh"
#include "c_streambuf_format_settings.h"
#include "c_streambuf.defs.hh"

namespace Parma_Polyhedra_Library {

class c_streambuf_format : public c_streambuf {
public:
  c_streambuf_format(std::ostream& stream,
		     c_streambuf_format_settings *settings);
  ~c_streambuf_format();
  void replace_settings(c_streambuf_format_settings *settings);
private:
  std::ostream& stream;
  c_streambuf_format_settings *settings;
  std::string str;
  bool first;
  unsigned int column;
  int available_length(c_streambuf_format_line_type type);
  int total_length(c_streambuf_format_line_type type, size_t n);
  void output_rep(const c_streambuf_format_settings::line::rep& rep);
  int wrap_point_before(const char *buf, int pos, int limit);
  int wrap_point_after(const char *buf, int pos, int limit);
  void add_char_untabify(char c);
  bool output_line(c_streambuf_format_line_type type, const char *s, unsigned int n);
  bool output_line1(c_streambuf_format_line_type type, const char *s, unsigned int n);
  bool output_line2(c_streambuf_format_line_type type, const char *s, unsigned int n);
  size_t cb_write(const char *buf, size_t size);
  int cb_flush();
};

} // namespace Parma_Polyhedra_Library

#include "c_streambuf_format.inlines.hh"

#endif // !defined(PPL_c_streambuf_format_defs_hh)
