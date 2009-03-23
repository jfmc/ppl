/* Definitions of pretty print helper functions.
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

#include <ppl-config.h>

#include "pretty_print.hh"
#include <cassert>
#include <malloc.h>

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

struct write_buffer {
  char* str;
  size_t alloc;
  size_t len;
  int dynamic;
};

size_t
write_to_buffer_static(void* data, const char* buf, size_t size) {
  struct write_buffer* wbuf = (struct write_buffer*) data;
  size_t n = wbuf->alloc - wbuf->len;
  if (n != 0) {
    if (n > size)
      n = size;
    memcpy(wbuf->str + wbuf->len, buf, n);
    wbuf->len += n;
  }
  return n;
}

size_t
write_to_buffer_dynamic(void* data, const char* buf, size_t size) {
  struct write_buffer* wbuf = (struct write_buffer*) data;
  size_t needed = wbuf->len + size + 1;
  if (needed > wbuf->alloc) {
    if (wbuf->alloc == 0)
      wbuf->alloc = 256;
    while (needed > wbuf->alloc)
      wbuf->alloc *= 2;
    wbuf->str =(char* ) realloc(wbuf->str, wbuf->alloc);
    if (!wbuf->str)
      return 0;
  }
  memcpy(wbuf->str + wbuf->len, buf, size);
  wbuf->len += size;
  return size;
}

struct write_buffer*
write_buffer_new_dynamic(size_t initial_size) {
  struct write_buffer* wbuf = (struct write_buffer*) malloc(sizeof(*wbuf));
  if (!wbuf)
    return 0;
  wbuf->alloc = initial_size;
  if (wbuf->alloc > 0) {
    wbuf->str = (char* ) malloc(wbuf->alloc);
    if (!wbuf->str) {
      free(wbuf);
      return 0;
    }
  }
  else
    wbuf->str = 0;
  wbuf->len = 0;
  return wbuf;
}

struct write_buffer*
write_buffer_new_static(char* buf, size_t size) {
  if (size == 0)
    return 0;
  struct write_buffer* wbuf = (struct write_buffer*) malloc(sizeof(*wbuf));
  if (!wbuf)
    return 0;
  wbuf->alloc = size - 1;
  wbuf->str = buf;
  wbuf->len = 0;
  return wbuf;
}

void
write_buffer_delete(struct write_buffer* wbuf) {
  if (wbuf->dynamic > 0)
    free(wbuf->str);
  free(wbuf);
}

char*
write_buffer_get(struct write_buffer* wbuf) {
  wbuf->str[wbuf->len] = '\0';
  return wbuf->str;
}

size_t wrap(Write_Function& wfunc,
            const char* src,
            unsigned indent_depth,
            unsigned preferred_first_line_length,
            unsigned preferred_line_length) {
  size_t written = 0;
  char indent[] = "                                                                                ";
  assert(indent_depth < sizeof(indent));
  for (int line = 0; ; ++line) {
    int linelen = (line == 0
                   ? preferred_first_line_length
                   : preferred_line_length);
    int last_comma = -1;
    int last_space = -1;
    int split_pos = -1;
    int i;
    for (i = 0; i <= linelen; ++i) {
      if (src[i] == '\0' || src[i] == '\n') {
	split_pos = i;
	break;
      }
      if (src[i] == ',' && i < linelen)
	last_comma = i;
      if (isspace(src[i]) && (i == 0 || !isspace(src[i-1])))
	last_space = i;
    }
    if (split_pos < 0) {
      if (last_comma >= 0)
	split_pos = last_comma + 1;
      else if (last_space >= 0)
	split_pos = last_space;
      else {
	for ( ; src[i]; ++i) {
	  if (src[i] == ',') {
	    ++i;
	    break;
	  }
	  if (isspace(src[i]))
	    break;
	}
	split_pos = i;
      }
    }
    size_t w;
    if (split_pos > 0 && line > 0) {
      if (indent_depth > 0) {
	w = wfunc.write(indent, indent_depth);
	written += w;
	if (w < indent_depth)
	  break;
      }
    }
    w = wfunc.write(src, split_pos);
    written += w;
    if (w < (size_t) split_pos)
      break;
    src += split_pos;
    if (isspace(*src))
      ++src;
    while (*src == ' ')
      ++src;
    if (*src == '\0')
      break;
    w = wfunc.write("\n", 1);
    written += w;
    if (w < 1)
      break;
  }
  return written;
}

void
wrap(std::string& dst_string, const std::string& src_string,
     unsigned indent_depth,
     unsigned preferred_first_line_length,
     unsigned preferred_line_length) {
  dst_string.clear();
  const char *src = src_string.c_str();
  for (int line = 0; ; ++line) {
    int linelen = (line == 0
                   ? preferred_first_line_length
                   : preferred_line_length);
    int last_comma = -1;
    int last_space = -1;
    int split_pos = -1;
    int i;
    for (i = 0; i <= linelen; ++i) {
      if (src[i] == '\0' || src[i] == '\n') {
	split_pos = i;
	break;
      }
      if (src[i] == ',' && i < linelen)
	last_comma = i;
      if (isspace(src[i]) && (i == 0 || !isspace(src[i-1])))
	last_space = i;
    }
    if (split_pos < 0) {
      if (last_comma >= 0)
	split_pos = last_comma + 1;
      else if (last_space >= 0)
	split_pos = last_space;
      else {
	for ( ; src[i]; ++i) {
	  if (src[i] == ',') {
	    ++i;
	    break;
	  }
	  if (isspace(src[i]))
	    break;
	}
	split_pos = i;
      }
    }
    if (split_pos > 0 && line > 0 && indent_depth > 0)
	dst_string.append(indent_depth, ' ');
    dst_string.append(src, split_pos);
    src += split_pos;
    if (isspace(*src))
      ++src;
    while (*src == ' ')
      ++src;
    if (*src == '\0')
      break;
    dst_string.push_back('\n');
  }
}

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library
