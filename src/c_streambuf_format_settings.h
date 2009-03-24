/* Declarations for the formatted output facility settings.
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

#ifndef PPL_c_streambuf_format_settings_h
#define PPL_c_streambuf_format_settings_h 1

enum c_streambuf_format_line_type {
  PPL_IO_FORMAT_LINE_FIRST = 0,
  PPL_IO_FORMAT_LINE_FIRSTLAST,
  PPL_IO_FORMAT_LINE_NEXT,
  PPL_IO_FORMAT_LINE_LAST,
  PPL_IO_FORMAT_LINE_CHOPPED_FIRST,
  PPL_IO_FORMAT_LINE_CHOPPED_NEXT,
  PPL_IO_FORMAT_LINE_LONGER_FIRST,
  PPL_IO_FORMAT_LINE_LONGER_FIRSTLAST,
  PPL_IO_FORMAT_LINE_LONGER_NEXT,
  PPL_IO_FORMAT_LINE_LONGER_LAST,
  PPL_IO_FORMAT_LINE_UNTERMINATED_FIRST,
  PPL_IO_FORMAT_LINE_UNTERMINATED_NEXT,
  PPL_IO_FORMAT_LINE_END
};
#define PPL_IO_FORMAT_ALIGN_LEFT 0
#define PPL_IO_FORMAT_ALIGN_CENTER 8
#define PPL_IO_FORMAT_ALIGN_RIGHT 16
#define PPL_IO_FORMAT_WRAP_POINTS 2

struct c_streambuf_format_settings {
  /* Char conversion table */
  const char *tr_in;
  const char *tr_out;
  /* String for detect end of paragraph */
  const char *paragraph_end;
  /* Lines can be wrapped on any of this characters */
  /* If none from wrap_at[n] is found then wrap_at[n+1] is used. */
  struct {
    const char *before;
    const char *after;
  } wrap_points[PPL_IO_FORMAT_WRAP_POINTS];
  /* Any of these characters are stripped at and after wrap point */
  const char *strip_wrap;
  /* This string is put at beginning of paragraph */
  const char *top;
  /* This string is put at end of paragraph */
  const char *bottom;
  struct {
    /* Length of line */
    unsigned int length;
    /* Left margin string */
    const char *left;
    /* Left margin repeated char count */
    unsigned int left_n;
    /* Left margin repeated char */
    char left_c;
    /* Right margin repeated char count */
    unsigned int right_n;
    /* Right margin repeated char */
    unsigned int right_c;
    /* Right margin string */
    const char *right;
    /* 0 left, 8 center, 16 right */
    unsigned int alignment;
    /* This char is used to fill line length */
    char fill_char;
  } lines[PPL_IO_FORMAT_LINE_END];
};

#endif /* !defined(PPL_c_streambuf_format_settings_h) */
