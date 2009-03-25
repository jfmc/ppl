/* Declarations of print_ppl_*_to_buffer() functions.
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

#include "ppl_c.h"
#include "print_to_buffer.h"
#include <malloc.h>
#include <string.h>

#define DEFINE_PRINT_TO_BUFFER(Type)                                    \
char*                                                                   \
 print_ppl_##Type##_to_buffer(ppl_const_##Type##_t p,			\
                         unsigned indent_depth,                         \
                         unsigned pfll,                                 \
                         unsigned pll) {                                \
  ppl_io_format_settings_t settings = {					\
    0,            /* tr_in */                   \
    0,            /* tr_out */                  \
    8,            /* tab_width */		\
    "\n",         /* paragraph_end */           \
    {             /* wrap points */             \
      { 0, "," }, /* before, after */           \
      { " ", 0 }  /* before, aftet */           \
    },                                          \
    " ",          /* strip_wrap */              \
    {0, 0},       /* top */			\
    {0, 0},       /* bottom */			\
    {                                           \
      /* begin, length, left, alignment, fill_char, right, end */ \
      {0, 0, {0, 0  }, 0, 0, {0, 0}, "\n"}, /* EXTERN */ \
      {0, 0, {0, " "}, 0, 0, {0, 0}, 0   }, /* FIRST */ \
      {0, 0, {0, " "}, 0, 0, {0, 0}, 0   }, /* FIRSTLAST */ \
      {0, 0, {0, " "}, 0, 0, {0, 0}, 0   }, /* NEXT */ \
      {0, 0, {0, " "}, 0, 0, {0, 0}, 0   }, /* LAST */ \
      {0, 0, {0, " "}, 0, 0, {0, 0}, 0   }, /* LONGER_FIRST */ \
      {0, 0, {0, " "}, 0, 0, {0, 0}, 0   }, /* LONGER_FIRSTLAST */ \
      {0, 0, {0, " "}, 0, 0, {0, 0}, 0   }, /* LONGER_NEXT */ \
      {0, 0, {0, " "}, 0, 0, {0, 0}, 0   }, /* LONGER_LAST */ \
      {0, 0, {0, " "}, 0, 0, {0, 0}, 0   }, /* CHOPPED_FIRST */ \
      {0, 0, {0, " "}, 0, 0, {0, 0}, 0   }, /* CHOPPED_NEXT */ \
      {0, 0, {0, " "}, 0, 0, {0, 0}, 0   }, /* UNTERMINATED_FIRST */ \
      {0, 0, {0, " "}, 0, 0, {0, 0}, 0   }, /* UNTERMINATED_NEXT */	\
    }                                                                   \
  };                                                                    \
  ppl_io_ostream_t target, stream;					\
  char *buf;                                                            \
  settings.lines[PPL_IO_FORMAT_LINE_FIRST].length = pfll;               \
  settings.lines[PPL_IO_FORMAT_LINE_FIRSTLAST].length = pfll;           \
  settings.lines[PPL_IO_FORMAT_LINE_LONGER_FIRST].length = pfll;        \
  settings.lines[PPL_IO_FORMAT_LINE_LONGER_FIRSTLAST].length = pfll;    \
  settings.lines[PPL_IO_FORMAT_LINE_UNTERMINATED_FIRST].length = pfll;  \
  settings.lines[PPL_IO_FORMAT_LINE_NEXT].length = pll;                 \
  settings.lines[PPL_IO_FORMAT_LINE_LAST].length = pll;                 \
  settings.lines[PPL_IO_FORMAT_LINE_LONGER_NEXT].length = pll;          \
  settings.lines[PPL_IO_FORMAT_LINE_LONGER_LAST].length = pll;          \
  settings.lines[PPL_IO_FORMAT_LINE_UNTERMINATED_NEXT].length = pll;    \
  settings.lines[PPL_IO_FORMAT_LINE_NEXT].left.count = indent_depth;        \
  settings.lines[PPL_IO_FORMAT_LINE_LAST].left.count = indent_depth;        \
  settings.lines[PPL_IO_FORMAT_LINE_LONGER_NEXT].left.count = indent_depth; \
  settings.lines[PPL_IO_FORMAT_LINE_LONGER_LAST].left.count = indent_depth; \
  settings.lines[PPL_IO_FORMAT_LINE_UNTERMINATED_NEXT].left.count = indent_depth; \
  target = ppl_io_ostream_buffer_new();                                 \
  stream = ppl_io_ostream_format_new(target, &settings);                \
  ppl_io_write_##Type(stream, p);					\
  ppl_io_ostream_delete(stream);                                        \
  ppl_io_ostream_buffer_get(target, &buf);                              \
  ppl_io_ostream_delete(target);                                        \
  return buf;                                                           \
}

DEFINE_PRINT_TO_BUFFER(Coefficient)

DEFINE_PRINT_TO_BUFFER(Linear_Expression)

DEFINE_PRINT_TO_BUFFER(Constraint)

DEFINE_PRINT_TO_BUFFER(Constraint_System)

DEFINE_PRINT_TO_BUFFER(Generator)

DEFINE_PRINT_TO_BUFFER(Generator_System)

DEFINE_PRINT_TO_BUFFER(Congruence)

DEFINE_PRINT_TO_BUFFER(Congruence_System)

DEFINE_PRINT_TO_BUFFER(Grid_Generator)

DEFINE_PRINT_TO_BUFFER(Grid_Generator_System)

DEFINE_PRINT_TO_BUFFER(MIP_Problem)
