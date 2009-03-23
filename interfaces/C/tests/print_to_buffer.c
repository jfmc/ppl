/* Declarations of print_ppl_*_t_to_buffer() functions.
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

#define DEFINE_PRINT_TO_BUFFER(Name)                                    \
char*                                                                   \
print_ppl_##Name##_t_to_buffer(ppl_const_##Name##_t p,			\
                         unsigned indent_depth,                         \
                         unsigned pfll,                                 \
                         unsigned pll) {                                \
  struct ppl_io_format_settings settings = {	\
    0,            /* tr_in */                   \
    0,            /* tr_out */                  \
    "\n",         /* paragraph_end */           \
    {             /* wrap points */             \
      { 0, "," }, /* before, after */           \
      { " ", 0 }  /* before, aftet */           \
    },                                          \
    " ",          /* strip_wrap */              \
    0,           /* top */                      \
    0,           /* bottom */                   \
    {                                           \
      /* length, left, left_n, left_c, right_n, right_c right, alignment, fill_char */ \
      { 0, 0, ' ', 0, 0, ' ', "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, /* FIRST */ \
      { 0, 0, ' ', 0, 0, ' ', "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, /* FIRSTLAST */ \
      { 0,  0, ' ', 0, 0, ' ', "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, /* NEXT */ \
      { 0,  0, ' ', 0, 0, ' ', "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, /* LAST */ \
      { 0,    0, ' ', 0, 0, ' ', "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, /* CHOPPED_FIRST */ \
      { 0,    0, ' ', 0, 0, ' ', "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, /* CHOPPED_NEXT */ \
      { 0, 0, ' ', 0, 0, ' ', "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, /* LONGER_FIRST */ \
      { 0, 0, ' ', 0, 0, ' ', "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, /* LONGER_FIRSTLAST */ \
      { 0,  0, ' ', 0, 0, ' ', "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, /* LONGER_NEXT */	\
      { 0,  0, ' ', 0, 0, ' ', "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, /* LONGER_LAST */	\
      { 0, 0, ' ', 0, 0, ' ', "",   PPL_IO_FORMAT_ALIGN_LEFT, 0 }, /* UNTERMINATED_FIRST */ \
      { 0,  0, ' ', 0, 0, ' ', "",   PPL_IO_FORMAT_ALIGN_LEFT, 0 }  /* UNTERMINATED_NEXT */ \
    }									\
  };									\
  struct ppl_io_ostream *target, *stream;				\
  char *buf;								\
  settings.lines[PPL_IO_FORMAT_LINE_FIRST].length = pfll;		\
  settings.lines[PPL_IO_FORMAT_LINE_FIRSTLAST].length = pfll;		\
  settings.lines[PPL_IO_FORMAT_LINE_LONGER_FIRST].length = pfll;	\
  settings.lines[PPL_IO_FORMAT_LINE_LONGER_FIRSTLAST].length = pfll;	\
  settings.lines[PPL_IO_FORMAT_LINE_UNTERMINATED_FIRST].length = pfll;	\
  settings.lines[PPL_IO_FORMAT_LINE_NEXT].length = pll;			\
  settings.lines[PPL_IO_FORMAT_LINE_LAST].length = pll;			\
  settings.lines[PPL_IO_FORMAT_LINE_LONGER_NEXT].length = pll;		\
  settings.lines[PPL_IO_FORMAT_LINE_LONGER_LAST].length = pll;		\
  settings.lines[PPL_IO_FORMAT_LINE_UNTERMINATED_NEXT].length = pll;	\
  settings.lines[PPL_IO_FORMAT_LINE_NEXT].left_n = indent_depth;	\
  settings.lines[PPL_IO_FORMAT_LINE_LAST].left_n = indent_depth;	\
  settings.lines[PPL_IO_FORMAT_LINE_LONGER_NEXT].left_n = indent_depth;	\
  settings.lines[PPL_IO_FORMAT_LINE_LONGER_LAST].left_n = indent_depth;	\
  settings.lines[PPL_IO_FORMAT_LINE_UNTERMINATED_NEXT].left_n = indent_depth; \
  target = ppl_io_ostream_buffer_new();					\
  stream = ppl_io_ostream_format_new(target, &settings);		\
  ppl_io_write_##Name(stream, p);					\
  ppl_io_ostream_delete(stream);					\
  ppl_io_ostream_buffer_get(target, &buf);				\
  ppl_io_ostream_delete(target);					\
  return buf;								\
}

#if 0
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
#endif
