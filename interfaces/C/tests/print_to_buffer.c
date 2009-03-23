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

#define DEFINE_PRINT_TO_BUFFER(Type)                                    \
char*                                                                   \
print_##Name##_to_buffer(##Type p,                                      \
                         unsigned indent_depth,                         \
                         unsigned pfll,                                 \
                         unsigned pll) {                                \
  char in[indent_depth + 1];			\
  memset(in, ' ', indent_depth);		\
  in[indent_depth] = '\0';			\
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
      /* length, left, right, alignment, fill_char */                         \
      { pfll, "", "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, /* FIRST */              \
      { pfll, "", "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, /* FIRSTLAST */          \
      { pll, in, "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, /* NEXT */               \
      { pll, in, "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, /* LAST */               \
      { 0,  "", "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, /* CHOPPED_FIRST */      \
      { 0,  "", "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, /* CHOPPED_NEXT */       \
      { pfll, "", "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, /* LONGER_FIRST */       \
      { pfll, "", "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, /* LONGER_FIRSTLAST */   \
      { pll, in, "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, /* LONGER_NEXT */        \
      { pll, in, "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, /* LONGER_LAST */        \
      { pfll, "", "",   PPL_IO_FORMAT_ALIGN_LEFT, 0 }, /* UNTERMINATED_FIRST */ \
      { pll, in, "",   PPL_IO_FORMAT_ALIGN_LEFT, 0 }  /* UNTERMINATED_NEXT */  \
    }									      \
  };									\
  struct ppl_io_ostream* target = ppl_io_ostream_buffer_new();		\
  struct ppl_io_ostream* stream = ppl_io_ostream_format_new(target, &settings); \
  ppl_io_write_##NAME(stream, object);					\
  ppl_io_ostream_delete(stream);					\
  char *buf;								\
  ppl_io_ostream_buffer_get(target, &buf);				\
  ppl_io_ostream_delete(target);					\
  return buf;								\
}

DEFINE_PRINT_TO_BUFFER(string, char);

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
