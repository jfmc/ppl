/* Declarations for the formmatted output facility.
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

#ifndef PPL_c_stream_h
#define PPL_c_stream_h 1

#include <stdio.h>
#include "c_streambuf_format_settings.h"

typedef struct ppl_io_ostream* ppl_io_ostream_t;
typedef struct c_streambuf_format_settings ppl_io_format_settings_t;

ppl_io_ostream_t ppl_io_ostream_stdio_new(FILE* fp);
ppl_io_ostream_t ppl_io_ostream_buffer_new();
ppl_io_ostream_t ppl_io_ostream_format_new(ppl_io_ostream_t stream, ppl_io_format_settings_t* settings);
void ppl_io_ostream_format_replace_settings(ppl_io_ostream_t stream, ppl_io_format_settings_t* settings);

void ppl_io_ostream_delete(ppl_io_ostream_t stream);
size_t ppl_io_ostream_buffer_get(ppl_io_ostream_t stream, char** buf);
void ppl_io_ostream_buffer_clear(ppl_io_ostream_t stream);

int ppl_io_write_endl(ppl_io_ostream_t s);

/* FIXME: */
/* Add ios_base methods: flags, setf, unsetf, width, precision (others?) */

#define DECLARE_WRITE_VAL(name, type)                                   \
  int ppl_io_write_##name(ppl_io_ostream_t s, const type o)

#define DECLARE_WRITE_REF(name, type)                                   \
  int ppl_io_write_##name(ppl_io_ostream_t s, const type* o)

DECLARE_WRITE_VAL(char, char);
DECLARE_WRITE_VAL(signed_char, signed char);
DECLARE_WRITE_VAL(unsigned_char, unsigned char);
DECLARE_WRITE_VAL(short, short);
DECLARE_WRITE_VAL(unsigned_short, unsigned short);
DECLARE_WRITE_VAL(int, int);
DECLARE_WRITE_VAL(unsigned_int, unsigned int);
DECLARE_WRITE_VAL(long, long);
DECLARE_WRITE_VAL(unsigned_long, unsigned long);
/*
DECLARE_WRITE_VAL(long_long, long long);
DECLARE_WRITE_VAL(unsigned_long_long, unsigned long long);
*/
DECLARE_WRITE_VAL(float, float);
DECLARE_WRITE_VAL(double, double);
DECLARE_WRITE_VAL(long_double, long double);
DECLARE_WRITE_VAL(string, char*);

#endif /* !defined(PPL_c_stream_h) */
