/* Header file for the C interface.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef _ppl_c_h
#define _ppl_c_h 1

/*
  __BEGIN_DECLS should be used at the beginning of the C declarations,
  so that C++ compilers don't mangle their names.  __END_DECLS is used
  at the end of C declarations.
*/
#undef __BEGIN_DECLS
#undef __END_DECLS
#ifdef __cplusplus
# define __BEGIN_DECLS extern "C" {
# define __END_DECLS }
#else
# define __BEGIN_DECLS /* empty */
# define __END_DECLS /* empty */
#endif

/*
  __P is a macro used to wrap function prototypes, so that compilers
  that don't understand ANSI C prototypes still work, and ANSI C
  compilers can issue warnings about type mismatches.
*/
#undef __P
#if defined (__STDC__) || defined (_AIX) \
        || (defined (__mips) && defined (_SYSTYPE_SVR4)) \
        || defined(WIN32) || defined(__cplusplus)
# define __P(protos) protos
#else
# define __P(protos) ()
#endif

__BEGIN_DECLS

typedef struct ppl_Polyhedron_tag *ppl_Polyhedron_t;

ppl_Polyhedron_t pplPolyhedronNew __P((unsigned int dimension));
void pplPolyhedronDelete __P((ppl_Polyhedron_t ph));

__END_DECLS

#endif /* !_ppl_c_h */
