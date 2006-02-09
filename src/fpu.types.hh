/* Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

This file is free software; as a special exception the author gives
unlimited permission to copy and/or distribute it, with or without
modifications, as long as this notice is preserved.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. */

#ifndef PPL_fpu_types_hh
#define PPL_fpu_types_hh 1

#ifdef HAVE_IEEEFP_H
#include <ieeefp.h>
#endif

namespace Parma_Polyhedra_Library {

#if i386
typedef int fpu_rounding_direction_type;
typedef int fpu_rounding_control_word_type;
#elif defined(HAVE_FENV_H)
typedef int fpu_rounding_direction_type;
typedef int fpu_rounding_control_word_type;
#elif sparc
typedef fp_rnd fpu_rounding_direction_type;
typedef fp_rnd fpu_rounding_control_word_type;
#else
typedef int fpu_rounding_direction_type;
typedef int fpu_rounding_control_word_type;
#endif

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_fpu_types_hh)
