/* ia32 Floating point unit related functions.
   Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "compiler.hh"

#define FPU_INVALID      0x01
#define FPU_DIVBYZERO    0x04
#define FPU_OVERFLOW     0x08
#define FPU_UNDERFLOW    0x10
#define FPU_INEXACT      0x20

#define FPU_ALL_EXCEPT \
        (FPU_INEXACT | FPU_DIVBYZERO | FPU_UNDERFLOW | FPU_OVERFLOW | FPU_INVALID)

#define FPU_TONEAREST    0
#define FPU_DOWNWARD     0x400
#define FPU_UPWARD       0x800
#define FPU_TOWARDZERO   0xc00

#define FPU_ROUNDING_MASK 0xc00
#ifndef FPU_CONTROL_DEFAULT
#define FPU_CONTROL_DEFAULT 0x37f
#endif

#ifndef HIJACK_FPU
#define HIJACK_FPU 1
#endif

namespace Parma_Polyhedra_Library {

typedef struct
{
  unsigned short control_word;
  unsigned short unused1;
  unsigned short status_word;
  unsigned short unused2;
  unsigned short tags;
  unsigned short unused3;
  unsigned int eip;
  unsigned short cs_selector;
  unsigned int opcode:11;
  unsigned int unused4:5;
  unsigned int data_offset;
  unsigned short data_selector;
  unsigned short unused5;
} fenv_t;

inline int
fpu_get_control() {
  unsigned short cw;
  __asm__ __volatile__ ("fnstcw %0" : "=m" (cw));
  return cw;
}

inline void
fpu_set_control(unsigned short cw) {
  __asm__ __volatile__ ("fldcw %0" : : "m" (cw));
}

inline int
fpu_get_status() {
  int sw;
  __asm__ __volatile__ ("fnstsw %0" : "=a" (sw));
  return sw;
}

inline void
fpu_clear_status(unsigned short bits) {
  /* There is no fldsw instruction */
  fenv_t env;
  __asm__ ("fnstenv %0" : "=m" (env));
  env.status_word &= ~bits;
  __asm__ ("fldenv %0" : : "m" (env));
}

inline void
fpu_clear_exceptions() {
  __asm__ __volatile__ ("fnclex" : /* No outputs.  */);
}

inline int
fpu_get_rounding_direction() {
  return fpu_get_control() & FPU_ROUNDING_MASK;
}

inline void
fpu_set_rounding_direction(int dir) {
#if HIJACK_FPU
  fpu_set_control(FPU_CONTROL_DEFAULT | dir);
#else
  int old = fpu_get_control();
  fpu_set_control((old & ~FPU_ROUNDING_MASK) | dir);
#endif
}

inline int
fpu_save_rounding_direction(int dir) {
#if HIJACK_FPU
  fpu_set_control(FPU_CONTROL_DEFAULT | dir);
  return 0;
#else
  int old = fpu_get_control();
  fpu_set_control((old & ~FPU_ROUNDING_MASK) | dir);
  return old;
#endif
}

inline void
fpu_reset_inexact() {
#if HIJACK_FPU
  fpu_clear_exceptions();
#else
  fpu_clear_status(FPU_INEXACT);
#endif
}

inline int
fpu_save_rounding_direction_reset_inexact(int dir) {
  fpu_reset_inexact();
  return fpu_save_rounding_direction(dir);
}

inline void
fpu_restore_rounding_direction(int control) {
  used(control);
#if HIJACK_FPU
  fpu_set_control(FPU_CONTROL_DEFAULT);
#else
  fpu_set_control(control);
#endif
}

inline int
fpu_check_inexact() {
  return (fpu_get_status() & FPU_INEXACT) ? 1 : 0;
}

} // namespace Parma_Polyhedra_Library
