/* C++ compiler related stuff.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_compiler_hh
#define PPL_compiler_hh 1

namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \brief
  No-op function that allows to avoid unused variable warnings from
  the compiler.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename T>
inline void
used(const T&) {
}

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \brief
  No-op function that prevents the compiler to subject the argument to CSE.
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
template <typename T>
inline void avoid_cse(const T& x) {
  __asm__ __volatile__ ("" : "+m" (const_cast<T&>(x)));
}

template <bool>
struct Compile_Time_Check;

template <>
struct Compile_Time_Check<true> {
  typedef int is_true;
};

template <>
struct Compile_Time_Check<false> {
  typedef int is_false;
};

#define COMPILE_TIME_CHECK_FUNC(suf) compile_time_check_ ## suf
#define COMPILE_TIME_CHECK_AUX(e, suf)					\
  int COMPILE_TIME_CHECK_FUNC(suf)(int, Compile_Time_Check<(e)>::is_true)

/*! \brief
  Produces a compilation error if the compile-time constant \p e does
  not evaluate to <CODE>true</CODE>
*/
#define COMPILE_TIME_CHECK(e) COMPILE_TIME_CHECK_AUX(e, __LINE__)

/*
  The const_bool and const_int macros allow to easily select a trick
  that avoids linker errors when the PPL is compiled with optimization
  levels lower than -O2.
*/
#if 0
#define const_bool(var, val) static const bool var = (val)
#else
#define const_bool(var, val) enum { var = (val) }
#endif

#if 0
#define const_int(var, val) static const int var = (val)
#else
#define const_int(var, val) enum { var = (val) }
#endif

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_compiler_hh)
