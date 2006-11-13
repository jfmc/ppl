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
};

#define COMPILE_TIME_CHECK_NAME(suf) compile_time_check_ ## suf
#define COMPILE_TIME_CHECK_AUX(e, suf)					\
  enum { COMPILE_TIME_CHECK_NAME(suf) = sizeof(Parma_Polyhedra_Library::Compile_Time_Check<(bool)(e)>) }

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \brief
  Produces a compilation error if the compile-time constant \p e does
  not evaluate to <CODE>true</CODE>
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
#define COMPILE_TIME_CHECK(e, msg) COMPILE_TIME_CHECK_AUX(e, __LINE__)

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \brief
  Declares a per-class constant of type <CODE>bool</CODE>, called \p name
  and with value \p value.

  Differently from static constants, \p name needs not (and cannot) be
  defined (for static constants, the need for a further definition is
  mandated by Section 9.4.2/4 of the C++ standard).
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
#define const_bool_nodef(name, value)		\
  enum { name = (value) }

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \brief
  Declares a per-class constant of type <CODE>int</CODE>, called \p name
  and with value \p value.

  Differently from static constants, \p name needs not (and cannot) be
  defined (for static constants, the need for a further definition is
  mandated by Section 9.4.2/4 of the C++ standard).
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
#define const_int_nodef(name, value) \
  enum { name = (value) }

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \brief
  Declares a per-class constant of type \p type, called \p name
  and with value \p value.  The value of the constant is accessible
  by means of the syntax <CODE>name()</CODE>.

  Differently from static constants, \p name needs not (and cannot) be
  defined (for static constants, the need for a further definition is
  mandated by Section 9.4.2/4 of the C++ standard).
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
#define const_value_nodef(type, name, value)	\
  static type name() {				\
    return value;				\
  }

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \brief
  Declares a per-class constant of type \p type, called \p name
  and with value \p value.  A constant reference to the constant
  is accessible by means of the syntax <CODE>name()</CODE>.

  Differently from static constants, \p name needs not (and cannot) be
  defined (for static constants, the need for a further definition is
  mandated by Section 9.4.2/4 of the C++ standard).
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
#define const_ref_nodef(type, name, value)				\
  static const type& name() {						\
    static type name(value);						\
    return name;							\
  }

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_compiler_hh)
