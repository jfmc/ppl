/* Implementation of PPL assert-like macros.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2012 BUGSENG srl (http://bugseng.com)

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
site: http://bugseng.com/products/ppl/ . */

#ifndef PPL_assert_hh
#define PPL_assert_hh 1

#include "globals.defs.hh"

// The PPL_UNREACHABLE_MSG macro flags a program point as unreachable.
// Argument `msg__' is added to output when assertions are turned on.
#if defined(NDEBUG)
#define PPL_UNREACHABLE_MSG(msg__) Parma_Polyhedra_Library::ppl_unreachable()
#else
#define PPL_UNREACHABLE_MSG(msg__) Parma_Polyhedra_Library:: \
  ppl_unreachable_msg(msg__, __FILE__, __LINE__, __func__)
#endif

// The PPL_UNREACHABLE macro flags a program point as unreachable.
#define PPL_UNREACHABLE PPL_UNREACHABLE_MSG("unreachable")


// Helper macro PPL_ASSERT_IMPL_: do not use it directly.
#if defined(NDEBUG)
#define PPL_ASSERT_IMPL_(cond__) ((void) 0)
#else
#define PPL_STRING_(s) #s
#define PPL_ASSERT_IMPL_(cond__) \
  ((cond__) ? (void) 0 : PPL_UNREACHABLE_MSG(PPL_STRING_(cond__)))
#endif


// Non zero to detect use of PPL_ASSERT instead of PPL_ASSERT_HEAVY
#define PPL_DEBUG_PPL_ASSERT 1

// The PPL_ASSERT macro states that Boolean condition cond__ should hold.
// This is meant to replace uses of C assert().
#if defined(NDEBUG) || (!PPL_DEBUG_PPL_ASSERT)
#define PPL_ASSERT(cond__) PPL_ASSERT_IMPL_(cond__)
#else
#define PPL_ASSERT(cond__)                                        \
  do {                                                            \
    typedef Parma_Polyhedra_Library::Weightwatch_Traits W_Traits; \
    W_Traits::Threshold old_weight__ = W_Traits::weight;          \
    PPL_ASSERT_IMPL_(cond__);                                     \
    PPL_ASSERT_IMPL_(old_weight__ == W_Traits::weight             \
                     && "PPL_ASSERT_HEAVY have to be used here"); \
  } while(0)
#endif // !defined(NDEBUG) && PPL_DEBUG_PPL_ASSERT


// Macro PPL_ASSERT_HEAVY is meant to be used when the evaluation of
// the assertion may change computational weights (via WEIGHT_ADD).
#if defined(NDEBUG)
#define PPL_ASSERT_HEAVY(cond__) PPL_ASSERT_IMPL_(cond__)
#else
#define PPL_ASSERT_HEAVY(cond__)                                \
  do {                                                          \
    ++Parma_Polyhedra_Library::Implementation::in_assert;       \
    PPL_ASSERT_IMPL_(cond__);                                   \
    --Parma_Polyhedra_Library::Implementation::in_assert;	\
  } while (0)
#endif // !defined(NDEBUG)


// Macro PPL_EXPECT (resp., PPL_EXPECT_HEAVY) should be used rather than
// PPL_ASSERT (resp., PPL_ASSERT_HEAVY) when the condition is assumed to
// hold but it is not under library control (typically, it depends on
// user provided input).
#define PPL_EXPECT(cond__) PPL_ASSERT(cond__)
#define PPL_EXPECT_HEAVY(cond__) PPL_ASSERT_HEAVY(cond__)


namespace Parma_Polyhedra_Library {

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
//! Helper function causing program termination by calling \c abort.
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
void ppl_unreachable()
#if PPL_CXX_SUPPORTS_ATTRIBUTE_WEAK
  __attribute__((weak, noreturn));
#else
  __attribute__((noreturn));
#endif

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \brief
  Helper function printing message on \c std::cerr and causing program
  termination by calling \c abort.
*/
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
void ppl_unreachable_msg(const char* msg,
                         const char* file, unsigned int line,
                         const char* function)
#if PPL_CXX_SUPPORTS_ATTRIBUTE_WEAK
  __attribute__((weak, noreturn));
#else
  __attribute__((noreturn));
#endif

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*! \brief
  Returns \c true if and only if \p x_copy contains \p y_copy.

  \note
  This is a helper function for debugging purposes, to be used in assertions.
  The two arguments are meant to be passed by value, i.e., <em>copied</em>,
  so that their representations will not be affected by the containment check.
*/
#endif // defined(PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS)
template <typename T>
bool copy_contains(T x_copy, T y_copy) {
  return x_copy.contains(y_copy);
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_assert_hh)
