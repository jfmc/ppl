/* Helper macros for the implementation of Native_Integer and Checked_Integer.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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

#define PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, type)        \
                                                                              \
template <typename T>                                                         \
cl<T>                                                                         \
op(cl<T> x, type y);                                                          \
                                                                              \
template <typename T>                                                         \
cl<T>                                                                         \
op(type x, cl<T> y);

#define PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR(cl, op)                        \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, signed char)         \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, unsigned char)       \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, signed short)        \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, unsigned short)      \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, signed int)          \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, unsigned int)        \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, signed long)         \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, unsigned long)       \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, signed long long)    \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, unsigned long long)

#define PPL_DECLARE_BINARY_ARITHMETIC_OPERATORS(cl)                           \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR(cl, operator+)                         \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR(cl, operator-)                         \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR(cl, operator*)                         \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR(cl, operator/)                         \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATOR(cl, operator%)

#define PPL_DECLARE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, type)               \
                                                                              \
template <typename T>                                                         \
bool                                                                          \
op(cl<T> x, type y);                                                          \
                                                                              \
template <typename T>                                                         \
bool                                                                          \
op(type x, cl<T> y);

#define PPL_DECLARE_RELATIONAL_OPERATOR(cl, op)                               \
PPL_DECLARE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, signed char)                \
PPL_DECLARE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, unsigned char)              \
PPL_DECLARE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, signed short)               \
PPL_DECLARE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, unsigned short)             \
PPL_DECLARE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, signed int)                 \
PPL_DECLARE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, unsigned int)               \
PPL_DECLARE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, signed long)                \
PPL_DECLARE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, unsigned long)              \
PPL_DECLARE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, signed long long)           \
PPL_DECLARE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, unsigned long long)

#define PPL_DECLARE_RELATIONAL_OPERATORS(cl)                                  \
PPL_DECLARE_RELATIONAL_OPERATOR(cl, operator==)                               \
PPL_DECLARE_RELATIONAL_OPERATOR(cl, operator!=)                               \
PPL_DECLARE_RELATIONAL_OPERATOR(cl, operator<=)                               \
PPL_DECLARE_RELATIONAL_OPERATOR(cl, operator<)                                \
PPL_DECLARE_RELATIONAL_OPERATOR(cl, operator>=)                               \
PPL_DECLARE_RELATIONAL_OPERATOR(cl, operator>)

#define PPL_INTEGER_DECLARE_NON_MEMBERS(cl)                                   \
PPL_DECLARE_BINARY_ARITHMETIC_OPERATORS(cl)                                   \
PPL_DECLARE_RELATIONAL_OPERATORS(cl)


#define PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, type)         \
                                                                              \
template <typename T>                                                         \
cl<T>                                                                         \
op(cl<T> x, type y) {                                                         \
  return op(x, cl<T>(y));                                                     \
}                                                                             \
                                                                              \
template <typename T>                                                         \
cl<T>                                                                         \
op(type x, cl<T> y) {                                                         \
  return op(cl<T>(x), y);                                                     \
}

#define PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR(cl, op)                         \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, signed char)          \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, unsigned char)        \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, signed short)         \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, unsigned short)       \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, signed int)           \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, unsigned int)         \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, signed long)          \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, unsigned long)        \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, signed long long)     \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR_WITH_TYPE(cl, op, unsigned long long)

#define PPL_DEFINE_BINARY_ARITHMETIC_OPERATORS(cl)                            \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR(cl, operator+)                          \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR(cl, operator-)                          \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR(cl, operator*)                          \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR(cl, operator/)                          \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATOR(cl, operator%)

#define PPL_DEFINE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, type)                \
                                                                              \
template <typename T>                                                         \
bool                                                                          \
op(cl<T> x, type y) {                                                         \
  return op(x, cl<T>(y));                                                     \
}                                                                             \
                                                                              \
template <typename T>                                                         \
bool                                                                          \
op(type x, cl<T> y) {                                                         \
  return op(cl<T>(x), y);                                                     \
}

#define PPL_DEFINE_RELATIONAL_OPERATOR(cl, op)                                \
PPL_DEFINE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, signed char)                 \
PPL_DEFINE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, unsigned char)               \
PPL_DEFINE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, signed short)                \
PPL_DEFINE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, unsigned short)              \
PPL_DEFINE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, signed int)                  \
PPL_DEFINE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, unsigned int)                \
PPL_DEFINE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, signed long)                 \
PPL_DEFINE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, unsigned long)               \
PPL_DEFINE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, signed long long)            \
PPL_DEFINE_RELATIONAL_OPERATOR_WITH_TYPE(cl, op, unsigned long long)

#define PPL_DEFINE_RELATIONAL_OPERATORS(cl)                                   \
PPL_DEFINE_RELATIONAL_OPERATOR(cl, operator==)                                \
PPL_DEFINE_RELATIONAL_OPERATOR(cl, operator!=)                                \
PPL_DEFINE_RELATIONAL_OPERATOR(cl, operator<=)                                \
PPL_DEFINE_RELATIONAL_OPERATOR(cl, operator<)                                 \
PPL_DEFINE_RELATIONAL_OPERATOR(cl, operator>=)                                \
PPL_DEFINE_RELATIONAL_OPERATOR(cl, operator>)

#define PPL_INTEGER_DEFINE_NON_MEMBERS(cl)                                    \
PPL_DEFINE_BINARY_ARITHMETIC_OPERATORS(cl)                                    \
PPL_DEFINE_RELATIONAL_OPERATORS(cl)
