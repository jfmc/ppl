/* Specializations of std::numeric_limits for "checked" types.
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
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_checked_numeric_limits_hh
#define PPL_checked_numeric_limits_hh 1

#include "Checked_Number.defs.hh"
#include "checked_int.inlines.hh"
#include <limits>

namespace std {

#define SPECIALIZE_LIMITS_INT(T) \
template <typename Policy> \
class \
numeric_limits<Parma_Polyhedra_Library::Checked_Number<T, Policy> > \
  : public numeric_limits<T> { \
private: \
  typedef Parma_Polyhedra_Library::Checked_Number<T, Policy> Type; \
 \
public: \
  static const bool has_infinity = Policy::store_infinity; \
  static const bool has_quiet_NaN =  Policy::store_nan; \
 \
  static Type min() { \
    return Parma_Polyhedra_Library::Checked::min_int<Policy, T>(); \
  } \
 \
  static Type max() { \
    return Parma_Polyhedra_Library::Checked::max_int<Policy, T>(); \
  } \
 \
  static Type infinity() { \
    return \
      Policy::store_infinity \
      ? Parma_Polyhedra_Library::PLUS_INFINITY \
      : static_cast<Type>(0); \
  } \
 \
  static Type quiet_NaN() { \
    return \
      Policy::store_nan \
      ? Parma_Polyhedra_Library::NOT_A_NUMBER \
      : static_cast<Type>(0); \
  } \
};

SPECIALIZE_LIMITS_INT(signed char)
SPECIALIZE_LIMITS_INT(signed short)
SPECIALIZE_LIMITS_INT(signed long)
SPECIALIZE_LIMITS_INT(signed long long)

#define SPECIALIZE_LIMITS_FLOAT(T) \
template <typename Policy> \
struct \
numeric_limits<Parma_Polyhedra_Library::Checked_Number<T, Policy> > \
  : public numeric_limits<T> { \
};

SPECIALIZE_LIMITS_FLOAT(float)
SPECIALIZE_LIMITS_FLOAT(double)
SPECIALIZE_LIMITS_FLOAT(long double)

template <typename Policy>
class
numeric_limits<Parma_Polyhedra_Library::Checked_Number<mpz_class, Policy> > {
private:
  typedef Parma_Polyhedra_Library::Checked_Number<mpz_class, Policy> Type;

public:
  static const bool is_specialized = false;
  static const int digits = 0;
  static const int digits10 = 0;
  static const bool is_signed = false;
  static const bool is_integer = true;
  static const bool is_exact = true;
  static const int radix = 2;
  static const int min_exponent = 0;
  static const int min_exponent10 = 0;
  static const int max_exponent = 0;
  static const int max_exponent10 = 0;
  static const bool has_infinity = Policy::store_infinity;
  static const bool has_quiet_NaN =  Policy::store_nan;
  static const bool has_signaling_NaN = false;
  static const float_denorm_style has_denorm = denorm_absent;
  static const bool has_denorm_loss = false;
  static const bool is_iec559 = false;
  static const bool is_bounded = false;
  static const bool is_modulo = false;
  static const bool traps = false;
  static const bool tinyness_before = false;
  static const float_round_style round_style = round_toward_zero;

  static Type min() {
    return static_cast<Type>(0);
  }

  static Type max() {
    return static_cast<Type>(0);
  }

  static Type epsilon() {
    return static_cast<Type>(1);
  }

  static Type round_error() {
    return static_cast<Type>(1);
  }

  static Type infinity() {
    return
      Policy::store_infinity
      ? Parma_Polyhedra_Library::PLUS_INFINITY
      : static_cast<Type>(0);
  }

  static Type quiet_NaN() {
    return
      Policy::store_nan
      ? Parma_Polyhedra_Library::NOT_A_NUMBER
      : static_cast<Type>(0);
  }

  static Type denorm_min() {
    return static_cast<Type>(1);
  }
};

template <typename Policy>
class
numeric_limits<Parma_Polyhedra_Library::Checked_Number<mpq_class, Policy> > {
private:
  typedef Parma_Polyhedra_Library::Checked_Number<mpq_class, Policy> Type;

public:
  static const bool is_specialized = false;
  static const int digits = 0;
  static const int digits10 = 0;
  static const bool is_signed = false;
  static const bool is_integer = false;
  static const bool is_exact = true;
  static const int radix = 2;
  static const int min_exponent = 0;
  static const int min_exponent10 = 0;
  static const int max_exponent = 0;
  static const int max_exponent10 = 0;
  static const bool has_infinity = Policy::store_infinity;
  static const bool has_quiet_NaN =  Policy::store_nan;
  static const bool has_signaling_NaN = false;
  static const float_denorm_style has_denorm = denorm_absent;
  static const bool has_denorm_loss = false;
  static const bool is_iec559 = false;
  static const bool is_bounded = false;
  static const bool is_modulo = false;
  static const bool traps = false;
  static const bool tinyness_before = false;
  static const float_round_style round_style = round_toward_zero;

  static Type min() {
    return static_cast<Type>(0);
  }

  static Type max() {
    return static_cast<Type>(0);
  }

  static Type epsilon() {
    return static_cast<Type>(0);
  }

  static Type round_error() {
    return static_cast<Type>(0);
  }

  static Type infinity() {
    return
      Policy::store_infinity
      ? Parma_Polyhedra_Library::PLUS_INFINITY
      : static_cast<Type>(0);
  }

  static Type quiet_NaN() {
    return
      Policy::store_nan
      ? Parma_Polyhedra_Library::NOT_A_NUMBER
      : static_cast<Type>(0);
  }

  static Type denorm_min() {
    return static_cast<Type>(0);
  }
};

} // namespace std

#endif // !defined(PPL_checked_numeric_limits_hh)
