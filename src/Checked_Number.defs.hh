/* Abstract checked arithmetic with exception throwing
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

#ifndef PPL_Checked_Number_defs_hh
#define PPL_Checked_Number_defs_hh 1

#include <iostream>
#include <gmpxx.h>
#include "float.types.hh"
#include "Checked_Number.types.hh"
#include "checked.defs.hh"

#ifndef REF
#define REF &
#endif

namespace Parma_Polyhedra_Library {

template <typename T, typename Policy>
class Checked_Number {

public:

  //! Default constructor.
  Checked_Number();

#if 0
  /*
    Don't enable this: the presence of a copy constructor (also 
    if defined identical to default one) inhibit some optimizations
    in gcc (verified with 3.4.1)
  */
  Checked_Number(const Checked_Number& y);
#endif

  Checked_Number(const signed char y);
  Checked_Number(const short y);
  Checked_Number(const int y);
  Checked_Number(const long y);
  Checked_Number(const long long y);
  Checked_Number(const unsigned char y);
  Checked_Number(const unsigned short y);
  Checked_Number(const unsigned int y);
  Checked_Number(const unsigned long y);
  Checked_Number(const unsigned long long y);
  Checked_Number(const float32_t y);
  Checked_Number(const float64_t y);
#ifdef FLOAT96_TYPE
  Checked_Number(const float96_t y);
#endif
#ifdef FLOAT128_TYPE
  Checked_Number(const float128_t y);
#endif
  Checked_Number(const mpq_class& y);
  Checked_Number(const mpz_class& y);

  operator T() const;
  T& raw_value();
  const T& raw_value() const;

  Checked_Number& operator=(const Checked_Number REF y);
  Checked_Number& operator+=(const Checked_Number REF y);
  Checked_Number& operator-=(const Checked_Number REF y);
  Checked_Number& operator*=(const Checked_Number REF y);
  Checked_Number& operator/=(const Checked_Number REF y);
  Checked_Number& operator%=(const Checked_Number REF y);
  Checked_Number& operator++();
  Checked_Number  operator++(int);
  Checked_Number& operator--();
  Checked_Number  operator--(int);

  //! Swaps \p *this with \p y.
  void swap(Checked_Number& y);

private:
  T v;

};

template <typename T, typename Policy>
Checked_Number<T, Policy> operator+(const Checked_Number<T, Policy> REF x);

template <typename T, typename Policy>
Checked_Number<T, Policy> operator-(const Checked_Number<T, Policy> REF x);

template <typename T, typename Policy>
Checked_Number<T, Policy> operator+(const Checked_Number<T, Policy> REF x, const Checked_Number<T, Policy> REF y);

template <typename T, typename Policy>
Checked_Number<T, Policy> operator-(const Checked_Number<T, Policy> REF x, const Checked_Number<T, Policy> REF y);

template <typename T, typename Policy>
Checked_Number<T, Policy> operator*(const Checked_Number<T, Policy> REF x, const Checked_Number<T, Policy> REF y);

template <typename T, typename Policy>
Checked_Number<T, Policy> operator/(const Checked_Number<T, Policy> REF x, const Checked_Number<T, Policy> REF y);

template <typename T, typename Policy>
Checked_Number<T, Policy> operator%(const Checked_Number<T, Policy> REF x, const Checked_Number<T, Policy> REF y);

template <typename T, typename Policy>
bool operator==(const Checked_Number<T, Policy> REF x, const Checked_Number<T, Policy> REF y);

template <typename T, typename Policy>
bool operator!=(const Checked_Number<T, Policy> REF x, const Checked_Number<T, Policy> REF y);

template <typename T, typename Policy>
bool operator>=(const Checked_Number<T, Policy> REF x, const Checked_Number<T, Policy> REF y);

template <typename T, typename Policy>
bool operator>(const Checked_Number<T, Policy> REF x, const Checked_Number<T, Policy> REF y);

template <typename T, typename Policy>
bool operator<=(const Checked_Number<T, Policy> REF x, const Checked_Number<T, Policy> REF y);

template <typename T, typename Policy>
bool operator<(const Checked_Number<T, Policy> REF x, const Checked_Number<T, Policy> REF y);

template <typename T, typename Policy>
int sgn(const Checked_Number<T, Policy> REF x);

template <typename T, typename Policy>
int cmp(const Checked_Number<T, Policy> REF x, const Checked_Number<T, Policy> REF y);

template <typename T, typename Policy>
void negate(Checked_Number<T, Policy>& x);

template <typename T, typename Policy>
void add_mul_assign(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy> REF y, const Checked_Number<T, Policy> REF z);

template <typename T, typename Policy>
void sub_mul_assign(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy> REF y, const Checked_Number<T, Policy> REF z);

template <typename T, typename Policy>
void gcd_assign(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy> REF y);

template <typename T, typename Policy>
void gcd_assign(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy> REF y, const Checked_Number<T, Policy> REF z);

template <typename T, typename Policy>
void lcm_assign(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy> REF y);

template <typename T, typename Policy>
void lcm_assign(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy> REF y, const Checked_Number<T, Policy> REF z);

template <typename T, typename Policy>
void exact_div_assign(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy> REF y);

template <typename T, typename Policy>
void exact_div_assign(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy> REF y, const Checked_Number<T, Policy> REF z);

template <typename T, typename Policy>
void sqrt_assign(Checked_Number<T, Policy>& x);

template <typename T, typename Policy>
void sqrt_assign(Checked_Number<T, Policy>& x, const Checked_Number<T, Policy> REF y);

template <typename T, typename Policy>
std::ostream& operator<<(std::ostream& os, const Checked_Number<T, Policy> REF x);

template <typename T, typename Policy>
std::istream& operator>>(std::istream& is, Checked_Number<T, Policy>& x);

} // namespace Parma_Polyhedra_Library

#include "checked_int.inlines.hh"
#include "checked_float.inlines.hh"
#include "checked_mpz.inlines.hh"
#include "checked_mpq.inlines.hh"
#include "Checked_Number.inlines.hh"

#endif // !defined(PPL_Checked_Number_defs_hh)
