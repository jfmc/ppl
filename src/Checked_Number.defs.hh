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
#include "Checked_Number.types.hh"
#include "checked.defs.hh"

#ifndef REF
#define REF &
#endif

namespace Parma_Polyhedra_Library {

struct Checked_Number_Policy {
  static const int check_overflow = 1;
  static const int check_exact = 0;
  static const int check_divbyzero = 0;
  static const int check_sqrt_neg = 0;
  static const int check_normal = 0;
};

template <typename T>
class Checked_Number {

public:

  //! Default constructor.
  Checked_Number();

  Checked_Number(const T REF y);
 
  template <typename T1>
  Checked_Number(const T1 REF _v);

  T& value();
  const T& value() const;

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

template <typename T>
Checked_Number<T> operator+(const Checked_Number<T> REF x);

template <typename T>
Checked_Number<T> operator-(const Checked_Number<T> REF x);

template <typename T>
Checked_Number<T> operator+(const Checked_Number<T> REF x, const Checked_Number<T> REF y);

template <typename T>
Checked_Number<T> operator-(const Checked_Number<T> REF x, const Checked_Number<T> REF y);

template <typename T>
Checked_Number<T> operator*(const Checked_Number<T> REF x, const Checked_Number<T> REF y);

template <typename T>
Checked_Number<T> operator/(const Checked_Number<T> REF x, const Checked_Number<T> REF y);

template <typename T>
Checked_Number<T> operator%(const Checked_Number<T> REF x, const Checked_Number<T> REF y);

template <typename T>
bool operator==(const Checked_Number<T> REF x, const Checked_Number<T> REF y);

template <typename T>
bool operator!=(const Checked_Number<T> REF x, const Checked_Number<T> REF y);

template <typename T>
bool operator>=(const Checked_Number<T> REF x, const Checked_Number<T> REF y);

template <typename T>
bool operator>(const Checked_Number<T> REF x, const Checked_Number<T> REF y);

template <typename T>
bool operator<=(const Checked_Number<T> REF x, const Checked_Number<T> REF y);

template <typename T>
bool operator<(const Checked_Number<T> REF x, const Checked_Number<T> REF y);

template <typename T>
int sgn(const Checked_Number<T> REF x);

template <typename T>
int cmp(const Checked_Number<T> REF x, const Checked_Number<T> REF y);

template <typename T>
void negate(Checked_Number<T>& x);

template <typename T>
void gcd_assign(Checked_Number<T>& x, const Checked_Number<T> REF y);

template <typename T>
void gcd_assign(Checked_Number<T>& x, const Checked_Number<T> REF y, const Checked_Number<T> REF z);

template <typename T>
void lcm_assign(Checked_Number<T>& x, const Checked_Number<T> REF y);

template <typename T>
void lcm_assign(Checked_Number<T>& x, const Checked_Number<T> REF y, const Checked_Number<T> REF z);

template <typename T>
void exact_div_assign(Checked_Number<T>& x, const Checked_Number<T> REF y);

template <typename T>
void exact_div_assign(Checked_Number<T>& x, const Checked_Number<T> REF y, const Checked_Number<T> REF z);

template <typename T>
void sqrt_assign(Checked_Number<T>& x);

template <typename T>
void sqrt_assign(Checked_Number<T>& x, const Checked_Number<T> REF y);

template <typename T>
std::ostream& operator<<(std::ostream& os, const Checked_Number<T> REF x);

template <typename T>
std::istream& operator>>(std::istream& is, Checked_Number<T>& x);

} // namespace Parma_Polyhedra_Library

#include "Checked_Number.inlines.hh"

#endif // !defined(PPL_Checked_Number_defs_hh)
