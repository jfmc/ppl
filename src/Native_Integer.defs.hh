/* Native_Integer class declaration.
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

#ifndef PPL_Native_Integer_defs_hh
#define PPL_Native_Integer_defs_hh 1

#include "Native_Integer.types.hh"
#include "Integer_macros.hh"
#include <gmpxx.h>
#include <limits>

namespace Parma_Polyhedra_Library {

template <typename T>
Native_Integer<T> operator+(const Native_Integer<T> x,
			    const Native_Integer<T> y);

template <typename T>
Native_Integer<T> operator+(const Native_Integer<T> x);

template <typename T>
Native_Integer<T> operator-(const Native_Integer<T> x,
			    const Native_Integer<T> y);

template <typename T>
Native_Integer<T> operator-(const Native_Integer<T> x);

template <typename T>
Native_Integer<T> operator*(const Native_Integer<T> x,
			    const Native_Integer<T> y);

template <typename T>
Native_Integer<T> operator/(const Native_Integer<T> x,
			    const Native_Integer<T> y);

template <typename T>
Native_Integer<T> operator%(const Native_Integer<T> x,
			    const Native_Integer<T> y);

template <typename T>
bool operator==(const Native_Integer<T> x, const Native_Integer<T> y);
template <typename T>
bool operator!=(const Native_Integer<T> x, const Native_Integer<T> y);
template <typename T>
bool operator>=(const Native_Integer<T> x, const Native_Integer<T> y);
template <typename T>
bool operator>(const Native_Integer<T> x, const Native_Integer<T> y);
template <typename T>
bool operator<=(const Native_Integer<T> x, const Native_Integer<T> y);
template <typename T>
bool operator<(const Native_Integer<T> x, const Native_Integer<T> y);

template <typename T>
int sgn(const Native_Integer<T> x);
template <typename T>
int cmp(const Native_Integer<T> x, const Native_Integer<T> y);

template <typename T>
void negate(Native_Integer<T>& x);

template <typename T>
void gcd_assign(Native_Integer<T>& x, const Native_Integer<T> y);

template <typename T>
void gcd_assign(Native_Integer<T>& x,
		const Native_Integer<T> y, const Native_Integer<T> z);

template <typename T>
void lcm_assign(Native_Integer<T>& x, const Native_Integer<T> y);

template <typename T>
void lcm_assign(Native_Integer<T>& x,
		const Native_Integer<T> y, const Native_Integer<T> z);

template <typename T>
void exact_div_assign(Native_Integer<T>& x, const Native_Integer<T> y);

template <typename T>
void exact_div_assign(Native_Integer<T>& x,
		      const Native_Integer<T> y, const Native_Integer<T> z);

template <typename T>
void sqrt_assign(Native_Integer<T>& x);

template <typename T>
void sqrt_assign(Native_Integer<T>& x, const Native_Integer<T> y);


template <typename T>
std::ostream& operator<<(std::ostream& os, const Native_Integer<T> x);

template <typename T>
std::istream& operator>>(std::istream& is, Native_Integer<T>& x);

PPL_INTEGER_DECLARE_NON_MEMBERS(Native_Integer)

} // namespace Parma_Polyhedra_Library

template <typename T>
class Parma_Polyhedra_Library::Native_Integer {

  friend Native_Integer
  Parma_Polyhedra_Library::operator+<>(const Native_Integer x,
				       const Native_Integer y);
  friend Native_Integer
  Parma_Polyhedra_Library::operator-<>(const Native_Integer x,
				       const Native_Integer y);
  friend Native_Integer
  Parma_Polyhedra_Library::operator*<>(const Native_Integer x,
				       const Native_Integer y);
  friend Native_Integer
  Parma_Polyhedra_Library::operator/<>(const Native_Integer x,
				       const Native_Integer y);
  friend Native_Integer
  Parma_Polyhedra_Library::operator%<>(const Native_Integer x,
				       const Native_Integer y);

  friend Native_Integer
  Parma_Polyhedra_Library::operator+<>(const Native_Integer x);
  friend Native_Integer
  Parma_Polyhedra_Library::operator-<>(const Native_Integer x);

  friend bool
  Parma_Polyhedra_Library::operator==<>(const Native_Integer x,
					const Native_Integer y);
  friend bool
  Parma_Polyhedra_Library::operator!=<>(const Native_Integer x,
					const Native_Integer y);
  friend bool
  Parma_Polyhedra_Library::operator>=<>(const Native_Integer x,
					const Native_Integer y);
  friend bool
  Parma_Polyhedra_Library::operator> <>(const Native_Integer x,
					const Native_Integer y);
  friend bool
  Parma_Polyhedra_Library::operator<=<>(const Native_Integer x,
					const Native_Integer y);
  friend bool
  Parma_Polyhedra_Library::operator< <>(const Native_Integer x,
					const Native_Integer y);

  friend int
  Parma_Polyhedra_Library::sgn<>(const Native_Integer x);
  friend int
  Parma_Polyhedra_Library::cmp<>(const Native_Integer x,
				 const Native_Integer y);

  friend void
  Parma_Polyhedra_Library::negate<>(Native_Integer& x);

  friend void
  Parma_Polyhedra_Library::gcd_assign<>(Native_Integer<T>& x,
					const Native_Integer<T> y);
  friend void
  Parma_Polyhedra_Library::gcd_assign<>(Native_Integer<T>& x,
					const Native_Integer<T> y,
					const Native_Integer<T> z);
  friend void
  Parma_Polyhedra_Library::lcm_assign<>(Native_Integer<T>& x,
					const Native_Integer<T> y);
  friend void
  Parma_Polyhedra_Library::lcm_assign<>(Native_Integer<T>& x,
					const Native_Integer<T> y,
					const Native_Integer<T> z);
  friend void
  Parma_Polyhedra_Library::exact_div_assign<>(Native_Integer<T>& x,
					      const Native_Integer<T> y);
  friend void
  Parma_Polyhedra_Library::exact_div_assign<>(Native_Integer<T>& x,
					      const Native_Integer<T> y,
					      const Native_Integer<T> z);
  friend void
  Parma_Polyhedra_Library::sqrt_assign<>(Native_Integer<T>& x);

  friend void
  Parma_Polyhedra_Library::sqrt_assign<>(Native_Integer<T>& x,
					 const Native_Integer<T> y);

  friend std::ostream&
  Parma_Polyhedra_Library::operator<< <>(std::ostream& os,
					 const Native_Integer<T> x);
  friend std::istream&
  Parma_Polyhedra_Library::operator>> <>(std::istream& is,
					 Native_Integer<T>& x);

public:
  //! Default constructor.
  Native_Integer();
  //! Copy-constructor.
  Native_Integer(const Native_Integer& y);
  //! Destructor.
  ~Native_Integer();

  Native_Integer(signed char z);
  Native_Integer(unsigned char z);
  Native_Integer(signed short z);
  Native_Integer(unsigned short z);
  Native_Integer(signed int z);
  Native_Integer(unsigned int z);
  Native_Integer(signed long z);
  Native_Integer(unsigned long z);
  Native_Integer(signed long long z);
  Native_Integer(unsigned long long z);

  Native_Integer& operator=(const Native_Integer y);
  Native_Integer& operator+=(const Native_Integer y);
  Native_Integer& operator-=(const Native_Integer y);
  Native_Integer& operator*=(const Native_Integer y);
  Native_Integer& operator/=(const Native_Integer y);
  Native_Integer& operator%=(const Native_Integer y);
  Native_Integer& operator++();
  Native_Integer  operator++(int);
  Native_Integer& operator--();
  Native_Integer  operator--(int);

  Native_Integer(const mpz_class& z);
  operator mpz_class() const;

  //! Swaps \p *this with \p y.
  void swap(Native_Integer& y);

  //  PPL_INTEGER_DECLARE_MEMBERS(Native_Integer)

private:
  T value_;

};

namespace std {

//! Specializes <CODE>std::numeric_limits</CODE>.
template <typename T>
class numeric_limits<Parma_Polyhedra_Library::Native_Integer<T> >
  : public numeric_limits<T> {
public:
  static const bool is_specialized = false;
};

//! Specializes <CODE>std::swap</CODE>.
template <typename T>
void swap(Parma_Polyhedra_Library::Native_Integer<T>& x,
	  Parma_Polyhedra_Library::Native_Integer<T>& y);

} // namespace std

#include "Native_Integer.inlines.hh"

#endif // !defined(PPL_Native_Integer_defs_hh)
