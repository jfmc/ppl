/* Checked_Integer class declaration.
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

#ifndef PPL_Checked_Integer_defs_hh
#define PPL_Checked_Integer_defs_hh 1

#include "Checked_Integer.types.hh"
#include "Integer_macros.hh"
#include <gmpxx.h>
#include <limits>

namespace Parma_Polyhedra_Library {

template <typename T>
Checked_Integer<T> operator+(const Checked_Integer<T> x,
			     const Checked_Integer<T> y);

template <typename T>
Checked_Integer<T> operator+(const Checked_Integer<T> x);

template <typename T>
Checked_Integer<T> operator-(const Checked_Integer<T> x,
			     const Checked_Integer<T> y);

template <typename T>
Checked_Integer<T> operator-(const Checked_Integer<T> x);

template <typename T>
Checked_Integer<T> operator*(const Checked_Integer<T> x,
			     const Checked_Integer<T> y);

template <typename T>
Checked_Integer<T> operator/(const Checked_Integer<T> x,
			     const Checked_Integer<T> y);

template <typename T>
Checked_Integer<T> operator%(const Checked_Integer<T> x,
			     const Checked_Integer<T> y);

template <typename T>
bool operator==(const Checked_Integer<T> x, const Checked_Integer<T> y);
template <typename T>
bool operator!=(const Checked_Integer<T> x, const Checked_Integer<T> y);
template <typename T>
bool operator>=(const Checked_Integer<T> x, const Checked_Integer<T> y);
template <typename T>
bool operator>(const Checked_Integer<T> x, const Checked_Integer<T> y);
template <typename T>
bool operator<=(const Checked_Integer<T> x, const Checked_Integer<T> y);
template <typename T>
bool operator<(const Checked_Integer<T> x, const Checked_Integer<T> y);

template <typename T>
int sgn(const Checked_Integer<T> x);
template <typename T>
int cmp(const Checked_Integer<T> x, const Checked_Integer<T> y);

template <typename T>
void negate(Checked_Integer<T>& x);

template <typename T>
void gcd_assign(Checked_Integer<T>& x, const Checked_Integer<T> y);

template <typename T>
void gcd_assign(Checked_Integer<T>& x,
		const Checked_Integer<T> y, const Checked_Integer<T> z);

template <typename T>
void lcm_assign(Checked_Integer<T>& x, const Checked_Integer<T> y);

template <typename T>
void lcm_assign(Checked_Integer<T>& x,
		const Checked_Integer<T> y, const Checked_Integer<T> z);

template <typename T>
void exact_div_assign(Checked_Integer<T>& x, const Checked_Integer<T> y);

template <typename T>
void exact_div_assign(Checked_Integer<T>& x,
		      const Checked_Integer<T> y, const Checked_Integer<T> z);

template <typename T>
void sqrt_assign(Checked_Integer<T>& x);

template <typename T>
void sqrt_assign(Checked_Integer<T>& x, const Checked_Integer<T> y);


template <typename T>
std::ostream& operator<<(std::ostream& os, const Checked_Integer<T> x);

template <typename T>
std::istream& operator>>(std::istream& is, Checked_Integer<T>& x);

PPL_INTEGER_DECLARE_NON_MEMBERS(Checked_Integer)

} // namespace Parma_Polyhedra_Library

template <typename T>
class Parma_Polyhedra_Library::Checked_Integer {

  friend Checked_Integer<T>
  Parma_Polyhedra_Library::operator+<T>(const Checked_Integer<T> x,
					const Checked_Integer<T> y);
  friend Checked_Integer<T>
  Parma_Polyhedra_Library::operator-<T>(const Checked_Integer<T> x,
				       const Checked_Integer<T> y);
  friend Checked_Integer<T>
  Parma_Polyhedra_Library::operator*<T>(const Checked_Integer<T> x,
				       const Checked_Integer<T> y);
  friend Checked_Integer<T>
  Parma_Polyhedra_Library::operator/<T>(const Checked_Integer<T> x,
				       const Checked_Integer<T> y);
  friend Checked_Integer<T>
  Parma_Polyhedra_Library::operator%<T>(const Checked_Integer<T> x,
				       const Checked_Integer<T> y);

  friend Checked_Integer<T>
  Parma_Polyhedra_Library::operator+<T>(const Checked_Integer<T> x);
  friend Checked_Integer<T>
  Parma_Polyhedra_Library::operator-<T>(const Checked_Integer<T> x);

  friend bool
  Parma_Polyhedra_Library::operator==<T>(const Checked_Integer<T> x,
					const Checked_Integer<T> y);
  friend bool
  Parma_Polyhedra_Library::operator!=<T>(const Checked_Integer<T> x,
					const Checked_Integer<T> y);
  friend bool
  Parma_Polyhedra_Library::operator>=<T>(const Checked_Integer<T> x,
					const Checked_Integer<T> y);
  friend bool
  Parma_Polyhedra_Library::operator> <T>(const Checked_Integer<T> x,
					const Checked_Integer<T> y);
  friend bool
  Parma_Polyhedra_Library::operator<=<T>(const Checked_Integer<T> x,
					const Checked_Integer<T> y);
  friend bool
  Parma_Polyhedra_Library::operator< <T>(const Checked_Integer<T> x,
					const Checked_Integer<T> y);

  friend int
  Parma_Polyhedra_Library::sgn<T>(const Checked_Integer<T> x);
  friend int
  Parma_Polyhedra_Library::cmp<T>(const Checked_Integer<T> x,
				 const Checked_Integer<T> y);

  friend void
  Parma_Polyhedra_Library::negate<T>(Checked_Integer<T>& x);

  friend void
  Parma_Polyhedra_Library::gcd_assign<T>(Checked_Integer<T>& x,
					const Checked_Integer<T> y);
  friend void
  Parma_Polyhedra_Library::gcd_assign<T>(Checked_Integer<T>& x,
					const Checked_Integer<T> y,
					const Checked_Integer<T> z);
  friend void
  Parma_Polyhedra_Library::lcm_assign<T>(Checked_Integer<T>& x,
					const Checked_Integer<T> y);
  friend void
  Parma_Polyhedra_Library::lcm_assign<T>(Checked_Integer<T>& x,
					const Checked_Integer<T> y,
					const Checked_Integer<T> z);
  friend void
  Parma_Polyhedra_Library::exact_div_assign<T>(Checked_Integer<T>& x,
					      const Checked_Integer<T> y);
  friend void
  Parma_Polyhedra_Library::exact_div_assign<T>(Checked_Integer<T>& x,
					      const Checked_Integer<T> y,
					      const Checked_Integer<T> z);
  friend void
  Parma_Polyhedra_Library::sqrt_assign<T>(Checked_Integer<T>& x);

  friend void
  Parma_Polyhedra_Library::sqrt_assign<T>(Checked_Integer<T>& x,
					 const Checked_Integer<T> y);

  friend std::ostream&
  Parma_Polyhedra_Library::operator<< <T>(std::ostream& os,
					 const Checked_Integer<T> x);
  friend std::istream&
  Parma_Polyhedra_Library::operator>> <T>(std::istream& is,
					 Checked_Integer<T>& x);

public:
  //! Default constructor.
  Checked_Integer();
  //! Copy-constructor.
  Checked_Integer(const Checked_Integer& y);
  //! Destructor.
  ~Checked_Integer();

  Checked_Integer(signed char z);
  Checked_Integer(unsigned char z);
  Checked_Integer(signed short z);
  Checked_Integer(unsigned short z);
  Checked_Integer(signed int z);
  Checked_Integer(unsigned int z);
  Checked_Integer(signed long z);
  Checked_Integer(unsigned long z);
  Checked_Integer(signed long long z);
  Checked_Integer(unsigned long long z);

  Checked_Integer& operator=(const Checked_Integer y);
  Checked_Integer& operator+=(const Checked_Integer y);
  Checked_Integer& operator-=(const Checked_Integer y);
  Checked_Integer& operator*=(const Checked_Integer y);
  Checked_Integer& operator/=(const Checked_Integer y);
  Checked_Integer& operator%=(const Checked_Integer y);
  Checked_Integer& operator++();
  Checked_Integer  operator++(int);
  Checked_Integer& operator--();
  Checked_Integer  operator--(int);

  //T value() const;

  Checked_Integer(const mpz_class& z);
  operator mpz_class() const;

  //! Swaps \p *this with \p y.
  void swap(Checked_Integer& y);

  //  PPL_INTEGER_DECLARE_MEMBERS(Checked_Integer)

private:
  T value_;

};

namespace std {

//! Specializes <CODE>std::numeric_limits</CODE>.
template <typename T>
class numeric_limits<Parma_Polyhedra_Library::Checked_Integer<T> >
  : public numeric_limits<T> {
public:
  //! Not a modulo type, as overflow results in an exception.
  static const bool is_modulo = false;
  static const bool is_specialized = false;
};

//! Specializes <CODE>std::swap</CODE>.
template <typename T>
void swap(Parma_Polyhedra_Library::Checked_Integer<T>& x,
	  Parma_Polyhedra_Library::Checked_Integer<T>& y);

} // namespace std

#include "Checked_Integer.inlines.hh"

#endif // !defined(PPL_Checked_Integer_defs_hh)
