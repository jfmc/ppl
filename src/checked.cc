/* Helper functions for checked numbers
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

#include <config.h>
#include "checked.defs.hh"

namespace Parma_Polyhedra_Library {

namespace Checked {

struct number_struct {
  unsigned int base;
  bool neg_mantissa;
  bool neg_exponent;
  std::string mantissa;
  unsigned long exponent;
};

// Returns the integer value associated with the ASCII code c if there
// is such an association, else -1.

inline int
get_digit(int c, int base = 10) {
  if (c >= '0' && c < '0' + (base > 10 ? 10 : base))
    return c - '0';
  if (base > 10) {
    base -= 10;
    if (c >= 'A' && c < 'A' + base)
      return c - 'A' + 10;
    if (c >= 'a' && c < 'a' + base)
      return c - 'a' + 10;
  }
  return -1;
}

// Adds a to b.  The signs of a and b are given by a_neg and b_neg.
// Adjusts a_neg if required.
//
// Returns false if the result would be out of bounds, else true.

inline bool
sum_sign(bool& a_neg, unsigned long& a,
	 bool b_neg, unsigned long b) {
  if (a_neg == b_neg) {
    if (a > ULONG_MAX - b)
      return false;
    a += b;
  }
  else if (a >= b)
    a -= b;
  else {
    a_neg = !a_neg;
    a = b - a;
  }
  return true;
}

// parse_number helper.  Reads the numerator or denominator part of a
// number from `is' into num.  Returns a Result.

Result
parse_number1(std::istream& is, number_struct& num) {
  num.base = 10;
  num.neg_mantissa = false;
  num.neg_exponent = false;
  num.mantissa.erase();
  num.exponent = 0;
  int c;

  // Whitespace.

  do {
    c = is.get();
  } while (isspace(c));

  // Initial sign, and possibly not-a-number or infinity.

  switch (c) {
  case '-':
    num.neg_mantissa = true;
    // Fall through.
  case '+':
    c = is.get();
    if (c != 'i' && c != 'I')
      break;
    // Else fall through.
  case 'i':
  case 'I':
    c = is.get();
    if (c != 'n' && c != 'N') {
    error:
      is.unget();
      return V_CVT_STR_UNK;
    }
    c = is.get();
    if (c != 'f' && c != 'F')
      goto error;
    return num.neg_mantissa ? VC_MINUS_INFINITY : VC_PLUS_INFINITY;
  case 'n':
  case 'N':
    c = is.get();
    if (c != 'a' && c != 'A')
      goto error;
    c = is.get();
    if (c != 'n' && c != 'N')
      goto error;
    return VC_NAN;
  }

  // Hexidecimal indicator or first mantissa/base digit.

  if (c == '0') {
    c = is.get();
    if (c == 'x' || c == 'X') {
      num.base = 16;
      c = is.get();
      goto hex;
    }
  }
  else {
    if (c < '1' || c > '9')
      goto error;
    num.mantissa += (char) c;
    c = is.get();
  }

  // Mantissa and base.

  // Leading digits.
  while (c >= '0' && c <= '9') {
    if (c != '0' || !num.mantissa.empty())
      num.mantissa += (char) c;
    c = is.get();
  }
  // Optional base.
  if (c == '^') {
    c = is.get();
    if (c != '^')
      goto error;
    num.base = 0;
    for (std::string::const_iterator i = num.mantissa.begin();
	 i != num.mantissa.end();
	 i++) {
      num.base = num.base * 10 + (*i - '0');
      if (num.base > 36)
	goto error;
    }
    if (num.base < 2)
      goto error;
    num.mantissa.erase();
    c = is.get();

  hex:
    // Mantissa digits (for '0x' hex format, or when base present).
    while (get_digit(c, num.base) >= 0) {
      // The leading digit of num.mantissa must be greater than zero.
      if (c != '0' || !num.mantissa.empty())
	num.mantissa += (char) c;
      c = is.get();
    }
  }

  // Fraction.

  long exponent_offset = 0;

  if (c == '.') {
    c = is.get();
    while (get_digit(c, num.base) >= 0) {
      exponent_offset--;
      // The leading digit of num.mantissa must be greater than zero.
      if (c != '0' || !num.mantissa.empty())
	num.mantissa += (char) c;
      c = is.get();
    }
    if (exponent_offset == 0)
      goto error;
  }

  // Exponent.

  if (c == 'e' || c == 'E')
    goto exp;
  if (c == '*') {
    c = is.get();
    if (c != '^')
      goto error;
  exp:
    c = is.get();
    if (c == '-') {
      num.neg_exponent = true;
      c = is.get();
    }
    if (c == '+')
      c = is.get();
    // Read exponent numeric digits.
    int d = get_digit(c, num.base);
    if (d < 0)
      // An exponent value is required.
      goto error;
    unsigned long max_exp_div = LONG_MAX / num.base;
    int max_exp_rem = LONG_MAX % num.base;
    while (d >= 0) {
      // Check that the exponent will be within bounds.
      if (num.exponent > max_exp_div
	  || (num.exponent == max_exp_div
	      && d > max_exp_rem))
	return V_CVT_STR_UNK;
      num.exponent = num.exponent * num.base + d;
      d = get_digit(is.get(), num.base);
    }
  }

  is.unget();

  // Transfer any trailing mantissa zeros to exponent_offset, so they
  // can be included in the exponent.
  unsigned int n = num.mantissa.size();
  while (n > 0 && num.mantissa[n - 1] == '0') {
    --n;
    exponent_offset++;
  }
  num.mantissa.erase(n);
  // Adjust the exponent to account for appending the fraction digits
  // to the mantissa.
  bool neg;
  if (exponent_offset < 0) {
    neg = true;
    exponent_offset = -exponent_offset;
  }
  else
    neg = false;
  sum_sign(num.neg_exponent, num.exponent,
	   neg, exponent_offset);
  return V_EQ;
}

// Reads a number from `is' into numerator num and denominator den,
// returning a Result.

Result
parse_number(std::istream& is, number_struct& num, number_struct& den) {
  // Read the numerator.
  Result r = parse_number1(is, num);
  if (r != V_EQ)
    return r;
  if (is.get() != '/') {
    is.unget();
    den.base = 0;
    return r;
  }
  // Read the denominator.
  r = parse_number1(is, den);
  if (r != V_EQ)
    return V_CVT_STR_UNK;
  if (num.base == den.base)
    // Ensure that one of the denominator and numerator exponents is
    // zero, leaving the other zero or positive.
    if (sum_sign(num.neg_exponent, num.exponent,
		 !den.neg_exponent, den.exponent))
      if (num.neg_exponent) {
	den.neg_exponent = false;
	den.exponent = num.exponent;
	num.exponent = 0;
      }
      else
	den.exponent = 0;
  return V_EQ;
}

// Reads a number from `is' into `to', returning a Result.

Result
input_mpq(mpq_class& to, std::istream& is) {
  number_struct num_struct;
  number_struct den_struct;
  Result r = parse_number(is, num_struct, den_struct);
  if (r != V_EQ)
    return r;
  if (den_struct.base && den_struct.mantissa.empty())
    return VC_NAN;
  if (num_struct.mantissa.empty()) {
    to = 0;
    return V_EQ;
  }
  // Convert the parsed results into GMP rational `to'.
  mpz_ptr num = to.get_num().get_mpz_t();
  mpz_ptr den = to.get_den().get_mpz_t();
  mpz_set_str(num, num_struct.mantissa.c_str(), num_struct.base);
  if (den_struct.base) {
    // There is a denominator.
    if (num_struct.neg_mantissa ^ den_struct.neg_mantissa)
      mpz_neg(num, num);
    mpz_set_str(den, den_struct.mantissa.c_str(), den_struct.base);
    if (num_struct.exponent || den_struct.exponent) {
      // Multiply the exponents into the numerator and denominator.
      mpz_t z;
      mpz_init(z);
      if (num_struct.exponent) {
	mpz_ui_pow_ui(z, num_struct.base, num_struct.exponent);
	if (num_struct.neg_exponent)
	  mpz_mul(den, den, z);
	else
	  mpz_mul(num, num, z);
      }
      if (den_struct.exponent) {
	mpz_ui_pow_ui(z, den_struct.base, den_struct.exponent);
	if (den_struct.neg_exponent)
	  mpz_mul(num, num, z);
	else
	  mpz_mul(den, den, z);
      }
      mpz_clear(z);
    }
  }
  else {
    // There is only a numerator.
    if (num_struct.neg_mantissa)
      mpz_neg(num, num);
    if (num_struct.exponent) {
      if (num_struct.neg_exponent) {
	// Add the negative exponent as a denominator.
	mpz_ui_pow_ui(den, num_struct.base, num_struct.exponent);
	goto end;
      }
      // Multiply the exponent into the numerator.
      mpz_t z;
      mpz_init(z);
      mpz_ui_pow_ui(z, num_struct.base, num_struct.exponent);
      mpz_mul(num, num, z);
      mpz_clear(z);
    }
    mpz_set_ui(den, 1);
    return V_EQ;
  }
 end:
  // GMP operators require rationals in canonical form.
  to.canonicalize();
  return V_EQ;
}

} // namespace Checked

} // namespace Parma_Polyhedra_Library
