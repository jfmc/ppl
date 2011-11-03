/* Helper functions for checked numbers
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2011 BUGSENG srl (http://bugseng.com)

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

#include "ppl-config.h"
#include "checked.defs.hh"
#include <climits>

namespace Parma_Polyhedra_Library {

Minus_Infinity MINUS_INFINITY;
Plus_Infinity PLUS_INFINITY;
Not_A_Number NOT_A_NUMBER;

namespace Checked {

//! Holds the precision parameter used for irrational calculations.
unsigned irrational_precision;

struct number_struct {
  unsigned int base;
  bool neg_mantissa;
  bool neg_exponent;
  std::string mantissa;
  unsigned int base_for_exponent;
  unsigned long exponent;
};

/*! \brief
  Returns the integer value associated with the ASCII code \p c, in
  the base \p base positional number system, if there is such an
  association; returns \f$-1\f$ otherwise.
*/
inline int
get_digit(char c, int base = 10) {
  int n;
  switch (c) {
  case '0': n = 0; break;
  case '1': n = 1; break;
  case '2': n = 2; break;
  case '3': n = 3; break;
  case '4': n = 4; break;
  case '5': n = 5; break;
  case '6': n = 6; break;
  case '7': n = 7; break;
  case '8': n = 8; break;
  case '9': n = 9; break;
  case 'a': case 'A': n = 10; break;
  case 'b': case 'B': n = 11; break;
  case 'c': case 'C': n = 12; break;
  case 'd': case 'D': n = 13; break;
  case 'e': case 'E': n = 14; break;
  case 'f': case 'F': n = 15; break;
  case 'g': case 'G': n = 16; break;
  case 'h': case 'H': n = 17; break;
  case 'i': case 'I': n = 18; break;
  case 'j': case 'J': n = 19; break;
  case 'k': case 'K': n = 20; break;
  case 'l': case 'L': n = 21; break;
  case 'm': case 'M': n = 22; break;
  case 'n': case 'N': n = 23; break;
  case 'o': case 'O': n = 24; break;
  case 'p': case 'P': n = 25; break;
  case 'q': case 'Q': n = 26; break;
  case 'r': case 'R': n = 27; break;
  case 's': case 'S': n = 28; break;
  case 't': case 'T': n = 29; break;
  case 'u': case 'U': n = 30; break;
  case 'v': case 'V': n = 31; break;
  case 'w': case 'W': n = 32; break;
  case 'x': case 'X': n = 33; break;
  case 'y': case 'Y': n = 34; break;
  case 'z': case 'Z': n = 35; break;
  default:
    return -1;
  }
  if (n >= base)
    return -1;
  return n;
}

/*! \brief
  Adds the number represented (in the modulus-and-sign representation)
  by \p b_neg and \p b_mod to the number represented by \p a_neg and
  \p a_mod, assigning the result to the latter.  Returns
  <CODE>false</CODE> is the result cannot be represented; returns
  <CODE>true</CODE> otherwise.
*/
inline bool
sum_sign(bool& a_neg, unsigned long& a_mod,
         bool b_neg, unsigned long b_mod) {
  if (a_neg == b_neg) {
    if (a_mod > ULONG_MAX - b_mod)
      return false;
    a_mod += b_mod;
  }
  else if (a_mod >= b_mod)
    a_mod -= b_mod;
  else {
    a_neg = !a_neg;
    a_mod = b_mod - a_mod;
  }
  return true;
}


/*! \brief
  Helper function for parse_number(): reads the numerator or
  denominator part of a number from \p is into \p num, returning the
  appropriate Result value.
*/
Result
parse_number_part(std::istream& is, number_struct& num) {
  enum anonymous_enum { BASE, INTEGER, FRACTIONAL, EXPONENT } state = BASE;
  PPL_UNINITIALIZED(unsigned long, max_exp_div);
  PPL_UNINITIALIZED(int, max_exp_rem);
  bool empty_exponent = true;
  bool empty_mantissa = true;
  long exponent_offset = 0;
  long exponent_offset_scale = 1;
  num.base = 10;
  num.base_for_exponent = 10;
  num.neg_mantissa = false;
  num.neg_exponent = false;
  num.mantissa.erase();
  num.exponent = 0;
  char c;
  do {
    if (!is.get(c))
      return V_CVT_STR_UNK;
  } while (isspace(c));
  switch (c) {
  case '-':
    num.neg_mantissa = true;
    // Fall through.
  case '+':
    if (!is.get(c))
      return V_CVT_STR_UNK;
    if (c == 'i' || c == 'I')
      goto inf;
    if (c != '.')
      break;
    // Fall through.
  case '.':
    state = FRACTIONAL;
    if (!is.get(c))
      return V_CVT_STR_UNK;
    break;
  case 'n':
  case 'N':
    if (!is.get(c))
      return V_CVT_STR_UNK;
    if (c != 'a' && c != 'A')
      goto unexpected;
    if (!is.get(c))
      return V_CVT_STR_UNK;
    if (c != 'n' && c != 'N')
      goto unexpected;
    return V_NAN;
  inf:
  case 'i':
  case 'I':
    if (!is.get(c))
      return V_CVT_STR_UNK;
    if (c != 'n' && c != 'n')
      goto unexpected;
    if (!is.get(c))
      return V_CVT_STR_UNK;
    if (c != 'f' && c != 'F')
      goto unexpected;
    return num.neg_mantissa ? V_EQ_MINUS_INFINITY : V_EQ_PLUS_INFINITY;
  }
  if (state != FRACTIONAL) {
    if (get_digit(c, 10) < 0)
      goto unexpected;
    char d;
    if (c == '0' && is.get(d)) {
      if (d == 'x' || d == 'X') {
        num.base = 16;
        num.base_for_exponent = 16;
        state = INTEGER;
        if (!is.get(c))
          return V_CVT_STR_UNK;
      }
      else
        is.unget();
    }
  }
  do {
    switch (state) {
    case BASE:
      if (get_digit(c, 10) >= 0) {
        if (c != '0' || !num.mantissa.empty())
          num.mantissa += (char) c;
        empty_mantissa = false;
        break;
      }
      if (c == '^') {
        if (!is.get(c))
          return V_CVT_STR_UNK;
        if (c != '^')
          goto unexpected;
        std::string::const_iterator i;
        num.base = 0;
        for (i = num.mantissa.begin(); i != num.mantissa.end(); i++) {
          num.base = num.base * 10 + (*i - '0');
          if (num.base > 36)
            goto unexpected;
        }
        if (num.base < 2)
          goto unexpected;
        num.base_for_exponent = num.base;
        num.mantissa.erase();
        empty_mantissa = true;
        state = INTEGER;
        break;
      }
      goto integer;
    case INTEGER:
      if (get_digit(c, num.base) >= 0) {
        if (c != '0' || !num.mantissa.empty())
          num.mantissa += (char) c;
        empty_mantissa = false;
        break;
      }
    integer:
      if (c == '.') {
        state = FRACTIONAL;
        break;
      }
      goto fractional;
    case FRACTIONAL:
      if (get_digit(c, num.base) >= 0) {
        --exponent_offset;
        if (c != '0' || !num.mantissa.empty())
          num.mantissa += (char) c;
        empty_mantissa = false;
        break;
      }
    fractional:
      if (empty_mantissa)
        goto unexpected;
      if (c == 'e' || c == 'E')
        goto exp;
      if (c == 'p' || c == 'P') {
        if (num.base == 16) {
          num.base_for_exponent = 2;
          exponent_offset_scale = 4;
          goto exp;
        }
        else
          goto unexpected;
      }
      if (c == '*') {
        if (!is.get(c))
          return V_CVT_STR_UNK;
        if (c != '^')
          goto unexpected;
      exp:
        state = EXPONENT;
        max_exp_div = LONG_MAX / num.base;
        max_exp_rem = static_cast<int>(LONG_MAX % num.base);
        if (!is.get(c))
          return V_CVT_STR_UNK;
        if (c == '-') {
          num.neg_exponent = true;
          break;
        }
        if (c == '+')
          break;
        continue;
      }
      is.unget();
      goto ok;
    case EXPONENT:
      int d = get_digit(c, 10);
      if (d >= 0) {
        empty_exponent = false;
        if (num.exponent > max_exp_div
            || (num.exponent == max_exp_div && d > max_exp_rem))
          return V_CVT_STR_UNK;
        num.exponent = 10*num.exponent + d;
        break;
      }
      if (empty_exponent)
        goto unexpected;
      is.unget();
      goto ok;
    }
    is.get(c);
  } while (is);

  if (empty_mantissa || is.bad())
    return V_CVT_STR_UNK;

 ok:
  {
    std::string::size_type n = num.mantissa.size();
    while (n > 0 && num.mantissa[n - 1] == '0') {
      --n;
      ++exponent_offset;
    }
    num.mantissa.erase(n);
    bool neg;
    if (exponent_offset < 0) {
      neg = true;
      exponent_offset = -exponent_offset;
    }
    else
      neg = false;
    sum_sign(num.neg_exponent, num.exponent,
             neg, exponent_offset * exponent_offset_scale);
    return V_EQ;
  }

 unexpected:
  is.unget();
  return V_CVT_STR_UNK;
}

/* \brief
   Reads a number from \p is writing it into \p num, the numerator,
   and \p den, the denominator; the appropriate Result value is
   returned.
*/
Result
parse_number(std::istream& is, number_struct& num, number_struct& den) {
  // Read the numerator.
  Result r = parse_number_part(is, num);
  if (r != V_EQ)
    return r;
  char c;
  is.get(c);
  if (is.bad())
    return V_CVT_STR_UNK;
  if (!is) {
    den.base = 0;
    return r;
  }
  if (c != '/') {
    is.unget();
    den.base = 0;
    return r;
  }
  // Read the denominator.
  r = parse_number_part(is, den);
  if (r != V_EQ)
    return V_CVT_STR_UNK;
  if (num.base == den.base
      && num.base_for_exponent == den.base_for_exponent) {
    if (sum_sign(num.neg_exponent, num.exponent,
                 !den.neg_exponent, den.exponent)) {
      if (num.neg_exponent) {
        den.neg_exponent = false;
        den.exponent = num.exponent;
        num.exponent = 0;
      }
      else
        den.exponent = 0;
    }
  }
  return V_EQ;
}


Result
input_mpq(mpq_class& to, std::istream& is) {
  number_struct num_struct;
  number_struct den_struct;
  Result r = parse_number(is, num_struct, den_struct);
  if (r == V_CVT_STR_UNK) {
    is.setstate(is.failbit);
    return r;
  }
  is.clear(is.rdstate() & ~is.failbit);
  if (r != V_EQ)
    return r;
  if (den_struct.base && den_struct.mantissa.empty())
      return V_NAN;
  if (num_struct.mantissa.empty()) {
    to = 0;
    return V_EQ;
  }
  mpz_ptr num = to.get_num().get_mpz_t();
  mpz_ptr den = to.get_den().get_mpz_t();
  mpz_set_str(num, num_struct.mantissa.c_str(), num_struct.base);
  if (den_struct.base) {
    if (num_struct.neg_mantissa != den_struct.neg_mantissa)
      mpz_neg(num, num);
    mpz_set_str(den, den_struct.mantissa.c_str(), den_struct.base);
    if (num_struct.exponent || den_struct.exponent) {
      // Multiply the exponents into the numerator and denominator.
      mpz_t z;
      mpz_init(z);
      if (num_struct.exponent) {
        mpz_ui_pow_ui(z, num_struct.base_for_exponent, num_struct.exponent);
        if (num_struct.neg_exponent)
          mpz_mul(den, den, z);
        else
          mpz_mul(num, num, z);
      }
      if (den_struct.exponent) {
        mpz_ui_pow_ui(z, den_struct.base_for_exponent, den_struct.exponent);
        if (den_struct.neg_exponent)
          mpz_mul(num, num, z);
        else
          mpz_mul(den, den, z);
      }
      mpz_clear(z);
    }
  }
  else {
    if (num_struct.neg_mantissa)
      mpz_neg(num, num);
    if (num_struct.exponent) {
      if (num_struct.neg_exponent) {
        // Add the negative exponent as a denominator.
        mpz_ui_pow_ui(den, num_struct.base_for_exponent, num_struct.exponent);
        goto end;
      }
      // Multiply the exponent into the numerator.
      mpz_t z;
      mpz_init(z);
      mpz_ui_pow_ui(z, num_struct.base_for_exponent, num_struct.exponent);
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

