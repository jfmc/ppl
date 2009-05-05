/* Boolean values set class.
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

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
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Bool4_defs_hh
#define PPL_Bool4_defs_hh 1

#include "globals.types.hh"

namespace Parma_Polyhedra_Library {

class Bool4 {
private:
  enum {
    B_FALSE = 0,
    B_TRUE,
    B_UNIVERSE,
    B_EMPTY,
  };
  unsigned char value;
  static bool b_from_b4_b4(unsigned int results, unsigned char x, unsigned char y) {
    return (results >> (x << 2 | y)) & 1;
  }
  static unsigned char b4_from_b4(unsigned int results, unsigned char x) {
    return (results >> (x << 1)) & 3;
  }
  static unsigned char b4_from_b4_b4(unsigned int results, unsigned char x, unsigned char y) {
    return (results >> (x << 3 | y << 1)) & 3;
  }
public:
  Bool4() {
  }
  explicit Bool4(bool v) {
    assign(v);
  }
  explicit Bool4(Degenerate_Element e) {
    assign(e);
  }
  bool is_empty() const {
    return value == B_EMPTY;
  }
  bool is_universe() const {
    return value == B_UNIVERSE;
  }
  bool is_true() const {
    return value == B_TRUE;
  }
  bool is_false() const {
    return value == B_FALSE;
  }
  bool contains(bool v) const {
    return value == B_UNIVERSE || value == (v ? B_TRUE : B_FALSE);
  }
  bool contains(const Bool4& x) const {
    // EEEE UUUU TTTT FFFF
    // EUTF EUTF EUTF EUTF
    // 1000 1111 1010 1001
    return b_from_b4_b4(0x8fa9U, value, x.value);
  }
  bool strictly_contains(const Bool4& x) const {
    // EEEE UUUU TTTT FFFF
    // EUTF EUTF EUTF EUTF
    // 0000 1011 1000 1000
    return b_from_b4_b4(0x0b88U, value, x.value);
  }
  bool is_disjoint_from(const Bool4& x) const {
    // EEEE UUUU TTTT FFFF
    // EUTF EUTF EUTF EUTF
    // 1111 1000 1001 1010
    return b_from_b4_b4(0xf89aU, value, x.value);
  }
  bool equal(const Bool4& x) const {
    return value == x.value;
  }
  void assign(Degenerate_Element e) {
    switch (e) {
    default:
      assert(0);
      /* Fall through */
    case EMPTY:
      value = B_EMPTY;
      break;
    case UNIVERSE:
      value = B_UNIVERSE;
      break;
    }
  }
  void assign(bool v) {
    value = v ? B_TRUE : B_FALSE;
  }
  void assign(const Bool4& x) {
    value = x.value;
  }
  void complement_assign(const Bool4& x) {
    value = x.value ^ B_TRUE;
  }
  void join_assign(const Bool4& x, const Bool4& y) {
    // E E  E E  U U  U U  T T  T T  F F  F F
    // E U  T F  E U  T F  E U  T F  E U  T F
    // E U  T F  U U  U U  T U  T U  F U  U F
    // 1110 0100 1010 1010 0110 0110 0010 1000
    value = b4_from_b4_b4(0xe4aa6628U, x.value, y.value);
  }
  void intersect_assign(const Bool4& x, const Bool4& y) {
    // E E  E E  U U  U U  T T  T T  F F  F F
    // E U  T F  E U  T F  E U  T F  E U  T F
    // E E  E E  E U  T F  E T  T E  E F  E F
    // 1111 1111 1110 0100 1101 0111 1100 1100
    value = b4_from_b4_b4(0xffe4d7ccU, x.value, y.value);
  }
  void difference_assign(const Bool4& x, const Bool4& y) {
    // E E  E E  U U  U U  T T  T T  F F  F F
    // E U  T F  E U  T F  E U  T F  E U  T F
    // E E  E E  U E  F T  T E  E T  F E  F E
    // 1111 1111 1011 0001 0111 1101 0011 0011
    value = b4_from_b4_b4(0xffb17d33U, x.value, y.value);
  }

#define r_bit(v, n) ((v) >> (n) & 1)
#define r_join(v, a, b) (r_bit(v, a) == r_bit(v, b) ? r_bit(v, a) : 2)

#define br2_to_b4(v)							\
  (r_bit(v, 0) | r_bit(v, 1) << 2 | r_join(v, 0, 1) << 4 |		\
   r_bit(v, 2) << 8 | r_bit(v, 3) << 10 | r_join(v, 2, 3) << 12 |	\
   r_join(v, 0, 2) << 16 | r_join(v, 1, 3) << 18 |			\
   0xffe0c0c0U)

#define br1_to_b4(v) \
  (r_bit(v, 0) | r_bit(v,1) << 2 | r_join(v, 0, 1) << 4 | 0xc0)

  void not_assign(const Bool4& x) {
    // F T
    // T F
    value = b4_from_b4(br1_to_b4(0x1), x.value);
  }
  void or_assign(const Bool4& x, const Bool4& y) {
    // T T F F
    // T F T F
    // T T T F
    value = b4_from_b4_b4(br2_to_b4(0xe), x.value, y.value);
  }
  void and_assign(const Bool4& x, const Bool4& y) {
    // T T F F
    // T F T F
    // T F F F
    value = b4_from_b4_b4(br2_to_b4(0x8), x.value, y.value);
  }
  void xor_assign(const Bool4& x, const Bool4& y) {
    // T T F F
    // T F T F
    // F T T F
    value = b4_from_b4_b4(br2_to_b4(0x6), x.value, y.value);
  }
  void implication_assign(const Bool4& x, const Bool4& y) {
    // T T F F
    // T F T F
    // T F T T
    value = b4_from_b4_b4(br2_to_b4(0xb), x.value, y.value);
  }
  void print(std::ostream& os) const {
    if (is_empty())
      os << "[]";
    else if (is_false())
      os << "0";
    else if (is_true())
      os << "1";
    else if (is_universe())
      os << "[0, 1]";
  }
};

inline std::ostream&
operator<<(std::ostream& os, const Bool4& x) {
  x.print(os);
  return os;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Bool4_defs_hh)

