/* Interval class to realize bounding boxes in the Prolog interface.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef _Interval_hh
#define _Interval_hh 1

class ExtendedRational {
private:
  //! Positive if +infinity, negative if -infinity, zero otherwise.
  int e;
  //! The finite value: valid only if \p e=0.
  mpq_class v;

public:
  ExtendedRational(const mpq_class& n)
    : e(0), v(n) {
  }

  ExtendedRational(const PPL::Integer& num, const PPL::Integer& den)
    : e(0), v(num, den) {
  }

  ExtendedRational(char sign)
    : e(sign == '+' ? 1 : -1) {
    assert(sign == '+' || sign == '-');
  }

  friend bool operator==(const ExtendedRational& x, const ExtendedRational& y);
  friend bool operator!=(const ExtendedRational& x, const ExtendedRational& y);
  friend bool operator< (const ExtendedRational& x, const ExtendedRational& y);
  friend bool operator> (const ExtendedRational& x, const ExtendedRational& y);
  friend bool operator<=(const ExtendedRational& x, const ExtendedRational& y);
  friend bool operator>=(const ExtendedRational& x, const ExtendedRational& y);
};  

inline bool
operator==(const ExtendedRational& x, const ExtendedRational& y) {
  return x.e == y.e && (x.e != 0 || x.v == y.v);
}

inline bool
operator!=(const ExtendedRational& x, const ExtendedRational& y) {
  return !(x == y);
}

inline bool
operator<(const ExtendedRational& x, const ExtendedRational& y) {
  return x.e < y.e || (x.e == 0 && y.e == 0 && x.v < y.v);
}

inline bool
operator>(const ExtendedRational& x, const ExtendedRational& y) {
  return y < x;
}

inline bool
operator<=(const ExtendedRational& x, const ExtendedRational& y) {
  return x < y || x == y;
}

inline bool
operator>=(const ExtendedRational& x, const ExtendedRational& y) {
  return y <= x;
}

class LBoundary;
class UBoundary;

class Boundary {
protected:
  typedef ExtendedRational Value;
  enum Flag { NEG = -1, ZERO = 0, POS = 1 };

  Value value;
  Flag flag;

  Boundary(const Value& v, Flag f)
    : value(v), flag(f) {
  }

  friend bool operator==(const Boundary& x, const Boundary& y);
  friend bool operator!=(const Boundary& x, const Boundary& y);
  friend bool operator< (const Boundary& x, const Boundary& y);
  friend bool operator> (const Boundary& x, const Boundary& y);
  friend bool operator<=(const Boundary& x, const Boundary& y);
  friend bool operator>=(const Boundary& x, const Boundary& y);

  friend bool operator==(const Boundary& x, Value y);
  friend bool operator!=(const Boundary& x, Value y);
  friend bool operator< (const Boundary& x, Value y);
  friend bool operator> (const Boundary& x, Value y);
  friend bool operator<=(const Boundary& x, Value y);
  friend bool operator>=(const Boundary& x, Value y);

  friend bool operator< (const LBoundary& x, const UBoundary& y);
  friend bool operator> (const UBoundary& x, const LBoundary& y);
  friend bool operator<=(const UBoundary& x, const LBoundary& y);
  friend bool operator>=(const LBoundary& x, const UBoundary& y);

  friend bool operator< (const LBoundary& x, Value y);
  friend bool operator> (const UBoundary& x, Value y);
  friend bool operator<=(const UBoundary& x, Value y);
  friend bool operator>=(const LBoundary& x, Value y);
};


class LBoundary : public Boundary {
public:
  //! Open or closed.
  enum OpenClosed { OPEN = Boundary::POS, CLOSED = Boundary::ZERO };

  LBoundary(const Boundary::Value& v, OpenClosed f = CLOSED)
    : Boundary(value, f == CLOSED ? ZERO : POS) {
  }
};

class UBoundary : public Boundary {
public:
  //! Open or closed.
  enum OpenClosed { OPEN = Boundary::NEG, CLOSED = Boundary::ZERO };

  UBoundary(const Boundary::Value& v, OpenClosed f = CLOSED)
    : Boundary(value, f == CLOSED ? ZERO : NEG) {
  }
};

inline bool
operator==(const Boundary& x, const Boundary& y) {
  return x.value == y.value && x.flag == y.flag;
}

inline bool
operator!=(const Boundary& x, const Boundary& y) {
  return !(x == y);
}

inline bool
operator<(const Boundary& x, const Boundary& y) {
  return x.value < y.value ||
    (x.value == y.value && x.flag < y.flag);
}

inline bool
operator<(const LBoundary& x, const UBoundary& y) {
  return x.value < y.value;
}

inline bool
operator>(const Boundary& x, const Boundary& y) {
  return x.value > y.value ||
    (x.value == y.value && x.flag > y.flag);
}

inline bool
operator>(const UBoundary& x, const LBoundary& y) {
  return x.value > y.value;
}

inline bool
operator<=(const Boundary& x, const Boundary& y) {
  return !(x > y);
}

inline bool
operator<=(const UBoundary& x, const LBoundary& y) {
  return !(x > y);
}

inline bool
operator>=(const Boundary& x, const Boundary& y) {
  return !(x < y);
}

inline bool
operator>=(const LBoundary& x, const UBoundary& y) {
  return !(x < y);
}

inline bool
operator==(const Boundary& x, Boundary::Value y) {
  return x.value == y && x.flag == Boundary::ZERO;
}

inline bool
operator!=(const Boundary& x, Boundary::Value y) {
  return !(x == y);
}

inline bool
operator<(const Boundary& x, Boundary::Value y) {
  return x.value < y ||
    (x.value == y && x.flag < Boundary::ZERO);
}

inline bool
operator<(const LBoundary& x, Boundary::Value y) {
  return x.value < y;
}

inline bool
operator>(const Boundary& x, Boundary::Value y) {
  return x.value > y ||
    (x.value == y && x.flag > Boundary::ZERO);
}

inline bool
operator>(const UBoundary& x, Boundary::Value y) {
  return x.value > y;
}

inline bool
operator<=(const Boundary& x, Boundary::Value y) {
  return !(x > y);
}

inline bool
operator<=(const UBoundary& x, Boundary::Value y) {
  return !(x > y);
}

inline bool
operator>=(const Boundary& x, Boundary::Value y) {
  return !(x < y);
}

inline bool
operator>=(const LBoundary& x, Boundary::Value y) {
  return !(x < y);
}

class Interval {
public:
  LBoundary lower;
  UBoundary upper;

  //! Construct the interval (-infinity, +infinity).
  Interval()
    : lower('-', LBoundary::OPEN), upper('+', UBoundary::OPEN) {
  }

  bool empty() const {
    return lower > upper;
  }

  void raise_lower_bound(LBoundary new_lower) {
    if (new_lower > lower)
      lower = new_lower;
  }

  void lower_upper_bound(UBoundary new_upper) {
    if (new_upper < upper)
      upper = new_upper;
  }

  void set_empty() {
    lower = LBoundary('+', LBoundary::OPEN);
    upper = UBoundary('-', UBoundary::OPEN);
    assert(empty());
  }
};

#endif
