/* Box<Interval>::Status class implementation: inline functions.
   Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_Box_Status_inlines_hh
#define PPL_Box_Status_inlines_hh 1

#include <string>

namespace Parma_Polyhedra_Library {

template <typename Interval>
inline
Box<Interval>::Status::Status(flags_t mask)
  : flags(mask) {
}

template <typename Interval>
inline
Box<Interval>::Status::Status()
  : flags(EMPTY_UP_TO_DATE) {
}

template <typename Interval>
inline bool
Box<Interval>::Status::test_all(flags_t mask) const {
  return (flags & mask) == mask;
}

template <typename Interval>
inline bool
Box<Interval>::Status::test_any(flags_t mask) const {
  return flags & mask;
}

template <typename Interval>
inline void
Box<Interval>::Status::set(flags_t mask) {
  flags |= mask;
}

template <typename Interval>
inline void
Box<Interval>::Status::reset(flags_t mask) {
  flags &= ~mask;
}

template <typename Interval>
inline bool
Box<Interval>::Status::test_empty_up_to_date() const {
  return flags == EMPTY_UP_TO_DATE;
}

template <typename Interval>
inline void
Box<Interval>::Status::reset_empty_up_to_date() {
  // This is a no-op if the current status is not zero-dim.
  if (flags == EMPTY_UP_TO_DATE)
    // In the zero-dim space, if it is not the universe it is empty.
    flags = EMPTY;
}

template <typename Interval>
inline void
Box<Interval>::Status::set_empty_up_to_date() {
  // Zero-dim universe is incompatible with anything else.
  flags = EMPTY_UP_TO_DATE;
}

template <typename Interval>
inline bool
Box<Interval>::Status::test_empty() const {
  return test_any(EMPTY);
}

template <typename Interval>
inline void
Box<Interval>::Status::reset_empty() {
  reset(EMPTY);
}

template <typename Interval>
inline void
Box<Interval>::Status::set_empty() {
  flags = EMPTY;
}

template <typename Interval>
inline bool
Box<Interval>::Status::test_universe() const {
  return test_any(UNIVERSE);
}

template <typename Interval>
inline void
Box<Interval>::Status::reset_universe() {
  reset(UNIVERSE);
}

template <typename Interval>
inline void
Box<Interval>::Status::set_universe() {
  set(UNIVERSE);
}

template <typename Interval>
bool
Box<Interval>::Status::OK() const {
  if (test_empty_up_to_date())
    // Zero-dim universe is OK.
    return true;

  if (test_empty()) {
    Status copy = *this;
    copy.reset_empty();
    if (copy.test_empty_up_to_date())
      return true;
    else {
#ifndef NDEBUG
      std::cerr << "The empty flag is incompatible with any other one."
		<< std::endl;
#endif
      return false;
    }
  }

  // Any other case is OK.
  return true;
}


namespace Implementation {

namespace Boxes {

// These are the keywords that indicate the individual assertions.
const std::string empty_up_to_date = "EUP";
const std::string empty = "EM";
const std::string universe = "UN";
const char yes = '+';
const char no = '-';
const char sep = ' ';

/*! \relates Parma_Polyhedra_Library::Box::Status
  Reads a keyword and its associated on/off flag from \p s.
  Returns <CODE>true</CODE> if the operation is successful,
  returns <CODE>false</CODE> otherwise.
  When successful, \p positive is set to <CODE>true</CODE> if the flag
  is on; it is set to <CODE>false</CODE> otherwise.
*/
inline bool
get_field(std::istream& s, const std::string& keyword, bool& positive) {
  std::string str;
  if (!(s >> str)
      || (str[0] != yes && str[0] != no)
      || str.substr(1) != keyword)
    return false;
  positive = (str[0] == yes);
  return true;
}

} // namespace Boxes

} // namespace Implementation

template <typename Interval>
void
Box<Interval>::Status::ascii_dump(std::ostream& s) const {
  using namespace Implementation::Boxes;
  s << (test_empty_up_to_date() ? yes : no) << empty_up_to_date << sep
    << (test_empty() ? yes : no) << empty << sep
    << (test_universe() ? yes : no) << universe << sep;
}

PPL_OUTPUT_TEMPLATE_DEFINITIONS_ASCII_ONLY(Interval, Box<Interval>::Status)

template <typename Interval>
bool
Box<Interval>::Status::ascii_load(std::istream& s) {
  using namespace Implementation::Boxes;
  bool positive;

  if (!get_field(s, Implementation::Boxes::empty_up_to_date, positive))
    return false;
  if (positive)
    set_empty_up_to_date();

  if (!get_field(s, Implementation::Boxes::empty, positive))
    return false;
  if (positive)
    set_empty();

  if (!get_field(s, universe, positive))
    return false;
  if (positive)
    set_universe();
  else
    reset_universe();

  // Check invariants.
  assert(OK());
  return true;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_Box_Status_inlines_hh)
