/* Checked extended arithmetic functions.
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

#ifndef PPL_checked_ext_defs_hh
#define PPL_checked_ext_defs_hh 1

#include <iostream>
#include "Float.defs.hh"
#include "checked.defs.hh"

namespace Parma_Polyhedra_Library {

namespace Checked {

template <typename To_Policy, typename From_Policy, typename To, typename From>
Result assign_ext(To& to, const From& from, Rounding_Dir dir);

template <typename Policy, typename Type>
Result sgn_ext(const Type& x);

template <typename Policy1, typename Policy2, typename Type1, typename Type2>
Result cmp_ext(const Type1& x, const Type2& y);

template <typename To_Policy, typename From_Policy, typename To, typename From>
Result neg_ext(To& to, const From& x, Rounding_Dir dir);

template <typename To_Policy, typename From_Policy, typename To, typename From>
Result abs_ext(To& to, const From& x, Rounding_Dir dir);

template <typename To_Policy, typename From1_Policy, typename From2_Policy, typename To, typename From1, typename From2>
Result add_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir);

template <typename To_Policy, typename From1_Policy, typename From2_Policy, typename To, typename From1, typename From2>
Result sub_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir);

template <typename To_Policy, typename From1_Policy, typename From2_Policy, typename To, typename From1, typename From2>
Result mul_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir);

template <typename To_Policy, typename From1_Policy, typename From2_Policy, typename To, typename From1, typename From2>
Result div_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir);

template <typename To_Policy, typename From1_Policy, typename From2_Policy, typename To, typename From1, typename From2>
Result rem_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir);

template <typename To_Policy, typename From1_Policy, typename From2_Policy, typename To, typename From1, typename From2>
Result add_mul_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir);

template <typename To_Policy, typename From1_Policy, typename From2_Policy, typename To, typename From1, typename From2>
Result sub_mul_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir);

template <typename To_Policy, typename From_Policy, typename To, typename From>
Result sqrt_ext(To& to, const From& x, Rounding_Dir dir);

template <typename To_Policy, typename From1_Policy, typename From2_Policy, typename To, typename From1, typename From2>
Result gcd_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir);

template <typename To_Policy, typename From1_Policy, typename From2_Policy, typename To, typename From1, typename From2>
Result lcm_ext(To& to, const From1& x, const From2& y, Rounding_Dir dir);

template <typename Policy, typename Type>
Result to_c_string_ext(char* str, size_t size, const Type& x, const Numeric_Format& format, Rounding_Dir dir);

template <typename Policy, typename Type>
Result from_c_string_ext(Type& x, const char* str, const Numeric_Format& format, Rounding_Dir dir);

} // namespace Checked

} // namespace Parma_Polyhedra_Library

#include "checked_ext.inlines.hh"

#endif // !defined(PPL_checked_ext_defs_hh)
