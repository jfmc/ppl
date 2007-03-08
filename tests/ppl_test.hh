/* Header file for test programs.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef PPL_ppl_test_hh
#define PPL_ppl_test_hh 1

#include "ppl.hh"
#include "print.hh"
#include "FCAIBVP.defs.hh"
#include "Partial_Function.defs.hh"
#include "Random_Number_Generator.defs.hh"
#include <stdexcept>
#include <sstream>
#include <list>
#include <iterator>
#include <string>
#include <algorithm>

void
set_handlers();

#define TRY try

#define CATCH \
catch (const std::overflow_error& e) { \
  std::cerr << "arithmetic overflow (" << e.what() << ")" \
            << std::endl; \
  exit(1); \
} \
catch (const std::exception& e) { \
  std::cerr << "std::exception caught: " \
            << e.what() << " (type == " << typeid(e).name() << ")" \
            << std::endl; \
  exit(1); \
}

#define BEGIN_MAIN				\
int						\
main() try {					\
  set_handlers();				\
  bool succeeded = false;			\
  bool overflow = false;			\
  std::list<std::string> failed_tests;

#define END_MAIN							\
  if (failed_tests.empty())						\
    return 0;								\
  else {								\
    std::cerr << "failed tests: ";					\
    std::copy(failed_tests.begin(), failed_tests.end(),			\
	      std::ostream_iterator<std::string>(std::cerr, " "));	\
    std::cerr << std::endl;						\
    return 1;								\
  }									\
}									\
catch (const std::overflow_error& e) {					\
  std::cerr << "arithmetic overflow (" << e.what() << ")"		\
            << std::endl;						\
  exit(1);								\
}									\
catch (const std::exception& e) {					\
  std::cerr << "std::exception caught: "				\
	    << e.what() << " (type == " << typeid(e).name() << ")"	\
	    << std::endl;						\
  exit(1);								\
}

#define ANNOUNCE_TEST(test)		 \
  nout << "\n=== " #test " ===" << endl

#define RUN_TEST(test)							\
  try {									\
    overflow = false;							\
    succeeded = test();							\
  }									\
  catch (const std::overflow_error& e) {				\
    nout << "arithmetic overflow (" << e.what() << ")"			\
	 << std::endl;							\
    overflow = true;							\
    succeeded = false;							\
  }									\
  catch (const std::exception& e) {					\
    nout << "std::exception caught: "					\
	 << e.what() << " (type == " << typeid(e).name() << ")"		\
	 << std::endl;							\
    succeeded = false;							\
  }									\
  catch (...) {								\
    nout << "unknown exception caught"					\
	 << std::endl;							\
    succeeded = false;							\
  }

#define DO_TEST(test)			 \
  ANNOUNCE_TEST(test);			 \
  RUN_TEST(test);			 \
  if (!succeeded)			 \
    failed_tests.push_back(#test);

#define DO_TEST_F(test)			 \
  ANNOUNCE_TEST(test);			 \
  RUN_TEST(test);			 \
  if (succeeded)			 \
    failed_tests.push_back(#test);

#define DO_TEST_OVERFLOW(test)		 \
  ANNOUNCE_TEST(test);			 \
  RUN_TEST(test);			 \
  if (succeeded || !overflow)		 \
    failed_tests.push_back(#test);

#define DO_TEST_MAY_OVERFLOW_WITH_FLOAT(test)	\
  ANNOUNCE_TEST(test);				\
  RUN_TEST(test);				\
  if (!succeeded)				\
    if (!overflow || all_instances_are_exact())	\
      failed_tests.push_back(#test);

#if COEFFICIENT_BITS == 0

#define DO_TEST_F64(test) DO_TEST(test)
#define DO_TEST_F64A(test) DO_TEST(test)
#define DO_TEST_F32(test) DO_TEST(test)
#define DO_TEST_F32A(test) DO_TEST(test)
#define DO_TEST_F16(test) DO_TEST(test)
#define DO_TEST_F16A(test) DO_TEST(test)
#define DO_TEST_F8(test) DO_TEST(test)
#define DO_TEST_F8A(test) DO_TEST(test)

#elif COEFFICIENT_BITS == 64

#ifdef NDEBUG

#define DO_TEST_F64(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F64A(test) DO_TEST(test)
#define DO_TEST_F32(test) DO_TEST(test)
#define DO_TEST_F32A(test) DO_TEST(test)
#define DO_TEST_F16(test) DO_TEST(test)
#define DO_TEST_F16A(test) DO_TEST(test)
#define DO_TEST_F8(test) DO_TEST(test)
#define DO_TEST_F8A(test) DO_TEST(test)

#else

#define DO_TEST_F64(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F64A(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F32(test) DO_TEST(test)
#define DO_TEST_F32A(test) DO_TEST(test)
#define DO_TEST_F16(test) DO_TEST(test)
#define DO_TEST_F16A(test) DO_TEST(test)
#define DO_TEST_F8(test) DO_TEST(test)
#define DO_TEST_F8A(test) DO_TEST(test)

#endif // !defined(NDEBUG)

#elif COEFFICIENT_BITS == 32

#ifdef NDEBUG

#define DO_TEST_F64(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F64A(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F32(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F32A(test) DO_TEST(test)
#define DO_TEST_F16(test) DO_TEST(test)
#define DO_TEST_F16A(test) DO_TEST(test)
#define DO_TEST_F8(test) DO_TEST(test)
#define DO_TEST_F8A(test) DO_TEST(test)

#else

#define DO_TEST_F64(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F64A(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F32(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F32A(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F16(test) DO_TEST(test)
#define DO_TEST_F16A(test) DO_TEST(test)
#define DO_TEST_F8(test) DO_TEST(test)
#define DO_TEST_F8A(test) DO_TEST(test)

#endif // !defined(NDEBUG)

#elif COEFFICIENT_BITS == 16

#ifdef NDEBUG

#define DO_TEST_F64(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F64A(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F32(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F32A(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F16(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F16A(test) DO_TEST(test)
#define DO_TEST_F8(test) DO_TEST(test)
#define DO_TEST_F8A(test) DO_TEST(test)

#else

#define DO_TEST_F64(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F64A(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F32(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F32A(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F16(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F16A(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F8(test) DO_TEST(test)
#define DO_TEST_F8A(test) DO_TEST(test)

#endif // !defined(NDEBUG)

#elif COEFFICIENT_BITS == 8

#ifdef NDEBUG

#define DO_TEST_F64(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F64A(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F32(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F32A(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F16(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F16A(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F8(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F8A(test) DO_TEST(test)

#else

#define DO_TEST_F64(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F64A(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F32(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F32A(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F16(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F16A(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F8(test) DO_TEST_OVERFLOW(test)
#define DO_TEST_F8A(test) DO_TEST_OVERFLOW(test)

#endif // !defined(NDEBUG)

#endif // COEFFICIENT_BITS == 8


// Turn s into a string: PPL_TEST_STR(x + y) => "x + y".
#define PPL_TEST_STR(s) #s

// Turn the expansion of s into a string: PPL_TEST_XSTR(x) => "s expanded".
#define PPL_TEST_XSTR(s) PPL_TEST_STR(s)

// These using directive and declaration are just to avoid the
// corresponding namespace qualifications in all the tests.
using namespace Parma_Polyhedra_Library;
using std::endl;


#ifdef DERIVED_TEST
#define C_Polyhedron NNC_Polyhedron
#endif

#ifndef BOX_INSTANCE
#define BOX_INSTANCE rt_r_oc
#endif

#ifndef BD_SHAPE_INSTANCE
#define BD_SHAPE_INSTANCE mpq_class
#endif

#ifndef OCTAGONAL_SHAPE_INSTANCE
#define OCTAGONAL_SHAPE_INSTANCE mpq_class
#endif

namespace Parma_Polyhedra_Library {

//! Utility typedef to allow a macro argument to denote the long double type.
typedef long double long_double;

struct Floating_Real_Open_Interval_Info_Policy {
  const_bool_nodef(store_special, false);
  const_bool_nodef(store_open, true);
  const_bool_nodef(cache_empty, true);
  const_bool_nodef(cache_singleton, true);
  const_bool_nodef(cache_normalized, false);
  const_int_nodef(next_bit, 0);
  const_bool_nodef(may_be_empty, true);
  const_bool_nodef(may_contain_infinity, false);
  const_bool_nodef(check_empty_result, false);
  const_bool_nodef(check_inexact, false);
};

typedef Interval_Restriction_None
<Interval_Info_Bitset<unsigned int, Floating_Real_Open_Interval_Info_Policy> >
Floating_Real_Open_Interval_Info;

typedef Interval<float, Floating_Real_Open_Interval_Info> fl_r_oc;
typedef Interval<double, Floating_Real_Open_Interval_Info> db_r_oc;
typedef Interval<long double, Floating_Real_Open_Interval_Info> ld_r_oc;

struct Rational_Real_Open_Interval_Info_Policy {
  const_bool_nodef(store_special, true);
  const_bool_nodef(store_open, true);
  const_bool_nodef(cache_empty, true);
  const_bool_nodef(cache_singleton, true);
  const_bool_nodef(cache_normalized, false);
  const_int_nodef(next_bit, 0);
  const_bool_nodef(may_be_empty, true);
  const_bool_nodef(may_contain_infinity, false);
  const_bool_nodef(check_empty_result, false);
  const_bool_nodef(check_inexact, false);
};

typedef Interval_Restriction_None
<Interval_Info_Bitset<unsigned int, Rational_Real_Open_Interval_Info_Policy> >
Rational_Real_Open_Interval_Info;

typedef Interval<mpq_class, Rational_Real_Open_Interval_Info> rt_r_oc;

//! The incarnation of Box under test.
typedef Box<BOX_INSTANCE> TBox;

//! The incarnation of BD_Shape under test.
typedef BD_Shape<BD_SHAPE_INSTANCE> TBD_Shape;

//! The incarnation of Octagonal_Shape under test.
typedef Octagonal_Shape<OCTAGONAL_SHAPE_INSTANCE> TOctagonal_Shape;

// CHECKME: here we rely on the fact that the `make check' will set
// at most one of the instances to a floating point datatype and the
// others will be set to an exact datatype.
inline bool
all_instances_are_exact() {
  return std::numeric_limits<TBox::interval_type::boundary_type>::is_exact
    && std::numeric_limits<TBD_Shape::base_type>::is_exact
    && std::numeric_limits<TOctagonal_Shape::base_type>::is_exact;
}

bool
check_distance(const Checked_Number<mpq_class, Extended_Number_Policy>& d,
	       const char* max_d_s, const char* d_name);

template <typename T>
bool
check_result_i(const BD_Shape<T>& computed_result,
	       const BD_Shape<mpq_class>& known_result,
	       const char* max_r_d_s,
	       const char* max_e_d_s,
	       const char* max_l_d_s) {
  using namespace IO_Operators;
  BD_Shape<mpq_class> q_computed_result(computed_result);
  // Handle in a more efficient way the case where equality is expected.
  if (max_r_d_s == 0 && max_e_d_s == 0 && max_l_d_s == 0) {
    if (q_computed_result != known_result) {
      nout << "Equality does not hold:"
	   << "\ncomputed result is\n"
	   << q_computed_result
	   << "\nknown result is\n"
	   << known_result
	   << endl;
      return false;
    }
    else
      return true;
  }

  if (!q_computed_result.contains(known_result)) {
    nout << "Containment does not hold:"
	 << "\ncomputed result is\n"
	 << q_computed_result
	 << "\nknown result is\n"
	 << known_result
	 << endl;
    return false;
  }

  Checked_Number<mpq_class, Extended_Number_Policy> r_d;
  rectilinear_distance_assign(r_d, known_result, q_computed_result, ROUND_UP);
  Checked_Number<mpq_class, Extended_Number_Policy> e_d;
  euclidean_distance_assign(e_d, known_result, q_computed_result, ROUND_UP);
  Checked_Number<mpq_class, Extended_Number_Policy> l_d;
  l_infinity_distance_assign(l_d, known_result, q_computed_result, ROUND_UP);
  bool ok_r = check_distance(r_d, max_r_d_s, "rectilinear");
  bool ok_e = check_distance(e_d, max_e_d_s, "euclidean");
  bool ok_l = check_distance(l_d, max_l_d_s, "l_infinity");
  bool ok = ok_r && ok_e && ok_l;
  if (!ok) {
    nout << "Computed result is\n"
	 << q_computed_result
	 << "\nknown result is\n"
	 << known_result
	 << endl;
  }
  return ok;
}

template <typename T>
bool
check_result(const BD_Shape<T>& computed_result,
	     const BD_Shape<mpq_class>& known_result,
	     const char* max_r_d_s,
	     const char* max_e_d_s,
	     const char* max_l_d_s) {
  return std::numeric_limits<T>::is_integer
    ? check_result_i(computed_result, known_result,
		     "+inf", "+inf", "+inf")
    : check_result_i(computed_result, known_result,
		     max_r_d_s, max_e_d_s, max_l_d_s);
}

template <>
inline bool
check_result(const BD_Shape<mpq_class>& computed_result,
	     const BD_Shape<mpq_class>& known_result,
	     const char*,
	     const char*,
	     const char*) {
  return check_result_i(computed_result, known_result,
			0, 0, 0);
}

template <typename T>
bool
check_result(const BD_Shape<T>& computed_result,
	     const BD_Shape<mpq_class>& known_result) {
  return std::numeric_limits<T>::is_integer
    ? check_result_i(computed_result, known_result, "+inf", "+inf", "+inf")
    : check_result_i(computed_result, known_result, 0, 0, 0);
}

template <typename T>
bool
check_result_i(const Octagonal_Shape<T>& computed_result,
	       const Octagonal_Shape<mpq_class>& known_result,
	       const char* max_r_d_s,
	       const char* max_e_d_s,
	       const char* max_l_d_s) {
  using namespace IO_Operators;
  Octagonal_Shape<mpq_class> q_computed_result(computed_result);
  // Handle in a more efficient way the case where equality is expected.
  if (max_r_d_s == 0 && max_e_d_s == 0 && max_l_d_s == 0) {
    if (q_computed_result != known_result) {
      nout << "Equality does not hold:"
	   << "\ncomputed result is\n"
	   << q_computed_result
	   << "\nknown result is\n"
	   << known_result
	   << endl;
      return false;
    }
    else
      return true;
  }

  if (!q_computed_result.contains(known_result)) {
    nout << "Containment does not hold:"
	 << "\ncomputed result is\n"
	 << q_computed_result
	 << "\nknown result is\n"
	 << known_result
	 << endl;
    return false;
  }

  Checked_Number<mpq_class, Extended_Number_Policy> r_d;
  rectilinear_distance_assign(r_d, known_result, q_computed_result, ROUND_UP);
  Checked_Number<mpq_class, Extended_Number_Policy> e_d;
  euclidean_distance_assign(e_d, known_result, q_computed_result, ROUND_UP);
  Checked_Number<mpq_class, Extended_Number_Policy> l_d;
  l_infinity_distance_assign(l_d, known_result, q_computed_result, ROUND_UP);
  bool ok_r = check_distance(r_d, max_r_d_s, "rectilinear");
  bool ok_e = check_distance(e_d, max_e_d_s, "euclidean");
  bool ok_l = check_distance(l_d, max_l_d_s, "l_infinity");
  bool ok = ok_r && ok_e && ok_l;
  if (!ok) {
    nout << "Computed result is\n"
	 << q_computed_result
	 << "\nknown result is\n"
	 << known_result
	 << endl;
  }
  return ok;
}

template <typename T>
bool
check_result(const Octagonal_Shape<T>& computed_result,
	     const Octagonal_Shape<mpq_class>& known_result,
	     const char* max_r_d_s,
	     const char* max_e_d_s,
	     const char* max_l_d_s) {
  return std::numeric_limits<T>::is_integer
    ? check_result_i(computed_result, known_result,
		     "+inf", "+inf", "+inf")
    : check_result_i(computed_result, known_result,
		     max_r_d_s, max_e_d_s, max_l_d_s);
}

template <>
inline bool
check_result(const Octagonal_Shape<mpq_class>& computed_result,
	     const Octagonal_Shape<mpq_class>& known_result,
	     const char*,
	     const char*,
	     const char*) {
  return check_result_i(computed_result, known_result,
			0, 0, 0);
}

template <typename T>
bool
check_result(const Octagonal_Shape<T>& computed_result,
	     const Octagonal_Shape<mpq_class>& known_result) {
  return std::numeric_limits<T>::is_integer
    ? check_result_i(computed_result, known_result, "+inf", "+inf", "+inf")
    : check_result_i(computed_result, known_result, 0, 0, 0);
}

// FIXME: stub definition always returning true.
template <typename T>
bool
check_result(const Box<T>& computed_result,
	     const Rational_Box& known_result,
	     const char* max_r_d_s = 0,
	     const char* max_e_d_s = 0,
	     const char* max_l_d_s = 0) {
  used(max_r_d_s);
  used(max_e_d_s);
  used(max_l_d_s);
  return true;
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_ppl_test_hh)
