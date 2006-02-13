/* Header file for test programs.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "ppl.hh"
#include "print.hh"
#include "ehandlers.hh"
#include "Random_Number_Generator.defs.hh"
#include <stdexcept>
#include <sstream>

using namespace std;
using namespace Parma_Polyhedra_Library;

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

#ifndef BD_SHAPE_INSTANCE
#define BD_SHAPE_INSTANCE mpq_class
#endif

#define DO_TEST(name) \
  nout << "\n=== " #name " ===" << endl; \
  name();

namespace Parma_Polyhedra_Library {

//! Utility typedef to allow a macro argument to denote the long double type.
typedef long double long_double;

//! The incarnation of BD_Shape under test.
typedef BD_Shape<BD_SHAPE_INSTANCE> TBD_Shape;

bool
check_distance(const Checked_Number<mpq_class, Extended_Number_Policy>& d,
	       const char* max_d_s, const char* d_name) {
  Checked_Number<mpq_class, Extended_Number_Policy>
    max_d((max_d_s ? max_d_s : "0"), ROUND_NOT_NEEDED);
  assert(max_d >= 0);
  if (d > max_d) {
    Checked_Number<float, Extended_Number_Policy> dd(d, ROUND_UP);
    nout << "Excessive " << d_name << " distance " << dd
	 << ": should be at most " << max_d << "."
	 << endl;
    return false;
  }
  else
    return true;
}

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
bool
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

//! Compare copies of \p a and \p b.
/*!
  Comparing temporary copies ensures that the underlying
  representation of \p a and \p b stays the same.
*/
bool
copy_compare(Grid& a, Grid& b) {
  const Grid tem_a = a;
  const Grid tem_b = b;
  return tem_a == tem_b;
}

//! Look for variation in \p a.
/*!
  Return <CODE>true</CODE> if \p a contains variation from
  consistency, else return <CODE>false</CODE>.  Variation can be found
  via the OK method, or via a comparison between \p a and an object
  created from the ASCII dump of \p a.

  It is assumed that \p a is up to date.

  If the loading of the ASCII dump fails then an error message is
  printed and `exit' is called.

  \p T must provide:
    void ascii_dump(std::ostream& s) const;
    bool ascii_load(std::istream& s);
  and there must be a:
    bool operator==(const T& x, const T& y);
*/
template <typename T>
static bool
find_variation(T& a) {
  using namespace Parma_Polyhedra_Library::IO_Operators;

  if (!a.OK()) {
    nout << "OK() failed\nASCII dump:" << endl;
    a.ascii_dump(nout);
    return true;
  }

  /* FIX In some PPL classes (e.g. Congruence) the simple constructors
     are private. */
  //T b;
  T b(a);
  stringstream dump;
  a.ascii_dump(dump);
  if (!b.ascii_load(dump)) {
    nout << "Failed to load `b' from the ASCII dump of `a'." << endl;
    nout << "ASCII dump of `a':" << endl;
    nout << dump.str();
    exit(1);
  }

  if (a == b)
    return false;

  nout << "`b' loaded from ASCII dump of `a' should equal `a'" << endl
       << "ASCII dump of `a':" << endl
       << "------------------" << endl;
  a.ascii_dump(nout);
  nout << "ASCII dump of `b' (after comparison):" << endl
       << "-------------------------------------" << endl;
  b.ascii_dump(nout);
  nout << "ASCII dump of `a' (after comparison):" << endl
       << "-------------------------------------" << endl;
  a.ascii_dump(nout);

  return true;
}

} // namespace Parma_Polyhedra_Library
