/* Test number input.
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

#include "ppl_test.hh"
#include <sstream>

using namespace std;
using namespace Parma_Polyhedra_Library;
using namespace Parma_Polyhedra_Library::IO_Operators;
using namespace Parma_Polyhedra_Library::Checked;

#ifndef NOISY
#define NOISY 0
#endif

void
dump_mpz_ptr(mpz_ptr& num) {
  std::cerr << num << std::endl;
}

using std::cout;
using std::endl;

/*
From Roberto:
    Ideally, a more refined test should check all the main
    instantiations, perhaps using a table where you have one strings
    per row and one checked type per column.  Any cell should contain
    the expected result (the number read and the portion of input not
    consumed or the exception thrown).  One could generate the table
    automatically, using an helper C++ program, and then inspect it to
    see if the results make sense.  Corner cases should also be
    checked more systematically (things like base-1 numbers, maximum
    base numbers, 'e' not followed by an exponent, ...).
*/

int ret;

struct Test_Extended_Number_Policy {
  static const int check_overflow = 1;
  static const int check_inf_add_inf = 0;
  static const int check_inf_sub_inf = 0;
  static const int check_inf_mul_zero = 0;
  static const int check_div_zero = 0;
  static const int check_inf_div_inf = 0;
  static const int check_inf_mod = 0;
  static const int check_sqrt_neg = 0;
  static const int store_nan = 1;
  static const int store_infinity = 1;
  static const int fpu_check_inexact = 0;
  static const int check_nan_args = 1;
  static const Rounding_Dir ROUND_DEFAULT = ROUND_UP;
  static void handle_result(Result r);
};

inline void
Test_Extended_Number_Policy::handle_result(Result r) {
  if (r == VC_NAN)
    return;
  Extended_Number_Policy::handle_result(r);
}

template <typename T>
void
test_template (string number, string expected, string expected_residual) {
  stringstream f(number);

  // Convert `number' to checked number cn1.
  cout << f.str() << ": ";
  Checked_Number<T, Test_Extended_Number_Policy> cn1;
  f >> cn1;
  string residual;
  f >> residual;
  cout << cn1 << endl;

  // Read cn2 from string output of cn1.
  stringstream out;
  out << cn1;
  Checked_Number<T, Test_Extended_Number_Policy> cn2;
  out >> cn2;
  // Check for a residual.
  string resid;
  out >> resid;
  if (resid.compare("")) {
    cout << "Residual after reading cn1 output into cn2 (\""
	 << resid << "\")." << endl
	 << "cn1: " << cn1 << endl
	 << "cn2: " << cn2 << endl;
    ret = 1;
    return;
  }

  // Check that cn1 equals cn2.
  if (!((cn1.classify() & VC_MASK) == VC_NAN
	&& (cn2.classify() & VC_MASK) == VC_NAN)
      && cn1 != cn2) {
    cout << "cn1 should equal cn2 (which was created from cn1 output)."
	 << endl;
    ret = 1;
    return;
  }

  // Compare the output of cn2 and the expected string.
  stringstream ss;
  ss << cn2;
  if (ss.str().compare(expected)) {
    cout << "cn2 output is \"" << ss.str()
	 << "\" (expected \"" << expected << "\")."
	 << endl
	 << "cn2: " << cn2 << endl;
    ret = 1;
    return;
  }

  // Compare residual from initial convertion to expected residual.
  if (expected_residual.compare(residual)) {
    cout << "Residual from conversion \"" << residual
	 << "\" (expected \"" << expected_residual << "\")" << endl;
    ret = 1;
    return;
  }

  // FIX If switch to mpq, compare result and expected result.
}

int
main() TRY {
  set_handlers();
  ret = 0;

#if 1
#define fix(a,b,c)
#else
#define fix(a,b,c) test(a,b,c)
#endif

#define test test_template<int>

  // Checked int.

  stringstream min;
  min << min_int<Test_Extended_Number_Policy, int>();

  cout << "Testing checked int:" << endl;

  test("inf", "+inf", "");
  fix("INF", "+inf", "");
  test("+inF", "+inf", "");
  test("-InF", "-inf", "");

  test("nan", "nan", "");
  test("NAN", "nan", "");
  test("Nan", "nan", "");

  test("nan/-3", "nan", "/-3");
  test("inf/3", "+inf", "/3");
  test("inf/-3", "+inf", "/-3");
  test("-inf/-3", "-inf", "/-3");

  // Integer.

  test("15", "15", "");
  test("34976098", "34976098", "");
  test("34976098349760983497609834976098", "+inf", "");
  fix("3/-inf", "3", "/-inf");
  test("+77", "77", "");
  test("-77", "-77", "");
  test("-777777777", "-777777777", "");
  test("-7777777777", min.str(), "");
  test("7777777777", "+inf", "");
  test("-7777777777777777777777777", min.str(), "");
  // Fraction.
  test("71.3", "72", "");
  test("0.123456", "1", "");
  test("12345678910111213141516.12345678910111213141516", "+inf", "");
  // Exponent.
  test("15e1", "150", "");
  test("15*^8", "1500000000", "");
  test("1*^009", "1000000000", "");
  test("15*^111", "+inf", "");
  test("151515e+1", "1515150", "");
  test("151515151515151515e+1", "+inf", "");
  test("9200e-2", "92", "");
  // Exponent and fraction.
  test("5.3e3", "5300", "");
  test("2.2e-1", "1", "");

  // Hexadecimal.

  test("0x0.f", "1", "");
  fix("0x.f", "0", "");
  test("0x.f*^1", "15", "");
  fix("0x-f", "0", "x-f");
  test("0xfa", "250", "");
  test("-0xfa", "-250", "");
  // Fraction.
  test("0xfa.a", "251", "");
  // Exponent.
  test("0x1e2", "482", "");
  test("0x1*^2", "256", "");
  // Fraction and exponent.
  test("0x0.1*^3", "256", "");
  test("-0x29382a093589c501594f729e672567.2f09f342582b4598*^-2", "-2147483646", "");
  test("-0x29382a093589c501594f729e672567.2f09f342582b4598*^-20b", "0", "");
  //test("-0x29382a093589c501594f729e672567.2f09f342582b4598*^-20b3029", "0", "");  // FIX segfs

  // Base.

  test("3^^", "0", "");
  test("^^3", "nan", "");
  test("3^^1", "1", "");
  test("2^^0", "0", "");
  test("2^^1", "1", "");
  test("2^^10", "2", "");
  test("2^^11", "3", "");
  test("36^^z", "35", "");
  test("36^^yz", "1259", "");
  test("36^^xyz", "44027", "");
  test("37^^2", "nan", "");
  // Fraction.
  test("2^^11.1", "4", "");
  // Exponent.
  test("10^^2e3", "2000", "");
  test("8^^2e3", "1024", "");
  // Fraction and exponent.
  test("8^^2.1e3", "1088", "");
  test("8^^20402543.120347e7", "+inf", "");

  // Denominator.

  //test("15/0", "nan", "");
  test("15/1", "15", "");
  test("15/3", "5", "");
  fix("15/-3", "15", "/-3");
  // Exponent.
  test("15/30e-1", "5", "");
  test("27e3/30e-1", "9000", "");
  // Fraction.
  test("27.9/3.1", "9", "");
  // Exponent and fraction.
  test("27.9e3/30e-1", "9300", "");
  // Hexadecimal.
  test("0xf/0x3", "5", "");
  test("3048227.23429e3/0x230abc43", "6", "");
  // Base.
  test("16^^f/4^^3.0e0", "5", "");

  // Erroneous input.

  fix(".333", "nan", ".333");

  cout << endl;


  // Checked mpq_class.

#undef test
#define test test_template<mpq_class>

  cout << "Testing checked mpq_class:" << endl;

  test("inf", "+inf", "");
  fix("INF", "+inf", "");
  test("+inF", "+inf", "");
  test("-InF", "-inf", "");

  test("nan", "nan", "");
  test("NAN", "nan", "");
  test("Nan", "nan", "");

  test("nan/-3", "nan", "/-3");
  test("inf/3", "+inf", "/3");
  test("inf/-3", "+inf", "/-3");
  test("-inf/-3", "-inf", "/-3");

  // Integer.

  test("15", "15", "");
  test("34976098", "34976098", "");
  test("34976098349760983497609834976098", "34976098349760983497609834976098", "");
  fix("3/-inf", "3", "/-inf");
  test("+77", "77", "");
  test("-77", "-77", "");
  test("-7777777777777777777777777", "-7777777777777777777777777", "");
  // Fraction.
  test("71.3", "713/10", "");
  test("0.123456", "1929/15625", "");
  test("12345678910111213141516.12345678910111213141516", "308641972752780328537903086419727527803285379/25000000000000000000000", "");
  // Exponent.
  test("15e1", "150", "");
  test("15*^8", "1500000000", "");
  test("1*^009", "1000000000", "");
  test("15*^111", "15000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000", "");
  test("151515e+1", "1515150", "");
  test("151515151515151515e+1", "1515151515151515150", "");
  test("9200e-2", "92", "");
  // Exponent and fraction.
  test("5.3e3", "5300", "");
  test("2.2e-1", "11/50", "");

  // Hexadecimal.

  test("0x0.f", "15/16", "");
  fix("0x.f", "0", "");
  test("0x.f*^1", "15", "");
  fix("0x-f", "0", "x-f");
  test("0xfa", "250", "");
  test("-0xfa", "-250", "");
  // Fraction.
  test("0xfa.a", "2005/8", "");
  // Exponent.
  test("0x1e2", "482", "");
  test("0x1*^2", "256", "");
  // Fraction and exponent.
  test("0x0.1*^3", "256", "");
  test("-0x29382a093589c501594f729e672567.2f09f342582b4598*^-2", "-493504168323155221903720496056512238754896365637429427/590295810358705651712", "");
  test("-0x29382a093589c501594f729e672567.2f09f342582b4598*^-20b", "-493504168323155221903720496056512238754896365637429427/1310933508604055428645639323137378916605714125627786059833620428064344929699983798856350724770249351343264006014785110634052203016928447702417826694914166499203532724061309761600565421336997063991815557515680099256435208755020866043671114406449028627331696698190741203965924596719013282704476143226108174949247429837123641776308586821274746853953307040976937411766310985422056170406984474085761281737469468808976890729698039324009144871950806544374270234375377739131156048222163582026729343976248181187638137223873724172759146299690233903325378612205820465841687984250694283465351797146791878992198286281436600229186585471120819282194789204326612992", "");
  //test("-0x29382a093589c501594f729e672567.2f09f342582b4598*^-20b3029", "256", "");  // FIX segf

  // Base.

  test("3^^", "0", "");
  test("^^3", "nan", "");
  test("3^^1", "1", "");
  test("2^^0", "0", "");
  test("2^^1", "1", "");
  test("2^^10", "2", "");
  test("2^^11", "3", "");
  test("36^^z", "35", "");
  test("36^^yz", "1259", "");
  test("36^^xyz", "44027", "");
  test("37^^2", "nan", "");
  // Fraction.
  test("2^^11.1", "7/2", "");
  // Exponent.
  test("10^^2e3", "2000", "");
  test("8^^2e3", "1024", "");
  // Fraction and exponent.
  test("8^^2.1e3", "1088", "");
  test("8^^20402543.120347e7", "9073863231288", "");

  // Denominator.

  test("15/0", "nan", "");
  test("15/1", "15", "");
  test("15/3", "5", "");
  fix("15/-3", "15", "/-3");
  // Exponent.
  test("15/30e-1", "5", "");
  test("27e3/30e-1", "9000", "");
  // Fraction.
  test("27.9/3.1", "9", "");
  // Exponent and fraction.
  test("27.9e3/30e-1", "9300", "");
  // Hexadecimal.
  test("0xf/0x3", "5", "");
  test("3048227.23429e3/0x230abc43", "304822723429/58790611500", "");
  // Base.
  test("16^^f/4^^3.0e0", "5", "");

  // Erroneous input.

  fix(".333", "nan", ".333");

  return ret;
}
CATCH
