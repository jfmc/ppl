dnl Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>
dnl  
dnl This file is free software; as a special exception the author gives
dnl unlimited permission to copy and/or distribute it, with or without 
dnl modifications, as long as this notice is preserved.
dnl 
dnl This program is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
dnl implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
dnl
AC_DEFUN([AC_CHECK_HAVE_GMP],
[AC_CACHE_CHECK([for the GMP library],
ac_cv_have_gmp,
[AC_LANG_SAVE
 AC_LANG_CPLUSPLUS
 AC_TRY_RUN([
#include <gmpxx.h>

using namespace std;

int main() {
  mpz_class pie = "3141592653589793238462643383279502884";
  exit(0);
}
],
 ac_cv_have_gmp=yes,
 ac_cv_have_gmp=no,
 ac_cv_have_gmp=no)
 AC_LANG_RESTORE
])
have_gmp=${ac_cv_have_gmp}
])
