dnl A function to check for the possibility to control the FPU.
dnl Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>
dnl
dnl This file is part of the Parma Polyhedra Library (PPL).
dnl
dnl The PPL is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by the
dnl Free Software Foundation; either version 3 of the License, or (at your
dnl option) any later version.
dnl
dnl The PPL is distributed in the hope that it will be useful, but WITHOUT
dnl ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
dnl FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
dnl for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software Foundation,
dnl Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.
dnl
dnl For the most up-to-date information see the Parma Polyhedra Library
dnl site: http://www.cs.unipr.it/ppl/ .

AC_DEFUN([AC_CHECK_FPU_CONTROL],
[
AC_LANG_PUSH(C++)
AC_CHECK_HEADERS([fenv.h ieeefp.h])
AC_MSG_CHECKING([for the possibility to control the FPU])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
int
main() {
#if i386
#elif defined(HAVE_FENV_H)
#elif sparc && defined( HAVE_IEEEFP_H)
#else
  choke me
#endif

  ;
  return 0;
}
]])],
  AC_MSG_RESULT(yes)
  ac_cv_can_control_fpu=1,
  AC_MSG_RESULT(no)
  ac_cv_can_control_fpu=0
),
AM_CONDITIONAL(CAN_CONTROL_FPU, test $ac_cv_can_control_fpu = 1)
AC_DEFINE_UNQUOTED(CAN_CONTROL_FPU, $ac_cv_can_control_fpu,
    [Not zero if the FPU can be controlled.])
AC_LANG_POP(C++)
])
