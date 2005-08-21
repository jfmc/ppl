dnl A function to check for the existence and usability of GMP.
dnl Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>
dnl
dnl This file is part of the Parma Polyhedra Library (PPL).
dnl
dnl The PPL is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by the
dnl Free Software Foundation; either version 2 of the License, or (at your
dnl option) any later version.
dnl
dnl The PPL is distributed in the hope that it will be useful, but WITHOUT
dnl ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
dnl FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
dnl for more details.
dnl
dnl You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.
dnl
dnl For the most up-to-date information see the Parma Polyhedra Library
dnl site: http://www.cs.unipr.it/ppl/ .
dnl
AC_DEFUN([AC_CHECK_SWI_PROLOG],
[
dnl By default, SWI-Prolog is installed as `pl', though some administrators
dnl call it `swipl' or `swi-prolog'.  In particular, on Mac OS X `pl' is
dnl the name of another program.
AC_PATH_PROG(swi_prolog, swi-prolog)
if test -z $swi_prolog
then
  AC_PATH_PROG(swi_prolog, swipl)
  if test -z $swi_prolog
  then
    AC_PATH_PROG(swi_prolog, pl)
  fi
fi
if test x$swi_prolog != x
then
  swi_prolog_base=`$swi_prolog -dump-runtime-variables | grep PLBASE= | sed 's/PLBASE="\(.*\)";/\1/'`
  swi_prolog_version=`$swi_prolog -dump-runtime-variables | grep PLVERSION= | sed 's/PLVERSION="\(.*\)";/\1/'`

  dnl Version checks should be inserted here, if necessary.

  SWI_PROLOG_INCLUDE_DIRECTORY="$swi_prolog_base/include"
  ac_save_CPPFLAGS="$CPPFLAGS"
  CPPFLAGS="$CPPFLAGS -I$SWI_PROLOG_INCLUDE_DIRECTORY"
  AC_LANG_PUSH(C++)
  AC_CHECK_HEADER(SWI-Prolog.h, [], swi_prolog="")
  AC_LANG_POP(C++)
  CPPFLAGS="$ac_save_CPPFLAGS"
  AC_SUBST(SWI_PROLOG_INCLUDE_DIRECTORY)
fi

AM_CONDITIONAL(HAVE_SWI_PROLOG, test x$swi_prolog != x)
])
