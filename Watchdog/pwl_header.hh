/* This is the header file of the Parma Watchdog Library.
   Copyright (C) 2002-2005 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Watchdog Library (PWL).

The PWL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The PWL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the CS@Parma software
site: http://www.cs.unipr.it/Software/ . */

#ifndef PWL_pwl_hh
#define PWL_pwl_hh 1

#ifdef NDEBUG
# define PWL_SAVE_NDEBUG 1
# undef NDEBUG
#endif

#include "config.h"
#include "pwl_include_files.hh"

#ifdef PWL_SAVE_NDEBUG
# define NDEBUG 1
# undef PWL_SAVE_NDEBUG
#else
# undef NDEBUG
#endif
#include <cassert>

#undef PACKAGE
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#undef VERSION

#endif
