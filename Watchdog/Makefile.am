# automake source file for the Parma Watchdog Library.
# Copyright (C) 2002 Roberto Bagnara <bagnara@cs.unipr.it>
#
# This file is part of the Parma Watchdog Library (PWL).
#
# The PWL is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2 of the License, or (at your
# option) any later version.
#
# The PWL is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
# USA.
#
# For the most up-to-date information see
# http://www.cs.unipr.it/~bagnara/ .

srcdir = @srcdir@
VPATH = @srcdir@

EXTRA_DIST = Watchdog.hh Watchdog.cc configure BUGS CREDITS

@SET_MAKE@

if BUILD_WATCHDOG_LIBRARY

lib_LTLIBRARIES = libwatchdog.la
libwatchdog_la_SOURCES = Watchdog.cc

include_HEADERS = Watchdog.hh

endif
