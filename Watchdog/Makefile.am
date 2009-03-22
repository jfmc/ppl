# Automake source file for the Parma Watchdog Library.
# Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>
#
# This file is part of the Parma Watchdog Library (PWL).
#
# The PWL is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at your
# option) any later version.
#
# The PWL is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.
#
# For the most up-to-date information see the Parma Polyhedra Library
# site: http://www.cs.unipr.it/ppl/ . */

# Tell aclocal where to find `.m4' files.
ACLOCAL_AMFLAGS = -I m4

dist_noinst_SCRIPTS = \
configure

if BUILD_WATCHDOG_LIBRARY

SUBDIRS = . m4 utils src tests doc

nodist_noinst_HEADERS = \
pwl-config.h

pwl-config.h: config.h $(srcdir)/pwl-config.sed
	$(SED) -f $(srcdir)/pwl-config.sed $< >$@

endif BUILD_WATCHDOG_LIBRARY

EXTRA_DIST = \
pwl-config.sed \
BUGS \
CREDITS

DISTCLEANFILES = \
pwl-config.h
