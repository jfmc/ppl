# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>
#
# This file is part of the Parma Polyhedra Library (PPL).
#
# The PPL is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2 of the License, or (at your
# option) any later version.
#
# The PPL is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
# USA.
#
# For the most up-to-date information see the Parma Polyhedra Library
# site: http://www.cs.unipr.it/ppl/ .

# Tell aclocal where to find `.m4' files.
ACLOCAL_AMFLAGS = -I m4

DISTCHECK_CONFIGURE_FLAGS = \
--enable-shared --enable-watchdog --with-cxxflags=-Wno-deprecated

srcdir = @srcdir@
VPATH = @srcdir@

EXTRA_DIST = \
configure \
ppl.lsm.in \
ppl.spec.in \
BUGS CREDITS STANDARDS

@SET_MAKE@

#SUBDIRS = src Watchdog tests interfaces demos doc m4
SUBDIRS = src Watchdog tests demos doc m4
