# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>
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
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.
#
# For the most up-to-date information see the Parma Polyhedra Library
# site: http://www.cs.unipr.it/ppl/ .

SUBDIRS = . Ciao GNU SICStus SWI XSB YAP tests

# Interface generator files.
interface_generator_files = \
ppl_interface_generator_prolog_dat.m4 \
ppl_interface_generator_prolog_icc.m4 \
ppl_interface_generator_prolog_systems.m4 \
ppl_interface_generator_prolog_icc_code.m4 \
ppl_interface_generator_prolog_icc_preamble

noinst_HEADERS = exceptions.hh track_allocation.hh \
                 $(interface_generator_files)

nodist_noinst_HEADERS = ppl_prolog.icc

ppl_prolog.icc: $(interface_generator_files) Makefile
	m4 -I.. -I$(srcdir) -I$(srcdir)/.. \
           $(srcdir)/ppl_interface_generator_prolog_icc.m4 > $@
