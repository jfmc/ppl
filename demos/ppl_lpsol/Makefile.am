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

srcdir = @srcdir@
VPATH = @srcdir@

@SET_MAKE@

SUBDIRS = examples

EXTRA_DIST = \
ppl_lpsol.c \
expected_mpz \
expected_mpz_a \
expected_int8 \
expected_int8_a \
expected_int16 \
expected_int16_a \
expected_int32 \
expected_int32_a \
expected_int64 \
expected_int64_a

if HAVE_GLPK

AM_CPPFLAGS = -I$(top_builddir)/interfaces/C @extra_includes@

noinst_PROGRAMS = ppl_lpsol
# dummy.cc serves the only purpose to force linking using the C++ compiler.
ppl_lpsol_SOURCES = \
ppl_lpsol.c \
dummy.cc
ppl_lpsol_LDADD = \
-lglpk \
$(top_builddir)/interfaces/C/libppl_c.la \
$(top_builddir)/src/libppl.la \
@extra_libraries@

COEFFICIENT_MNEMONIC = @coefficient_mnemonic@

if ASSERTIONS_ENABLED

TOUGH_EXAMPLES_IF_ASSERTIONS_ENABLED = \
afiro.mps

EXPECTED = expected_$(COEFFICIENT_MNEMONIC)_a

else !ASSERTIONS_ENABLED

EXPECTED = expected_$(COEFFICIENT_MNEMONIC)

endif !ASSERTIONS_ENABLED

TOUGH_EXAMPLES = \
$(TOUGH_EXAMPLES_IF_ASSERTIONS_ENABLED)

check-local: ppl_lpsol
	echo $(TOUGH_EXAMPLES) >tough_examples
	echo -n >obtained
	LANG=C; \
	for path in $(srcdir)/examples/*.mps; \
	do \
	  file=`basename $$path`; \
	  grep -q $$file tough_examples && continue; \
	  for m in --min --max; \
	  do \
	    echo "*** ppl_lpsol -v $$m -oobtained $$file" >>obtained; \
	    ./ppl_lpsol -v $$m -oobtained $$path >/dev/null; \
	  done \
	done
	diff -q --ignore-all-space $(srcdir)/$(EXPECTED) obtained

MOSTLYCLEANFILES = \
tough_examples \
obtained

endif HAVE_GLPK
