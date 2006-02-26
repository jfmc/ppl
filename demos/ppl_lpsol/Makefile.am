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

if BUILD_C_INTERFACE
if HAVE_GLPK

AM_CPPFLAGS = -I$(top_builddir)/interfaces/C @extra_includes@

bin_PROGRAMS = ppl_lpsol
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

TOUGH_EXAMPLES_FOR_ENUM_IF_ASSERTIONS_ENABLED = \
afiro.mps

EXPECTED = expected_$(COEFFICIENT_MNEMONIC)_a

else !ASSERTIONS_ENABLED

EXPECTED = expected_$(COEFFICIENT_MNEMONIC)

endif !ASSERTIONS_ENABLED

TOUGH_EXAMPLES_FOR_ENUM = \
adlittle.mps \
blend.mps \
boeing1.mps \
boeing2.mps \
kb2.mps \
mip.mps \
sc105.mps \
sc50a.mps \
sc50b.mps \
ship08l.mps \
$(TOUGH_EXAMPLES_FOR_ENUM_IF_ASSERTIONS_ENABLED)

TOUGH_EXAMPLES_FOR_SIMPLEX = \
ship08l.mps

check-local: ppl_lpsol
	echo -n >obtained
	echo $(TOUGH_EXAMPLES_FOR_ENUM) >tough_examples
	LANG=C; \
	for path in $(srcdir)/examples/*.mps; \
	do \
	  file=`basename $$path`; \
	  grep $$file tough_examples >/dev/null 2>&1 && continue; \
	  for m in -m -M; \
	  do \
	    echo "*** ppl_lpsol -e $$m -oobtained $$file" >>obtained; \
	    ./ppl_lpsol -e $$m -oobtained $$path >/dev/null; \
	  done \
	done
	echo $(TOUGH_EXAMPLES_FOR_SIMPLEX) >tough_examples
	for path in $(srcdir)/examples/*.mps; \
	do \
	  file=`basename $$path`; \
	  grep $$file tough_examples >/dev/null 2>&1 && continue; \
	  for m in -m -M; \
	  do \
	    echo "*** ppl_lpsol -s $$m -oobtained $$file" >>obtained; \
	    ./ppl_lpsol -s $$m -oobtained $$path >/dev/null; \
	  done \
	done
	diff -u --ignore-all-space $(srcdir)/$(EXPECTED) obtained

MOSTLYCLEANFILES = \
tough_examples \
obtained

endif HAVE_GLPK
endif BUILD_C_INTERFACE
