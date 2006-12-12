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

abs_srcdir = @abs_srcdir@

if BUILD_OCAML_INTERFACE

OCAMLC_FLAGS = -I +gmp
OCAMLC_ROOT = @ocamlc_root@

%.cmi: $(srcdir)/%.mli
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $< . ; \
	fi
	ocamlc -g -c $(OCAMLC_FLAGS) `basename $<`
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f `basename $<`; \
	fi

%.cmo: $(srcdir)/%.ml
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $< . ; \
	fi
	ocamlc -g -c $(OCAMLC_FLAGS) `basename $<`
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f `basename $<`; \
	fi

noinst_LIBRARIES = libppl_ocaml.a
libppl_ocaml_a_SOURCES = \
ppl_ocaml.cc

AM_CPPFLAGS = -I$(OCAMLC_ROOT) -I$(top_srcdir)/src -I$(top_builddir)/src @extra_includes@

noinst_PROGRAM = test1

test1$(EXEEXT): ppl_ocaml_globals.cmo ppl_ocaml.cmi ppl_ocaml.cmo libppl_ocaml.a test1.cmo
	ocamlc -o $@ -cc g++ -g -custom $(OCAMLC_FLAGS) gmp.cma  ppl_ocaml.cmo -ccopt -g test1.cmo -cclib -L. -cclib -lppl_ocaml -cclib -L../../src/.libs -cclib -lppl -cclib -lmlgmp -cclib -lgmpxx -cclib -lgmp

EXTRA_DIST=

#mostlyclean-local:
#	rm -rf $(ALL_TARGETS)

CLEANFILES = \
ppl_ocaml_globals.cmi \
ppl_ocaml_globals.cmo \
ppl_ocaml.cmi \
ppl_ocaml.cmo \
test1.cmi \
test1.cmo

else # !HAVE_OCAML_COMPILER

EXTRA_DIST =

endif
