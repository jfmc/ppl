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
abs_srcdir = @abs_srcdir@
VPATH = @srcdir@

@SET_MAKE@

if HAVE_SWIG

SWIG_GENERATED_FILES = \
$(srcdir)/ppl_ocaml_c.ml \
$(srcdir)/ppl_ocaml_c.mli \
$(srcdir)/ppl_ocaml_c_wrap.c

$(SWIG_GENERATED_FILES): ppl_ocaml_c.swg $(top_srcdir)/interfaces/C
	$(SWIG) -ocaml -I../C $<

SWIG_SUPPORT_FILES = \
$(srcdir)/swig.mli \
$(srcdir)/swig.ml

$(SWIG_SUPPORT_FILES):
	$(SWIG) -ocaml -o $@ -co `basename $@`

%.cmi: $(srcdir)/%.mli
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $< . ; \
	fi
	ocamlc -g -c `basename $<`
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f `basename $<`; \
	fi

%.cmo: $(srcdir)/%.ml
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $< . ; \
	fi
	ocamlc -g -c `basename $<`
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f `basename $<`; \
	fi


##nodist_PROGRAM = test

test:	libppl_ocaml_c.la swig.cmi swig.cmo ppl_ocaml_c.cmi ppl_ocaml_c.cmo test.cmo 
	ocamlc -g -custom -ccopt -g swig.cmo ppl_ocaml_c.cmo  test.cmo -cclib -lppl -cclib -lppl_c -cclib -lgmpxx -cclib -lgmp -cclib .libs/libppl_ocaml_c.a

dist-hook: $(SWIG_GENERATED_FILES) $(SWIG_SUPPORT_FILES)

EXTRA_DIST= \
gmp.h \
ppl_ocaml_c.swg \
$(SWIG_GENERATED_FILES) \
$(SWIG_SUPPORT_FILES)

# Libtool -version-info for libppl_ocaml_c.la.
#
#  1. Start with version information of `0:0:0' for each libtool library.
#
#  2. Update the version information only immediately before a public
#     release of your software.  More frequent updates are unnecessary,
#     and only guarantee that the current interface number gets larger
#     faster.
#
#  3. If the library source code has changed at all since the last
#     update, then increment REVISION (`C:R:A' becomes `C:r+1:A').
#
#  4. If any interfaces have been added, removed, or changed since the
#     last update, increment CURRENT, and set REVISION to 0.
#
#  5. If any interfaces have been added since the last public release,
#     then increment AGE.
#
#  6. If any interfaces have been removed since the last public release,
#     then set AGE to 0.
#
#	PPL release		-version-info
#	    0.1			    -----
#	    0.2			    -----
#	    0.3			    -----
#	    0.4			    -----
#	    0.5			    -----
#	    0.6			    0:0:0

LIBPPL_OCAML_C_LT_CURRENT =  0
LIBPPL_OCAML_C_LT_REVISION = 0
LIBPPL_OCAML_C_LT_AGE =      0

lib_LTLIBRARIES = libppl_ocaml_c.la
libppl_ocaml_c_la_CFLAGS = -w
libppl_ocaml_c_la_CPPFLAGS = -I/usr/lib/ocaml
libppl_ocaml_c_la_SOURCES = $(srcdir)/ppl_ocaml_c_wrap.c
libppl_ocaml_c_la_LDFLAGS = \
-version-info $(LIBPPL_OCAML_C_LT_CURRENT):$(LIBPPL_OCAML_C_LT_REVISION):$(LIBPPL_OCAML_C_LT_AGE)

#mostlyclean-local:
#	rm -rf $(ALL_TARGETS) $(LATEX_DIRS) *.aux *.log

CLEANFILES = \
swig.cmi \
swig.cmo \
ppl_ocaml_c.cmi \
ppl_ocaml_c.cmo

else # !HAVE_SWIG

EXTRA_DIST = \
gmp.h \
ppl_ocaml_c.swg

endif
