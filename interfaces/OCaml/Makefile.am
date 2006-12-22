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

interface_generator_files = \
ppl_interface_generator_ocaml_dat.m4 \
ppl_interface_generator_ocaml_ml.m4 \
ppl_interface_generator_ocaml_ml_code.m4 \
ppl_interface_generator_ocaml_cc.m4 \
ppl_interface_generator_ocaml_cc_code.m4

EXTRA_DIST = $(interface_generator_files)

BUILT_SOURCES = \
ppl_ocaml.ml \
ppl_ocaml.cc

interface_generator_dependencies = \
../ppl_interface_instantiations.m4 \
../ppl_interface_generator_common.m4 \
../ppl_interface_generator_common_dat.m4 \
../ppl_interface_generator_copyright \
ppl_interface_generator_ocaml_dat.m4 \
ppl_interface_generator_ocaml_ml.m4 \
ppl_interface_generator_ocaml_ml_code.m4 \
ppl_interface_generator_ocaml_cc.m4 \
ppl_interface_generator_ocaml_cc_code.m4

ppl_ocaml.ml: $(interface_generator_dependencies) Makefile
	m4 --prefix-builtin -I.. -I$(srcdir) -I$(srcdir)/.. \
           $(srcdir)/ppl_interface_generator_ocaml_ml.m4 > $@

ppl_ocaml.cc: $(interface_generator_dependencies) Makefile
	m4 --prefix-builtin -I.. -I$(srcdir) -I$(srcdir)/.. \
           $(srcdir)/ppl_interface_generator_ocaml_cc.m4 > $@

OCAMLC_FLAGS = -I +gmp
OCAMLC_ROOT = @ocamlc_root@

%.cmi: %.mli
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $< . ; \
	fi
	ocamlc -g -c $(OCAMLC_FLAGS) `basename $<`
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f `basename $<`; \
	fi

%.cmo: %.ml
	if [ -a `basename $<` ]; then \
	ocamlc -g -c $(OCAMLC_FLAGS) `basename $<`; \
	else \
	if [ $(abs_srcdir) != `pwd` ]; then \
	  cp -f $< . ; \
	fi; \
	ocamlc -g -c $(OCAMLC_FLAGS) `basename $<`; \
	if [ $(abs_srcdir) != `pwd` ]; then \
	  rm -f `basename $<`; \
	fi \
	fi

noinst_LIBRARIES = libppl_ocaml.a

libppl_ocaml_a_SOURCES = \
ppl_ocaml.cc

AM_CPPFLAGS = -I$(OCAMLC_ROOT) -I$(top_srcdir)/src -I$(top_builddir)/src @extra_includes@

noinst_PROGRAM = test1

ppl_ocaml.cma: ppl_ocaml.cmo
	 ocamlc -a -custom ppl_ocaml.cmo -I . $(OCAMLC_FLAGS) -cclib -lppl_ocaml   -cclib -lppl -o ppl_ocaml.cma


# Force to create the interface when a `make' is given.
# FIXME: this is a temporary patch.
all: ppl_ocaml_types.cmo ppl_ocaml.cmi ppl_ocaml_globals.cmo ppl_ocaml.cmo libppl_ocaml.a test1.cmo ppl_ocaml.cma


test1$(EXEEXT):  all
	ocamlc -o $@ -cc g++ -g -custom $(OCAMLC_FLAGS)  ppl_ocaml.cma gmp.cma -ccopt -g test1.cmo

#mostlyclean-local:
#	rm -rf $(ALL_TARGETS)


ocamldir = $(ocamlc_root)/ppl

ocaml_DATA = \
ppl_ocaml_types.cmi \
ppl_ocaml_types.cmo \
ppl_ocaml_globals.cmi \
ppl_ocaml_globals.cmo \
ppl_ocaml.cmi \
ppl_ocaml.cmo \
ppl_ocaml.cma \
ppl_ocaml.mli \
libppl_ocaml.a

CLEANFILES = \
ppl_ocaml_types.cmi \
ppl_ocaml_types.cmo \
ppl_ocaml_globals.cmi \
ppl_ocaml_globals.cmo \
ppl_ocaml.cma \
ppl_ocaml.cmi \
ppl_ocaml.cmo \
ppl_ocaml.ml \
ppl_ocaml.cc \
test1.cmi \
test1.cmo

else # !HAVE_OCAML_COMPILER

EXTRA_DIST =

endif
