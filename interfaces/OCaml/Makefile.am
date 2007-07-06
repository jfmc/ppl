# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>
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
ppl_interface_generator_ocaml_cc.m4 \
ppl_interface_generator_ocaml_cc_code.m4 \
ppl_interface_generator_ocaml_dat.m4 \
ppl_interface_generator_ocaml_ml.m4 \
ppl_interface_generator_ocaml_ml_code.m4 \
ppl_interface_generator_ocaml_mli.m4 \
ppl_interface_generator_ocaml_mli_code.m4

original_mls = \
ppl_ocaml_globals.ml \
ppl_ocaml_types.ml \
test1.ml

EXTRA_DIST = \
$(interface_generator_files) \
$(original_mls) \
ppl_ocaml_globals.cc


BUILT_SOURCES = \
ppl_ocaml.cc \
ppl_ocaml.ml \
ppl_ocaml.mli \
test1.ml

interface_generator_dependencies = \
$(top_builddir)/interfaces/ppl_interface_instantiations.m4 \
../ppl_interface_generator_common.m4 \
../ppl_interface_generator_common_dat.m4 \
../ppl_interface_generator_copyright \
ppl_interface_generator_ocaml_dat.m4 \
ppl_interface_generator_ocaml_ml.m4 \
ppl_interface_generator_ocaml_ml_code.m4 \
ppl_interface_generator_ocaml_cc.m4 \
ppl_interface_generator_ocaml_cc_code.m4

copied_mls.stamp:
	if [ $(abs_srcdir) != `pwd` ]; then \
	for mlfile in $(original_mls); \
	 do \
	cp -f $(srcdir)/$$mlfile . ; \
	done \
	fi
	echo timestamp > copied_mls.stamp

# Use basename to always use the build directory sources.
.mli.cmi:
	ocamlc -g -c  $(OCAMLC_FLAGS) `basename $<`

.ml.cmo:
	ocamlc -g -c  $(OCAMLC_FLAGS) `basename  $<`


ppl_ocaml.cmo: copied_mls.stamp ppl_ocaml_types.cmo ppl_ocaml_globals.cmo ppl_ocaml.cmi

delete_mls.stamp: ppl_ocaml_types.cmo ppl_ocaml_globals.cmo
	if [ $(abs_srcdir) != `pwd` ]; then \
	for mlfile in $(original_mls); \
	 do \
	if [ -a $(top_builddir)/interfaces/OCaml/$$mlfile ]; then \
	  rm -f $(top_builddir)/interfaces/OCaml/$$mlfile  ; \
	fi \
	done \
	fi
	echo timestamp > delete_mls.stamp

ppl_ocaml.ml: $(interface_generator_dependencies) Makefile
	m4 --prefix-builtin -I.. -I$(srcdir) -I$(srcdir)/.. \
           $(srcdir)/ppl_interface_generator_ocaml_ml.m4 > $@

ppl_ocaml.mli: $(interface_generator_dependencies) Makefile
	m4 --prefix-builtin -I.. -I$(srcdir) -I$(srcdir)/.. \
           $(srcdir)/ppl_interface_generator_ocaml_mli.m4 > $@

ppl_ocaml.cc: $(interface_generator_dependencies) Makefile
	m4 --prefix-builtin -I.. -I$(srcdir) -I$(srcdir)/.. \
           $(srcdir)/ppl_interface_generator_ocaml_cc.m4 > $@

OCAMLC_FLAGS = -I +gmp -I $(srcdir)

OCAMLC_ROOT = @ocamlc_root@

noinst_LIBRARIES = libppl_ocaml.a

nodist_libppl_ocaml_a_SOURCES = \
ppl_ocaml.cc

AM_CPPFLAGS = -I$(OCAMLC_ROOT) -I$(top_srcdir)/src -I$(top_builddir)/src @extra_includes@

noinst_PROGRAM = test1

ppl_ocaml.cma: ppl_ocaml.cmo
	 ocamlc -a -custom ppl_ocaml.cmo -I . $(OCAMLC_FLAGS) -cclib -lppl_ocaml   -cclib -lppl -o ppl_ocaml.cma


# Force to create the interface when a `make' is given.
# FIXME: this is a temporary patch.
all:  libppl_ocaml.a ppl_ocaml.cma delete_mls.stamp

test1$(EXEEXT):  all
	if [ $(abs_srcdir) != `pwd` ]; then \
	cp -f $(srcdir)/test1.ml . ; \
	fi
	ocamlc -g -c  $(OCAMLC_FLAGS) test1.ml
	ocamlc -o $@ -cc g++ -g -custom $(OCAMLC_FLAGS)  ppl_ocaml.cma gmp.cma -ccopt -g test1.cmo
	if [ $(abs_srcdir) != `pwd` ]; then \
	rm -f test1.ml ; \
	fi

ocamldir = $(ocamlc_root)/ppl

ocaml_DATA = \
libppl_ocaml.a \
ppl_ocaml.cma \
ppl_ocaml.cmi \
ppl_ocaml.cmo \
ppl_ocaml.mli \
ppl_ocaml_globals.cmi \
ppl_ocaml_globals.cmo \
ppl_ocaml_types.cmi \
ppl_ocaml_types.cmo

CLEANFILES = \
delete_mls.stamp \
copied_mls.stamp \
ppl_ocaml.cc \
ppl_ocaml.cma \
ppl_ocaml.cmi \
ppl_ocaml.cmo \
ppl_ocaml.ml \
ppl_ocaml.mli \
ppl_ocaml_globals.cmi \
ppl_ocaml_globals.cmo \
ppl_ocaml_types.cmi \
ppl_ocaml_types.cmo \
test1.cmi \
test1.cmo


else # !HAVE_OCAML_COMPILER

EXTRA_DIST =

endif
