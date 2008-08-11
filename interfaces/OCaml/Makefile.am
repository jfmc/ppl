# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>
#
# This file is part of the Parma Polyhedra Library (PPL).
#
# The PPL is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at your
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

SUBDIRS = . tests

abs_srcdir = @abs_srcdir@

interface_generator_files = \
ppl_interface_generator_ocaml_cc.m4 \
ppl_interface_generator_ocaml_cc_code.m4 \
ppl_interface_generator_ocaml_procedure_generators.m4 \
ppl_interface_generator_ocaml_ml.m4 \
ppl_interface_generator_ocaml_ml_code.m4 \
ppl_interface_generator_ocaml_mli.m4 \
ppl_interface_generator_ocaml_mli_code.m4

ml_source_files = \
ppl_ocaml_globals.ml \
ppl_ocaml_types.ml

EXTRA_DIST = \
$(interface_generator_files) \
$(ml_source_files) \
ppl_ocaml_globals.cc


if BUILD_OCAML_INTERFACE

BUILT_SOURCES = \
ppl_ocaml.cc \
ppl_ocaml.ml \
ppl_ocaml.mli

interface_generator_dependencies = \
$(top_builddir)/interfaces/ppl_interface_instantiations.m4 \
../ppl_interface_generator_common.m4 \
../ppl_interface_generator_common_dat.m4 \
../ppl_interface_generator_copyright \
ppl_interface_generator_ocaml_procedure_generators.m4 \
ppl_interface_generator_ocaml_ml.m4 \
ppl_interface_generator_ocaml_ml_code.m4 \
ppl_interface_generator_ocaml_mli.m4 \
ppl_interface_generator_ocaml_mli_code.m4 \
ppl_interface_generator_ocaml_cc.m4 \
ppl_interface_generator_ocaml_cc_code.m4

.mli.cmi:
	ocamlc -g -c $(OCAMLC_FLAGS) $<

.ml.cmo:
	ocamlc -g -c $(OCAMLC_FLAGS) $<

ppl_ocaml_globals.cmo: ppl_ocaml_types.cmo
ppl_ocaml.cmo: ppl_ocaml_globals.cmo ppl_ocaml.cmi

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

AM_CPPFLAGS = \
-I$(OCAMLC_ROOT) \
-I$(top_srcdir)/interfaces \
-I$(top_srcdir)/src \
-I$(top_builddir)/src \
@extra_includes@

ppl_ocaml.cma: ppl_ocaml_types.cmo ppl_ocaml_globals.cmo ppl_ocaml.cmo
	ocamlc -o $@ -a -custom \
		ppl_ocaml_types.cmo ppl_ocaml_globals.cmo ppl_ocaml.cmo \
		$(OCAMLC_FLAGS) -cclib -lppl_ocaml   -cclib -lppl


# Force to create the interface when a `make' is given.
# FIXME: this is a temporary patch.
all:  libppl_ocaml.a ppl_ocaml.cma

ocamldir = $(pkglibdir)

ocaml_DATA = \
libppl_ocaml.a \
ppl_ocaml.cma \
ppl_ocaml.cmi \
ppl_ocaml.mli \
ppl_ocaml_globals.cmi \
ppl_ocaml_types.cmi

endif BUILD_OCAML_INTERFACE

CLEANFILES = \
ppl_ocaml.cc \
ppl_ocaml.cma \
ppl_ocaml.cmi \
ppl_ocaml.cmo \
ppl_ocaml.ml \
ppl_ocaml.mli \
ppl_ocaml_globals.cmi \
ppl_ocaml_globals.cmo \
ppl_ocaml_types.cmi \
ppl_ocaml_types.cmo
