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

interface_generator_files = \
ppl_interface_generator_ocaml_procedure_generators.m4 \
ppl_interface_generator_ocaml_cc.m4 \
ppl_interface_generator_ocaml_hh_files.m4 \
ppl_interface_generator_ocaml_cc_files.m4 \
ppl_interface_generator_ocaml_hh_code.m4 \
ppl_interface_generator_ocaml_cc_code.m4 \
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
ppl_ocaml_globals.hh \
ppl_ocaml_globals.cc \
OCaml_interface.dox


if BUILD_OCAML_INTERFACE

ocamldir = $(pkglibdir)

ocaml_DATA = \
libppl_ocaml.a \
ppl_ocaml.cma \
ppl_ocaml.cmi \
ppl_ocaml.mli \
ppl_ocaml_globals.cmi \
ppl_ocaml_types.cmi

noinst_LIBRARIES = libppl_ocaml.a

nodist_libppl_ocaml_a_SOURCES = \
ppl_ocaml.cc

OCAMLC_COMPILE_FLAGS = \
-I +gmp -I $(srcdir) -I .. \
-ccopt -g

OCAMLC_LINK_FLAGS = \
`echo " $(extra_libraries) " | $(SED) -e "s/ -R[^ ]*//g" -e "s/ -/ -cclib -/g"`

OCAMLC_ROOT = @ocamlc_root@

AM_CPPFLAGS = \
-I$(OCAMLC_ROOT) \
-I$(top_srcdir)/interfaces \
-I$(top_srcdir)/src \
-I$(top_builddir)/src \
@extra_includes@

interface_generator_dependencies = \
$(top_builddir)/interfaces/ppl_interface_instantiations.m4 \
../ppl_interface_generator_common.m4 \
../ppl_interface_generator_common_dat.m4 \
../ppl_interface_generator_copyright \
$(interface_generator_files)

ppl_ocaml.cc: $(interface_generator_dependencies)
	m4 --prefix-builtin -I.. -I$(srcdir) -I$(srcdir)/.. \
           $(srcdir)/ppl_interface_generator_ocaml_cc.m4 > $@

ppl_ocaml.cma: ppl_ocaml_types.cmo ppl_ocaml_globals.cmo ppl_ocaml.cmo
	ocamlc -o $@ -a -custom \
		ppl_ocaml_types.cmo ppl_ocaml_globals.cmo ppl_ocaml.cmo \
		$(OCAMLC_COMPILE_FLAGS) -cclib -lppl_ocaml \
		$(OCAMLC_LINK_FLAGS)

ppl_ocaml.cmo: ppl_ocaml.ml ppl_ocaml.cmi ppl_ocaml_types.cmi
	ocamlc -o $@ -g -c $(OCAMLC_COMPILE_FLAGS) ppl_ocaml.ml

ppl_ocaml.cmi: ppl_ocaml.mli
	ocamlc -o $@ -g -c $(OCAMLC_COMPILE_FLAGS) $<

ppl_ocaml_globals.cmo: ppl_ocaml_globals.ml ppl_ocaml_types.cmi
	ocamlc -o $@ -g -c $(OCAMLC_COMPILE_FLAGS) \
		$(srcdir)/ppl_ocaml_globals.ml

ppl_ocaml_globals.cmi: ppl_ocaml_globals.cmo
	@if test -f $@; then :; else \
		rm -f ppl_ocaml_globals.cmo; \
		$(MAKE) $(AM_MAKEFLAGS) ppl_ocaml_globals.cmo; \
	fi

ppl_ocaml_types.cmo: ppl_ocaml_types.ml
	ocamlc -o $@ -g -c $(OCAMLC_COMPILE_FLAGS) \
		$(srcdir)/ppl_ocaml_types.ml

ppl_ocaml_types.cmi: ppl_ocaml_types.cmo
	@if test -f $@; then :; else \
		rm -f ppl_ocaml_types.cmo; \
		$(MAKE) $(AM_MAKEFLAGS) ppl_ocaml_types.cmo; \
	fi

ppl_ocaml.ml: $(interface_generator_dependencies)
	m4 --prefix-builtin -I.. -I$(srcdir) -I$(srcdir)/.. \
           $(srcdir)/ppl_interface_generator_ocaml_ml.m4 > $@

ppl_ocaml.mli: $(interface_generator_dependencies)
	m4 --prefix-builtin -I.. -I$(srcdir) -I$(srcdir)/.. \
           $(srcdir)/ppl_interface_generator_ocaml_mli.m4 > $@

ppl_ocaml_domains_cc_files: $(interface_generator_dependencies)
	m4 --prefix-builtin -I.. -I$(srcdir) -I$(srcdir)/.. \
           $(srcdir)/ppl_interface_generator_ocaml_cc_files.m4 \
                > ppl_ocaml_cc_blob
	$(top_srcdir)/utils/cm_cleaner.sh < ppl_ocaml_cc_blob
	$(top_srcdir)/utils/cm_splitter.sh < ppl_ocaml_cc_blob
	rm -f ppl_ocaml_cc_blob
	echo timestamp >$@

ppl_ocaml_domains_hh_files: $(interface_generator_dependencies)
	m4 --prefix-builtin -I.. -I$(srcdir) -I$(srcdir)/.. \
           $(srcdir)/ppl_interface_generator_ocaml_hh_files.m4 \
                > ppl_ocaml_hh_blob
	$(top_srcdir)/utils/cm_cleaner.sh < ppl_ocaml_hh_blob
	$(top_srcdir)/utils/cm_splitter.sh < ppl_ocaml_hh_blob
	rm -f ppl_ocaml_hh_blob
	echo timestamp >$@

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
