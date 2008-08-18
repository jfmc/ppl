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

interface_generator_files = \
ppl_interface_generator_ocaml_test_ml.m4 \
ppl_interface_generator_ocaml_test_ml_code.m4

if BUILD_OCAML_INTERFACE

interface_generator_dependencies = \
$(top_builddir)/interfaces/ppl_interface_instantiations.m4 \
../../ppl_interface_generator_common.m4 \
../../ppl_interface_generator_common_dat.m4 \
../../ppl_interface_generator_copyright \
../../ppl_interface_generator_common_procedure_generators.m4 \
../ppl_interface_generator_ocaml_procedure_generators.m4 \
ppl_interface_generator_ocaml_test_ml.m4 \
ppl_interface_generator_ocaml_test_ml_code.m4 \
ppl_ocaml_tests_common

check_OCAML = \
test1.ml \
ppl_ocaml_generated_test.ml

BUILT_SOURCES = \
ppl_ocaml_generated_test.ml

# Even if the OCaml sources are not really needed, they are built by m4,
# we need to include them to invoke the Automake OCaml build system.
EXTRA_DIST = \
ppl_interface_generator_ocaml_test_ml_code.m4 \
ppl_interface_generator_ocaml_test_ml.m4 \
ppl_ocaml_tests_common

OCAMLC_FLAGS = -I +gmp -I $(srcdir) -I ..

.ml.cmo:
	ocamlc -o $@ -g -c $(OCAMLC_FLAGS) $<

LDADD = \
$(top_builddir)/utils/libppl_utils.a \
$(top_builddir)/tests/libppl_tests.a \
$(top_builddir)/src/libppl.la \
@extra_libraries@

TESTS_ENVIRONMENT = \
$(LIBTOOL) --mode=execute \
	-dlopen ../../../src/libppl.la \
	$(WATCHDOG_DLOPEN)

ppl_ocaml_generated_test.ml: $(interface_generator_dependencies) Makefile
	m4 --prefix-builtin -I../.. \
		-I$(srcdir) -I$(srcdir)/.. -I$(srcdir)/../.. \
		$(srcdir)/ppl_interface_generator_ocaml_test_ml.m4 \
			> $@

ppl_ocaml_generated_test_SOURCES = ppl_ocaml_generated_test.ml

ppl_ocaml_generated_test$(EXEEXT): ppl_ocaml_generated_test.cmo
	ocamlc -o $@ -cc g++ -g -custom $(OCAMLC_FLAGS) ppl_ocaml.cma gmp.cma -ccopt -g -ccopt -L$(top_builddir)/src/.libs $<

TESTS = \
test1 \
ppl_ocaml_generated_test

check_PROGRAMS = $(DISABLED_TESTS)

test1_SOURCES = test1.ml

test1$(EXEEXT): test1.cmo
	ocamlc -o $@ -cc g++ -g -custom $(OCAMLC_FLAGS) ppl_ocaml.cma gmp.cma -ccopt -g -ccopt -L$(top_builddir)/src/.libs $<


#ocamlc -verbose -o test1 -cc g++ -g -I +gmp -I /home/roberto/ppl/ppl/interfaces/OCaml/tests -I .. ppl_ocaml.cma gmp.cma -ccopt "-g -L.. -lppl_ocaml" test1.cmo

endif BUILD_OCAML_INTERFACE


CLEANFILES = \
test1.cmi \
test1.cmo \
test1 \
ppl_ocaml_generated_test.ml \
ppl_ocaml_generated_test.cmi \
ppl_ocaml_generated_test.cmo \
ppl_ocaml_generated_test
