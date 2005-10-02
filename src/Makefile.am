# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2005 Roberto Bagnara <bagnara@cs.unipr.it>
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

srcdir = @srcdir@
VPATH = @srcdir@

@SET_MAKE@

SUBDIRS =


# Libtool -version-info for libppl.la.
#
#  1. Start with version information of `0:0:0' for each Libtool library.
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
#	    0.3			    0:0:0
#	    0.4			    1:0:1
#	    0.5			    2:0:0
#	    0.6			    3:0:0
#	    0.7			    4:0:0
#	    0.8			    5:0:0

LIBPPL_LT_CURRENT =  5
LIBPPL_LT_REVISION = 0
LIBPPL_LT_AGE =      0

AM_CPPFLAGS = -I$(top_builddir) -I$(top_srcdir)/src @extra_includes@

lib_LTLIBRARIES = libppl.la

bin_PROGRAMS = ppl-config
ppl_config_SOURCES = \
ppl-config.cc \
BUGS.cc \
COPYING.cc \
CREDITS.cc

ppl_config_LDADD = \
libppl.la \
@extra_libraries@

if USE_NATIVE_INTEGERS

COEFFICIENT_TYPES_INCLUDE_FILES = \
  Native_Integer.types.hh

COEFFICIENT_DEFS_INCLUDE_FILES = \
  Native_Integer.defs.hh \
  Native_Integer.inlines.hh

endif

if USE_CHECKED_INTEGERS

COEFFICIENT_TYPES_INCLUDE_FILES =

COEFFICIENT_DEFS_INCLUDE_FILES =

endif

if USE_GMP_INTEGERS

COEFFICIENT_TYPES_INCLUDE_FILES = \
  GMP_Integer.types.hh

COEFFICIENT_DEFS_INCLUDE_FILES = \
  GMP_Integer.defs.hh \
  GMP_Integer.inlines.hh

endif

# The ordering of the following list *matters*!
INCLUDE_FILES = \
namespaces.hh \
compiler.hh \
float.types.hh \
Coefficient_traits_template.hh \
Checked_Number.types.hh \
$(COEFFICIENT_TYPES_INCLUDE_FILES) \
Coefficient.types.hh \
globals.types.hh \
Interval.types.hh \
Bounding_Box.types.hh \
Constraint.types.hh \
Generator.types.hh \
Init.types.hh \
Row.types.hh \
Linear_Row.types.hh \
Matrix.types.hh \
Variable.types.hh \
Linear_Expression.types.hh \
Linear_System.types.hh \
Saturation_Row.types.hh \
Saturation_Matrix.types.hh \
Constraint_System.types.hh \
Generator_System.types.hh \
Poly_Con_Relation.types.hh \
Poly_Gen_Relation.types.hh \
BHRZ03_Certificate.types.hh \
H79_Certificate.types.hh \
Polyhedron.types.hh \
C_Polyhedron.types.hh \
NNC_Polyhedron.types.hh \
Ptr_Iterator.types.hh \
DB_Row.types.hh \
DB_Matrix.types.hh \
BD_Shape.types.hh \
float.types.hh \
Float.defs.hh \
Float.inlines.hh \
Limits.hh \
fpu.defs.hh \
fpu-c99.inlines.hh \
fpu-ia32.inlines.hh \
Rounding.types.hh \
Rounding.defs.hh \
Result.defs.hh \
Result.inlines.hh \
Numeric_Format.defs.hh \
checked.defs.hh \
checked.inlines.hh \
checked_int.inlines.hh \
checked_float.inlines.hh \
checked_mpz.inlines.hh \
checked_mpq.inlines.hh \
checked_ext.inlines.hh \
Checked_Number.defs.hh \
Checked_Number.inlines.hh \
mp_numeric_limits.hh \
checked_numeric_limits.hh \
$(COEFFICIENT_DEFS_INCLUDE_FILES) \
Coefficient.defs.hh \
Coefficient.inlines.hh \
globals.defs.hh \
globals.inlines.hh \
Interval.defs.hh \
Interval.inlines.hh \
Bounding_Box.defs.hh \
Bounding_Box.inlines.hh \
Variable.defs.hh \
Variable.inlines.hh \
Init.defs.hh \
initializer.hh \
Topology.hh \
Row.defs.hh \
Row.inlines.hh \
Linear_Row.defs.hh \
Linear_Row.inlines.hh \
Matrix.defs.hh \
Matrix.inlines.hh \
Saturation_Row.defs.hh \
Saturation_Row.inlines.hh \
Linear_System.defs.hh \
Linear_System.inlines.hh \
Saturation_Matrix.defs.hh \
Saturation_Matrix.inlines.hh \
Constraint_System.defs.hh \
Generator_System.defs.hh \
Linear_Expression.defs.hh \
Linear_Expression.inlines.hh \
Constraint.defs.hh \
Constraint.inlines.hh \
Constraint_System.inlines.hh \
Generator.defs.hh \
Generator.inlines.hh \
Generator_System.inlines.hh \
scalar_products.defs.hh \
scalar_products.inlines.hh \
Poly_Con_Relation.defs.hh \
Poly_Con_Relation.inlines.hh \
Poly_Gen_Relation.defs.hh \
Poly_Gen_Relation.inlines.hh \
BHRZ03_Certificate.defs.hh \
BHRZ03_Certificate.inlines.hh \
H79_Certificate.defs.hh \
H79_Certificate.inlines.hh \
Polyhedron.defs.hh \
Ph_Status.inlines.hh \
Polyhedron.inlines.hh \
C_Polyhedron.defs.hh \
C_Polyhedron.inlines.hh \
NNC_Polyhedron.defs.hh \
NNC_Polyhedron.inlines.hh \
Widening_Function.types.hh \
Widening_Function.defs.hh \
Widening_Function.inlines.hh \
Ptr_Iterator.defs.hh \
Ptr_Iterator.inlines.hh \
DB_Row.defs.hh \
DB_Row.inlines.hh \
DB_Matrix.defs.hh \
DB_Matrix.inlines.hh \
BD_Shape.defs.hh \
BDS_Status.inlines.hh \
BD_Shape.inlines.hh \
Determinate.types.hh \
Determinate.defs.hh \
Determinate.inlines.hh \
Powerset.types.hh \
Powerset.defs.hh \
Powerset.inlines.hh \
Polyhedra_Powerset.types.hh \
Polyhedra_Powerset.defs.hh \
Polyhedra_Powerset.inlines.hh \
max_space_dimension.hh \
algorithms.hh

NESTED_INCLUDE_FILES = \
Ph_Status.idefs.hh \
BDS_Status.idefs.hh

libppl_la_SOURCES = \
Bounding_Box.cc \
checked.cc \
Checked_Number.cc \
Constraint.cc \
Constraint_System.cc \
simplex.cc \
Generator_System.cc \
Generator.cc \
Init.cc \
Coefficient.cc \
Interval.cc \
Linear_Expression.cc \
Linear_System.cc \
Matrix.cc \
scalar_products.cc \
Poly_Con_Relation.cc \
Poly_Gen_Relation.cc \
BHRZ03_Certificate.cc \
H79_Certificate.cc \
Polyhedron_nonpublic.cc \
Polyhedron_public.cc \
Polyhedron_chdims.cc \
Polyhedron_widenings.cc \
C_Polyhedron.cc \
NNC_Polyhedron.cc \
Polyhedra_Powerset.cc \
Row.cc \
Linear_Row.cc \
Saturation_Matrix.cc \
Saturation_Row.cc \
Ph_Status.cc \
Variable.cc \
conversion.cc \
minimize.cc \
simplify.cc \
globals.cc \
version.cc \
$(INCLUDE_FILES) \
$(NESTED_INCLUDE_FILES) \
swapping_sort.icc

# The ordering of the following list *matters*!
DOC_FILES = \
namespaces.hh \
GMP_Integer.types.hh \
Checked_Number.types.hh \
Native_Integer.types.hh \
Coefficient.types.hh \
globals.defs.hh \
Variable.types.hh \
Linear_Expression.types.hh \
Constraint.types.hh \
Constraint_System.types.hh \
Generator.types.hh \
Generator_System.types.hh \
Poly_Con_Relation.types.hh \
Poly_Gen_Relation.types.hh \
BHRZ03_Certificate.types.hh \
H79_Certificate.types.hh \
Polyhedron.types.hh \
C_Polyhedron.types.hh \
NNC_Polyhedron.types.hh \
BD_Shape.types.hh \
GMP_Integer.defs.hh \
Checked_Number.defs.hh \
Native_Integer.defs.hh \
Coefficient.defs.hh \
Variable.defs.hh \
Linear_Expression.defs.hh \
Constraint.defs.hh \
Constraint_System.defs.hh \
Generator.defs.hh \
Generator_System.defs.hh \
Poly_Con_Relation.defs.hh \
Poly_Gen_Relation.defs.hh \
BHRZ03_Certificate.defs.hh \
H79_Certificate.defs.hh \
Polyhedron.defs.hh \
C_Polyhedron.defs.hh \
NNC_Polyhedron.defs.hh \
globals.defs.hh \
Widening_Function.types.hh \
Widening_Function.defs.hh \
Widening_Function.inlines.hh \
BD_Shape.defs.hh \
BD_Shape.inlines.hh \
Determinate.types.hh \
Determinate.defs.hh \
Determinate.inlines.hh \
Powerset.types.hh \
Powerset.defs.hh \
Powerset.inlines.hh \
Polyhedra_Powerset.types.hh \
Polyhedra_Powerset.defs.hh \
Polyhedra_Powerset.inlines.hh \
algorithms.hh

if NO_UNDEFINED

NO_UNDEFINED_FLAG = -no-undefined

libppl_la_LIBADD = @extra_libraries@

endif NO_UNDEFINED

libppl_la_LDFLAGS = \
$(NO_UNDEFINED_FLAG) \
-version-info $(LIBPPL_LT_CURRENT):$(LIBPPL_LT_REVISION):$(LIBPPL_LT_AGE)

noinst_HEADERS = \
ppl.hh \
ppl_include_files.hh \
ppl_install.hh

ppl.hh:	$(DOC_FILES) $(top_builddir)/src/version.hh Makefile
	cat $(top_builddir)/src/version.hh >$@
	(cd $(srcdir) ; cat $(DOC_FILES)) >>$@

ppl_include_files.hh: $(INCLUDE_FILES) Makefile
	rm -f $@
	for file in $(INCLUDE_FILES); \
	do \
	  printf "#include \"%s\"\n" $$file >>$@; \
	done

if HAVE_PERL

ppl_install.hh:	$(top_builddir)/config.h $(top_builddir)/src/version.hh ppl_header.hh ppl_include_files.hh $(NESTED_INCLUDE_FILES) Makefile $(top_builddir)/utils/build_header
	$(top_builddir)/utils/build_header \
		-I $(top_builddir) -I $(top_builddir)/src \
		$(top_srcdir)/src/ppl_header.hh >$@

BUGS.cc:	$(top_builddir)/utils/text2cxxarray $(top_srcdir)/BUGS
	$(top_builddir)/utils/text2cxxarray --name=BUGS_array \
		$(top_srcdir)/BUGS >$@
COPYING.cc:	$(top_builddir)/utils/text2cxxarray $(top_srcdir)/COPYING
	$(top_builddir)/utils/text2cxxarray --name=COPYING_array \
		$(top_srcdir)/COPYING >$@
CREDITS.cc:	$(top_builddir)/utils/text2cxxarray $(top_srcdir)/CREDITS
	$(top_builddir)/utils/text2cxxarray --name=CREDITS_array \
		$(top_srcdir)/CREDITS >$@

else !HAVE_PERL

ppl_install.hh:	ppl_install.hh.dist
	cp -f $< $@

BUGS.cc:	BUGS.cc.dist
	cp -f $< $@
COPYING.cc:	COPYING.cc.dist
	cp -f $< $@
CREDITS.cc:	CREDITS.cc.dist
	cp -f $< $@

endif !HAVE_PERL

BUILT_SOURCES = \
$(noinst_HEADERS) \
BUGS.cc \
COPYING.cc \
CREDITS.cc

# ppl.hh and ppl_install.hh are not distributed.
# ppl_install.hh.dist, which is distributed, is a copy of ppl_install.hh;
# likewise for BUGS.cc.dist, COPYING.cc.dist and CREDITS.cc.dist.
dist-hook:
	mv -f $(distdir)/ppl_install.hh $(distdir)/ppl_install.hh.dist
	mv -f $(distdir)/BUGS.cc $(distdir)/BUGS.cc.dist
	mv -f $(distdir)/COPYING.cc $(distdir)/COPYING.cc.dist
	mv -f $(distdir)/CREDITS.cc $(distdir)/CREDITS.cc.dist
	rm -f $(distdir)/ppl.hh

install-data-local: ppl_install.hh
	$(mkinstalldirs) $(DESTDIR)$(includedir)
	$(INSTALL_DATA) ppl_install.hh $(DESTDIR)$(includedir)/ppl.hh

uninstall-local:
	rm -f $(DESTDIR)$(includedir)/ppl.hh

EXTRA_DIST = ppl_header.hh ppl-config.cc.in version.hh.in

MOSTLYCLEANFILES = $(BUILT_SOURCES)
