# Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

srcdir = @srcdir@
VPATH = @srcdir@

@SET_MAKE@

SUBDIRS = Integer .

INCLUDES = -I$(top_builddir) -I$(top_srcdir)/src -I$(top_srcdir)/src/Integer @extra_includes@

CPPFLAGS = -DPPL_INTERNAL=public @extra_cpp_flags@

lib_LIBRARIES = libppl.a

# The ordering of the following list *matters*!
INCLUDE_FILES = \
SatRow.types.hh \
SatMatrix.types.hh \
Row.types.hh \
Matrix.types.hh \
Variable.types.hh \
LinExpression.types.hh \
Constraint.types.hh \
ConSys.types.hh \
Generator.types.hh \
GenSys.types.hh \
Status.types.hh \
Polyhedron.types.hh \
SatRow.defs.hh \
SatRow.inlines.hh \
SatMatrix.defs.hh \
SatMatrix.inlines.hh \
Row.defs.hh \
Row.inlines.hh \
Matrix.defs.hh \
Matrix.inlines.hh \
Variable.defs.hh \
Variable.inlines.hh \
LinExpression.defs.hh \
LinExpression.inlines.hh \
Constraint.defs.hh \
Constraint.inlines.hh \
ConSys.defs.hh \
ConSys.inlines.hh \
Generator.defs.hh \
Generator.inlines.hh \
GenSys.defs.hh \
GenSys.inlines.hh \
Status.defs.hh \
Status.inlines.hh \
globals.hh \
maxmin.hh \
Polyhedron.defs.hh \
Polyhedron.inlines.hh

libppl_a_SOURCES = \
Constraint.cc \
ConSys.cc \
GenSys.cc \
Generator.cc \
Matrix.cc \
Polyhedron.cc \
Row.cc \
SatMatrix.cc \
SatRow.cc \
Status.cc \
conversion.cc \
globals.cc \
minimize.cc \
simplify.cc \
LinExpression.cc \
Variable.cc \
$(INCLUDE_FILES)

# The ordering of the following list *matters*!
DOC_FILES = \
Variable.types.hh \
LinExpression.types.hh \
Constraint.types.hh \
ConSys.types.hh \
Generator.types.hh \
GenSys.types.hh \
Polyhedron.types.hh \
Variable.defs.hh \
LinExpression.defs.hh \
Constraint.defs.hh \
ConSys.defs.hh \
Generator.defs.hh \
GenSys.defs.hh \
Polyhedron.defs.hh

libppl_a_LIBADD = Integer/Integer.o

noinst_HEADERS = ppl.hh ppl_install.hh

BUILT_SOURCES = ppl.hh ppl_install.hh

MORE_INCLUDE_FILES = \
Integer/Integer.types.hh \
Integer/Integer.defs.hh \
Integer/Integer.inlines.hh

ppl.hh:	$(DOC_FILES)
	(cd $(srcdir) ; cat $(DOC_FILES)) \
		| sed -e "s/PPL_INTERNAL/private/g" >ppl.hh

ppl_install.hh:	ppl_header.top $(top_builddir)/config.h ppl_header.middle $(INCLUDE_FILES) $(MORE_INCLUDE_FILES) ppl_header.bottom
	mkdir -p tmp
	(cd $(srcdir) ; cat $(MORE_INCLUDE_FILES) $(INCLUDE_FILES)) >tmp/concat
	grep -e "^#include <" <tmp/concat | sort -u >tmp/includes
	sed -e "/^#include/d" -e "s/PPL_INTERNAL/public/g" <tmp/concat >tmp/noincludes.cc
	cat $(srcdir)/ppl_header.top $(top_builddir)/config.h \
		$(srcdir)/ppl_header.middle tmp/includes >ppl_install.hh
	$(CXXCPP) -P @debug_flag@ tmp/noincludes.cc >>ppl_install.hh
	cat $(srcdir)/ppl_header.bottom >>ppl_install.hh
	rm -rf tmp*

# ppl.hh and ppl_install.hh are not distributed.
dist-hook:
	rm -f $(distdir)/ppl.hh $(distdir)/ppl_install.hh

install-data-local: ppl_install.hh
	$(mkinstalldirs) $(DESTDIR)$(includedir)
	$(INSTALL_DATA) ppl_install.hh $(DESTDIR)$(includedir)/ppl.hh

uninstall-local:
	rm -f $(DESTDIR)$(includedir)/ppl.hh

EXTRA_DIST = ppl_header.top ppl_header.middle ppl_header.bottom

MOSTLYCLEANFILES = $(BUILT_SOURCES)
