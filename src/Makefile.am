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

SUBDIRS = BitSet Integer .

INCLUDES = -I$(top_builddir) -I$(top_srcdir)/src -I$(top_srcdir)/src/BitSet -I$(top_srcdir)/src/Integer @extra_includes@

# See http://www.cs.unipr.it/pipermail/ppl-devel/2001-October/000281.html
#MYCPP = cpp
MYCPP = kgcc -E

CPPFLAGS = -DPPL_INTERNAL=public @extra_cpp_flags@

lib_LIBRARIES = libppl.a

include_files = \
Constraint.types.hh \
Constraint.defs.hh \
Constraint.inlines.hh \
ConSys.types.hh \
ConSys.defs.hh \
ConSys.inlines.hh \
GenSys.types.hh \
GenSys.defs.hh \
GenSys.inlines.hh \
Generator.types.hh \
Generator.defs.hh \
Generator.inlines.hh \
Matrix.types.hh \
Matrix.defs.hh \
Matrix.inlines.hh \
Polyhedron.types.hh \
Polyhedron.defs.hh \
Polyhedron.inlines.hh \
Row.types.hh \
Row.defs.hh \
Row.inlines.hh \
SatMatrix.types.hh \
SatMatrix.defs.hh \
SatMatrix.inlines.hh \
SatRow.types.hh \
SatRow.defs.hh \
SatRow.inlines.hh \
Status.types.hh \
Status.defs.hh \
Status.inlines.hh \
globals.hh \
LinExpression.types.hh \
LinExpression.defs.hh \
LinExpression.inlines.hh \
Variable.types.hh \
Variable.defs.hh \
Variable.inlines.hh \
maxmin.hh

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
$(include_files)

libppl_a_LIBADD = Integer/Integer.o BitSet/BitSet.o

noinst_HEADERS = ppl.cc

BUILT_SOURCES = ppl.hh

include_HEADERS = ppl.hh

MORE_PPL_HH_DEPS = \
BitSet/BitSet.types.hh \
BitSet/BitSet.defs.hh \
BitSet/BitSet.inlines.hh \
Integer/Integer.types.hh \
Integer/Integer.defs.hh \
Integer/Integer.inlines.hh

ppl.hh:	$(include_files) ppl.cc protect.sed restore.sed $(MORE_PPL_HH_DEPS)
	cpp -C -P -H $(INCLUDES) \
		@debug_flag@ -DOUTLINE=0 $(srcdir)/ppl.cc >/dev/null 2>tmp.1
	sed -e "s/^\.* //g" -e "/^\/usr/d" -e "/Multiple /d" <tmp.1 \
		| sort -u >tmp.2
	mkdir -p tmp
	for k in `cat tmp.2` ; \
	do \
		sed -f $(srcdir)/protect.sed <$$k >tmp/`basename $$k` ;\
	done
	$(MYCPP) -C -P -nostdinc -I- -Itmp \
		@debug_flag@ -DOUTLINE=0 -DINLINE=inline \
		-DPPL_INTERNAL=private $(srcdir)/ppl.cc >tmp.3
	echo "#ifndef _ppl_hh" >ppl.hh
	echo "#define _ppl_hh 1" >>ppl.hh
	sed -f $(srcdir)/restore.sed <tmp.3 >>ppl.hh
	echo "#endif" >>ppl.hh
	rm -rf tmp*

# ppl.hh is not distributed.
dist-hook:
	rm -f $(distdir)/ppl.hh

EXTRA_DIST = ppl.cc protect.sed restore.sed

MOSTLYCLEANFILES = $(BUILT_SOURCES)
