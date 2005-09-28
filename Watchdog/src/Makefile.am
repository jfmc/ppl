# Automake source file for the Parma Watchdog Library.
# Copyright (C) 2002-2005 Roberto Bagnara <bagnara@cs.unipr.it>
#
# This file is part of the Parma Watchdog Library (PWL).
#
# The PWL is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2 of the License, or (at your
# option) any later version.
#
# The PWL is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.
#
# For the most up-to-date information see the CS@Parma software
# site: http://www.cs.unipr.it/Software/ . */

srcdir = @srcdir@
VPATH = @srcdir@

@SET_MAKE@

# The ordering of the following list *matters*!
INCLUDE_FILES = \
Doubly_Linked_Object.types.hh \
EList.types.hh \
EList_Iterator.types.hh \
Handler.types.hh \
Pending_Element.types.hh \
Pending_List.types.hh \
Time.types.hh \
Watchdog.types.hh \
Handler.defs.hh \
Handler.inlines.hh \
Time.defs.hh \
Time.inlines.hh \
Doubly_Linked_Object.defs.hh \
Doubly_Linked_Object.inlines.hh \
EList_Iterator.defs.hh \
EList_Iterator.inlines.hh \
EList.defs.hh \
EList.inlines.hh \
Pending_Element.defs.hh \
Pending_Element.inlines.hh \
Pending_List.defs.hh \
Pending_List.inlines.hh \
Watchdog.defs.hh \
Watchdog.inlines.hh

SOURCE_FILES = \
$(INCLUDE_FILES) \
Pending_Element.cc \
Pending_List.cc \
Time.cc \
Watchdog.cc

# The ordering of the following list *matters*!
DOC_FILES = \
Watchdog.types.hh \
Watchdog.defs.hh

EXTRA_DIST = \
$(SOURCE_FILES) \
pwl_header.hh

# pwl.hh and pwl_install.hh are not distributed.
dist-hook:
	rm -f $(distdir)/pwl.hh $(distdir)/pwl_install.hh

if BUILD_WATCHDOG_LIBRARY

# Libtool -version-info for libpwl.la.
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
#	PWL release	(with PPL release)	-version-info
#	    0.1			0.4		    0:0:0
#	    0.2			0.5		    1:0:0
#	    0.3			0.6		    2:0:0
#	    0.4			0.7		    3:0:0

LIBPWL_LT_CURRENT =  3
LIBPWL_LT_REVISION = 0
LIBPWL_LT_AGE =      0

lib_LTLIBRARIES = libpwl.la
libpwl_la_SOURCES = $(SOURCE_FILES)

if NO_UNDEFINED

NO_UNDEFINED_FLAG = -no-undefined

endif NO_UNDEFINED

libpwl_la_LDFLAGS = \
$(NO_UNDEFINED_FLAG) \
-version-info $(LIBPWL_LT_CURRENT):$(LIBPWL_LT_REVISION):$(LIBPWL_LT_AGE)

noinst_HEADERS = pwl.hh pwl_install.hh

BUILT_SOURCES = pwl.hh pwl_install.hh

pwl.hh:	$(DOC_FILES)
	(cd $(srcdir) ; cat $(DOC_FILES)) >pwl.hh

pwl_include_files.hh: $(INCLUDE_FILES) Makefile
	rm -f $@
	for file in $(INCLUDE_FILES); \
	do \
	  printf "#include \"%s\"\n" $$file >>$@; \
	done

pwl_install.hh:	$(top_builddir)/config.h pwl_header.hh pwl_include_files.hh Makefile $(top_builddir)/utils/build_header
	$(top_builddir)/utils/build_header \
		-I $(top_builddir) -I $(top_builddir)/src \
		$(top_srcdir)/src/pwl_header.hh >$@

install-data-local: pwl_install.hh
	$(mkinstalldirs) $(DESTDIR)$(includedir)
	$(INSTALL_DATA) pwl_install.hh $(DESTDIR)$(includedir)/pwl.hh

uninstall-local:
	rm -f $(DESTDIR)$(includedir)/pwl.hh

MOSTLYCLEANFILES = $(BUILT_SOURCES)

endif BUILD_WATCHDOG_LIBRARY
