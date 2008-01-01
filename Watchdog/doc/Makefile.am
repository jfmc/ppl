# Automake source file for the Parma Watchdog Library.
# Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>
#
# This file is part of the Parma Watchdog Library (PWL).
#
# The PWL is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at your
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
# For the most up-to-date information see the Parma Watchdog Library
# site: http://www.cs.unipr.it/pwl/ .

abs_srcdir = @abs_srcdir@

# All the documentation in docdir.
docdir = @docdir@
htmldir = $(docdir)
pdfdir = $(docdir)
psdir = $(docdir)
txtdir = $(docdir)

DOXYGEN = doxygen
INSTALLDOX = ./installdox -q

PDF_LICENSES = \
gpl.pdf \
fdl.pdf

PS_LICENSES = \
gpl.ps.gz \
fdl.ps.gz

TXT_LICENSES = \
gpl.txt \
fdl.txt

LICENSES = \
$(PDF_LICENSES) \
$(TXT_LICENSES) \
$(PS_LICENSES)

# Library Core Manuals (C++ interface)

HTML_CORE_MANUALS = \
pwl-user-@VERSION@-html.tar.gz \
pwl-devref-@VERSION@-html.tar.gz

PDF_CORE_MANUALS = \
pwl-user-@VERSION@.pdf \
pwl-devref-@VERSION@.pdf

PS_CORE_MANUALS = \
pwl-user-@VERSION@.ps.gz \
pwl-devref-@VERSION@.ps.gz

USER_HTML_DIR = pwl-user-@VERSION@-html
DEVREF_HTML_DIR = pwl-devref-@VERSION@-html

CORE_HTML_DIRS = $(USER_HTML_DIR) $(DEVREF_HTML_DIR)

DIST_CORE_HTML_DOCS = \
pwl-user-@VERSION@-html.tar.gz

DIST_CORE_PDF_DOCS = \
pwl-user-@VERSION@.pdf

DIST_CORE_PS_DOCS = \
pwl-user-@VERSION@.ps.gz

DOX_FILES = \
gpl.dox \
fdl.dox

USER_CORE_STUFF = \
$(DOX_FILES) \
user.tex \
pwl.sty \
$(top_builddir)/src/pwl.hh

DEVREF_CORE_STUFF = \
$(DOX_FILES) \
devref.tex \
pwl.sty \
$(PWL_SOURCE_FILES)


MANUALS = \
$(HTML_CORE_MANUALS) \
$(PDF_CORE_MANUALS) \
$(PS_CORE_MANUALS)

HTML_DIRS = \
$(CORE_HTML_DIRS)

DIST_HTML_DOCS = \
$(DIST_CORE_HTML_DOCS)

DIST_PDF_DOCS = \
$(PDF_LICENSES) \
$(DIST_CORE_PDF_DOCS)

DIST_PS_DOCS = \
$(PS_LICENSES) \
$(DIST_CORE_PS_DOCS)

DIST_TXT_DOCS = \
$(TXT_LICENSES)

# We distribute some preprocessed user's documentation.
DIST_DOCS = \
$(DIST_HTML_DOCS) \
$(DIST_PDF_DOCS) \
$(DIST_PS_DOCS) \
$(DIST_TXT_DOCS)

ALL_TARGETS = \
$(HTML_DIRS) \
$(PDF_LICENSES) \
$(PS_LICENSES) \
$(MANUALS)

# Do nothing by default.
all:

user: pwl-user-@VERSION@.pdf

world: $(ALL_TARGETS)

install-data-local: \
install-html \
install-pdf \
install-ps \
install-txt

installdirs-local: \
installdirs-html \
installdirs-pdf \
installdirs-ps \
installdirs-txt

uninstall-local: \
uninstall-html \
uninstall-pdf \
uninstall-ps \
uninstall-txt

# Installation and uninstallation of HTML documentation.
install-html:
	for ball in $(DIST_HTML_DOCS); do \
	  test -f $$ball || test -f $(srcdir)/$$ball || $(MAKE) $$ball; \
	  dir=`basename $$ball .tar.gz`; \
	  $(mkinstalldirs) $(DESTDIR)$(htmldir)/$$dir; \
	  gunzip -c `if test -f $$ball; then echo .; else echo $(srcdir); fi`/$$ball | tar xf - ; \
	  for file in $$dir/*; do \
	    $(INSTALL_DATA) $$file $(DESTDIR)$(htmldir)/$$dir/`basename $$file`; \
	  done; \
	done

installdirs-html:
	for ball in $(DIST_HTML_DOCS); do \
	  dir=`basename $$ball .tar.gz`; \
	  $(mkinstalldirs) $(DESTDIR)$(htmldir)/$$dir; \
	done

uninstall-html:
	for ball in $(DIST_HTML_DOCS); do \
	  dir=`basename $$ball .tar.gz`; \
	  rm -rf $(DESTDIR)$(htmldir)/$$dir; \
	done

# Installation and uninstallation of PDF documentation.
install-pdf:
	$(mkinstalldirs) $(DESTDIR)$(pdfdir)
	for file in $(DIST_PDF_DOCS); do \
	  test -f $$file || test -f $(srcdir)/$$file || $(MAKE) $$file; \
	  $(INSTALL_DATA) `if test -f $$file; then echo .; else echo $(srcdir); fi`/$$file $(DESTDIR)$(pdfdir)/$$file; \
	done

installdirs-pdf:
	$(mkinstalldirs) $(DESTDIR)$(pdfdir)

uninstall-pdf:
	rm -f $(DESTDIR)$(pdfdir)/*.pdf

# Installation and uninstallation of PostScript documentation.
install-ps:
	$(mkinstalldirs) $(DESTDIR)$(psdir)
	for file in $(DIST_PS_DOCS); do \
	  test -f $$file || test -f $(srcdir)/$$file || $(MAKE) $$file; \
	  $(INSTALL_DATA) `if test -f $$file; then echo .; else echo $(srcdir); fi`/$$file $(DESTDIR)$(psdir)/$$file; \
	done

installdirs-ps:
	$(mkinstalldirs) $(DESTDIR)$(psdir)

uninstall-ps:
	rm -f $(DESTDIR)$(psdir)/*.ps.gz

# Installation and uninstallation of pure text documentation.
install-txt:
	$(mkinstalldirs) $(DESTDIR)$(txtdir)
	for file in $(srcdir)/*.txt $(srcdir)/README.doc; do \
	  $(INSTALL_DATA) $$file $(DESTDIR)$(txtdir)/`basename $$file`; \
	done
	for file in BUGS COPYING CREDITS ChangeLog NEWS README; do \
	  $(INSTALL_DATA) $(srcdir)/../$$file $(DESTDIR)$(txtdir)/$$file; \
	done

installdirs-txt:
	$(mkinstalldirs) $(DESTDIR)$(txtdir)

uninstall-txt:
	rm -f $(DESTDIR)$(txtdir)/*.txt $(DESTDIR)$(txtdir)/README.doc
	for file in BUGS COPYING CREDITS ChangeLog NEWS README README.configure ; do \
	  rm -f $(DESTDIR)$(txtdir)/$$file; \
	done

PWL_SOURCE_FILES = $(wildcard $(srcdir)/../src/*.hh $(srcdir)/../src/*.cc)

LATEX_DIRS = \
user.latex-dir \
devref.latex-dir \
user-c-interface.latex-dir \
devref-c-interface.latex-dir \
user-prolog-interface.latex-dir \
devref-prolog-interface.latex-dir

TEX_ENV = TEXINPUTS=$(abs_srcdir):$(TEXINPUTS)

.SECONDARY: $(LATEX_DIRS)

user.latex-dir: user.doxyconf-latex $(USER_CORE_STUFF)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<

devref.latex-dir: devref.doxyconf-latex $(DEVREF_CORE_STUFF)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<

pwl-user-@VERSION@.pdf: user.latex-dir
	$(TEX_ENV) make -C $< refman.pdf && mv -f $</refman.pdf $@

pwl-devref-@VERSION@.pdf: devref.latex-dir
	$(TEX_ENV) make -C $< refman.pdf && mv -f $</refman.pdf $@

$(USER_HTML_DIR): user.doxyconf-html $(USER_CORE_STUFF)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<

$(DEVREF_HTML_DIR): devref.doxyconf-html $(DEVREF_CORE_STUFF)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<


%.tar: %
	tar cf $@ $<

%.gz: %
	gzip --best --force $<

%.pdf: %.tex
	$(TEX_ENV) pdflatex $<

%.ps:	%.pdf
	pdftops $< $@

EXTRA_DIST= \
$(DIST_DOCS) \
$(DOX_FILES) \
user.doxyconf-latex \
devref.doxyconf-latex \
user.doxyconf-html \
devref.doxyconf-html \
user.tex \
devref.tex \
gpl.tex \
fdl.tex \
pwl.sty \
README.doc

mostlyclean-local:
	rm -rf $(ALL_TARGETS) $(LATEX_DIRS) $(HTML_DIRS) *.aux *.log

CLEANFILES = \
$(DIST_HTML_DOCS) \
$(DIST_PDF_DOCS) \
$(DIST_PS_DOCS) \
$(USER_HTML_DIR).tag \
$(DEVREF_HTML_DIR).tag
