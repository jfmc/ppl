# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>
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
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
# USA.
#
# For the most up-to-date information see the Parma Polyhedra Library
# site: http://www.cs.unipr.it/ppl/ .

srcdir = @srcdir@
abs_srcdir = @abs_srcdir@
VPATH = @srcdir@

@SET_MAKE@

# All the documentation in docdir.
docdir = @docdir@
dvidir = $(docdir)
htmldir = $(docdir)
pdfdir = $(docdir)
psdir = $(docdir)
txtdir = $(docdir)

DOXYGEN = doxygen

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
$(PS_LICENSES) \
$(TXT_LICENSES)

HTML_MANUALS = \
ppl-user-@VERSION@-html.tar.gz \
ppl-devref-@VERSION@-html.tar.gz

PDF_MANUALS = \
ppl-user-@VERSION@-browse.pdf \
ppl-user-@VERSION@-print.pdf \
ppl-devref-@VERSION@-browse.pdf \
ppl-devref-@VERSION@-print.pdf

PS_MANUALS = \
ppl-user-@VERSION@-print.ps.gz \
ppl-devref-@VERSION@-print.ps.gz

MANUALS = \
$(HTML_MANUALS) \
$(PDF_MANUALS) \
$(PS_MANUALS)

HTML_DIRS = \
ppl-user-@VERSION@-html \
ppl-devref-@VERSION@-html

DIST_HTML_DOCS = \
ppl-user-@VERSION@-html.tar.gz

DIST_PDF_DOCS = \
$(PDF_LICENSES) \
ppl-user-@VERSION@-browse.pdf \
ppl-user-@VERSION@-print.pdf

DIST_PS_DOCS = \
$(PS_LICENSES) \
ppl-user-@VERSION@-print.ps.gz

DIST_TXT_DOCS = \
$(TXT_LICENSES)

# We distribute some preprocessed user's documentation, both for
# browsing and for printing.
DIST_DOCS = \
$(DIST_HTML_DOCS) \
$(DIST_PDF_DOCS) \
$(DIST_PS_DOCS) \
$(DIST_TXT_DOCS)

ALL_TARGETS = \
$(HTML_DIRS) \
$(LICENSES) \
$(MANUALS)


# Do nothing by default.
all:

dist-hook: $(DIST_DOCS)

user-browse: ppl-user-@VERSION@-browse.pdf

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
	  tar zxf `if test -f $$ball; then echo .; else echo $(srcdir); fi`/$$ball; \
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
	for file in $(srcdir)/*.txt; do \
	  $(INSTALL_DATA) $$file $(DESTDIR)$(txtdir)/`basename $$file`; \
	done
	for file in BUGS COPYING CREDITS ChangeLog NEWS README README.configure ; do \
	  $(INSTALL_DATA) $(srcdir)/../$$file $(DESTDIR)$(txtdir)/$$file; \
	done
	$(INSTALL_DATA) $(srcdir)/README $(DESTDIR)$(txtdir)/README.doc

installdirs-txt:
	$(mkinstalldirs) $(DESTDIR)$(txtdir)

uninstall-txt:
	rm -f $(DESTDIR)$(txtdir)/*.txt
	for file in BUGS COPYING CREDITS ChangeLog NEWS README README.configure ; do \
	  rm -f $(DESTDIR)$(txtdir)/$$file; \
	done
	rm -f $(DESTDIR)$(txtdir)/README.doc

PPL_SOURCE_FILES = $(wildcard $(srcdir)/../src/*.hh $(srcdir)/../src/*.cc)

DOX_FILES = \
definitions.dox \
../interfaces/Prolog/Prolog_interface.dox \
gpl.dox \
fdl.dox

USER_STUFF = \
$(DOX_FILES) \
user.tex \
ppl.sty \
$(top_builddir)/src/ppl.hh \
../interfaces/C/ppl_c.h

DEVREF_STUFF = \
$(DOX_FILES) \
devref.tex \
ppl.sty \
$(PPL_SOURCE_FILES)

LATEX_DIRS = \
user-browse.pdf-dir \
user-print.pdf-dir \
devref-browse.pdf-dir \
devref-print.pdf-dir \
user-print.ps-dir \
devref-print.ps-dir

TEX_ENV = TEXINPUTS=$(abs_srcdir):$(TEXINPUTS)

.SECONDARY: $(LATEX_DIRS)

%.doxyconf-pdf: %.doxyconf-latex
	sed -e "s/^USE_PDFLATEX[ ]*=.*$$/USE_PDFLATEX           = YES/g" \
	    -e "s/latex-dir$$/pdf-dir/g" \
		$< >$@

%.doxyconf-ps: %.doxyconf-latex
	sed -e "s/^USE_PDFLATEX[ ]*=.*$$/USE_PDFLATEX           = NO/g" \
	    -e "s/latex-dir$$/ps-dir/g" \
		$< >$@

user-%.pdf-dir: user-%.doxyconf-pdf $(USER_STUFF)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<

devref-%.pdf-dir: devref-%.doxyconf-pdf $(DEVREF_STUFF)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<

user-%.ps-dir: user-%.doxyconf-ps $(USER_STUFF)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<

devref-%.ps-dir: devref-%.doxyconf-ps $(DEVREF_STUFF)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<

ppl-user-@VERSION@-%.pdf: user-%.pdf-dir
	$(TEX_ENV) make -C $< refman.pdf && mv -f $</refman.pdf $@

ppl-devref-@VERSION@-%.pdf: devref-%.pdf-dir
	$(TEX_ENV) make -C $< refman.pdf && mv -f $</refman.pdf $@

ppl-user-@VERSION@-%.ps: user-%.ps-dir
	$(TEX_ENV) make -C $< refman.ps && mv -f $</refman.ps $@

ppl-devref-@VERSION@-%.ps: devref-%.ps-dir
	$(TEX_ENV) make -C $< refman.ps && mv -f $</refman.ps $@

ppl-%-@VERSION@-html: %.doxyconf-html $(DEVREF_STUFF)
	rm -rf html
	$(TEX_ENV) $(DOXYGEN) $< && rm -rf $@ && mv -f html $@

%.tar: %
	tar cf $@ $<

%.gz: %
	gzip --best --force $<

%.pdf: %.tex
	$(TEX_ENV) pdflatex $<

%.dvi: %.tex
	$(TEX_ENV) latex $<

%.ps:	%.dvi
	dvips -o $@ $<

EXTRA_DIST= \
$(DIST_DOCS) \
$(DOX_FILES) \
user-browse.doxyconf-latex \
devref-browse.doxyconf-latex \
user-print.doxyconf-latex \
devref-print.doxyconf-latex \
user.doxyconf-html \
devref.doxyconf-html \
user.tex \
devref.tex \
gpl.tex \
fdl.tex \
gpl.txt \
fdl.txt \
ppl.sty

mostlyclean-local:
	rm -rf $(ALL_TARGETS) $(LATEX_DIRS) *.aux *.log

CLEANFILES = $(DIST_DOCS)
