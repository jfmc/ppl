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

LICENSES = \
gpl.pdf \
gpl.ps.gz \
gpl.txt \
fdl.pdf \
fdl.ps.gz \
fdl.txt

HTML_DIRS = \
ppl-user-@VERSION@-html \
ppl-devref-@VERSION@-html

ALL_TARGETS = \
$(HTML_DIRS) \
$(LICENSES) \
ppl-user-@VERSION@-browse.pdf \
ppl-user-@VERSION@-print.pdf \
ppl-user-@VERSION@-print.ps.gz \
ppl-user-@VERSION@-html.tar.gz \
ppl-devref-@VERSION@-browse.pdf \
ppl-devref-@VERSION@-print.pdf \
ppl-devref-@VERSION@-print.ps.gz \
ppl-devref-@VERSION@-html.tar.gz

# We distribute some preprocessed user's documentation, both for
# browsing and for printing.
DIST_DOCS = \
$(LICENSES) \
ppl-user-@VERSION@-browse.pdf \
ppl-user-@VERSION@-print.pdf \
ppl-user-@VERSION@-print.ps.gz \
ppl-user-@VERSION@-html.tar.gz

# Do nothing by default.
all:

dist-hook: $(DIST_DOCS)

user-browse: ppl-user-@VERSION@-browse.pdf

world: $(ALL_TARGETS)

install-data-local: install-html install-pdf install-ps install-txt
installdirs-local: installdirs-html installdirs-pdf installdirs-ps installdirs-txt
uninstall-local: uninstall-html uninstall-pdf uninstall-ps uninstall-txt

# Installation and uninstallation of HTML documentation.
install-html:
	$(mkinstalldirs) $(DESTDIR)$(htmldir)/ppl-user-@VERSION@-html
	tar zxf `if test -f ppl-user-@VERSION@-html.tar.gz; then echo .; else echo $(srcdir); fi`/ppl-user-@VERSION@-html.tar.gz
	for file in ppl-user-@VERSION@-html/*; do \
	  $(INSTALL_DATA) $$file $(DESTDIR)$(htmldir)/ppl-user-@VERSION@-html/`basename $$file`; \
	done

installdirs-html:
	$(mkinstalldirs) $(DESTDIR)$(htmldir)/ppl-user-@VERSION@-html

uninstall-html:
	rm -rf $(DESTDIR)$(htmldir)/ppl-user-@VERSION@-html

# Installation and uninstallation of PDF documentation.
install-pdf:
	$(mkinstalldirs) $(DESTDIR)$(pdfdir)
	for file in `if test -f ppl-user-@VERSION@-browse.pdf; then echo .; else echo $(srcdir); fi`/*.pdf; do \
	  $(INSTALL_DATA) $$file $(DESTDIR)$(pdfdir)/`basename $$file`; \
	done

installdirs-pdf:
	$(mkinstalldirs) $(DESTDIR)$(pdfdir)

uninstall-pdf:
	rm -f $(DESTDIR)$(pdfdir)/ppl-user-@VERSION@-*.pdf

# Installation and uninstallation of PostScript documentation.
install-ps:
	$(mkinstalldirs) $(DESTDIR)$(psdir)
	for file in `if test -f ppl-user-@VERSION@-print.ps.gz; then echo .; else echo $(srcdir); fi`/*.ps.gz; do \
	  $(INSTALL_DATA) $$file $(DESTDIR)$(psdir)/`basename $$file`; \
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
	rm -f $(DESTDIR)$(psdir)/*.txt


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
