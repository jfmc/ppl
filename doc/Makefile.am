# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>
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

LICENSES = \
gpl.pdf \
gpl.ps.gz \
fdl.pdf \
fdl.ps.gz

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

# We distribute some preprocessed user's documentation,
# both for browsing and for printing.
DIST_DOCS = \
ppl-user-@VERSION@-browse.pdf \
ppl-user-@VERSION@-print.pdf \
ppl-user-@VERSION@-print.ps.gz

# Do nothing by default.
all:

dist-hook: $(DIST_DOCS)

user-browse: ppl-user-@VERSION@-browse.pdf

world: $(ALL_TARGETS)

PPL_SOURCE_FILES = $(wildcard $(srcdir)/../src/*.hh $(srcdir)/../src/*.cc)

DOX_FILES = \
definitions.dox \
../interfaces/Prolog/Prolog_interface.dox \
gpl.dox \
fdl.dox

USER_STUFF = \
$(DOX_FILES) \
user.tex \
$(top_builddir)/src/ppl.hh \
$(top_srcdir)/interfaces/C/ppl_c.h

DEVREF_STUFF = \
$(DOX_FILES) \
devref.tex \
$(PPL_SOURCE_FILES)

LATEX_DIRS = \
user-browse.latex-dir \
user-print.latex-dir \
devref-browse.latex-dir \
devref-print.latex-dir

TEX_ENV = TEXINPUTS=$(abs_srcdir):$(TEXINPUTS)

.SECONDARY: $(LATEX_DIRS)

user-%.latex-dir: user-%.doxyconf-latex $(USER_STUFF)
	rm -rf $@
	$(TEX_ENV) doxygen $<

devref-%.latex-dir: devref-%.doxyconf-latex $(DEVREF_STUFF)
	rm -rf $@
	$(TEX_ENV) doxygen $<

ppl-user-@VERSION@-%.pdf: user-%.latex-dir
	$(TEX_ENV) make -C $< pdf && mv -f $</refman.pdf $@

ppl-devref-@VERSION@-%.pdf: devref-%.latex-dir
	$(TEX_ENV) make -C $< pdf && mv -f $</refman.pdf $@

ppl-user-@VERSION@-%.ps: user-%.latex-dir
	$(TEX_ENV) make -C $< ps && mv -f $</refman.ps $@

ppl-devref-@VERSION@-%.ps: devref-%.latex-dir
	$(TEX_ENV) make -C $< ps && mv -f $</refman.ps $@

ppl-%-@VERSION@-html: %.doxyconf-html $(DEVREF_STUFF)
	rm -rf html
	$(TEX_ENV) doxygen $< && rm -rf $@ && mv -f html $@

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
ppl.sty

mostlyclean-local:
	rm -rf $(LATEX_DIRS) $(HTML_DIRS)

CLEANFILES = $(DIST_DOCS)
