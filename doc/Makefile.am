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

ALL_TARGETS = \
ppl-user-@VERSION@-browse.pdf    \
ppl-user-@VERSION@-print.pdf     \
ppl-user-@VERSION@-print.ps.gz   \
ppl-devref-@VERSION@-browse.pdf  \
ppl-devref-@VERSION@-print.pdf   \
ppl-devref-@VERSION@-print.ps.gz \
user.html-dir                    \
devref.html-dir

# We distribute some user's documentation both for browsing and for printing.
DIST_DOCS = \
ppl-user-@VERSION@-browse.pdf  \
ppl-user-@VERSION@-print.pdf   \
ppl-user-@VERSION@-print.ps.gz

# Do nothing by default.
all:

dist-hook: $(DIST_DOCS)

user-browse: ppl-user-@VERSION@-browse.pdf

world: $(ALL_TARGETS)

PPL_SOURCE_FILES = $(wildcard $(srcdir)/../src/*.hh $(srcdir)/../src/*.cc)

DOX_FILES = definitions.dox gpl.dox fdl.dox

USER_STUFF = $(DOX_FILES) user.tex $(top_builddir)/src/ppl.hh

DEVREF_STUFF = $(DOX_FILES) devref.tex $(PPL_SOURCE_FILES)

.SECONDARY: user-browse.latex-dir user-print.latex-dir devref-browse.latex-dir devref-print.latex-dir gpl.dvi fdl.dvi

user-%.latex-dir: user-%.doxyconf-latex $(USER_STUFF)
	rm -rf $@
	doxygen $<

devref-%.latex-dir: devref-%.doxyconf-latex $(DEVREF_STUFF)
	rm -rf $@
	doxygen $<

ppl-user-@VERSION@-%.pdf: user-%.latex-dir
	make -C $< pdf && mv -f $</refman.pdf $@

ppl-devref-@VERSION@-%.pdf: devref-%.latex-dir
	make -C $< pdf && mv -f $</refman.pdf $@

ppl-user-@VERSION@-%.ps: user-%.latex-dir
	make -C $< ps && mv -f $</refman.ps $@

ppl-devref-@VERSION@-%.ps: devref-%.latex-dir
	make -C $< ps && mv -f $</refman.ps $@

%.html-dir: %.doxyconf-html $(DEVREF_STUFF)
	rm -rf html
	doxygen $< && rm -rf $@ && mv -f html $@

%.gz: %
	gzip --best --force $<

%.pdf: %.tex
	pdflatex $<

%.dvi: %.tex
	latex $<

%.ps:	%.dvi
	dvips -o $@ $<

EXTRA_DIST= \
$(DIST_DOCS)                 \
$(DOX_FILES)                 \
user-browse.doxyconf-latex   \
devref-browse.doxyconf-latex \
user-print.doxyconf-latex    \
devref-print.doxyconf-latex  \
user.doxyconf-html           \
devref.doxyconf-html         \
user.tex                     \
devref.tex                   \
gpl.tex                      \
fdl.tex
