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

LATEX = latex
PDFLATEX = pdflatex

all: ppl-user-@VERSION@.pdf ppl-devref-@VERSION@.pdf user.html-dir devref.html-dir

PPL_SOURCE_FILES = $(wildcard $(srcdir)/../src/*.hh $(srcdir)/../src/*.cc)

COMMON_STUFF = $(srcdir)/definitions.dox $(srcdir)/gpl.dox $(srcdir)/gfdl.dox

USER_STUFF = $(COMMON_STUFF) $(srcdir)/user.tex $(top_builddir)/src/ppl.hh

DEVREF_STUFF = $(COMMON_STUFF) $(srcdir)/devref.tex $(PPL_SOURCE_FILES)

ppl-user-@VERSION@.pdf: user.doxyconf-pdf $(USER_STUFF)
	rm -rf latex
	doxygen $<
	cd latex ; $(PDFLATEX) refman.tex ; makeindex refman.idx ; $(PDFLATEX) refman.tex ; $(PDFLATEX) refman.tex ; mv -f refman.pdf ../$@
	cd latex ; $(LATEX) refman.tex ; makeindex refman.idx ; $(LATEX) refman.tex ; $(LATEX) refman.tex ; dvips -D600 -f refman.dvi >refman.ps ; mv -f refman.ps ../ppl-user-@VERSION@.ps ; rm -f ../ppl-user-@VERSION@.ps.gz ; gzip -9 ../ppl-user-@VERSION@.ps

ppl-devref-@VERSION@.pdf: devref.doxyconf-pdf $(DEVREF_STUFF) 
	rm -rf latex
	doxygen $<
	cd latex ; $(PDFLATEX) refman.tex ; makeindex refman.idx ; $(PDFLATEX) refman.tex ; $(PDFLATEX) refman.tex ; mv -f refman.pdf ../$@
	cd latex ; $(LATEX) refman.tex ; makeindex refman.idx ; $(LATEX) refman.tex ; $(LATEX) refman.tex ; dvips -D600 -f refman.dvi >refman.ps ; mv -f refman.ps ../ppl-devref-@VERSION@.ps ; rm -f ../ppl-devref-@VERSION@.ps.gz ; gzip -9 ../ppl-devref-@VERSION@.ps

%.html-dir: %.doxyconf-html $(DEVREF_STUFF)
	rm -rf html
	doxygen $< && rm -rf $@ && mv -f html $@

EXTRA_DIST= \
Makefile \
user.doxyconf-pdf \
devref.doxyconf-pdf \
devref.doxyconf-html \
user.tex \
devref.tex
