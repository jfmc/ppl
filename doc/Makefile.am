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

# Make sure developers need not make the world every time.
all: ppl-user-@VERSION@-browse.pdf

world: $(ALL_TARGETS)

PPL_SOURCE_FILES = $(wildcard $(srcdir)/../src/*.hh $(srcdir)/../src/*.cc)

COMMON_STUFF = $(srcdir)/definitions.dox $(srcdir)/gpl.dox $(srcdir)/gfdl.dox

USER_STUFF = $(COMMON_STUFF) $(srcdir)/user.tex $(top_builddir)/src/ppl.hh

DEVREF_STUFF = $(COMMON_STUFF) $(srcdir)/devref.tex $(PPL_SOURCE_FILES)

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

%.gz: %
	gzip -9 -f $<

%.html-dir: %.doxyconf-html $(DEVREF_STUFF)
	rm -rf html
	doxygen $< && rm -rf $@ && mv -f html $@

EXTRA_DIST= \
Makefile \
user-browse.doxyconf-latex \
devref-browse.doxyconf-latex \
user-print.doxyconf-latex \
devref-print.doxyconf-latex \
user.doxyconf-html \
devref.doxyconf-html \
user.tex \
devref.tex
