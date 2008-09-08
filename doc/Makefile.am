# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>
#
# This file is part of the Parma Polyhedra Library (PPL).
#
# The PPL is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at your
# option) any later version.
#
# The PPL is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.
#
# For the most up-to-date information see the Parma Polyhedra Library
# site: http://www.cs.unipr.it/ppl/ .

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
ppl-user-@VERSION@-html.tar.gz \
ppl-devref-@VERSION@-html.tar.gz

PDF_CORE_MANUALS = \
ppl-user-@VERSION@.pdf \
ppl-devref-@VERSION@.pdf

PS_CORE_MANUALS = \
ppl-user-@VERSION@.ps.gz \
ppl-devref-@VERSION@.ps.gz

USER_HTML_DIR = ppl-user-@VERSION@-html
DEVREF_HTML_DIR = ppl-devref-@VERSION@-html

CORE_HTML_DIRS = $(USER_HTML_DIR) $(DEVREF_HTML_DIR)

DIST_CORE_HTML_DOCS = \
ppl-user-@VERSION@-html.tar.gz

DIST_CORE_PDF_DOCS = \
ppl-user-@VERSION@.pdf

DIST_CORE_PS_DOCS = \
ppl-user-@VERSION@.ps.gz

DOX_FILES = \
definitions.dox \
gpl.dox \
fdl.dox

USER_CORE_STUFF = \
$(DOX_FILES) \
user.tex \
ppl.sty \
$(top_builddir)/src/ppl.hh

DEVREF_CORE_STUFF = \
$(DOX_FILES) \
devref.tex \
ppl.sty \
$(PPL_SOURCE_FILES)


# C Language Interface Manuals
if BUILD_C_INTERFACE

HTML_C_INTERFACE_MANUALS = \
ppl-user-c-interface-@VERSION@-html.tar.gz \
ppl-devref-c-interface-@VERSION@-html.tar.gz

PDF_C_INTERFACE_MANUALS = \
ppl-user-c-interface-@VERSION@.pdf \
ppl-devref-c-interface-@VERSION@.pdf

PS_C_INTERFACE_MANUALS = \
ppl-user-c-interface-@VERSION@.ps.gz \
ppl-devref-c-interface-@VERSION@.ps.gz

C_INTERFACE_HTML_DIRS = \
ppl-user-c-interface-@VERSION@-html \
ppl-devref-c-interface-@VERSION@-html

DIST_C_INTERFACE_HTML_DOCS = \
ppl-user-c-interface-@VERSION@-html.tar.gz

DIST_C_INTERFACE_PDF_DOCS = \
ppl-user-c-interface-@VERSION@.pdf

DIST_C_INTERFACE_PS_DOCS = \
ppl-user-c-interface-@VERSION@.ps.gz

USER_C_INTERFACE_STUFF = \
user-c-interface.tex \
ppl.sty \
../interfaces/C/C_interface.dox \
$(top_builddir)/interfaces/C/ppl_c.h

DEVREF_C_INTERFACE_STUFF = \
devref-c-interface.tex \
ppl.sty \
$(top_builddir)/interfaces/C/ppl_c.cc

endif BUILD_C_INTERFACE


# Prolog Language Interface Manuals
if BUILD_SOME_PROLOG_INTERFACES

HTML_PROLOG_INTERFACE_MANUALS = \
ppl-user-prolog-interface-@VERSION@-html.tar.gz \
ppl-devref-prolog-interface-@VERSION@-html.tar.gz

PDF_PROLOG_INTERFACE_MANUALS = \
ppl-user-prolog-interface-@VERSION@.pdf \
ppl-devref-prolog-interface-@VERSION@.pdf

PS_PROLOG_INTERFACE_MANUALS = \
ppl-user-prolog-interface-@VERSION@.ps.gz \
ppl-devref-prolog-interface-@VERSION@.ps.gz

PROLOG_INTERFACE_HTML_DIRS = \
ppl-user-prolog-interface-@VERSION@-html \
ppl-devref-prolog-interface-@VERSION@-html

DIST_PROLOG_INTERFACE_HTML_DOCS = \
ppl-user-prolog-interface-@VERSION@-html.tar.gz

DIST_PROLOG_INTERFACE_PDF_DOCS = \
ppl-user-prolog-interface-@VERSION@.pdf

DIST_PROLOG_INTERFACE_PS_DOCS = \
ppl-user-prolog-interface-@VERSION@.ps.gz

USER_PROLOG_INTERFACE_STUFF = \
user-prolog-interface.tex \
ppl.sty \
$(top_builddir)/interfaces/Prolog/Prolog_interface.dox

DEVREF_PROLOG_INTERFACE_STUFF = \
devref-prolog-interface.tex \
ppl.sty \
$(top_builddir)/interfaces/Prolog/Prolog_interface.dox \
../interfaces/Prolog/ppl_prolog_common.defs.hh \
../interfaces/Prolog/ppl_prolog_common.inlines.hh \
../interfaces/Prolog/ppl_prolog_common.cc  \
$(top_builddir)/interfaces/Prolog/ppl_prolog_domains.hh \
$(top_builddir)/interfaces/Prolog/ppl_prolog_domains.cc

endif BUILD_SOME_PROLOG_INTERFACES


# OCaml Language Interface Manuals
if BUILD_OCAML_INTERFACE

HTML_OCAML_INTERFACE_MANUALS = \
ppl-user-ocaml-interface-@VERSION@-html.tar.gz \
ppl-devref-ocaml-interface-@VERSION@-html.tar.gz

PDF_OCAML_INTERFACE_MANUALS = \
ppl-user-ocaml-interface-@VERSION@.pdf \
ppl-devref-ocaml-interface-@VERSION@.pdf

PS_OCAML_INTERFACE_MANUALS = \
ppl-user-ocaml-interface-@VERSION@.ps.gz \
ppl-devref-ocaml-interface-@VERSION@.ps.gz

OCAML_INTERFACE_HTML_DIRS = \
ppl-user-ocaml-interface-@VERSION@-html \
ppl-devref-ocaml-interface-@VERSION@-html

DIST_OCAML_INTERFACE_HTML_DOCS = \
ppl-user-ocaml-interface-@VERSION@-html.tar.gz

DIST_OCAML_INTERFACE_PDF_DOCS = \
ppl-user-ocaml-interface-@VERSION@.pdf

DIST_OCAML_INTERFACE_PS_DOCS = \
ppl-user-ocaml-interface-@VERSION@.ps.gz

USER_OCAML_INTERFACE_STUFF = \
user-ocaml-interface.tex \
ppl.sty \
../interfaces/OCaml/OCaml_interface.dox \
../interfaces/OCaml/ppl_ocaml.mli

DEVREF_OCAML_INTERFACE_STUFF = \
devref-ocaml-interface.tex \
ppl.sty \
$(top_builddir)/interfaces/OCaml/ppl_ocaml.cc

endif BUILD_OCAML_INTERFACE


# Java Language Interface Manuals
if BUILD_JAVA_INTERFACE

HTML_JAVA_INTERFACE_MANUALS = \
ppl-user-java-interface-@VERSION@-html.tar.gz \
ppl-devref-java-interface-@VERSION@-html.tar.gz

PDF_JAVA_INTERFACE_MANUALS = \
ppl-user-java-interface-@VERSION@.pdf \
ppl-devref-java-interface-@VERSION@.pdf

PS_JAVA_INTERFACE_MANUALS = \
ppl-user-java-interface-@VERSION@.ps.gz \
ppl-devref-java-interface-@VERSION@.ps.gz

JAVA_INTERFACE_HTML_DIRS = \
ppl-user-java-interface-@VERSION@-html \
ppl-devref-java-interface-@VERSION@-html

DIST_JAVA_INTERFACE_HTML_DOCS = \
ppl-user-java-interface-@VERSION@-html.tar.gz

DIST_JAVA_INTERFACE_PDF_DOCS = \
ppl-user-java-interface-@VERSION@.pdf

DIST_JAVA_INTERFACE_PS_DOCS = \
ppl-user-java-interface-@VERSION@.ps.gz

JAVA_INTERFACE_JAVA_FILES = \
../interfaces/Java/Java_interface.dox \
../interfaces/Java/ppl_java/By_Reference.java \
../interfaces/Java/ppl_java/Coefficient.java \
../interfaces/Java/ppl_java/Congruence.java \
../interfaces/Java/ppl_java/Congruence_System.java \
../interfaces/Java/ppl_java/Constraint.java \
../interfaces/Java/ppl_java/Constraint_System.java \
../interfaces/Java/ppl_java/Degenerate_Element.java \
../interfaces/Java/ppl_java/Domain_Error_Exception.java \
../interfaces/Java/ppl_java/Generator.java \
../interfaces/Java/ppl_java/Generator_System.java \
../interfaces/Java/ppl_java/Generator_Type.java \
../interfaces/Java/ppl_java/Grid_Generator.java \
../interfaces/Java/ppl_java/Grid_Generator_System.java \
../interfaces/Java/ppl_java/Grid_Generator_Type.java \
../interfaces/Java/ppl_java/Invalid_Argument_Exception.java \
../interfaces/Java/ppl_java/Length_Error_Exception.java \
../interfaces/Java/ppl_java/Linear_Expression_Coefficient.java \
../interfaces/Java/ppl_java/Linear_Expression_Difference.java \
../interfaces/Java/ppl_java/Linear_Expression.java \
../interfaces/Java/ppl_java/Linear_Expression_Sum.java \
../interfaces/Java/ppl_java/Linear_Expression_Times.java \
../interfaces/Java/ppl_java/Linear_Expression_Unary_Minus.java \
../interfaces/Java/ppl_java/Linear_Expression_Variable.java \
../interfaces/Java/ppl_java/Logic_Error_Exception.java \
../interfaces/Java/ppl_java/MIP_Problem.java \
../interfaces/Java/ppl_java/MIP_Problem_Status.java \
../interfaces/Java/ppl_java/Optimization_Mode.java \
../interfaces/Java/ppl_java/Overflow_Error_Exception.java \
../interfaces/Java/ppl_java/Parma_Polyhedra_Library.java \
../interfaces/Java/ppl_java/Partial_Function.java \
../interfaces/Java/ppl_java/Poly_Con_Relation.java \
../interfaces/Java/ppl_java/Poly_Gen_Relation.java \
../interfaces/Java/ppl_java/PPL_Object.java \
../interfaces/Java/ppl_java/Relation_Symbol.java \
../interfaces/Java/ppl_java/Variable.java \
../interfaces/Java/ppl_java/Variables_Set.java
# FIXME add the automatically-generated files.

USER_JAVA_INTERFACE_STUFF = \
user-java-interface.tex \
ppl.sty \
$(JAVA_INTERFACE_JAVA_FILES)

DEVREF_JAVA_INTERFACE_STUFF = \
devref-java-interface.tex \
ppl.sty \
$(JAVA_INTERFACE_JAVA_FILES) \
../interfaces/Java/jni/ppl_java_common.cc \
../interfaces/Java/jni/ppl_java_common.hh \
../interfaces/Java/jni/ppl_java_globals.cc
# FIXME add the automatically-generated files.

endif BUILD_JAVA_INTERFACE


MANUALS = \
$(HTML_CORE_MANUALS) \
$(HTML_C_INTERFACE_MANUALS) \
$(HTML_PROLOG_INTERFACE_MANUALS) \
$(HTML_OCAML_INTERFACE_MANUALS) \
$(HTML_JAVA_INTERFACE_MANUALS) \
$(PDF_CORE_MANUALS) \
$(PDF_C_INTERFACE_MANUALS) \
$(PDF_PROLOG_INTERFACE_MANUALS) \
$(PDF_OCAML_INTERFACE_MANUALS) \
$(PDF_JAVA_INTERFACE_MANUALS) \
$(PS_CORE_MANUALS) \
$(PS_C_INTERFACE_MANUALS) \
$(PS_PROLOG_INTERFACE_MANUALS) \
$(PS_OCAML_INTERFACE_MANUALS) \
$(PS_JAVA_INTERFACE_MANUALS)

HTML_DIRS = \
$(CORE_HTML_DIRS) \
$(C_INTERFACE_HTML_DIRS) \
$(PROLOG_INTERFACE_HTML_DIRS) \
$(OCAML_INTERFACE_HTML_DIRS) \
$(JAVA_INTERFACE_HTML_DIRS)

DIST_HTML_DOCS = \
$(DIST_CORE_HTML_DOCS)
# FIXME: should we distribute manuals for the "foreign" interfaces?
#$(DIST_C_INTERFACE_HTML_DOCS) \
#$(DIST_PROLOG_INTERFACE_HTML_DOCS) \
#$(DIST_OCAML_INTERFACE_HTML_DOCS) \
#$(DIST_JAVA_INTERFACE_HTML_DOCS)

DIST_PDF_DOCS = \
$(PDF_LICENSES) \
$(DIST_CORE_PDF_DOCS)
# FIXME: should we distribute manuals for the "foreign" interfaces?
#$(DIST_C_INTERFACE_PDF_DOCS) \
#$(DIST_PROLOG_INTERFACE_PDF_DOCS) \
#$(DIST_OCAML_INTERFACE_PDF_DOCS) \
#$(DIST_JAVA_INTERFACE_PDF_DOCS)

DIST_PS_DOCS = \
$(PS_LICENSES) \
$(DIST_CORE_PS_DOCS)
# FIXME: should we distribute manuals for the "foreign" interfaces?
#$(DIST_C_INTERFACE_PS_DOCS) \
#$(DIST_PROLOG_INTERFACE_PS_DOCS) \
#$(DIST_OCAML_INTERFACE_PS_DOCS) \
#$(DIST_JAVA_INTERFACE_PS_DOCS)

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

dist_man_MANS = \
libppl.3 \
ppl-config.1 \
ppl_lcdd.1 \
ppl_lpsol.1

# Do nothing by default.
all:

user: ppl-user-@VERSION@.pdf

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
	for file in BUGS COPYING CREDITS ChangeLog NEWS README README.configure TODO ; do \
	  $(INSTALL_DATA) $(srcdir)/../$$file $(DESTDIR)$(txtdir)/$$file; \
	done

installdirs-txt:
	$(mkinstalldirs) $(DESTDIR)$(txtdir)

uninstall-txt:
	rm -f $(DESTDIR)$(txtdir)/*.txt $(DESTDIR)$(txtdir)/README.doc
	for file in BUGS COPYING CREDITS ChangeLog NEWS README README.configure TODO ; do \
	  rm -f $(DESTDIR)$(txtdir)/$$file; \
	done

PPL_SOURCE_FILES = $(wildcard $(srcdir)/../src/*.hh $(srcdir)/../src/*.cc)

LATEX_DIRS = \
user.latex-dir \
devref.latex-dir \
user-c-interface.latex-dir \
devref-c-interface.latex-dir \
user-prolog-interface.latex-dir \
devref-prolog-interface.latex-dir \
user-ocaml-interface.latex-dir \
devref-ocaml-interface.latex-dir \
user-java-interface.latex-dir \
devref-java-interface.latex-dir

TEX_ENV = TEXINPUTS=$(abs_srcdir):$(TEXINPUTS)

.SECONDARY: $(LATEX_DIRS)

user.latex-dir: user.doxyconf-latex $(USER_CORE_STUFF)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<

devref.latex-dir: devref.doxyconf-latex $(DEVREF_CORE_STUFF)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<

ppl-user-@VERSION@.pdf: user.latex-dir
	$(TEX_ENV) make -C $< refman.pdf && mv -f $</refman.pdf $@

ppl-devref-@VERSION@.pdf: devref.latex-dir
	$(TEX_ENV) make -C $< refman.pdf && mv -f $</refman.pdf $@

$(USER_HTML_DIR): user.doxyconf-html $(USER_CORE_STUFF)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<

$(DEVREF_HTML_DIR): devref.doxyconf-html $(DEVREF_CORE_STUFF)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<


# C Language Interface
if BUILD_C_INTERFACE

user-c-interface.latex-dir: \
user-c-interface.doxyconf-latex $(USER_C_INTERFACE_STUFF)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<

devref-c-interface.latex-dir: \
devref-c-interface.doxyconf-latex $(DEVREF_C_INTERFACE_STUFF)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<

ppl-user-c-interface-@VERSION@.pdf: user-c-interface.latex-dir
	$(TEX_ENV) make -C $< refman.pdf && mv -f $</refman.pdf $@

ppl-devref-c-interface-@VERSION@.pdf: devref-c-interface.latex-dir
	$(TEX_ENV) make -C $< refman.pdf && mv -f $</refman.pdf $@

ppl-user-c-interface-@VERSION@-html: \
user-c-interface.doxyconf-html \
$(USER_C_INTERFACE_STUFF) $(USER_HTML_DIR)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<
	(cd $@; $(INSTALLDOX) -l $(USER_HTML_DIR).tag@../$(USER_HTML_DIR))

ppl-devref-c-interface-@VERSION@-html: \
devref-c-interface.doxyconf-html \
$(DEVREF_C_INTERFACE_STUFF) $(DEVREF_HTML_DIR)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<
	(cd $@; $(INSTALLDOX) -l $(DEVREF_HTML_DIR).tag@../$(DEVREF_HTML_DIR))

endif BUILD_C_INTERFACE

# (System Independent) Prolog Language Interface
if BUILD_SOME_PROLOG_INTERFACES

user-prolog-interface.latex-dir: \
user-prolog-interface.doxyconf-latex $(USER_PROLOG_INTERFACE_STUFF)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<

devref-prolog-interface.latex-dir: \
devref-prolog-interface.doxyconf-latex $(DEVREF_PROLOG_INTERFACE_STUFF)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<

ppl-user-prolog-interface-@VERSION@.pdf: user-prolog-interface.latex-dir
	$(TEX_ENV) make -C $< refman.pdf && mv -f $</refman.pdf $@

ppl-devref-prolog-interface-@VERSION@.pdf: devref-prolog-interface.latex-dir
	$(TEX_ENV) make -C $< refman.pdf && mv -f $</refman.pdf $@

ppl-user-prolog-interface-@VERSION@-html: \
user-prolog-interface.doxyconf-html \
$(USER_PROLOG_INTERFACE_STUFF) $(USER_HTML_DIR)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<
	(cd $@; $(INSTALLDOX) -l $(USER_HTML_DIR).tag@../$(USER_HTML_DIR))

ppl-devref-prolog-interface-@VERSION@-html: \
devref-prolog-interface.doxyconf-html \
$(DEVREF_PROLOG_INTERFACE_STUFF) $(DEVREF_HTML_DIR)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<
	(cd $@; $(INSTALLDOX) -l $(DEVREF_HTML_DIR).tag@../$(DEVREF_HTML_DIR))

endif BUILD_SOME_PROLOG_INTERFACES


# OCaml Language Interface
if BUILD_OCAML_INTERFACE

user-ocaml-interface.latex-dir: \
user-ocaml-interface.doxyconf-latex \
$(USER_OCAML_INTERFACE_STUFF)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<
	(cd $@; ocamldoc -I +gmp \
			-latex -noheader -notrailer -notoc -o ocamldoc.tex \
			../../interfaces/OCaml/ppl_ocaml.mli)

devref-ocaml-interface.latex-dir: \
devref-ocaml-interface.doxyconf-latex \
$(DEVREF_OCAML_INTERFACE_STUFF)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<
	(cd $@; ocamldoc -I +gmp \
			-latex -noheader -notrailer -notoc -o ocamldoc.tex \
			../../interfaces/OCaml/ppl_ocaml.mli)

ppl-user-ocaml-interface-@VERSION@.pdf: user-ocaml-interface.latex-dir
	$(TEX_ENV) make -C $< refman.pdf && mv -f $</refman.pdf $@

ppl-devref-ocaml-interface-@VERSION@.pdf: devref-ocaml-interface.latex-dir
	$(TEX_ENV) make -C $< refman.pdf && mv -f $</refman.pdf $@

ppl-user-ocaml-interface-@VERSION@-html: \
user-ocaml-interface.doxyconf-html \
$(USER_OCAML_INTERFACE_STUFF) $(USER_HTML_DIR)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<
	mkdir $@/ocamldoc
	ocamldoc -I +gmp \
		-html -d $@/ocamldoc ../interfaces/OCaml/ppl_ocaml.mli
	(cd $@; $(INSTALLDOX) -l $(USER_HTML_DIR).tag@../$(USER_HTML_DIR))

ppl-devref-ocaml-interface-@VERSION@-html: \
devref-ocaml-interface.doxyconf-html \
$(DEVREF_OCAML_INTERFACE_STUFF) $(DEVREF_HTML_DIR)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<
	mkdir $@/ocamldoc
	ocamldoc -I +gmp \
		-html -d $@/ocamldoc ../interfaces/OCaml/ppl_ocaml.mli
	(cd $@; $(INSTALLDOX) -l $(DEVREF_HTML_DIR).tag@../$(DEVREF_HTML_DIR))

endif BUILD_OCAML_INTERFACE


# Java Language Interface
if BUILD_JAVA_INTERFACE

user-java-interface.latex-dir: \
user-java-interface.doxyconf-latex \
$(USER_JAVA_INTERFACE_STUFF)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<

devref-java-interface.latex-dir: \
devref-java-interface.doxyconf-latex \
$(DEVREF_JAVA_INTERFACE_STUFF)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<

ppl-user-java-interface-@VERSION@.pdf: user-java-interface.latex-dir
	$(TEX_ENV) make -C $< refman.pdf && mv -f $</refman.pdf $@

ppl-devref-java-interface-@VERSION@.pdf: devref-java-interface.latex-dir
	$(TEX_ENV) make -C $< refman.pdf && mv -f $</refman.pdf $@

ppl-user-java-interface-@VERSION@-html: \
user-java-interface.doxyconf-html \
$(USER_JAVA_INTERFACE_STUFF) $(USER_HTML_DIR)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<
	(cd $@; $(INSTALLDOX) -l $(USER_HTML_DIR).tag@../$(USER_HTML_DIR))

ppl-devref-java-interface-@VERSION@-html: \
devref-java-interface.doxyconf-html \
$(DEVREF_JAVA_INTERFACE_STUFF) $(DEVREF_HTML_DIR)
	rm -rf $@
	$(TEX_ENV) $(DOXYGEN) $<
	(cd $@; $(INSTALLDOX) -l $(DEVREF_HTML_DIR).tag@../$(DEVREF_HTML_DIR))

endif BUILD_JAVA_INTERFACE


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
README.doc \
devref-c-interface.doxyconf-html \
devref-c-interface.doxyconf-latex \
devref-c-interface.tex \
devref-java-interface.doxyconf-latex \
devref-java-interface.tex \
devref-ocaml-interface.doxyconf-latex \
devref-ocaml-interface.tex \
devref-prolog-interface.doxyconf-html \
devref-prolog-interface.doxyconf-latex \
devref-prolog-interface.tex \
devref.doxyconf-html \
devref.doxyconf-latex \
devref.tex \
fdl.tex \
gpl.tex \
ppl.sty \
user-c-interface.doxyconf-html \
user-c-interface.doxyconf-latex \
user-c-interface.tex \
user-java-interface.doxyconf-latex \
user-java-interface.tex \
user-ocaml-interface.doxyconf-latex \
user-ocaml-interface.tex \
user-prolog-interface.doxyconf-html \
user-prolog-interface.doxyconf-latex \
user-prolog-interface.tex \
user.doxyconf-html \
user.doxyconf-latex \
user.tex

mostlyclean-local:
	rm -rf $(ALL_TARGETS) $(LATEX_DIRS) $(HTML_DIRS) *.aux *.log

CLEANFILES = \
$(DIST_HTML_DOCS) \
$(DIST_PDF_DOCS) \
$(DIST_PS_DOCS) \
$(USER_HTML_DIR).tag \
$(DEVREF_HTML_DIR).tag
