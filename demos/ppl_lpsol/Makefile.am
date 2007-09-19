# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>
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

SUBDIRS = examples

EXTRA_DIST = \
ppl_lpsol.c \
expected_mpz \
expected_mpz_a \
expected_int8 \
expected_int8_a \
expected_int16 \
expected_int16_a \
expected_int32 \
expected_int32_a \
expected_int64 \
expected_int64_a

if BUILD_C_INTERFACE
if HAVE_GLPK

AM_CPPFLAGS = -I$(top_builddir)/interfaces/C @extra_includes@

bin_PROGRAMS = ppl_lpsol
# dummy.cc serves the only purpose to force linking using the C++ compiler.
ppl_lpsol_SOURCES = \
ppl_lpsol.c \
dummy.cc
ppl_lpsol_LDADD = \
-lglpk \
$(top_builddir)/interfaces/C/libppl_c.la \
@extra_libraries@

COEFFICIENT_MNEMONIC = @coefficient_mnemonic@

if ASSERTIONS_ENABLED

TOUGH_EXAMPLES_FOR_ENUM_IF_ASSERTIONS_ENABLED = \
afiro.mps

TOUGH_EXAMPLES_FOR_SIMPLEX_IF_ASSERTIONS_ENABLED = \
opt1217.mps \
rout.mps

TOUGH_EXAMPLES_FOR_BB_SATISFIABILTY_IF_ASSERTIONS_ENABLED = \
mas74.mps \
mas76.mps \
modglob.mps \
noswot.mps \
opt1217.mps \
pk1.mps \
rout.mps

EXPECTED = expected_$(COEFFICIENT_MNEMONIC)_a

CHECK_OPTION =

else !ASSERTIONS_ENABLED

EXPECTED = expected_$(COEFFICIENT_MNEMONIC)

CHECK_OPTION = -c

endif !ASSERTIONS_ENABLED

TOUGH_EXAMPLES_FOR_ENUM = \
adlittle.mps \
blend.mps \
boeing1.mps \
boeing2.mps \
egout.mps \
kb2.mps \
lseu.mps \
markshare1.mps \
markshare2.mps \
mas74.mps \
mas76.mps \
modglob.mps \
noswot.mps \
opt1217.mps \
p0033.mps \
pk1.mps \
rout.mps \
sc105.mps \
sc50a.mps \
sc50b.mps \
ship08l.mps \
$(TOUGH_EXAMPLES_FOR_ENUM_IF_ASSERTIONS_ENABLED)

TOUGH_EXAMPLES_FOR_SIMPLEX = \
ship08l.mps\
$(TOUGH_EXAMPLES_FOR_SIMPLEX_IF_ASSERTIONS_ENABLED)

TOUGH_EXAMPLES_FOR_BB_SATISFIABILITY = \
$(TOUGH_EXAMPLES_FOR_BB_SATISFIABILTY_IF_ASSERTIONS_ENABLED)

TOUGH_EXAMPLES_FOR_BB_OPTIMALITY = \
egout.mps \
lseu.mps \
markshare1.mps \
markshare2.mps \
mas74.mps \
mas76.mps \
modglob.mps \
noswot.mps \
opt1217.mps \
pk1.mps \
rout.mps

LP_PROBLEMS = \
adlittle.mps \
afiro.mps \
bgprtr.mps \
blend.mps \
boeing1.mps \
boeing2.mps \
kb2.mps \
sample.mps \
sc105.mps \
sc50a.mps \
sc50b.mps \
ship08l.mps

DO_NOT_CHECK_PROBLEMS = \
ex12.mps \
mas74.mps \
mas76.mps \
modglob.mps

if HOST_OS_CYGWIN

DIFF_COMMAND = \
grep "^Optimum value: " $(srcdir)/$(EXPECTED) >expected_optima \
&& grep "^Optimum value: " obtained >obtained_optima \
&& diff -u expected_optima obtained_optima

else !HOST_OS_CYGWIN

if HOST_OS_DARWIN

DIFF_COMMAND = \
grep "^Optimum value: " $(srcdir)/$(EXPECTED) >expected_optima \
&& grep "^Optimum value: " obtained >obtained_optima \
&& diff -u expected_optima obtained_optima

else !HOST_OS_DARWIN

if HOST_OS_SOLARIS

DIFF_COMMAND = \
grep "^Optimum value: " $(srcdir)/$(EXPECTED) \
| sed -e "s/^Optimum value: \(-*[0-9\.][0-9\.][0-9\.][0-9\.][0-9\.][0-9\.][0-9\.][0-9\.][0-9\.]\)[0-9]*$/Optimum value: \1/g" >expected_optima \
&& grep "^Optimum value: " obtained \
| sed -e "s/^Optimum value: \(-*[0-9\.][0-9\.][0-9\.][0-9\.][0-9\.][0-9\.][0-9\\.][0-9\.][0-9\.]\)[0-9]*$/Optimum value: \1/g" >obtained_optima \
&& diff -u expected_optima obtained_optima

else !HOST_OS_SOLARIS

DIFF_COMMAND = diff -u $(srcdir)/$(EXPECTED) obtained

endif !HOST_OS_SOLARIS

endif !HOST_OS_DARWIN

endif !HOST_OS_CYGWIN

check-local: ppl_lpsol
	echo $(ECHO_N) "$(ECHO_C)" >obtained
	echo $(LP_PROBLEMS) >lp_problems
	echo $(DO_NOT_CHECK_PROBLEMS) >do_not_check_problems
	LANG=C; \
	echo $(TOUGH_EXAMPLES_FOR_BB_SATISFIABILITY) >tough_examples
	-for path in $(srcdir)/examples/*.mps; \
	do \
	  file=`basename $$path`; \
	  grep $$file lp_problems >/dev/null 2>&1 && continue; \
	  grep $$file do_not_check_problems >/dev/null 2>&1 && continue; \
	  grep $$file tough_examples >/dev/null 2>&1 && continue; \
	  echo "*** ppl_lpsol -s -n $$m -oobtained $$file" >>obtained; \
	  ./ppl_lpsol -s -n $(CHECK_OPTION) -oobtained $$path >/dev/null; \
	done
	echo $(LP_PROBLEMS) >lp_problems
	echo $(TOUGH_EXAMPLES_FOR_BB_OPTIMALITY) >tough_examples
	-for path in $(srcdir)/examples/*.mps; \
	do \
	  file=`basename $$path`; \
	  grep $$file do_not_check_problems >/dev/null 2>&1 && continue; \
	  grep $$file lp_problems >/dev/null 2>&1 && continue; \
	  grep $$file tough_examples >/dev/null 2>&1 && continue; \
	  for m in -m -M; \
	  do \
	    echo "*** ppl_lpsol -s $$m -oobtained $$file" >>obtained; \
	    ./ppl_lpsol -s $(CHECK_OPTION) $$m -oobtained $$path >/dev/null; \
	  done \
	done
	echo $(TOUGH_EXAMPLES_FOR_SIMPLEX) >tough_examples
	-for path in $(srcdir)/examples/*.mps; \
	do \
	  file=`basename $$path`; \
	 grep $$file do_not_check_problems >/dev/null 2>&1 && continue; \
	 grep $$file tough_examples >/dev/null 2>&1 && continue; \
	 for m in -m -M; \
	 do \
	    echo "*** ppl_lpsol -r -s $$m -oobtained $$file" >>obtained; \
	    ./ppl_lpsol -r -s $(CHECK_OPTION) $$m -oobtained $$path >/dev/null; \
	  done \
	done
	echo $(TOUGH_EXAMPLES_FOR_ENUM) >tough_examples
	-for path in $(srcdir)/examples/*.mps; \
	do \
	  file=`basename $$path`; \
	  grep $$file do_not_check_problems >/dev/null 2>&1 && continue; \
	  grep $$file tough_examples >/dev/null 2>&1 && continue; \
	  echo "*** ppl_lpsol -e -r $$m -oobtained $$file" >>obtained; \
	  ./ppl_lpsol -e -r $(CHECK_OPTION) $$m -oobtained $$path >/dev/null; \
	done
	echo "*** ppl_lpsol -r -s -m -oobtained mas74.mps" >>obtained
	-./ppl_lpsol -r -s -m $(CHECK_OPTION) $(srcdir)/examples/mas74.mps -oobtained >/dev/null
	echo "*** ppl_lpsol -r -s -M -c0.000123 -oobtained mas74.mps" >>obtained
	-./ppl_lpsol -r -s -M -c0.000123 $(srcdir)/examples/mas74.mps $$m -oobtained >/dev/null
	echo "*** ppl_lpsol -n -s -oobtained mas74.mps" >>obtained
	-./ppl_lpsol -n -s $(CHECK_OPTION) $(srcdir)/examples/mas74.mps -oobtained >/dev/null
	echo "*** ppl_lpsol -r -s -m -oobtained mas76.mps" >>obtained
	-./ppl_lpsol -r -s -m $(CHECK_OPTION) $(srcdir)/examples/mas76.mps -oobtained >/dev/null
	echo "*** ppl_lpsol -r -s -M -c0.000123 -oobtained mas76.mps" >>obtained
	-./ppl_lpsol -r -s -M -c0.000123 $(srcdir)/examples/mas76.mps -oobtained >/dev/null
	echo "*** ppl_lpsol -n -s -oobtained mas76.mps" >>obtained
	-./ppl_lpsol -n -s $(CHECK_OPTION) $(srcdir)/examples/mas76.mps -oobtained >/dev/null
	echo "*** ppl_lpsol -r -s -m -c0.0000002 -oobtained modglob.mps" >>obtained
	-./ppl_lpsol -r -s -m -c0.0000002 $(srcdir)/examples/modglob.mps -oobtained >/dev/null
	echo "*** ppl_lpsol -r -s -M -c0.000003 -oobtained modglob.mps" >>obtained
	-./ppl_lpsol -r -s -M -c0.000003 $(srcdir)/examples/modglob.mps -oobtained >/dev/null
	echo "*** ppl_lpsol -n -s -oobtained modglob.mps" >>obtained
	-./ppl_lpsol -n -s $(CHECK_OPTION) $(srcdir)/examples/modglob.mps -oobtained >/dev/null
	$(DIFF_COMMAND)

MOSTLYCLEANFILES = \
do_not_check_problems \
tough_examples \
obtained \
expected_optima \
obtained_optima \
lp_problems

endif HAVE_GLPK
endif BUILD_C_INTERFACE
