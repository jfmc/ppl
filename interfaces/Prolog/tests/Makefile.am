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
VPATH = @srcdir@

@SET_MAKE@

SUBDIRS =

CLPQ_TESTS_NONSTRICT = \
ack.clpq \
ackn.clpq \
fib.clpq \
mc91.clpq \
smm.clpq \
sumto.clpq \
tak.clpq

CLPQ_TESTS_STRICT = \
schedule.clpq

CLPQ_TESTS = $(CLPQ_TESTS_NONSTRICT) $(CLPQ_TESTS_STRICT)

EXTRA_DIST = \
clpq.pl check_script \
clpq2.pl check_script2 check_script2_int8 \
$(CLPQ_TESTS) \
pl_check.pl \
expected_mpz expected2_mpz expected3_mpz \
expected_int8 expected2_int8 expected3_int8 \
expected_int16 expected2_int16 expected3_int16 \
expected_int32 expected2_int32 expected3_int32 \
expected_int64 expected2_int64 expected3_int64 \
expected_mpz_a expected2_mpz_a expected3_mpz_a \
expected_int8_a expected2_int8_a expected3_int8_a \
expected_int16_a expected2_int16_a expected3_int16_a \
expected_int32_a expected2_int32_a expected3_int32_a \
expected_int64_a expected2_int64_a expected3_int64_a
