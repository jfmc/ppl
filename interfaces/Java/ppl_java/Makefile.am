# Automake source file for the Parma Polyhedra Library.
# Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>
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
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.
#
# For the most up-to-date information see the Parma Polyhedra Library
# site: http://www.cs.unipr.it/ppl/ .

if BUILD_JAVA_INTERFACE

ppl_java_version = 1:0:0

ppl_javadir = $(prefix)/lib/java/classes/ppl

ppl_java_JAVA = \
Coefficient.java \
Variable.java

ppl_java_CLASSES = $(ppl_java_JAVA:.java=.class)

all:	$(ppl_java_CLASSES)

Coefficient.class: Coefficient.java
	$(JAVAC) -d $(top_builddir)/interfaces/Java $(srcdir)/Coefficient.java
Variable.class: Variable.java
	$(JAVAC) -d $(top_builddir)/interfaces/Java $(srcdir)/Variable.java

endif BUILD_JAVA_INTERFACE
