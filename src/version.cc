/* Definition of functions providing version and licensing information.
   Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include <config.h>
#include "version.hh"

namespace PPL = Parma_Polyhedra_Library;

namespace {

const char version_string[] = PACKAGE_VERSION;

const char banner_string[] =
"This is "PACKAGE_NAME" (PPL) version "PACKAGE_VERSION".\n"
"Copyright (C) 2001-2003 Roberto Bagnara <bagnara@cs.unipr.it>.\n"
"The PPL is free software; see the source for copying conditions.\n"
"There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A\n"
"PARTICULAR PURPOSE.\n"
#if defined(__GNUC__)
"Compiled by the GNU C++ compiler version "__VERSION__".\n"
#elif defined(__INTEL_COMPILER)
#define str(s) # s
#define xstr(s) str(s)
"Compiled by the Intel C++ compiler version "xstr(__INTEL_COMPILER)".\n"
#elif defined(__COMO__)
#define str(s) # s
#define xstr(s) str(s)
"Compiled by the Comeau C++ compiler version "xstr(__COMO_VERSION__)".\n"
#endif
"Report bugs to "PACKAGE_BUGREPORT".\n"
"For the most up-to-date information see the Parma Polyhedra Library\n"
"site: http://www.cs.unipr.it/ppl/ .\n";

} // namespace

const char*
PPL::version() {
  return version_string;
}

const char*
PPL::banner() {
  return banner_string;
}
