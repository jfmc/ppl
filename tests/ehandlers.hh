/* Default exception handlers useful for debugging purposes.
   Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef _ehandlers_hh
#define _ehandlers_hh 1

#include <exception>
#include <iostream>
#include <cstdlib>

void
my_unexpected_exception() {
  std::cerr << "unexpected exception thrown" << std::endl;
  abort();
}

void
my_uncaught_exception() {
  std::cerr << "uncaught exception" << std::endl;
  abort();
}

void
set_handlers() {
  std::set_unexpected(my_unexpected_exception);
  std::set_terminate(my_uncaught_exception);
}
#endif

