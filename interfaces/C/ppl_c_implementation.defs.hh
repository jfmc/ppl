/* Implementation of the C interface: declarations.
   Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_ppl_c_implementation_defs_hh
#define PPL_ppl_c_implementation_defs_hh 1

#define PPL_NO_AUTOMATIC_INITIALIZATION
#include "ppl.hh"
#include "ppl_c.h"
#include <stdexcept>
#include <limits>
#include <sstream>
#include <cstdio>
#include <cerrno>
#include <climits>

using namespace Parma_Polyhedra_Library;


namespace {

extern "C" typedef void
(*error_handler_type)(enum ppl_enum_error_code code, const char* description);

error_handler_type user_error_handler = 0;

void
notify_error(enum ppl_enum_error_code code, const char* description) {
  if (user_error_handler != 0)
    user_error_handler(code, description);
}

} // namespace

int
ppl_set_error_handler(error_handler_type h) {
  user_error_handler = h;
  return 0;
}

#define CATCH_STD_EXCEPTION(exception, code) \
catch (const std::exception& e) {	     \
  notify_error(code, e.what()); \
  return code; \
}

#define CATCH_ALL \
CATCH_STD_EXCEPTION(bad_alloc, PPL_ERROR_OUT_OF_MEMORY) \
CATCH_STD_EXCEPTION(invalid_argument, PPL_ERROR_INVALID_ARGUMENT) \
CATCH_STD_EXCEPTION(domain_error, PPL_ERROR_DOMAIN_ERROR) \
CATCH_STD_EXCEPTION(length_error, PPL_ERROR_LENGTH_ERROR) \
CATCH_STD_EXCEPTION(overflow_error, PPL_ARITHMETIC_OVERFLOW) \
CATCH_STD_EXCEPTION(runtime_error, PPL_ERROR_INTERNAL_ERROR) \
CATCH_STD_EXCEPTION(exception, PPL_ERROR_UNKNOWN_STANDARD_EXCEPTION) \
catch (...) {						     \
  notify_error(PPL_ERROR_UNEXPECTED_ERROR, \
	       "completely unexpected error: a bug in the PPL"); \
  return PPL_ERROR_UNEXPECTED_ERROR; \
}

unsigned int PPL_POLY_CON_RELATION_IS_DISJOINT;
unsigned int PPL_POLY_CON_RELATION_STRICTLY_INTERSECTS;
unsigned int PPL_POLY_CON_RELATION_IS_INCLUDED;
unsigned int PPL_POLY_CON_RELATION_SATURATES;

unsigned int PPL_POLY_GEN_RELATION_SUBSUMES;

unsigned int PPL_COMPLEXITY_CLASS_POLYNOMIAL;
unsigned int PPL_COMPLEXITY_CLASS_SIMPLEX;
unsigned int PPL_COMPLEXITY_CLASS_ANY;

int PPL_MIP_PROBLEM_STATUS_UNFEASIBLE;
int PPL_MIP_PROBLEM_STATUS_UNBOUNDED;
int PPL_MIP_PROBLEM_STATUS_OPTIMIZED;

int PPL_OPTIMIZATION_MODE_MINIMIZATION;
int PPL_OPTIMIZATION_MODE_MAXIMIZATION;

#include "ppl_c_implementation.inlines.hh"

#endif // !defined(PPL_ppl_c_implementation_defs_hh)
