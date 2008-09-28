/* Implementation of utility functions used in test programs.
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

#include "ppl_test.hh"
#include <csignal>
#include <iostream>
#include <exception>
#ifdef PPL_HAVE_FENV_H
#include <fenv.h>
#endif

namespace PPL = Parma_Polyhedra_Library;

using namespace Parma_Polyhedra_Library;
using namespace Parma_Polyhedra_Library::IO_Operators;

namespace {

void
unexpected_exception_handler() {
  std::cerr << "unexpected exception thrown" << std::endl;
  exit(1);
}

void
uncaught_exception_handler() {
  std::cerr << "uncaught exception" << std::endl;
  exit(1);
}

void
fpe_handler(int sig, siginfo_t* sip, void*) {
  if (sig != SIGFPE) {
    std::cerr << "fpe_handler called on signal different from SIGFPE"
	      << std::endl;
    exit(1);
  }
  const char* s = 0;
  switch (sip->si_code) {
  case FPE_INTDIV:
    s = "integer divide by zero";
    break;
  case FPE_INTOVF:
    s = "integer overflow";
    break;
  case FPE_FLTDIV:
    s = "floating point divide by zero";
    break;
  case FPE_FLTOVF:
    s = "floating point overflow";
    break;
  case FPE_FLTUND:
    s = "floating point underflow";
    break;
  case FPE_FLTRES:
    s = "floating point inexact result";
    break;
  case FPE_FLTINV:
    s = "floating point invalid operation";
    break;
  case FPE_FLTSUB:
    s = "subscript out of range";
    break;
  default:
    break;
  }
  if (s != 0)
    std::cerr << "SIGFPE caught (cause: " << s << ")"
	      << std::endl;
  else {
    std::cerr << "SIGFPE caught (unknown si_code " << sip->si_code << ")"
	      << std::endl;
    // FIXME: as of GCC 4.3.0, defined(PPL_HAVE_FENV_H) does not provide
    // the information we need (fenv.h is present, but does not contain
    // the required definitions).
#if defined(PPL_HAVE_FENV_H) && !defined(__CYGWIN__)
    std::cerr << "Inquire with fetestexcept(): ";
    if (fetestexcept(FE_INEXACT))
      std::cerr << "FE_INEXACT ";
    if (fetestexcept(FE_DIVBYZERO))
      std::cerr << "FE_DIVBYZERO ";
    if (fetestexcept(FE_UNDERFLOW))
      std::cerr << "FE_UNDERFLOW ";
    if (fetestexcept(FE_OVERFLOW))
      std::cerr << "FE_OVERFLOW ";
    if (fetestexcept(FE_INVALID))
      std::cerr << "FE_INVALID ";
    std::cerr << std::endl;
#endif
  }
  exit(1);
}

} // namespace

void
set_handlers() {
  struct sigaction action;
  action.sa_sigaction = fpe_handler;
  sigemptyset(&action.sa_mask);
  action.sa_flags = SA_SIGINFO;
  if (sigaction(SIGFPE, &action, NULL) != 0) {
    std::cerr << "sigaction() failed"
	      << std::endl;
    abort();
  }

  std::set_unexpected(unexpected_exception_handler);
  std::set_terminate(uncaught_exception_handler);
}

bool
PPL::check_distance(const Checked_Number<mpq_class, Extended_Number_Policy>& d,
		    const char* max_d_s, const char* d_name) {
  Checked_Number<mpq_class, Extended_Number_Policy>
    max_d((max_d_s ? max_d_s : "0"), ROUND_NOT_NEEDED);
  assert(max_d >= 0);
  if (d > max_d) {
#if 0
    // FIXME: avoid the conversion involving float.
    Checked_Number<float, Extended_Number_Policy> dd(d, ROUND_UP);
    nout << "Excessive " << d_name << " distance " << dd
	 << ": should be at most " << max_d << "."
	 << std::endl;
#endif
    return false;
  }
  else
    return true;
}
