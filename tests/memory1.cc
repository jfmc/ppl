/* Test the allocation error recovery facility of the library.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "ppl_install.hh"
#include "print.hh"
#include "ehandlers.hh"

#include <new>
#include <iostream>
#include <cstring>
#include <cerrno>

#ifdef HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif

using namespace std;
using namespace Parma_Polyhedra_Library;

#define NOISY 0

void
compute_open_hypercube_generators(unsigned int dimension) {
  NNC_Polyhedron hypercube(dimension);
  for (unsigned int i = 0; i < dimension; ++i) {
    Variable x(i);
    hypercube.add_constraint(x > 0);
    hypercube.add_constraint(x < 1);
  }
  (void) hypercube.generators();
}

void
limit_virtual_memory(unsigned int bytes) {
  struct rlimit t;
  if (getrlimit(RLIMIT_AS, &t) != 0) {
    cerr << "getrlimit failed: " << strerror(errno) << endl;
    exit(1);
  }
  t.rlim_cur = bytes;
  if (setrlimit(RLIMIT_AS, &t) != 0) {
    cerr << "setrlimit failed: " << strerror(errno) << endl;
    exit(1);
  }
}

bool
guarded_compute_open_hypercube_generators(unsigned int dimension,
					  unsigned int max_memory_in_bytes) {
  try {
    limit_virtual_memory(max_memory_in_bytes);
    compute_open_hypercube_generators(dimension);
    return true;
  }
  catch (const std::bad_alloc&) {
#if NOISY
    cout << "out of virtual memory" << endl;
#endif
    return false;
  }
  catch (...) {
    exit(1);
  }
}

#if GMP_SUPPORTS_EXCEPTIONS
static void*
cxx_malloc(size_t size) {
  return ::operator new(size);
}

static void*
cxx_realloc(void* p, size_t old_size, size_t new_size) {
  if (new_size <= old_size)
    return p;
  else {
    void* new_p = ::operator new(new_size);
    memcpy(new_p, p, old_size);
    ::operator delete(p);
    return new_p;
  }
}

static void
cxx_free(void* p, size_t) {
  ::operator delete(p);
}
#endif

#define INIT_MEMORY 3*1024*1024

int
main() {
#if GMP_SUPPORTS_EXCEPTIONS
  mp_set_memory_functions(cxx_malloc, cxx_realloc, cxx_free);
#endif

  set_handlers();

  // Find a dimension that cannot be computed with INIT_MEMORY bytes.
  unsigned int dimension = 0;
  do {
    ++dimension;
#if NOISY
    cout << "Trying dimension " << dimension << endl;
#endif
  }
  while (guarded_compute_open_hypercube_generators(dimension, INIT_MEMORY));

  // Now find an upper bound to the memory necessary to compute it.
  unsigned int upper_bound = INIT_MEMORY;
  do {
    upper_bound *= 2;
#if NOISY
    cout << "Trying upper bound " << upper_bound << endl;
#endif
  }
  while (!guarded_compute_open_hypercube_generators(dimension, upper_bound));

  // Search the "exact" amount of memory.
  int lower_bound = upper_bound/2;
  do {
    int test_memory = (lower_bound+upper_bound)/2;
#if NOISY
    cout << "Probing " << test_memory << endl;
#endif
    if (guarded_compute_open_hypercube_generators(dimension, test_memory))
      upper_bound = test_memory;
    else
      lower_bound = test_memory;
  } while (upper_bound-lower_bound > 1024);

#if NOISY
  cout << "Estimated memory for dimension " << dimension
       << ": " << (lower_bound+upper_bound)/2 << " bytes" << endl;
#endif

  return 0;
}
