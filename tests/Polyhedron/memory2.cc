/* Test the allocation error recovery facility of the library.
   Copyright (C) 2001-2004 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "ppl_test.hh"
#include <new>

using namespace std;
using namespace Parma_Polyhedra_Library;

#ifndef NOISY
#define NOISY 0
#endif

// If GMP does not support exceptions the test is pointless.
#if !GMP_SUPPORTS_EXCEPTIONS

int
main() TRY {
  return 0;
}
CATCH

#else // GMP_SUPPORTS_EXCEPTIONS

namespace {

unsigned allocated = 0;
unsigned deallocated = 0;
unsigned threshold = 0;

void
reset_allocators(unsigned new_threshold) {
  allocated = deallocated = 0;
  threshold = new_threshold;
}

extern "C" void*
cxx_malloc(size_t size) {
  if (allocated > threshold) {
#if NOISY
    cout << "std::bad_alloc thrown" << endl;
#endif
    throw std::bad_alloc();
  }
  void* p = ::operator new(size);
#if NOISY
  cout << "allocated " << size << " @ " << p << endl;
#endif
  ++allocated;
  return p;
}

extern "C" void*
cxx_realloc(void* p, size_t old_size, size_t new_size) {
  if (new_size <= old_size) {
#if NOISY
  cout << "reallocated " << old_size << " @ " << p
       << " to " << new_size << " @ " << p
       << endl;
#endif
    return p;
  }
  else {
    if (allocated > threshold) {
#if NOISY
      cout << "std::bad_alloc thrown" << endl;
#endif
      throw std::bad_alloc();
    }
    void* new_p = ::operator new(new_size);
    memcpy(new_p, p, old_size);
    ::operator delete(p);
#if NOISY
    cout << "reallocated " << old_size << " @ " << p
	 << " to " << new_size << " @ " << new_p
	 << endl;
#endif
    ++allocated;
    ++deallocated;
    return new_p;
  }
}

extern "C" void
cxx_free(void* p, size_t) {
#if NOISY
  cout << "deallocated " << p << endl;
#endif
  ::operator delete(p);
  ++deallocated;
}

void
test1() {
  reset_allocators(7);
  try {
    Matrix* matrix = new Matrix(2, 5);
    // We should never get here.
    delete matrix;
    exit(1);
  }
  catch (const std::bad_alloc&) {
#if NOISY
    cout << "std::bad_alloc caught" << endl;
#endif
  }

  if (allocated != deallocated)
    exit(1);
}

} // namespace


int
main() TRY {
  mp_set_memory_functions(cxx_malloc, cxx_realloc, cxx_free);

  set_handlers();

  test1();

  return 0;
}
CATCH

#endif // GMP_SUPPORTS_EXCEPTIONS
