
#include "ppl_install.hh"
#include <sicstus/sicstus.h>
#include <exception>

//int SP_put_address(SP_term_ref t, void *pointer) 
//void SP_raise_exception(SP_term_ref t)

namespace PPL = Parma_Polyhedra_Library;

static void
handle_exception() {
}

static void
handle_exception(const std::exception& /* e */) {
}

#define CATCH_ALL \
  catch (const std::exception& e) { \
    handle_exception(e); \
  } \
  catch (...) { \
    handle_exception(); \
  } \
  abort();  // This is only to silence a warning.

static SP_atom a_plus;
static SP_atom a_minus;
static SP_atom a_asterisk;
static SP_atom a_equal_equal;
static SP_atom a_greater_than_equal;
static SP_atom a_equal_less_than;

extern "C"
void
ppl_init(int /* when */) {
  struct {
    SP_atom* p_SP_atom;
    const char* name;
  } atom[] = {
    { &a_plus,               "+" },
    { &a_minus,              "-" },
    { &a_asterisk,           "*" },

    { &a_equal_equal,        "==" },
    { &a_greater_than_equal, ">=" },
    { &a_equal_less_than,    "=<" },
  };

  for (size_t i = 0; i < sizeof(atom)/sizeof(atom[0]); ++i) {
    *atom[i].p_SP_atom = SP_atom_from_string(atom[i].name);
  }
}

extern "C"
void
ppl_deinit(int /* when */) {
}

extern "C"
void*
ppl_new_polyhedron() {
  try {
    return new PPL::Polyhedron();
  }
  CATCH_ALL;
}

extern "C"
void
ppl_delete_polyhedron(void* pp) {
  // If destructors throw it is a catastrophy.
  // Anyway...
  try {
    delete static_cast<PPL::Polyhedron*>(pp);
  }
  CATCH_ALL;
}

#if 0
extern "C"
void
ppl_insert(void* pp, SP_term_) {
  // If destructors throw it is a catastrophy.
  // Anyway...
  try {
    delete static_cast<PPL::Polyhedron*>(pp);
  }
  CATCH_ALL;
}
#endif
