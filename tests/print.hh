#ifndef _print_hh
#define _print_hh 1

#include <iostream>
#include <string>
#include "ppl.hh"

using namespace std;
using namespace Parma_Polyhedra_Library;

inline bool
easy_print(const Polyhedron& ph,
	   const string& intro = "",
	   ostream& s = cout) {
  if (!intro.empty())
    s << intro << endl;
  if (ph.is_zero_dim()) {
    s << "true" << endl;
    return true;
  }
  else if (ph.check_empty()) {
    s << "false" << endl;
    return true;
  }
  return false;
}

inline void
print_constraints(const Polyhedron& ph,
		  const string& intro = "",
		  ostream& s = cout) {
  if (!easy_print(ph, intro, s)) {
    const ConSys& cs = ph.constraints();
    ConSys::const_iterator i = cs.begin();
    ConSys::const_iterator cs_end = cs.end();
    while (i != cs_end) {
      s << *i++;
      if (i != cs_end)
	s << "," << endl;
    }
    s << "." << endl;
  }
}

inline void
print_generators(const Polyhedron& ph,
		 const string& intro = "",
		 ostream& s = cout) {
  if (!easy_print(ph, intro, s)) {
    const GenSys& gs = ph.generators();
    GenSys::const_iterator i = gs.begin();
    GenSys::const_iterator gs_end = gs.end();
    while (i != gs_end) {
      s << *i++;
      if (i != gs_end)
	s << "," << endl;
    }
    s << "." << endl;
  }
}
#endif
