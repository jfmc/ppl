#include "ppl_install.hh"
#include "print.hh"
#include "ehandlers.hh"
#include <sstream>

using namespace std;
using namespace Parma_Polyhedra_Library;

int main() {
  set_handlers();

  istringstream s("space_dim 9\n"
		  "-ZE -EM  -CM -GM  -CS +GS  -SC -SG \n"
		  "con_sys (not_up-to-date)\n"
		  "0 x 0 (not_sorted)\n"
		  "\n"
		  "gen_sys (up-to-date)\n"
		  "8 x 10 (not_sorted)\n"
		  "0 0 0 0 0 0 0 0 0 1   L\n"
		  "0 0 0 0 0 0 0 0 1 0   L\n"
		  "0 0 0 0 0 0 0 1 0 0   L\n"
		  "0 0 0 0 1 0 1 0 0 0   R\n"
		  "0 0 0 1 0 1 0 0 0 0   R\n"
		  "1 0 0 0 0 0 0 0 0 0   V\n"
		  "0 0 1 0 0 0 0 0 0 0   R\n"
		  "0 1 0 0 0 1 1 0 0 0   R\n"
		  "\n"
		  "sat_c\n"
		  "0 x 0\n"
		  "\n"
		  "sat_g\n"
		  "0 x 0\n"
		  "\n"
		  "\n"
		  "2 x 10 (not_sorted)\n"
		  "-3 0 -1 0 0 -1 -1 0 1 0   =\n"
		  "-3 0 -1 -1 -1 0 0 0 0 1   =\n");

  Polyhedron ph;
  s >> ph;

  ConSys cs;
  s >> cs;

  ph.add_constraints(cs);

  return 0;
}

