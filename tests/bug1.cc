#include <config.h>

#include "ppl_test.hh"
#include <iostream>
#include <fstream>

using namespace std;
using namespace Parma_Polyhedra_Library;

int
main() {
  ifstream s("bug1.dat");
  if (!s) {
    cerr << "Cannot open data file!!!" << endl;
    exit(1);
  }

  C_Polyhedron x;
  C_Polyhedron y;

  x.ascii_load(s);
  y.ascii_load(s);

  assert(x.OK());
  assert(y.OK());

  // Now see the program explode in unexpected and interesting ways.
  x.concatenate_assign(y);

}
