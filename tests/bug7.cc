#include <config.h>

#include "ppl_install.hh"
#include <iostream>
#include <fstream>

using namespace std;
using namespace Parma_Polyhedra_Library;

int
main() {
  ifstream s("bug7.dat");
  //ifstream s("bug8.dat");
  if (!s) {
    cerr << "Cannot open data file!!!" << endl;
    exit(1);
  }

  Polyhedron x;

  s >> x;
  //  cout << "*** x *** " << endl << x << endl;
  assert(x.OK());
  
  // Now see the program explode in unexpected and interesting ways.
  x.check_empty();
  assert(x.OK());

  // cout << "--- x ---" << endl << x << endl;
}
