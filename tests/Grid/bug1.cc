
#include "ppl_test.hh"

#include <fstream>

int
main() {
  ifstream s(SRCDIR "/bug1.dat");
  if (!s) {
    cerr << "Cannot open data file!!!" << endl;
    exit(1);
  }

  Grid x;
  x.ascii_load(s);

  Constraint_System cs;
  cs.ascii_load(s);

  assert(x.OK());
  assert(cs.OK());

  cerr << "Inputs are OK." << endl;

  // Now see the program explode in unexpected and interesting ways.
  x.add_congruences_and_minimize(cs);

  return 0;
}
