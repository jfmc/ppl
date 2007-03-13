#include "ppl_test.hh"

#include "timings.hh"
#include "files.hh"
#include "stdio.h"
#include "string.h"

using namespace std;

// dim = 400 per i double; dim = 200 per gli mpz_class;
// dim = 100 per gli mpq_class.
const int max_rand = 1000;
dimension_type dim = 100;
unsigned int default_inst = 0;
//char* fname = "closure1.dat";
char fname[80];

void set_defaults(int type) {
  switch (type) {
    // double
  case 0: { 
    dim = 400;
    strcat(fname,".double.");
    strcat(fname,"universal");
    break;
  }
    // mpq_class
  case 1: { 
    dim = 100;
    strcat(fname,".mpq_class.");
    strcat(fname,"universal");
    break;
  }
  }
}

template <typename INST>
void
generate_dat_file() {
//   std::cout << "Generating DAT file with percentage " << p << " ... ";
  Octagonal_Shape<INST> oct(dim, UNIVERSE);

  fstream f;
  open(f, fname, ios_base::out);
  oct.ascii_dump(f);
  close(f);
//   std::cout << " done.\n\n";
 }


template <typename INST>
int
universal_strong_closure() {
  Octagonal_Shape<INST> oct(dim, UNIVERSE);

  fstream f;
  open(f, fname, ios_base::in);
  oct.ascii_load(f);
  close(f);

  start_clock();
  oct.add_constraint(Variable(0) >= 5);
  bool empty = oct.is_empty();
  std::cout << "Universal_strong: ";
  print_clock(std::cout);
  std::cout << std::endl;
  if (empty)
    std::cerr << "Oct is empty." << std::endl;

  return 0;
}


int
main(int argc, char* argv[]) {
  strcpy(fname,"closure1.dat");
  if (argc < 2 || argc > 3) {
    std::cerr << "Usage: " << argv[0] << " numtype [generate]" 
	      << std::endl
	      << "Example: " << argv[0] << " double generate" << std::endl
	      << "Example: " << argv[0] << " mpq_class" << std::endl;
    return 1;
  }
  if  (strcmp(argv[1],"mpq_class")==0)
    default_inst = 1;
  else if  (strcmp(argv[1],"double")==0)
    default_inst = 0;
  else {
    std::cerr << "Invalid numerical type " << argv[1] << std::endl;
    return 1;
  }
  set_defaults(default_inst);
  if (argc>=3 &&   strcmp(argv[2],"generate")==0) {
    switch(default_inst) {
    case 0: {
      generate_dat_file<double>();
      break;
    }
    case 1: {
      generate_dat_file<mpq_class>();
      break;
    }
    
    }
  }

  switch(default_inst) {
  case 0: {
    //    std::cerr << "double\n";
    universal_strong_closure<double>();
    break;
  }
  case 1: {
    //    std::cerr << "mpq_class\n";
    universal_strong_closure<mpq_class>();
    break;
  }
    
  }

  return 0;
}
