#include "ppl_test.hh"

#include "timings.hh"
#include "files.hh"
#include "stdio.h"
#include "string.h"

using namespace std;

// dim = 400 per i double; dim = 200 per gli mpz_class;
// dim = 100 per gli mpq_class.
const int max = 1000;
dimension_type dim = 100;
unsigned int num_iterations = 20;
unsigned int default_inst = 0;
double percentage = 0;
//char* fname = "closure1.dat";
char fname[80];
char* percentage_argv;

void set_defaults(int type) {
  switch (type) {
    // double
  case 0: { 
    dim = 400;
    num_iterations = 80;
    strcat(fname,".double.");
    strcat(fname,percentage_argv);
    break;
  }
    // mpq_class
  case 1: { 
    dim = 100;
    num_iterations = 20;
    //   strcat(fname,"mpq_class");
    break;
  }
  }
}

bool
generate_inf(double p) {
  double q = rand() / static_cast<double>(RAND_MAX);
  return q < p;
}

int
my_rand(int max) {
  double q = rand() / static_cast<double>(RAND_MAX);
  return ceil(q * max);
}


template <typename INST>
void
generate_dat_file(const double p) {
  std::cout << "Generating DAT file with percentage " << p << " ... ";
  Octagonal_Shape<INST> oct(dim, UNIVERSE);

#if 1
  // Percentage controlling generation of plus infinities.
  // const double p = percentage / 100;
  //  const double p = 0.9;
  //  const double p = 0.7;
  //  const double p = 0.5;
  //  const double p = 0.3;
  //  const double p = 0.1;
  for (dimension_type i = 0; i < dim; ++i) {
    Variable x(i);
    for (dimension_type j = 0; j < dim; ++j) {
      Variable y(j);
      if (!generate_inf(p))
	oct.add_constraint( x + y <= my_rand(max));
      if (!generate_inf(p))
	oct.add_constraint( x - y <= my_rand(max));
      if (!generate_inf(p))
        oct.add_constraint(-x + y <= my_rand(max));
      if (!generate_inf(p))
        oct.add_constraint(-x - y <= my_rand(max));
    }
  }
#else
  for (dimension_type i = 0; i < dim; ++i)
    oct.add_constraint(Variable(i) == i);
#endif

  fstream f;
  open(f, fname, ios_base::out);
  oct.ascii_dump(f);
  close(f);
  std::cout << " done.\n\n";
 }


template <typename INST>
int
incremental_strong_closure() {
  Octagonal_Shape<INST> oct(dim, UNIVERSE);

  fstream f;
  open(f, fname, ios_base::in);
  oct.ascii_load(f);
  close(f);

#if 1
  std::cout << "Clock started ... ";
  start_clock();
  bool empty = oct.is_empty();
  std::cout << "Stopping clock for strong closure: " << std::endl;;
  std::cout << "Strong: ";
  print_clock(std::cout);
  std::cout << std::endl;
  if (oct.is_empty())
    std::cerr << "Oct is empty." << std::endl;
#endif

  std::cout << "Clock started ... ";
  start_clock();
  // Le iterazioni sono 80 per i double, 30 per gli mpz_class,
  // 20 per gli mpq_class.
  for (dimension_type i = 0; i < num_iterations; ++i)
    oct.add_constraint_and_minimize(Variable(dim - i - 5) <= 0);

  std::cout << "Stopping clock for incremental closure: " << std::endl;
  std::cout << "Incremental: ";
  print_clock(std::cout);
  std::cout << std::endl;

  if (oct.is_empty())
    std::cout << "Oct is empty." << std::endl;

#if 0
  open(f, "closure1-out.dat", ios_base::out);
  oct.ascii_dump(f);
  close(f);
#endif

  return 0;
}


int
main(int argc, char* argv[]) {
  strcpy(fname,"closure1.dat");
  if (argc < 2 || argc > 4) {
    std::cerr << "Usage: " << argv[0] << " percentage numtype [generate]" 
	      << std::endl
	      << "Example: " << argv[0] << " 90 double generate" << std::endl
	      << "Example: " << argv[0] << " 50 mpq_class" << std::endl;
    return 1;
  }
  percentage_argv = argv[1];
  if  (strcmp(argv[2],"mpq_class")==0)
    default_inst = 1;
  else if  (strcmp(argv[2],"double")==0)
    default_inst = 0;
  else {
    std::cerr << "Invalid numerical type " << argv[2] << std::endl;
    return 1;
  }
  percentage = atof(argv[1]) / 100;
  set_defaults(default_inst);
  if (argc>=4 &&   strcmp(argv[3],"generate")==0) {
    switch(default_inst) {
    case 0: {
      generate_dat_file<double>(percentage);
      break;
    }
    case 1: {
      generate_dat_file<mpq_class>(percentage);
      break;
    }
    
    }
    // generate_dat_file<double>(percentage);
    //       generate_dat_file<mpq_class>(percentage);
    //  generate_dat_file<mpz_class>(percentage);
  }

#if 0
  std::cout << "\nTesting int16_t:\n";
  incremental_strong_closure<int16_t>();

  std::cout << "\nTesting int32_t:\n";
  incremental_strong_closure<int32_t>();

  std::cout << "\nTesting int64_t:\n";
  incremental_strong_closure<int64_t>();

  std::cout << "\nTesting double:\n";
  incremental_strong_closure<double>();

  std::cout << "\nTesting mpz_class:\n";
  incremental_strong_closure<mpz_class>();

  std::cout << "\nTesting float:\n";
  incremental_strong_closure<float>();

  std::cout << "\nTesting long double:\n";
  incremental_strong_closure<long double>();
#endif

  //  std::cerr << "Test parameters:" << std::endl
  //	    << ALTERNATE_OS_STRONG_CLOSURE_ASSIGN << ":" 
  //	    << ALTERNATE_OS_INCREMENTAL_STRONG_CLOSURE_ASSIGN << ":"
  //	    << percentage_argv << ":";
  //  // Numeric type is printed later.

  switch(default_inst) {
  case 0: {
    //    std::cerr << "double\n";
    incremental_strong_closure<double>();
    break;
  }
  case 1: {
    //    std::cerr << "mpq_class\n";
    incremental_strong_closure<mpq_class>();
    break;
  }
    
  }


  return 0;
}
