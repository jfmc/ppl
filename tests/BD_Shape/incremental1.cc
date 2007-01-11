#include "ppl_test.hh"

#include "timings.hh"
#include "files.hh"

using namespace std;

// dim = 600 per i double; dim = 200 per gli mpz_class;
// dim = 200 per gli mpq_class.
const dimension_type dim = 200;
const char* fname = "closure1.dat";

bool
generate_inf(double p) {
  double q = rand() / static_cast<double>(RAND_MAX);
  return q < p;
}


template <typename INST>
void
generate_dat_file() {
  std::cerr << "Generating DAT file ... ";
  BD_Shape<INST> bd(dim, UNIVERSE);

#if 1
  // Percentage controlling generation of plus infinities.
  const double p = 0.9;
  //  const double p = 0.7;
  //  const double p = 0.5;
  //  const double p = 0.3;
  //  const double p = 0.1;
  for (dimension_type i = 0; i < dim; ++i) {
    Variable x(i);
    for (dimension_type j = 0; j < dim; ++j) {
      Variable y(j);
      if (!generate_inf(p))
	bd.add_constraint( x - y <= rand());
      if (!generate_inf(p))
        bd.add_constraint(-x + y <= rand());
    }
  }
#else
  for (dimension_type i = 0; i < dim; ++i)
    bd.add_constraint(Variable(i) == i);
#endif

  fstream f;
  open(f, fname, ios_base::out);
  bd.ascii_dump(f);
  close(f);
  std::cerr << " done.\n\n";
 }


template <typename INST>
int
incremental_shortest_path_closure() {
  BD_Shape<INST> bd(dim, UNIVERSE);

  fstream f;
  open(f, fname, ios_base::in);
  bd.ascii_load(f);
  close(f);

#if 1
  std::cerr << "Clock started ... ";
  start_clock();
  bool empty = bd.is_empty();
  std::cout << "Stopping clock: " << std::endl;;
  print_clock(std::cerr);
  std::cerr << std::endl;
  if (bd.is_empty())
    std::cerr << "BD is empty." << std::endl;
#endif

#if 1
  std::cerr << "Clock started ... ";
  start_clock(); 
  // Le iterazioni sono 120 per i double, 30 per gli mpz_class,
  // 40 per gli mpq_class.
  for (dimension_type i = 0; i < 40; ++i)
    bd.add_constraint_and_minimize(Variable(dim - i - 5) <= 0);

  std::cout << "Stopping clock: " << std::endl;
  print_clock(std::cerr);
  std::cerr << std::endl;

  if (bd.is_empty())
    std::cerr << "bd is empty." << std::endl;
#endif

#if 0
  open(f, "closure1-out.dat", ios_base::out);
  bd.ascii_dump(f);
  close(f);
#endif

  return 0;
}


int
main() {

#if 1
  generate_dat_file<mpq_class>();
  //  generate_dat_file<double>();
  //  generate_dat_file<mpz_class>();
#endif

#if 0
  std::cerr << "\nTesting int16_t:\n";
  incremental_shortest_path_closure<int16_t>();

  std::cerr << "\nTesting int32_t:\n";
  incremental_shortest_path_closure<int32_t>();

  std::cerr << "\nTesting int64_t:\n";
  incremental_shortest_path_closure<int64_t>();

  std::cerr << "\nTesting mpz_class:\n";
  incremental_shortest_path_closure<mpz_class>();
#endif

#if 0
  std::cerr << "\nTesting float:\n";
  incremental_shortest_path_closure<float>();

  std::cerr << "\nTesting double:\n";
  incremental_shortest_path_closure<double>();

  std::cerr << "\nTesting long double:\n";
  incremental_shortest_path_closure<long double>();
#endif

  std::cerr << "\nTesting mpq_class:\n";
  incremental_shortest_path_closure<mpq_class>();

  return 0;
}
