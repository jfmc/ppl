
#include <SWI-Prolog.h>

extern install_t
install();

int
main(int, char** argv) {
  install();

  char* pl_args[2];
  pl_args[0] = argv[0];
  pl_args[1] = 0;
  if (!PL_initialise(1, pl_args))
    PL_halt(1);

  {
    predicate_t pred = PL_predicate("main", 0, "user");
    term_t h0 = PL_new_term_refs(0);
    int ret_val = PL_call_predicate(0, PL_Q_NORMAL, pred, h0);
    PL_halt(ret_val ? 0 : 1);
  }

  return 0;
}

