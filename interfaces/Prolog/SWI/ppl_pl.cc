#include <SWI-Prolog.h>

extern install_t install();

int
main(int argc, char **argv) {
  install();
  if (!PL_initialise(argc, argv))
    PL_halt(1);
  PL_install_readline();
  PL_halt(PL_toplevel() ? 0 : 1);
}
