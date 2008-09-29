
#include "ppl_c.h"

/*
  Returns a buffer allocated with malloc() containing a printable
  representation of the PPL object referenced by `p', where each
  newline is followed by `n' blank spaces.
*/
#define DECLARE_PRINT_TO_BUFFER(Type)                                   \
char* print_ppl_##Type##_t_to_buffer(ppl_##Type##_t p,                  \
                                     unsigned indent_depth,             \
                                     unsigned preferred_first_line_length, \
                                     unsigned preferred_line_length);

#ifdef __cplusplus
extern "C" {
#endif

DECLARE_PRINT_TO_BUFFER(Coefficient);

DECLARE_PRINT_TO_BUFFER(Linear_Expression);

DECLARE_PRINT_TO_BUFFER(Constraint);

DECLARE_PRINT_TO_BUFFER(Constraint_System);

DECLARE_PRINT_TO_BUFFER(Constraint_System_const_iterator);

DECLARE_PRINT_TO_BUFFER(Generator);

DECLARE_PRINT_TO_BUFFER(Generator_System);

DECLARE_PRINT_TO_BUFFER(Generator_System_const_iterator);

DECLARE_PRINT_TO_BUFFER(Congruence);

DECLARE_PRINT_TO_BUFFER(Congruence_System);

DECLARE_PRINT_TO_BUFFER(Grid_Generator);

DECLARE_PRINT_TO_BUFFER(Grid_Generator_System);

DECLARE_PRINT_TO_BUFFER(MIP_Problem);

#ifdef __cplusplus
} /* extern "C" */
#endif

#undef DECLARE_PRINT_TO_BUFFER
