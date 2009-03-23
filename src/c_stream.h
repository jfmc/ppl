#include <stdio.h>

enum ppl_io_format_line_type {
  PPL_IO_FORMAT_LINE_FIRST = 0,
  PPL_IO_FORMAT_LINE_FIRSTLAST,
  PPL_IO_FORMAT_LINE_NEXT,
  PPL_IO_FORMAT_LINE_LAST,
  PPL_IO_FORMAT_LINE_FORCED_FIRST,
  PPL_IO_FORMAT_LINE_FORCED_NEXT,
  PPL_IO_FORMAT_LINE_UNWRAPPED_FIRSTLAST,
  PPL_IO_FORMAT_LINE_UNWRAPPED_NEXT,
  PPL_IO_FORMAT_LINE_UNTERMINATED_FIRST,
  PPL_IO_FORMAT_LINE_UNTERMINATED_NEXT,
  PPL_IO_FORMAT_LINE_END
};
#define PPL_IO_FORMAT_ALIGN_LEFT 0
#define PPL_IO_FORMAT_ALIGN_CENTER 8
#define PPL_IO_FORMAT_ALIGN_RIGHT 16
#define PPL_IO_FORMAT_WRAP_CHARS_SIZE 2

struct ppl_io_format_settings {
  // Char conversion table
  const char *tr_in;
  const char *tr_out;
  // String for detect end of paragraph
  const char *paragraph_end;
  // Lines can be wrapped on any of this characters
  // If none from wrap_at[n] is found then wrap_at[n+1] is used.
  const char *wrap_chars[PPL_IO_FORMAT_WRAP_CHARS_SIZE];
  // Any of these characters are stripped at and after wrap point
  const char *strip_wrap;
  // This string is put at beginning of paragraph
  const char *top;
  // This string is put at end of paragraph
  const char *bottom;
  struct {
    // Length of line
    unsigned int length;
    // Left margin string
    const char *left;
    // Right margin string
    const char *right;
    // 0 left, 8 center, 16 right
    unsigned int alignment;
    // This char is used to fill line length
    char fill_char;
  } lines[PPL_IO_FORMAT_LINE_END];
};

extern struct ppl_io_format_settings ppl_io_format_default_settings;

struct ppl_io_ostream* ppl_io_ostream_stdio_new(FILE* fp);
struct ppl_io_ostream* ppl_io_ostream_buffer_new();
struct ppl_io_ostream* ppl_io_ostream_format_new(struct ppl_io_ostream* stream, struct ppl_io_format_settings* settings);
void ppl_io_ostream_format_replace_settings(struct ppl_io_ostream* stream, struct ppl_io_format_settings* settings);

void ppl_io_ostream_delete(struct ppl_io_ostream* stream);
size_t ppl_io_ostream_buffer_get(struct ppl_io_ostream* stream, const char** buf);

int ppl_io_write_endl(struct ppl_io_ostream* s);

// FIXME:
// Add ios_base methods: flags, setf, unsetf, width, precision (others?)

#define DECLARE_WRITE_VAL(name, type) int ppl_io_write_##name(struct ppl_io_ostream* s, const type o)
#define DECLARE_WRITE_REF(name, type) int ppl_io_write_##name(struct ppl_io_ostream* s, const type* o)

DECLARE_WRITE_VAL(char, char);
DECLARE_WRITE_VAL(int, int);
DECLARE_WRITE_VAL(double, double);
DECLARE_WRITE_VAL(string, char*);
