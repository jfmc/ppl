#include <cstdio>
#include <cstring>
#include <sstream>
#include "ppl.hh"
#include "ppl_c_stream.defs.hh"

c_streambuf_format_settings ppl_io_format_default_settings = {
  0,            // tr_in
  0,            // tr_out
  "\n",         // paragraph_end
  {             // wrap points
    { 0, "," }, // before, after
    { " ", 0 }  // before, aftet
  },
  " ",          // strip_wrap
  0,           // top
  0,           // bottom
  {
    // length, left, left_n, left_c, right_n, right_c, right, alignment, fill_char
    { 80, 0, 0, 0, 0, 0, "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, // FIRST
    { 80, 0, 0, 0, 0, 0, "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, // FIRSTLAST
    { 80, 0, 0, 0, 0, 0, "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, // NEXT
    { 80, 0, 0, 0, 0, 0, "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, // LAST
    { 80, 0, 0, 0, 0, 0, "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, // CHOPPED_FIRST
    { 80, 0, 0, 0, 0, 0, "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, // CHOPPED_NEXT
    { 80, 0, 0, 0, 0, 0, "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, // LONGER_FIRST
    { 80, 0, 0, 0, 0, 0, "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, // LONGER_FIRSTLAST
    { 80, 0, 0, 0, 0, 0, "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, // LONGER_NEXT
    { 80, 0, 0, 0, 0, 0, "\n", PPL_IO_FORMAT_ALIGN_LEFT, 0 }, // LONGER_LAST
    { 80, 0, 0, 0, 0, 0, 0,    PPL_IO_FORMAT_ALIGN_LEFT, 0 }, // UNTERMINATED_FIRST
    { 80, 0, 0, 0, 0, 0, 0,    PPL_IO_FORMAT_ALIGN_LEFT, 0 }  // UNTERMINATED_NEXT
  }
};

#define DEFINE_WRITE_VAL(name, type)		\
int ppl_io_write_##name(ppl_io_ostream* s, const type o) {	\
  (*s->stream) << o;				\
  return *s->stream ? 0 : -1;			\
}

#define DEFINE_WRITE_REF(name, type)		       \
int ppl_io_write_##name(ppl_io_ostream* s, const type* o) {  \
  (*s->stream) << *o;				       \
  return *s->stream ? 0 : -1;			       \
}

extern "C" {

using namespace Parma_Polyhedra_Library;

ppl_io_ostream* ppl_io_ostream_stdio_new(FILE *fp) {
  stdiobuf* buf = new stdiobuf(fp);
  return new ppl_io_ostream(buf);
}

ppl_io_ostream* ppl_io_ostream_format_new(ppl_io_ostream* stream, c_streambuf_format_settings* settings) {
  c_streambuf_format* buf = new c_streambuf_format(*stream->stream, settings);
  return new ppl_io_ostream(buf);
}

void ppl_io_ostream_format_replace_settings(ppl_io_ostream* stream, c_streambuf_format_settings* settings) {
  static_cast<c_streambuf_format*>(stream->sbuf)->replace_settings(settings);
}

size_t ppl_io_ostream_buffer_get(ppl_io_ostream* s, char **buf) {
  std::ostringstream* ss = static_cast<std::ostringstream*>(s->stream);
  std::string str = ss->str();
  *buf = strdup(str.c_str());
  return str.size();
}

void ppl_io_ostream_buffer_clear(ppl_io_ostream* s) {
  std::ostringstream* ss = static_cast<std::ostringstream*>(s->stream);
  ss->str("");
}

ppl_io_ostream* ppl_io_ostream_buffer_new() {
  return new ppl_io_ostream(new std::ostringstream());
}

void ppl_io_ostream_delete(ppl_io_ostream* s) {
  delete s;
}

int ppl_io_write_endl(ppl_io_ostream* s) {
  (*s->stream) << std::endl;
  return *s->stream ? 0 : -1;			       \
}

DEFINE_WRITE_VAL(char, char);
DEFINE_WRITE_VAL(signed_char, signed char);
DEFINE_WRITE_VAL(unsigned_char, unsigned char);
DEFINE_WRITE_VAL(short, short);
DEFINE_WRITE_VAL(unsigned_short, unsigned short);
DEFINE_WRITE_VAL(int, int);
DEFINE_WRITE_VAL(unsigned_int, unsigned int);
DEFINE_WRITE_VAL(long, long);
DEFINE_WRITE_VAL(unsigned_long, unsigned long);
DEFINE_WRITE_VAL(long_long, long long);
DEFINE_WRITE_VAL(unsigned_long_long, unsigned long long);
DEFINE_WRITE_VAL(float, float);
DEFINE_WRITE_VAL(double, double);
DEFINE_WRITE_VAL(long_double, long double);
DEFINE_WRITE_VAL(string, char*);

}

