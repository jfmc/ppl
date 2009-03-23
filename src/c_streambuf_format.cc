#include <cstring>
#include "c_streambuf_format.defs.hh"

namespace Parma_Polyhedra_Library {

size_t c_streambuf_format::cb_write(const char *buf, size_t size) {
  if (!settings)
    return stream.write(buf, size) ? size : 0;
  if (settings->tr_in) {
    int tr_out_len = strlen(settings->tr_out);
    for (unsigned int i = 0; i < size; ++i) {
      char c = buf[i];
      char *p = strchr(settings->tr_in, c);
      if (p) {
	if (p - settings->tr_in >= tr_out_len)
	  continue;
	c = settings->tr_out[p - settings->tr_in];
      }
      str.push_back(c);
    }
  }
  else
    str.append(buf, size);
  const char *rest = str.c_str();
  unsigned int len = str.size();
  while (1) {
    ppl_io_format_line_type type;
    char *parend = strstr(rest, settings->paragraph_end);
    unsigned int n;
    if (parend) {
      n = parend - rest;
      if (n <= settings->lines[PPL_IO_FORMAT_LINE_LAST].length) {
	type = first ? PPL_IO_FORMAT_LINE_FIRSTLAST : PPL_IO_FORMAT_LINE_LAST;
      endpar:
	if (!output_line(rest, n, type))
	  return 0;
	unsigned int l = strlen(settings->paragraph_end);
	rest = parend + l;
	len -= n + l;
	continue;
      }
    }
    unsigned int line_length = settings->lines[first ? PPL_IO_FORMAT_LINE_FIRST : PPL_IO_FORMAT_LINE_NEXT].length;
    if (len < line_length)
      break;
    unsigned int i;
    const char* wrap;
    for (i = 0; i < PPL_IO_FORMAT_WRAP_CHARS_SIZE; ++i) {
      wrap = rest + line_length;
      while (--wrap >= rest) {
	if (strchr(settings->wrap_chars[i], *wrap))
	  break;
      }
      if (wrap >= rest)
	break;
    }
    if (i < PPL_IO_FORMAT_WRAP_CHARS_SIZE) {
      if (!strchr(settings->strip_wrap, *wrap))
	++wrap;
      n = wrap - rest;
      if (n > 0) {
	if (!output_line(rest, n, first ? PPL_IO_FORMAT_LINE_FIRST : PPL_IO_FORMAT_LINE_NEXT))
	  return 0;
	rest = wrap;
	len -= n;
	while (len > 0 && strchr(settings->strip_wrap, *rest)) {
	  ++rest;
	  --len;
	}
	continue;
      }
    }
    type = first ? PPL_IO_FORMAT_LINE_FORCED_FIRST : PPL_IO_FORMAT_LINE_FORCED_NEXT;
    n = settings->lines[type].length;
    if (len >= n) {
      if (!output_line(rest, n, type))
	return 0;
      rest += n;
      len -= n;
    }
    else {
      if (parend) {
	type = first ? PPL_IO_FORMAT_LINE_UNWRAPPED_FIRSTLAST : PPL_IO_FORMAT_LINE_UNWRAPPED_NEXT;
	goto endpar;
      }
      break;
    }
  }
  str.assign(rest, len);
  return size;
}

int c_streambuf_format::cb_flush() {
  unsigned int n = str.size();
  if (n > 0) {
    if (!output_line(str.c_str(), n, first ? PPL_IO_FORMAT_LINE_UNTERMINATED_FIRST : PPL_IO_FORMAT_LINE_UNTERMINATED_NEXT))
      return -1;
  }
  return 0;
}

bool c_streambuf_format::output_line(const char *s, unsigned int n, ppl_io_format_line_type type) {
  switch (type) {
  case PPL_IO_FORMAT_LINE_FIRST:
  case PPL_IO_FORMAT_LINE_FIRSTLAST:
  case PPL_IO_FORMAT_LINE_FORCED_FIRST:
  case PPL_IO_FORMAT_LINE_UNWRAPPED_FIRSTLAST:
  case PPL_IO_FORMAT_LINE_UNTERMINATED_FIRST:
    stream << settings->top;
    break;
  default:
    break;
  }
  stream << settings->lines[type].left;
  if (settings->lines[type].fill_char && n < settings->lines[type].length) {
    unsigned int left = 0;
    unsigned int right = 0;
    unsigned int f = settings->lines[type].length - n;
    left = f * settings->lines[type].alignment / PPL_IO_FORMAT_ALIGN_RIGHT;
    right = f - left;
    unsigned int i;
    for (i = 0; i < left; ++i)
      stream.put(settings->lines[type].fill_char);
    stream.write(s, n);
    for (i = 0; i < right; ++i)
      stream.put(settings->lines[type].fill_char);
  }
  else
    stream.write(s, n);
  stream << settings->lines[type].right;
  switch (type) {
  case PPL_IO_FORMAT_LINE_FIRSTLAST:
  case PPL_IO_FORMAT_LINE_LAST:
  case PPL_IO_FORMAT_LINE_UNWRAPPED_FIRSTLAST:
    stream << settings->bottom;
    first = true;
    break;
  default:
    first = false;
  }
  return stream;
}

}
