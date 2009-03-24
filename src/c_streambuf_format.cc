#include <cstring>
#include "c_streambuf_format.defs.hh"

namespace Parma_Polyhedra_Library {

c_streambuf_format::~c_streambuf_format() {
  cb_flush();
}

int c_streambuf_format::wrap_point_before(const char *buf, int pos, int limit) {
  for (unsigned int i = 0; i < PPL_IO_FORMAT_WRAP_POINTS; ++i) {
    for (int p = pos - 1; p >= limit; --p){
      if (settings->wrap_points[i].before
	  && strchr(settings->wrap_points[i].before, buf[p]))
	return p;
      if (settings->wrap_points[i].after
	  && strchr(settings->wrap_points[i].after, buf[p]))
	return p + 1;
    }
  }
  return -1;
}

int c_streambuf_format::wrap_point_after(const char *buf, int pos, int limit) {
  for (int p = pos; p < limit; p++) {
    for (unsigned int i = 0; i < PPL_IO_FORMAT_WRAP_POINTS; ++i) {
      if (settings->wrap_points[i].before
	  && strchr(settings->wrap_points[i].before, buf[p]))
	return p;
      if (settings->wrap_points[i].after
	  && strchr(settings->wrap_points[i].after, buf[p]))
	return p + 1;
    }
  }
  return -1;
}

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
    c_streambuf_format_line_type type;
    char *parend = strstr(rest, settings->paragraph_end);
    unsigned int parend_pos;
    if (parend) {
      parend_pos = parend - rest;
      if (parend_pos <= settings->lines[PPL_IO_FORMAT_LINE_LAST].length) {
	type = first ? PPL_IO_FORMAT_LINE_FIRSTLAST : PPL_IO_FORMAT_LINE_LAST;
      endpar:
	if (!output_line(rest, parend_pos, type))
	  return 0;
	unsigned int l = strlen(settings->paragraph_end);
	rest = parend + l;
	len -= parend_pos + l;
	continue;
      }
    }
    type = first ? PPL_IO_FORMAT_LINE_FIRST : PPL_IO_FORMAT_LINE_NEXT;
    unsigned int line_length = settings->lines[type].length;
    if (len < line_length)
      break;
    int w = wrap_point_before(rest, line_length, 0);
    if (w > 0) {
    wrap:
      if (!output_line(rest, w, type))
	return 0;
      rest += w;
      len -= w;
      while (len > 0 && strchr(settings->strip_wrap, *rest)) {
	++rest;
	--len;
      }
      continue;
    }
    type = first ? PPL_IO_FORMAT_LINE_CHOPPED_FIRST : PPL_IO_FORMAT_LINE_CHOPPED_NEXT;
    unsigned int limit = len;
    unsigned int max_len = settings->lines[type].length;
    if (max_len > 0 && limit > max_len + 1)
      limit = max_len + 1;
    if (parend && limit > parend_pos)
      limit = parend_pos;
    w = wrap_point_after(rest, line_length, limit);
    if (w >= 0 &&
	(!parend || (unsigned) w < parend_pos) &&
	(!max_len || (unsigned) w <= max_len)) {
      type = first ? PPL_IO_FORMAT_LINE_LONGER_FIRST : PPL_IO_FORMAT_LINE_LONGER_NEXT;
      goto wrap;
    }
    if (parend && (max_len == 0 || parend_pos <= max_len)) {
      type = first ? PPL_IO_FORMAT_LINE_LONGER_FIRSTLAST : PPL_IO_FORMAT_LINE_LONGER_LAST;
      goto endpar;
    }
    if (max_len > 0) {
      if (!output_line(rest, max_len, type))
	return 0;
      rest += max_len;
      len -= max_len;
      continue;
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
    str.clear();
  }
  return 0;
}

bool c_streambuf_format::output_line(const char *s, unsigned int n, c_streambuf_format_line_type type) {
  switch (type) {
  case PPL_IO_FORMAT_LINE_FIRST:
  case PPL_IO_FORMAT_LINE_FIRSTLAST:
  case PPL_IO_FORMAT_LINE_CHOPPED_FIRST:
  case PPL_IO_FORMAT_LINE_LONGER_FIRST:
  case PPL_IO_FORMAT_LINE_LONGER_FIRSTLAST:
  case PPL_IO_FORMAT_LINE_UNTERMINATED_FIRST:
    if (settings->top)
      stream << settings->top;
    break;
  default:
    break;
  }
  if (settings->lines[type].left)
    stream << settings->lines[type].left;
  unsigned int i;
  for (i = 0; i < settings->lines[type].left_n; ++i)
    stream.put(settings->lines[type].left_c);
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
  for (i = 0; i < settings->lines[type].right_n; ++i)
    stream.put(settings->lines[type].right_c);
  if (settings->lines[type].right)
    stream << settings->lines[type].right;
  switch (type) {
  case PPL_IO_FORMAT_LINE_FIRSTLAST:
  case PPL_IO_FORMAT_LINE_LAST:
  case PPL_IO_FORMAT_LINE_LONGER_FIRSTLAST:
  case PPL_IO_FORMAT_LINE_LONGER_LAST:
    if (settings->bottom)
      stream << settings->bottom;
    first = true;
    break;
  default:
    first = false;
  }
  return stream;
}

}
