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

void c_streambuf_format::add_char_untabify(char c) {
  switch (c) {
  case '\r':
  case '\n':
    column = 0;
    break;
  case '\t':
    do {
      str.push_back(' ');
      ++column;
    } while (column % settings->tab_width == 0);
    return;
  default:
    ++column;
    break;
  }
  str.push_back(c);
}

int c_streambuf_format::available_length(c_streambuf_format_line_type type) {
  c_streambuf_format_settings::line& l = settings->lines[type];
  int len = l.length;
  if (l.left.count && l.left.str)
    len -= l.left.count * strlen(l.left.str);
  if (l.right.count && l.right.str)
    len -= l.right.count * strlen(l.right.str);
  return len;
}

int c_streambuf_format::total_length(c_streambuf_format_line_type type, size_t size) {
  c_streambuf_format_settings::line& l = settings->lines[type];
  if (l.left.count && l.left.str)
    size += l.left.count * strlen(l.left.str);
  if (l.right.count && l.right.str)
    size += l.right.count * strlen(l.right.str);
  if (l.fill_char && size < l.length)
    size = l.length;
  if (l.begin)
    size += strlen(l.begin);
  if (l.end)
    size += strlen(l.end);
  return size;
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
      if (settings->tab_width)
	add_char_untabify(c);
      else
	str.push_back(c);
    }
  }
  else if (settings->tab_width) {
    for (unsigned int i = 0; i < size; ++i)
      add_char_untabify(buf[i]);
  }
  else
    str.append(buf, size);
  const char *rest = str.c_str();
  unsigned int len = str.size();
  while (1) {
    c_streambuf_format_line_type type;
    char *parend = strstr(rest, settings->paragraph_end);
    int parend_pos;
    if (parend) {
      parend_pos = parend - rest;
      if (parend_pos <= available_length(PPL_IO_FORMAT_LINE_LAST)) {
	type = first ? PPL_IO_FORMAT_LINE_FIRSTLAST : PPL_IO_FORMAT_LINE_LAST;
      endpar:
	if (!output_line(type, rest, parend_pos))
	  return 0;
	first = true;
	unsigned int l = strlen(settings->paragraph_end);
	rest = parend + l;
	len -= parend_pos + l;
	continue;
      }
    }
    type = first ? PPL_IO_FORMAT_LINE_FIRST : PPL_IO_FORMAT_LINE_NEXT;
    unsigned int linelen = available_length(type);
    if (len < linelen)
      break;
    int w = wrap_point_before(rest, linelen, 0);
    if (w > 0) {
    wrap:
      if (!output_line(type, rest, w))
	return 0;
      first = false;
      rest += w;
      len -= w;
      while (len > 0 && strchr(settings->strip_wrap, *rest)) {
	++rest;
	--len;
      }
      continue;
    }
    type = first ? PPL_IO_FORMAT_LINE_CHOPPED_FIRST : PPL_IO_FORMAT_LINE_CHOPPED_NEXT;
    int limit = len;
    int max_len = available_length(type);
    if (max_len > 0 && limit > max_len + 1)
      limit = max_len + 1;
    if (parend && limit > parend_pos)
      limit = parend_pos;
    w = wrap_point_after(rest, linelen, limit);
    if (w >= 0 &&
	(!parend || w < parend_pos) &&
	(!max_len || w <= max_len)) {
      type = first ? PPL_IO_FORMAT_LINE_LONGER_FIRST : PPL_IO_FORMAT_LINE_LONGER_NEXT;
      goto wrap;
    }
    if (parend && (max_len == 0 || parend_pos <= max_len)) {
      type = first ? PPL_IO_FORMAT_LINE_LONGER_FIRSTLAST : PPL_IO_FORMAT_LINE_LONGER_LAST;
      goto endpar;
    }
    if (max_len > 0) {
      if (!output_line(type, rest, max_len))
	return 0;
      first = false;
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
    c_streambuf_format_line_type type = first ? PPL_IO_FORMAT_LINE_UNTERMINATED_FIRST : PPL_IO_FORMAT_LINE_UNTERMINATED_NEXT;
    if (!output_line(type, str.c_str(), n))
      return -1;
    first = false;
    str.clear();
  }
  return 0;
}

void c_streambuf_format::output_rep(const c_streambuf_format_settings::line::rep& rep) {
  if (rep.count && rep.str) {
    for (unsigned int i = 0; i < rep.count; ++i)
      stream << rep.str;
  }
}

bool c_streambuf_format::output_line(c_streambuf_format_line_type type, const char *s, unsigned int n) {
  switch (type) {
  case PPL_IO_FORMAT_LINE_FIRST:
  case PPL_IO_FORMAT_LINE_FIRSTLAST:
  case PPL_IO_FORMAT_LINE_CHOPPED_FIRST:
  case PPL_IO_FORMAT_LINE_LONGER_FIRST:
  case PPL_IO_FORMAT_LINE_LONGER_FIRSTLAST:
  case PPL_IO_FORMAT_LINE_UNTERMINATED_FIRST:
    for (unsigned int i = 0; i < 2; ++i)
      if (settings->top[i] && !output_line1(static_cast<c_streambuf_format_line_type>(PPL_IO_FORMAT_LINE_TOP1 + i),
					    settings->top[i], strlen(settings->top[i])))
	return false;
    break;
  default:
    break;
  }
  if (!output_line1(type, s, n))
    return false;
  switch (type) {
  case PPL_IO_FORMAT_LINE_FIRSTLAST:
  case PPL_IO_FORMAT_LINE_LAST:
  case PPL_IO_FORMAT_LINE_LONGER_FIRSTLAST:
  case PPL_IO_FORMAT_LINE_LONGER_LAST:
    for (unsigned int i = 0; i < 2; ++i)
      if (settings->bottom[i] && !output_line1(static_cast<c_streambuf_format_line_type>(PPL_IO_FORMAT_LINE_BOTTOM1 + i),
					       settings->bottom[i], strlen(settings->bottom[i])))
	return false;
    break;
  default:
    break;
  }
  return true;
}

bool c_streambuf_format::output_line1(c_streambuf_format_line_type type, const char *s, unsigned int n) {
  c_streambuf_format_settings::line& l = settings->lines[PPL_IO_FORMAT_LINE_EXTERN];
  if (l.begin)
    stream << l.begin;
  output_rep(l.left);
  switch (type) {
  case PPL_IO_FORMAT_LINE_UNTERMINATED_FIRST:
  case PPL_IO_FORMAT_LINE_UNTERMINATED_NEXT:
    return output_line2(type, s, n);
  default:
    break;
  }
  int w;
  int len;
  if (l.fill_char && (w = total_length(type, n)) < (len = available_length(PPL_IO_FORMAT_LINE_EXTERN))) {
    unsigned int left = 0;
    unsigned int right = 0;
    unsigned int f = len - w;
    left = f * l.alignment / PPL_IO_FORMAT_ALIGN_RIGHT;
    right = f - left;
    unsigned int i;
    for (i = 0; i < left; ++i)
      stream.put(l.fill_char);
    if (!output_line2(type, s, n))
      return false;
    for (i = 0; i < right; ++i)
      stream.put(l.fill_char);
  }
  else {
    if (!output_line2(type, s, n))
      return false;
  }
  output_rep(l.right);
  if (l.end)
    stream << l.end;
  return stream;
}

bool c_streambuf_format::output_line2(c_streambuf_format_line_type type, const char *s, unsigned int n) {
  c_streambuf_format_settings::line& l = settings->lines[type];
  if (l.begin)
    stream << l.begin;
  output_rep(l.left);
  int len;
  if (l.fill_char && (int) n < (len = available_length(type))) {
    unsigned int left = 0;
    unsigned int right = 0;
    unsigned int f = len - n;
    left = f * l.alignment / PPL_IO_FORMAT_ALIGN_RIGHT;
    right = f - left;
    unsigned int i;
    for (i = 0; i < left; ++i)
      stream.put(l.fill_char);
    stream.write(s, n);
    for (i = 0; i < right; ++i)
      stream.put(l.fill_char);
  }
  else
    stream.write(s, n);
  output_rep(l.right);
  if (l.end)
    stream << l.end;
  return stream;
}

}
