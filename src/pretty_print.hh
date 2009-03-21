/* Declaration of pretty print helper functions.
   Copyright (C) 2001-2009 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_pretty_print_hh
#define PPL_pretty_print_hh 1

#include "globals.defs.hh"

#include <ostream>

namespace Parma_Polyhedra_Library {

namespace IO_Operators {

//! The abstract class for writers.
class Write_Function {
public:
  /*! \brief
    Writes the first \p size characters from buffer \p buf to destination.

    \return
    The number of characters written.

    \param buf
    The source character buffer; it is assumed to be at least
    \p size characters wide.

    \param size
    The number of characters to be taken from \p buf.
  */
  virtual size_t write(const char* buf, size_t size) = 0;
};

//! Helper function for the wrapping of lines.
/*!
  \return
  The number of characters written through \p wfunc.

  \param wfunc
  The write function taking care of actual output.

  \param src
  The source buffer holding the lines to wrap.

  \param indent_depth
  The indentation depth.

  \param preferred_first_line_length
  The preferred length for the first line of text.

  \param preferred_line_length
  The preferred length for all the lines but the first one.
*/
size_t wrap(Write_Function& wfunc,
            const char* src,
            unsigned indent_depth,
            unsigned preferred_first_line_length,
            unsigned preferred_line_length);


//! A C++ output stream writer.
class Write_To_Stream : public Write_Function {
public:
  //! Constructor: object will write on stream \p os.
  Write_To_Stream(std::ostream& os);

  //! Writer function.
  size_t write(const char *buf, size_t size);

private:
  //! The output stream.
  std::ostream& os;

  // Private and not implemented: copy and assignment not allowed.
  Write_To_Stream(const Write_To_Stream&);
  Write_To_Stream& operator=(const Write_To_Stream&);

}; // class Write_To_Stream


// FIXME: properly comment.
struct write_buffer;
int write_to_buffer(void* data, const char* buf, size_t size);
struct write_buffer* write_buffer_new_dynamic(size_t initial_size);
struct write_buffer* write_buffer_new_static(char* buf, size_t size);
void write_buffer_delete(struct write_buffer* wbuf);
char* write_buffer_get(struct write_buffer* wbuf);
typedef size_t (*write_function)(void* data, const char* buf, size_t size);
size_t write_to_stdio(void* data, const char* buf, size_t size);
size_t write_to_count(void* data, const char* buf, size_t size);
size_t write_to_buffer_static(void* data, const char* buf, size_t size);
size_t write_to_buffer_dynamic(void* data, const char* buf, size_t size);

class Write_Function_C_Wrapper : public Write_Function {
public:
  // FIXME: avoid name clash.
  Write_Function_C_Wrapper(write_function func, void* data)
    : func(func), data(data) {
  }
  size_t write(const char* buf, size_t size) {
    return func(data, buf, size);
  }
private:
  write_function func;
  void* data;
};

template <typename T>
size_t
pretty_print(const T& o, Write_Function& wfunc,
             unsigned indent_depth,
             unsigned preferred_first_line_length,
             unsigned preferred_line_length);

template <typename T>
size_t
c_pretty_print(const T& o, write_function wfunc, void* data,
               unsigned indent_depth,
               unsigned preferred_first_line_length,
               unsigned preferred_line_length);

} // namespace IO_Operators

} // namespace Parma_Polyhedra_Library

#include "pretty_print.inlines.hh"

#endif // !defined(PPL_pretty_print_hh)
