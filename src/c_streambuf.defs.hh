/* c_streambuf class declaration.
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

#ifndef PPL_c_streambuf_defs_hh
#define PPL_c_streambuf_defs_hh 1

#include "c_streambuf.types.hh"
#include <streambuf>

typedef size_t (*read_function)(void* data, void *buf, size_t size);
typedef size_t (*write_function)(void* data, const void *buf, size_t size);

class Parma_Polyhedra_Library::c_streambuf
  : public std::basic_streambuf<char, std::char_traits<char> > {
public:
  //! Constructor.
  c_streambuf(read_function read, write_function write, void *data);

protected:
  /*! \brief
    Gets a character in case of underflow.

    \remarks
    Specified by ISO/IEC 14882:1998: 27.5.2.4.3.
  */
  int_type underflow();

  /*! \brief
    In case of underflow, gets a character and advances the next pointer.

    \remarks
    Specified by ISO/IEC 14882:1998: 27.5.2.4.3.
  */
  int_type uflow();

  /*! \brief
    Gets a sequence of characters.

    \remarks
    Specified by ISO/IEC 14882:1998: 27.5.2.4.3.
  */
  std::streamsize xsgetn(char_type* s, std::streamsize n);

  /*! \brief
    Puts character back in case of backup underflow.

    \remarks
    Specified by ISO/IEC 14882:1998: 27.5.2.4.4.
  */
  int_type pbackfail(int_type c = traits_type::eof());

  /*! \brief
    Writes a sequence of characters.

    \remarks
    Specified by ISO/IEC 14882:1998: 27.5.2.4.5.
  */
  std::streamsize xsputn(const char_type* s, std::streamsize n);

  /*! \brief
    Writes a character in case of overflow.

    Specified by ISO/IEC 14882:1998: 27.5.2.4.5.
  */
  int_type overflow(int_type c);

  /*! \brief
    Synchronizes the stream buffer.

    Specified by ISO/IEC 14882:1998: 27.5.2.4.2.
  */
  int sync();

private:
  //! Character type of the streambuf.
  typedef char char_type;

  //! Traits type of the streambuf.
  typedef std::char_traits<char_type> traits_type;

  //! Integer type of the streambuf.
  typedef traits_type::int_type int_type;

  //! The read callback
  read_function read;

  //! The write callback
  write_function write;

  //! The callback data
  void *data;

  //! Buffer for the last character read.
  int_type ungetc_buf;

  //! Buffer for next character
  int_type nextc_buf;
};

#include "c_streambuf.inlines.hh"

#endif // !defined(PPL_c_streambuf_defs_hh)
