/* Serializer class declaration
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#ifndef PPL_Serializer_defs_hh
#define PPL_Serializer_defs_hh 1

#ifndef ENDIAN_SOLVER
#define ENDIAN_SOLVER 0
#endif

#include "Polyhedron.defs.hh"
#include "Constraint_System.defs.hh"
#include "Generator_System.defs.hh"
// #include "SatMatrix.defs.hh"
// #include "SatRow.defs.hh"
#include "Row.defs.hh"
#include "Topology.hh"
#include "Coefficient.defs.hh"
#include <gmp.h>
#include <zlib.h>
#include <bzlib.h>
#include <fstream>
#include <string>
#include <vector>
#include <stdexcept>
#include "Serializer.types.hh"

namespace Parma_Polyhedra_Library {

  //! Serializer open mode.
  enum openmode {
    in,
    out,
    out1,
    out2,
    out3,
    out4,
    out5,
    out6,
    out7,
    out8,
    out9,
    closed
  };

  //! Serializer compression mode.
  enum compressionmode {
    none,
    zlib,
    bzlib
  };

  //! Serializer endian type.
  enum endian {
    little,
    big
  };

namespace IO_Operators {

//! Output Serializer operator.
/*!
  \relates Parma_Polyhedra_Library::Serializer
  Write a serial representation of \p ph on \p s.
  The Serializer will open in writing mode automatically.

  \return <CODE>void</CODE> value because the
  Serializers cannot concatenate.
*/
void
operator<<(Serializer& s, const Polyhedron& ph);

//! Output Serializer operator.
/*!
  \relates Parma_Polyhedra_Library::Serializer
  Write a serial representation of \p gs on \p s.
  The Serializer will open in writing mode automatically.

  \return <CODE>void</CODE> value because the
  Serializers cannot concatenate.
*/
void
operator<<(Serializer& s, const Generator_System& gs);

//! Output Serializer operator.
/*!
  \relates Parma_Polyhedra_Library::Serializer
  Write a serial representation of \p cs on \p s.
  The Serializer will open in writing mode automatically.

  \return <CODE>void</CODE> value because the
  Serializers cannot concatenate.
*/
void
operator<<(Serializer& s, const Constraint_System& cs);

//! Output Serializer operator.
/*!
  \relates Parma_Polyhedra_Library::Serializer
  Write a serial representation of \p sat_mat on \p s.
  The Serializer will open in writing mode automatically.

  \return <CODE>void</CODE> value because the
  Serializers cannot concatenate.
*/
void
operator<<(Serializer& s, const Bit_Matrix& sat_mat);

//! Input Serializer operator.
/*!
  \relates Parma_Polyhedra_Library::Serializer
  Read a serial representation of \p ph from \p s.
  The Serializer will open in reading mode automatically.

  \return <CODE>void</CODE> value because the
  Serializers cannot concatenate.
*/
void
operator>>(Serializer& s, Polyhedron& ph);

//! Input Serializer operator.
/*!
  \relates Parma_Polyhedra_Library::Serializer
  Read a serial representation of \p gs from \p s.
  The Serializer will open in reading mode automatically.

  \return <CODE>void</CODE> value because the
  Serializers cannot concatenate.
*/
void
operator>>(Serializer& s, Generator_System& gs);

//! Input Serializer operator.
/*!
  \relates Parma_Polyhedra_Library::Serializer
  Read a serial representation of \p cs from \p s.
  The Serializer will open in reading mode automatically.

  \return <CODE>void</CODE> value because the
  Serializers cannot concatenate.
*/
void
operator>>(Serializer& s, Constraint_System& cs);

//! Input Serializer operator.
/*!
  \relates Parma_Polyhedra_Library::Serializer
  Read a serial representation of \p sat_mat from \p s.
  The Serializer will open in reading mode automatically.

  \return <CODE>void</CODE> value because the
  Serializers cannot concatenate.
*/
 void
 operator>>(Serializer& s, Bit_Matrix& sat_mat);

}// namespace IO_Operators
}// namespace Parma_Polyhedra_Library

#ifdef PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
/*!
  Class to read and write on a specified file a serial
  representation of Polyhedron, Constraint_System, Generator_System and Bit_Matrix.
  An object of this class behaves and uses like an
  input-output file stream.
  Serializer read and write on standard data file,
  gzipped data file and bzipped data file.
  The following example code shows some kind of correct use
  of Serializer:

  \par Example 1
  \code
  C_Polyhedron ph;
  Serializer s;
  s.open("myfile.dat", in);
  s.deserialize(ph);
  \encode

  \par Example 2
  \code
  C_Polyhedron ph;
  Serializer s("myfile.gz");
  s << ph;
  s >> ph;
  \encode

  \par Example 2
  \code
  C_Polyhedron ph;
  Serializer s("myfile.bz2", out);
  s.open();
  s.serialize(ph);
  \encode
*/
#endif // PPL_DOXYGEN_INCLUDE_IMPLEMENTATION_DETAILS
class Parma_Polyhedra_Library::Serializer {
public:
  //! Builds a Serializer whithout specifications.
  Serializer();

  //! Build a Serializer with specified name and open mode.
  /*!
    \param path   The name or path of working file.
    \param mode   The open file mode:
                  in, out for standard file;
		  in, out, out1, out2, out3, out4, out5,
                  out6, out7, out8, out9 for zip file;
                  It assume the <CODE>closed</CODE> value if
		  isn't specified.
  */
  Serializer(const std::string& path, openmode mode = closed);

  //! Open a Serializer without specifications.
  void open() throw(std::ios_base::failure, std::runtime_error);

  //! Open a Serializer with specified name and open mode.
  /*!
    \param path   The name or path of working file.
    \param mode   The open file mode:
                  in, out for standard file;
		  in, out, out1, out2, out3, out4, out5,
                  out6, out7, out8, out9 for zip file.
  */
  void open(const std::string& path, openmode mode);

  //! Close a Serializer.
  void close() throw(std::ios_base::failure, std::runtime_error);

  //! Serialize a Polyhedron.
  void serialize(const Polyhedron&);

  //! Serialize a Generator_System.
  void serialize(const Generator_System&);

  //! Serialize a Constraint_System.
  void serialize(const Constraint_System&);

   //! Serialize a Bit_Matrix.
   void serialize(const Bit_Matrix&);

  //! Deserialize a Polyhedron.
  void deserialize(Polyhedron&);

  //! Deserialize a Generator_System.
  void deserialize(Generator_System&);

  //! Deserialize a Constraint_System.
  void deserialize(Constraint_System&);

   //! Deserialize a Bit_Matrix.
   void deserialize(Bit_Matrix&);

private:

  //! Ordinary copy-costructor without implementation.
  Serializer(const Serializer&);

  //! Assignment operator without implementation.
  Serializer& operator=(const Serializer&);

  //! Serializer's open mode: in, out.
  openmode open_mode;

  //! Serializer's compression mode: none, zlib, bzlib.
  compressionmode compress_mode;

  //! Readables elements on buffer.
  unsigned n_buffer;

  //! Index of buffer.
  unsigned i_buffer;

  //! Readables elements on minibuffer.
  unsigned n_minibuffer;

  //! Index of minibuffer.
  unsigned i_minibuffer;

  //! Buffer for compression and decompression.
  std::vector<unsigned char> buffer;

  //! A little support Buffer during decompression.
  std::vector<unsigned char> minibuffer;

  //! Buffer for reading and writing in binary mode the mpz_t.
  std::vector<unsigned long> buffer_mpz_t;

  //! Uses in compact memorization method of mpz_t.
  unsigned char little_mpz_t;

  //! A parameter of mpz_import and mpz_export functions.
  int order_mpz_t;

  //! A parameter of mpz_import and mpz_export functions.
  int endian_mpz_t;

  //! A parameter of mpz_import and mpz_export functions.
  size_t nail_mpz_t;

  //! A parameter of mpz_import and mpz_export functions.
  size_t size_mpz_t;

  //! A parameter of mpz_import and mpz_export functions.
  size_t numb_mpz_t;

  //! \brief
  //! <CODE>true</CODE> if there's a difference between
  //! the working endian and the reading file endian,
  //! <CODE>false</CODE> otherwise.
  bool different_endian;

  //! The compression level of zip file.
  std::string compression_level;

  //! The name or path of working file.
  std::string path;

  //! Standard file.
  std::fstream file;

  //! The gzlib file.
  gzFile gzfile;

  //! The bzlib file.
  BZFILE* bzfile;

  //! The error flag of gzlib and bzlib functions.
  int zerror;

  //! The working endian.
  endian host_endian;

  //! \brief
  //! Check the style of file's path and, eventually,
  //! assign the right value at compress_mode.
  void compression_path_assign();

  //! \brief
  //! Check the open_mode of zip file and
  //! assign the right value at compression_level.
  void compression_level_assign() throw(std::runtime_error);

  //! Put on buffer the binary representation of \p t.
  template <typename T>
  void put(T t);

  //! Get the binary representation of \p t from buffer \p buf.
  template <typename A, typename B>
  void get(A& t, const std::vector<B>& buf, unsigned&);

  //! Change the endian of \p t if necessary.
  template <typename T>
  void change_endian(T& t);

  //! Write on file the working endian.
  void put_file_endian();

  //! Read from file its endian.
  void get_file_endian();

  //! Low level serialization private functions.
  template <typename T>
  void low_level_serialize(T) throw(std::ios_base::failure, std::runtime_error);

  //! Low level deserialization private functions.
  template <typename T>
  void low_level_deserialize(T&) throw(std::ios_base::failure, std::runtime_error);

  //! Serialize a Polyhedron::Status.
  void serialize(const Polyhedron::Status&);

  //! Serialize a Linear_System.
  void serialize_matrix(const Linear_System&);

  //! Serialize a bool.
  void serialize(bool);

  //! Serialize a dimension_type.
  void serialize(dimension_type);

  //! Serialize a Topology.
  void serialize(const Topology&);

  //! Serialize a mpz_t.
  void serialize(const mpz_t);

  //! Serialize an Coefficient.
  void serialize(const Coefficient&);

    //! Serialize a Row::Type.
  void serialize(const Row::Flags&);

  //! Serialize a SatRow.
  void serialize(const Bit_Row&);

  //! Deserialize a Polyhedron::Status.
  void deserialize(Polyhedron::Status&);

  //! Deserialize a Linear_System.
  void deserialize_matrix(Linear_System&);

  //! Deserialize a bool.
  void deserialize(bool&);

  //! Deserialize a dimension_type.
  void deserialize(dimension_type&);

  //! Deserialize a Topology.
  void deserialize(Topology&);

  //! Deserialize a mpz_t.
  void deserialize(mpz_t);

  //! Deserialize an Coefficient.
  void deserialize(Coefficient&);

   //! Deserialize a Row::Type.
  void deserialize(Row::Flags&);

    //! Deserialize a SatRow.
    void deserialize(Bit_Row&);

  friend void
  Parma_Polyhedra_Library::IO_Operators::operator<<(Serializer&,
						    const Polyhedron&);
  friend void
  Parma_Polyhedra_Library::IO_Operators::operator<<(Serializer&,
						    const Generator_System&);
  friend void
  Parma_Polyhedra_Library::IO_Operators::operator<<(Serializer&,
						    const Constraint_System&);
  friend void
  Parma_Polyhedra_Library::IO_Operators::operator<<(Serializer&,
 						    const Bit_Matrix&);
  friend void
  Parma_Polyhedra_Library::IO_Operators::operator>>(Serializer&,
						    Polyhedron&);
  friend void
  Parma_Polyhedra_Library::IO_Operators::operator>>(Serializer&,
						    Generator_System&);
  friend void
  Parma_Polyhedra_Library::IO_Operators::operator>>(Serializer&,
						    Constraint_System&);
   friend void
   Parma_Polyhedra_Library::IO_Operators::operator>>(Serializer&,
 						    Bit_Matrix&);
};
#include "Serializer.inlines.hh"
#endif// !defined(PPL_Serializer_defs_hh)
