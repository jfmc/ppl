/* Generator class declaration.
   Copyright (C) 2001 Roberto Bagnara <bagnara@cs.unipr.it>

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

#ifndef _Generator_defs_hh
#define _Generator_defs_hh 1

#include "Generator.types.hh"
#include "Row.defs.hh"
#include "LinExpression.types.hh"
#include <iosfwd>

namespace Parma_Polyhedra_Library {
  // Put them in the namespace here to declare them friend later.

  //! @name Generator.
  //@{
  //! Returns the (bidirectional) line of direction \p e.
  Generator operator |(int, const LinExpression& e);
  //! Returns the (unidirectional) ray of direction \p e.
  Generator operator ^(int, const LinExpression& e);
  //! Returns the vertex at \p e / \p n.
  Generator operator /=(const LinExpression& e, const Integer& n);
  //@}
}

//! A line, ray or vertex.
/*!
  An object of the class Generator is either: 
  
  - a line: \f$\sum_{i=0}^{d-1} a_i \vec{x}_i\f$;
	
  - a ray: \f$\sum_{i=0}^{d-1} a_i \vec{x}_i\f$;
       
  - a vertex: \f$\sum_{i=0}^{d-1} \frac{a_i}{b} \vec{x}_i\f$;

  where \f$d\f$ is the dimension of the space.

  Each type of generator is built by specifying a linear expressions,
  representing a direction in the space.
  To provide directions, only integer coefficients are used:
  thus, in order to be able to specify <EM>vertices</EM>
  having also non-integral (rational) coordinates,
  an integer denominator \f$b\f$ is required;
  such a denominator is not needed for lines and rays.
*/

class Parma_Polyhedra_Library::Generator : public Row {
private:
  Generator(LinExpression& e);

  friend Generator
  Parma_Polyhedra_Library::operator |(int, const LinExpression& e);
  friend Generator
  Parma_Polyhedra_Library::operator ^(int, const LinExpression& e);
  friend Generator
  Parma_Polyhedra_Library::operator /=(const LinExpression& e,
				       const Integer& n);

public:
  //! Default constructor.
  Generator();
  //! Ordinary copy-constructor.
  Generator(const Generator& g);
  //! Destructor.
  ~Generator();

  enum Type {
    LINE = Row::LINE_OR_EQUALITY,
    RAY = Row::RAY_OR_VERTEX_OR_INEQUALITY,
    VERTEX = RAY+1
  };

  //! Returns the type of \p *this.
  Type type() const;

PPL_INTERNAL:
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is a line.
  bool is_line() const;
  //! Returns <CODE>true</CODE> if and only if
  //! \p *this is either a ray or a vertex.
  bool is_ray_or_vertex() const;
  //! Sets the type to <CODE>LINE</CODE>.
  void set_is_line();
  //! Sets the type to either <CODE>RAY</CODE>.
  void set_is_ray_or_vertex();
};

namespace Parma_Polyhedra_Library {

std::ostream&
operator <<(std::ostream& s, const Generator& g);

}

#if !OUTLINE
#include "Generator.inlines.hh"
#endif

#endif
