/* GenSys class implementation (non-inline functions).
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

#include <config.h>

#include "GenSys.defs.hh"
#if OUTLINE
#include "GenSys.inlines.hh"
#endif

#include "Constraint.defs.hh"
#include <cassert>
#include <string>
#include <vector>
#include <iostream>
#include <stdexcept>

namespace PPL = Parma_Polyhedra_Library;

size_t
Parma_Polyhedra_Library::GenSys::num_lines() const {
  size_t n = 0;
  // If the Matrix happens to be sorted, take advantage of the fact
  // that lines are at the top of the system.
  if (is_sorted()) {
    size_t nrows = num_rows();
    for (size_t i = 0; i < nrows && (*this)[i].is_line(); ++i)
      ++n;
  }
  else
    for (size_t i = num_rows(); i-- > 0 ; )
      if ((*this)[i].is_line())
	++n;
  return n;
}

size_t
Parma_Polyhedra_Library::GenSys::num_rays() const {
  size_t n = 0;
  // If the Matrix happens to be sorted, take advantage of the fact
  // that rays and vertices are at the bottom of the system and 
  // rays have the inhomogeneous term equal to zero.
  if (is_sorted()) {
    const GenSys& x = *this;
    for (size_t i = num_rows(); i != 0 && x[--i].is_ray_or_vertex(); ) 
      if (x[i][0] == 0) 
	++n;
  }
  else
    for (size_t i = num_rows(); i-- > 0 ; ) {
      const Generator& g = (*this)[i];
      if (g.is_ray_or_vertex() && g[0] == 0)
	++n;
    }
  return n;
}

/*!
  Returns:
  - <CODE>ALL_SATURATE</CODE> if all generators belong to the hyper-plane 
    defined by \p c (saturate \p c),
  - <CODE>ALL_SATISFY</CODE> if all generators satisfy \p c but do not 
    belong to the hyper-plane defined by \p c,
  - <CODE>NONE_SATISFIES</CODE> if no generators satisfy \p c,
  - <CODE>SOME_SATISFY</CODE> if one or more generators do not satisfy \p c.

  If \p c is an equality, only <CODE>ALL_SATISFY</CODE> 
  or <CODE>NONE_SATISFIES</CODE> can be returned.
*/
PPL::GenSys_Con_Rel
PPL::GenSys::satisfy(const Constraint& c) const {
  // Generators and constraint `c' have to be given in 
  // spaces having the same dimension.
  assert(num_columns() == c.size());
  const  GenSys& gen_sys = *this;
  // Number of generators.
  size_t n_rows = num_rows();
  if (c.is_equality()) {
    for (size_t i = n_rows; i-- > 0;)
      if (gen_sys[i] * c != 0)
	// There is at least one generator that does not satisfy `c'.
	return SOME_SATISFY;
    // All generators satisfy `c' i.e., saturate it because 
    // `c' is an equality.
    return ALL_SATURATE;
  }
  else {
    // The constraint c is an inequality.
    // first_ray_or_vertex will be set to `false'
    // after finding a ray or a vertex (starting from the
    // last row of the matrix).
    bool first_ray_or_vertex = true;
    GenSys_Con_Rel res = ALL_SATURATE;
    for (size_t i = n_rows; i-- > 0;) {
      const Generator& r = gen_sys[i];
      int sp_sign = sgn(r * c);
      if (r.is_line()) {
	if (sp_sign != 0)
	  // Lines must saturate every constraints.
	  return SOME_SATISFY;
      }
      else {
	// The generator `r' is a vertex or a ray.
	if (r[0] == 0) {
	  // The generator r is a ray.
	  if (sp_sign != 0) {
	    if (first_ray_or_vertex) {
	      // It is the first time that we have
	      // a ray and we have never had a vertex.
	      res = (sp_sign > 0) ? ALL_SATISFY : NONE_SATISFIES;
	      first_ray_or_vertex = false;
	    }
	    else {
	      // It is not the first time we find a ray/vertex.
	      if ((sp_sign > 0 && res == NONE_SATISFIES)
		  || (sp_sign < 0 && res != NONE_SATISFIES))
		// There are some generators satisfying c in two cases:
		// - if r satisfy c but none of the generators that
		//   we have already considered satisfy c;
		// - if r does not satisfy c and there was some generators 
		//   that we have already considered that do not satisfy c.
		return SOME_SATISFY;
	      if (sp_sign > 0)
		// Since we always return if res == SOME_SATISFIES, 
		// at this point r and all the previous generators 
		// satisfy c.
		res = ALL_SATISFY;
	    }
	  }
	}
	else {
	  // The generator r is a vertex.
	  if (first_ray_or_vertex) {
	    // It is the first time that we have
	    // a vertex and we have never had a ray.
	    // If some lines do not saturate c we have already returned, 
	    // so here all lines checked until here (if any) saturate c.
	    // - If r (that is a vertex) satisfy c then all
	    //   the generators checked until here satisfy c.
	    // - If r saturate c then all the generators 
	    //   checked until here saturate c.
	    // - If r does not verify c we have only
	    //   generators that saturate or do not verify c,
	    //   then none of them satisfy c.
	    res = (sp_sign > 0) ? ALL_SATISFY : 
	      ((sp_sign == 0) ? ALL_SATURATE : NONE_SATISFIES);
	    first_ray_or_vertex = false;
	  }
	  else{
	    // It is not the first time we find a 
	    // vertex or it is the first found but 
	    // but it has been preceded by a ray.
	    if ((sp_sign >= 0 && res == NONE_SATISFIES)
		|| (sp_sign < 0 && res != NONE_SATISFIES))
	      // We return SOME_SATISFY in two cases:
	      // - if r satisfies c and all the previous
	      //   generators do not;
	      // - if r does not verify and all the 
	      //   the previous generators verify.
	      return SOME_SATISFY;
	    if (sp_sign > 0)
	      // Since we always return if res == SOME_SATISFIES, 
	      // at this point r and all the previous generators 
	      // satisfy c.
	      res = ALL_SATISFY;
	  }
	}
      }
    }
    return res;
  }
}


/*!
  \param var          Index of the column to which the 
                      affine transformation is assigned.
  \param expr         The affine transformation:
                      \f$\sum_{i = 0}^{n - 1} a_i x_i + b\f$.
  \param denominator  The denominator of the affine transformation.

  We want to allow affine transformations (see definitions.dox) having 
  any rational coefficients. Since the coefficients of the 
  constraints are integers we must also provide an integer \p denominator 
  that will be used as denominator of the affine transformation.

  The affine transformation assigns to each element of \p var -th 
  column the follow expression:
  \f[
    \frac{\sum_{i = 0}^{n - 1} a_i x_i + b}
         {denominator}.
  \f]

  \p expr is a constant parameter and unaltered by this computation
*/
void 
PPL::GenSys::assign_variable(size_t var,
			     const LinExpression& expr,
			     Integer& denominator) {
  GenSys& x = *this;
  size_t num_columns = x.num_columns();
  size_t num_rows = x.num_rows();

  // The first coefficient is the inhomogeneous term.
  assert(var != 0);
  assert(num_columns = expr.size());
  assert(denominator != 0);
  assert(var < num_columns);

  // Computing the numerator of the affine transformation and assigning
  // it to the column of `*this' indexed by `var'.
  for (size_t i = 0; i < num_rows; ++i) {
    Generator& row = x[i]; 
    row[var] *= expr[var];
    for (size_t j = 0; j < num_columns; ++j) 
      if (j != var) 
	row[var] += row[j] * expr[j];
  }
  if (denominator != 1) 
    // Since we want integer elements in the matrix and the 
    // `var'-th columns is a multiple of `denominator', we 
    // multiply by `denominator' all the other columns of `*this'.
    for (size_t i = 0; i < num_rows; ++i)
      for (size_t j = 0; j < num_columns; ++j)
	if( j != var)
	  x[i][j] *= denominator;
  x.normalize();
}
 
/*!
  Like <CODE>ConSys::print()</CODE>, this prints the number of rows, 
  the number of columns and value of \p sorted, using the 
  <CODE>Matrix::print()</CODE> method, then prints the contents of 
  all the rows, specifying whether a row represent a line or a vertex/ray.
*/
void 
PPL::GenSys::print(std::ostream& s) const {
  Matrix::print(s);
  const char separator = ' ';
  const GenSys& x = *this;
  for (size_t i = 0; i < x.num_rows(); ++i) {
    for (size_t j = 0; j < x.num_columns(); ++j)
      s << x[i][j] << separator;
    s << separator << separator
      << (x[i].is_ray_or_vertex()
	  ? (x[i][0] == 0 ? "R" : "V")
	  : "L")
      << std::endl;
  }
}

/*!
  Like <CODE>ConSys::get()</CODE>, this uses <CODE>Matrix::get()</CODE> 
  to resize the matrix of generators taking information from \p s, 
  then initializes the coefficients of each generator and its type 
  (line or ray/vertex).
*/
void 
PPL::GenSys::get(std::istream& s) {
  Matrix::get(s);
  std::string tempstr;
  GenSys& x = *this;
  for (size_t i = 0; i < x.num_rows(); ++i) {
    for (size_t j = 0; j < x.num_columns(); ++j)
      s >> x[i][j];
    s >> tempstr;
    if (tempstr == "L")
      x[i].set_is_line();
    else if (tempstr == "R" || tempstr == "V")
      x[i].set_is_ray_or_vertex();
    else
      throw std::runtime_error("void PPL::GenSys::get(s)");
  }
}

/*!
  Returns <CODE>true</CODE> if and only if \p *this actually represents
  a set of generators. So, \p *this must satisfy some rule:
  -# it must have a column for the inhomogeneous term and one for 
     a variable;
  -# it can have no row; otherwise it must have at least a vertex;
  -# every line and ray must have the inhomogeneous term equal to zero;
  -# the inhomogeneous term of all vertices must be positive.  
*/
bool
PPL::GenSys::OK() const {
  using std::endl;
  using std::cerr;

  // GenSys must have at least two columns: one for the inhomogeneous 
  // terms and one for the coefficients of at least one variable.
  if (!Matrix::OK()) {
    cerr << "A GenSys must have at least two columns!"
	 << endl;
    return false;
  }
  if (num_rows() == 0)
    // A valid system of generators can be empty.
    return true;
  bool no_vertex = true;
  for (size_t i = num_rows(); i-- > 0; ) { 
    const Generator& g = (*this)[i];
    bool ray_or_line = false;
    // Looking for vertices.
    if (g.is_ray_or_vertex()) {
      // A vertex is legal only if its inhomogeneous term 
      // is strictly positive.
      if (g[0] > 0)
	// We found a vertex.
	no_vertex = false;
      else if (g[0] < 0) {
	cerr << "Vertices cannot have a negative inhomogeneous term!"
	     << endl;
	return false;
      }
      else
	// Since rays and lines have a zero inhomogeneous term,
	// we found a ray.
	ray_or_line = true;
    }
    else if (g[0] != 0) {
      cerr << "Lines must have a zero inhomogeneous term!"
	   << endl;
      return false;
    }
    else
      // We found a line.
      ray_or_line = true;

    if (ray_or_line) {
      // Looking for a non-zero coefficient in rays and lines.
      bool all_zeroes = true;
      for (size_t j = num_columns(); j-- > 1; )
	if (g[j] != 0) {
	  all_zeroes = false;
	  break;
	}
      if (all_zeroes) {
	// Rays and lines must have at least one non-zero homogeneous term.
	cerr << "Generators must have \
at least one nonzero homogeneous coefficient!"
	     << endl;
	return false;
      }
    }
  }

  if (no_vertex) {
    // A valid, non-empty system 
    // of generators must have at least one vertex.
    cerr << "There must be at least one vertex!"
	 << endl;
    return false;
  }

  return true;
}
