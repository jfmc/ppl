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

#include "Constraint.defs.hh"
#include <cassert>
#include <string>
#include <vector>
#include <iostream>
#include <stdexcept>

namespace PPL = Parma_Polyhedra_Library;

std::ostream&
PPL::operator <<(std::ostream& s, GenSys_Con_Rel r) {
  const char* p = 0;
  switch (r) {
  case NONE_SATISFIES:
    p = "NONE_SATISFIES";
    break;
  case ALL_SATISFY:
    p = "ALL_SATISFY";
    break;
  case ALL_SATURATE:
    p = "ALL_SATURATE";
    break;
  case SOME_SATISFY:
    p = "SOME_SATISFY";
    break;
  }
  assert(p != 0);
  s << p;
  return s;
}

size_t
PPL::GenSys::num_lines() const {
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
PPL::GenSys::num_rays() const {
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
  size_t space_dim = space_dimension();
  size_t c_space_dim = c.space_dimension();
  // Generators and constraint `c' have to be given in
  // spaces having the same dimension.
  assert(space_dim >= c_space_dim);
  const  GenSys& gen_sys = *this;
  // Number of generators.
  size_t n_rows = num_rows();
  if (c.is_equality()) {
    for (size_t i = n_rows; i-- > 0;)
      if (c_space_dim < space_dim)
	if (c * gen_sys[i] != 0)
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
    for (size_t i = n_rows; i-- > 0; ) {
      const Generator& r = gen_sys[i];
      int sp_sign = sgn(c * r);
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
  \param v            Index of the column to which the
                      affine transformation is assigned.
  \param expr         The numerator of the affine transformation:
                      \f$\sum_{i = 0}^{n - 1} a_i x_i + b\f$.
  \param denominator  The denominator of the affine transformation.

  We want to allow affine transformations (see the Introduction) having
  any rational coefficients. Since the coefficients of the
  constraints are integers we must also provide an integer \p denominator
  that will be used as denominator of the affine transformation.

  The affine transformation assigns to each element of \p v -th
  column the follow expression:
  \f[
    \frac{\sum_{i = 0}^{n - 1} a_i x_i + b}
         {denominator}.
  \f]

  \p expr is a constant parameter and unaltered by this computation
*/
void
PPL::GenSys::affine_image(size_t v,
			  const LinExpression& expr,
			  const Integer& denominator) {
  GenSys& x = *this;
  size_t num_columns = x.num_columns();
  size_t num_rows = x.num_rows();

  // The first coefficient is the inhomogeneous term.
  assert(v != 0);
  assert(num_columns = expr.size());
  assert(denominator != 0);
  assert(v < num_columns);

  // Computing the numerator of the affine transformation and assigning
  // it to the column of `*this' indexed by `v'.
  for (size_t i = 0; i < num_rows; ++i) {
    Generator& row = x[i];
    row[v] *= expr[v];
    for (size_t j = 0; j < num_columns; ++j)
      if (j != v)
	row[v] += row[j] * expr[j];	
  }
  if (denominator != 1)
    // Since we want integer elements in the matrix and the
    // `v'-th columns is a multiple of `denominator', we
    // multiply by `denominator' all the other columns of `*this'.
    for (size_t i = 0; i < num_rows; ++i)
      for (size_t j = 0; j < num_columns; ++j)
	if( j != v)
	  x[i][j] *= denominator;
  for (size_t i = num_rows; i--> 0; )
    if (x[i][0] == 0) {
      bool is_origin = true;
      for (size_t j = 1; j < num_columns; ++j)
	if (x[i][j] != 0) {
	  is_origin = false;
	  break;
	}
      if (is_origin) {
	--num_rows;
	std::swap(x[i], x[num_rows]);
      }
    }
  x.erase_to_end(num_rows);
  x.strong_normalize();
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

void
PPL::GenSys::remove_invalid_lines_and_rays() {
  GenSys& gs = *this;
  size_t nrows = num_rows();
  size_t i;
  for (i = 0; i < nrows; ++i) {
    const Generator& g = gs[i];
    if (g[0] == 0 && g.all_homogeneous_terms_are_zero())
      // An invalid line or ray.
      break;
  }
  // If `i < nrows', `gs[i]' is a generator to remove.
  for (size_t j = i+1; j < nrows; ++j) {
    const Generator& g = gs[j];
    if (g[0] != 0 || !g.all_homogeneous_terms_are_zero()) {
      // Found some good stuff.
      std::swap(gs[i], gs[j]);
      // Now, if `i+1 < nrows', `gs[i+1]' is a generator to remove.
      ++i;
    }
  }
  if (i < nrows)
    gs.erase_to_end(i);
}

/*!
  Returns <CODE>true</CODE> if and only if \p *this actually represents
  a system of generators. So, \p *this must satisfy some rule:
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

  // A GenSys must be a valid Matrix.
  if (!Matrix::OK())
    return false;

  if (num_rows() == 0)
    // A valid system of generators can be empty.
    return true;

  bool no_vertex = true;
  for (size_t i = num_rows(); i-- > 0; ) {
    const Generator& g = (*this)[i];

    if (!g.OK())
      return false;

    // Looking for a vertex.
    if (g.is_ray_or_vertex() && g[0] != 0)
      // We found a vertex.
      no_vertex = false;
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
