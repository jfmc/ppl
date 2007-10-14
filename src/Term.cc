/* Term class implementation (non-inline functions).
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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


#include <ppl-config.h>
#include "Term.defs.hh"
#include "Variables_Set.defs.hh"
#include <stdexcept>

namespace PPL = Parma_Polyhedra_Library;

PPL::dimension_type
PPL::Term::space_dimension() const {
  dimension_type i;
  for (i = vec.size(); i-- > 0; )
    if (vec[i] != 0)
      break;
  return i+1;
}

/*! \relates Parma_Polyhedra_Library::Term */
PPL::Term
PPL::operator*(const Term& t1, const Term& t2) {
  const dimension_type t1_space_dim = t1.space_dimension();
  const dimension_type t2_space_dim = t2.space_dimension();
  dimension_type min_space_dim;
  dimension_type max_space_dim;
  const Term* p_e_max;
  if (t1_space_dim > t2_space_dim) {
    min_space_dim = t2_space_dim;
    max_space_dim = t1_space_dim;
    p_e_max = &t1;
  }
  else {
    min_space_dim = t1_space_dim;
    max_space_dim = t2_space_dim;
    p_e_max = &t2;
  }

  Term r(max_space_dim);
  dimension_type i = max_space_dim;
  while (i > min_space_dim) {
    --i;
    r[i] = (*p_e_max)[i];
  }
  while (i > 0) {
    --i;
    r[i] = t1[i] + t2[i];
  }

  return r;
}

/*! \relates Parma_Polyhedra_Library::Term */
PPL::Term&
PPL::operator*=(Term& t1, const Term& t2) {
  const dimension_type t1_vec_size = t1.vec.size();
  const dimension_type t2_space_dim = t2.space_dimension();
  if (t1_vec_size >= t2_space_dim)
    ;
  else if (t1.vec.capacity() >= t2_space_dim)
    t1.vec.insert(t1.vec.end(), t2_space_dim - t1_vec_size, 0);
  else {
    assert(t1_vec_size < t2_space_dim);
    Term::Vector vec1(t2_space_dim);
    dimension_type i;
    for (i = t2_space_dim; i-- > t1_vec_size; )
      vec1[i] = 0;
    for (++i; i-- > 0; )
      vec1[i] = t1[i];
    std::swap(t1.vec, vec1);
  }
  for (dimension_type i = t2_space_dim; i-- > 0; )
    t1[i] += t2[i];
  return t1;
}

/*! \relates Parma_Polyhedra_Library::Term */
PPL::Term
PPL::operator/(const Term& t1, const Term& t2) {
  const dimension_type t1_space_dim = t1.space_dimension();
  const dimension_type t2_space_dim = t2.space_dimension();
  if (t2_space_dim <= t1_space_dim) {
    dimension_type i;
    for (i = t2_space_dim; i-- > 0; )
      if (t1[i] != t2[i])
	break;
    ++i;
    Term r(i);
    while (i-- > 0)
      if (t1[i] >= t2[i])
	r[i] = t1[i] - t2[i];
      else
	goto error;
    return r;
  }
 error:
  throw std::invalid_argument("PPL::operator/(t1, t2):\n"
			      "t2 does not divide t1.");
}

/*! \relates Parma_Polyhedra_Library::Term */
PPL::Term
PPL::div_pow(const Term& t, const Variable v, exponent_type n) {
  if (n == 0)
    return t;
  const dimension_type v_space_dim = v.space_dimension();
  if (v_space_dim > Term::max_space_dimension())
    throw std::length_error("PPL::operator/(t, v):\n"
			    "v exceeds the maximum allowed space dimension.");
  const dimension_type t_space_dim = t.space_dimension();
  if (v_space_dim <= t_space_dim) {
    dimension_type i = t_space_dim;
    if (t[i] == n) {
      while (i-- > 0)
	if (t[i] != 0)
	  break;
      ++i;
      Term r(t, i);
      return r;
    }
    else if (t[i] > n) {
      Term r(t, i);
      r[i] -= n;
      return r;
    }
  }
  throw std::invalid_argument("PPL::operator/(t, v):\n"
			      "v does not divide t.");
}

/*! \relates Parma_Polyhedra_Library::Term */
PPL::Term&
PPL::operator/=(Term& t1, const Term& t2) {
  const dimension_type t1_space_dim = t1.space_dimension();
  const dimension_type t2_space_dim = t2.space_dimension();
  if (t2_space_dim <= t1_space_dim) {
    for (dimension_type i = t2_space_dim; i-- > 0; )
      if (t1[i] >= t2[i])
	t1[i] -= t2[i];
      else
	goto error;
    return t1;
  }
 error:
  throw std::invalid_argument("PPL::operator/=(t1, t2):\n"
			      "t2 does not divide t1.");
}

/*! \relates Parma_Polyhedra_Library::Term */
PPL::Term&
PPL::div_pow_assign(Term& t, const Variable v, const exponent_type n) {
  if (n == 0)
    return t;
  const dimension_type v_space_dim = v.space_dimension();
  if (v_space_dim > Term::max_space_dimension())
    throw std::length_error("PPL::operator/=(t, v):\n"
			    "v exceeds the maximum allowed space dimension.");
  const dimension_type t_space_dim = t.space_dimension();
  Term::Checked_Exponent_Type& ev = t[v_space_dim-1];
  if (v_space_dim <= t_space_dim && ev >= n) {
    ev -= n;
    return t;
  }
  throw std::invalid_argument("PPL::div_pow_assign(t, v, n):\n"
			      "v does not divide t.");
}

/*! \relates Parma_Polyhedra_Library::Term */
PPL::Term&
PPL::mul_pow_assign(Term& t, const Variable v, const exponent_type n) {
  if (n == 0)
    return t;
  const dimension_type v_space_dim = v.space_dimension();
  if (v_space_dim > Term::max_space_dimension())
    throw std::length_error("PPL::mul_pow_assign(t, v, n):\n"
			    "v exceeds the maximum allowed space dimension.");
  dimension_type t_vec_size = t.vec.size();
  if (t_vec_size >= v_space_dim)
    ;
  else if (t.vec.capacity() >= v_space_dim)
    t.vec.insert(t.vec.end(), v_space_dim - t_vec_size, 0);
  else {
    assert(t_vec_size < v_space_dim);
    Term::Vector vec(v_space_dim);
    dimension_type i;
    for (i = v_space_dim; i-- > t_vec_size; )
      vec[i] = 0;
    for (++i; i-- > 0; )
      vec[i] = t[i];
    std::swap(t.vec, vec);
  }
  t[v_space_dim-1] += n;
  return t;
}

/*! \relates Parma_Polyhedra_Library::Term */
PPL::Term
PPL::mul_pow(const Term& t, const Variable v, const exponent_type n) {
  if (n == 0)
    return t;
  const dimension_type v_space_dim = v.space_dimension();
  if (v_space_dim > Term::max_space_dimension())
    throw std::length_error("PPL::operator*(t, v):\n"
			    "v exceeds the maximum allowed space dimension.");
  dimension_type t_space_dim = t.space_dimension();
  Term u(t, std::max(t_space_dim, v_space_dim));
  u[v_space_dim-1] += n;
  return u;
}

/*! \relates Parma_Polyhedra_Library::Term */
PPL::Term
PPL::operator*(const Variable v, const Variable w) {
  const dimension_type v_space_dim = v.space_dimension();
  const dimension_type w_space_dim = w.space_dimension();
  Term t(std::max(v_space_dim, w_space_dim));
  t[v_space_dim-1] += 1;
  t[w_space_dim-1] += 1;
  return t;
}

/*! \relates Parma_Polyhedra_Library::Term */
PPL::Term&
PPL::pow_assign(Term& t, const exponent_type n) {
  for (dimension_type i = t.space_dimension(); i-- > 0; )
    t[i] *= n;
  return t;
}

/*! \relates Parma_Polyhedra_Library::Term */
PPL::Term&
PPL::exact_div_assign(Term& t1, const Term& t2) {
  assert(t1.space_dimension() >= t2.space_dimension());
  for (dimension_type i = t2.space_dimension(); i-- > 0; ) {
    assert(t1[i] >= t2[i]);
    t1[i] -= t2[i];
  }
  return t1;
}

/*! \relates Parma_Polyhedra_Library::Term */
bool
PPL::operator==(const Term& x, const Term& y) {
  const Term::Vector& x_vec = x.vec;
  const Term::Vector& y_vec = y.vec;
  const dimension_type x_size = x_vec.size();
  const dimension_type y_size = y_vec.size();
  dimension_type i;
  if (x_size < y_size) {
    i = y_size;
    do {
      --i;
      if (y_vec[i] > 0)
	return false;
    } while (i > x_size);
  }
  else if (x_size > y_size) {
    i = x_size;
    do {
      --i;
      if (x_vec[i] > 0)
	return false;
    } while (i > y_size);
  }
  else
    i = x_size;
  while (i > 0) {
    --i;
    if (x_vec[i] != y_vec[i])
      return false;
  }
  return true;
}

/*! \relates Parma_Polyhedra_Library::Term */
bool
PPL::lexicographic_less(const Term& x, const Term& y) {
  const Term::Vector& x_vec = x.vec;
  const Term::Vector& y_vec = y.vec;
  const dimension_type x_size = x_vec.size();
  const dimension_type y_size = y_vec.size();
  dimension_type i;
  if (x_size < y_size) {
    i = y_size;
    do {
      --i;
      if (y_vec[i] > 0)
	return true;
    } while (i > x_size);
  }
  else if (x_size > y_size) {
    i = x_size;
    do {
      --i;
      if (x_vec[i] > 0)
	return false;
    } while (i > y_size);
  }
  else
    i = x_size;
  while (i > 0) {
    --i;
    if (x_vec[i] < y_vec[i])
      return true;
    else if (x_vec[i] > y_vec[i])
      return false;
  }
  return false;
}

/*! \relates Parma_Polyhedra_Library::Term */
bool
PPL::graded_lexicographic_less(const Term& x, const Term& y) {
  const Term::Vector& x_vec = x.vec;
  const Term::Vector& y_vec = y.vec;
  degree_type x_deg = 0;
  degree_type y_deg = 0;
  bool x_lex_less_y;
  bool defined = false;
  const dimension_type x_size = x_vec.size();
  const dimension_type y_size = y_vec.size();
  dimension_type i;
  if (x_size < y_size) {
    i = y_size;
    do {
      --i;
      if (y_vec[i] > 0) {
	x_lex_less_y = true;
	defined = true;
	y_deg += y_vec[i];
      }
    } while (i > x_size);
  }
  else if (x_size > y_size) {
    i = x_size;
    do {
      --i;
      if (x_vec[i] > 0) {
	x_lex_less_y = false;
	defined = true;
	x_deg += x_vec[i];
      }
    } while (i > y_size);
  }
  else
    i = x_size;

  while (i > 0 && !defined) {
    --i;
    if (x_vec[i] < y_vec[i]) {
      x_lex_less_y = true;
      defined = true;
    }
    else if (x_vec[i] > y_vec[i]) {
      x_lex_less_y = false;
      defined = true;
    }
    x_deg += x_vec[i];
    y_deg += y_vec[i];
  }
  if (!defined)
    x_lex_less_y = false;

  while (i > 0) {
    --i;
    x_deg += x_vec[i];
    y_deg += y_vec[i];
  }

  if (x_deg < y_deg)
    return true;
  else if (x_deg > y_deg)
    return false;
  else
    return x_lex_less_y;
}

bool
PPL::Term::OK() const {
  return true;
}

void
PPL::Term::shift_space_dimensions(const Variables_Set& unused) {
  const dimension_type vec_size = vec.size();
  for (Variables_Set::const_iterator ui = unused.begin(),
	 unused_end = unused.end(); ui != unused_end; ++ui) {
    const dimension_type i = *ui;
    if (i >= vec_size)
      break;
    else {
      assert(vec[i] == 0);
      vec[i] = NOT_A_NUMBER;
    }
  }
  dimension_type displacement = 0;
  for (dimension_type i = 0; i < vec_size; ++i)
    if (is_not_a_number(vec[i]))
      ++displacement;
    else
      vec[i - displacement] = vec[i];
  Vector::iterator vec_end = vec.end();
  vec.erase(vec_end - displacement, vec_end);
  assert(OK());
}

PPL::Term::Term(const Term& t, const std::vector<dimension_type>& perm)
  : vec(perm.size(), 0) {
  const dimension_type sz = std::min(t.vec.size(), perm.size());
  for (dimension_type i = sz; i-- > 0; ) {
    assert(perm[i] < vec.size());
    vec[perm[i]] = t.vec[i];
  }
  assert(OK());
}

void
PPL::Term::ascii_dump(std::ostream& s) const {
  s << "t( " << vec.size() << " :";
  for (dimension_type i = 0; i < vec.size(); ++i)
    s << ' ' << vec[i];
  s << " )";
}

PPL_OUTPUT_DEFINITIONS(Term)

bool
PPL::Term::ascii_load(std::istream& s) {
  std::string str;

  if (!(s >> str) || str != "t(")
    return false;

  dimension_type sz;
  if (!(s >> sz))
    return false;

  if (!(s >> str) || (str != ":"))
    return false;

  vec.resize(sz);
  for (dimension_type i = 0; i < sz; ++i)
    if (!(s >> vec[i]))
      return false;

  if (!(s >> str) || (str != ")"))
    return false;

  // Check for well-formedness.
  assert(OK());
  return true;
}

/*! \relates Parma_Polyhedra_Library::Term */
std::ostream&
PPL::IO_Operators::operator<<(std::ostream& s, const Term& t) {
  bool first = true;
  for (int v = 0, t_space_dim = t.space_dimension(); v < t_space_dim; ++v) {
    exponent_type tv = t[v];
    if (tv != 0) {
      first = false;
      s << PPL::Variable(v);
      if (tv != 1)
	s << "^" << tv;
    }
  }
  if (first)
    // The unit term.
    s << Coefficient_one();
  return s;
}
