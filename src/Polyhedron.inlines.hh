/* Polyhedron class implementation: inline functions.
   Copyright (C) 2001, 2002 Roberto Bagnara <bagnara@cs.unipr.it>

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


#ifndef _Polyhedron_inlines_hh
#define _Polyhedron_inlines_hh 1

#include <algorithm>

namespace Parma_Polyhedra_Library {

inline void
Polyhedron::swap(Polyhedron& y) {
  if (topology() != y.topology())
    throw_topology_incompatible("swap(y)", y);
  std::swap(con_sys, y.con_sys);
  std::swap(gen_sys, y.gen_sys);
  std::swap(sat_c, y.sat_c);
  std::swap(sat_g, y.sat_g);
  std::swap(status, y.status);
  std::swap(space_dim, y.space_dim);
}


/*!
  Returns the topological kind of the polyhedron.
*/
inline Topology
Polyhedron::topology() const {
  // We can check either one of the two matrices.
  // (`con_sys' is slightly better, since it is placed at offset 0.)
  return con_sys.topology();
}

/*!
  Returns <CODE>true</CODE> if and only if \p *this is a
  necessarily closed polyhedron.
*/
inline bool
Polyhedron::is_necessarily_closed() const {
  // We can check either one of the two matrices.
  // (`con_sys' is slightly better, since it is placed at offset 0.)
  return con_sys.is_necessarily_closed();
}

/*!
  Returns <CODE>true</CODE> if \p *this is definitely known to be
  an empty polyhedron.
  Note that the return value <CODE>false</CODE> does not necessarily
  implies that \p *this is non-empty.
*/
inline bool
Polyhedron::is_empty() const {
  return status.test_empty();
}

/*!
  Returns <CODE>true</CODE> if the system of constraints is up-to-date.
*/
inline bool
Polyhedron::constraints_are_up_to_date() const {
  return status.test_c_up_to_date();
}


/*!
  Returns <CODE>true</CODE> if the system of generators is up-to-date.
*/
inline bool
Polyhedron::generators_are_up_to_date() const {
  return status.test_g_up_to_date();
}

/*!
  Returns <CODE>true</CODE> if the system of constraints is minimized.
*/
inline bool
Polyhedron::constraints_are_minimized() const {
  return status.test_c_minimized();
}

/*!
  Returns <CODE>true</CODE> if the system of generators is minimized.
*/
inline bool
Polyhedron::generators_are_minimized() const {
  return status.test_g_minimized();
}

/*!
  Returns <CODE>true</CODE> if \p sat_c is up-to-date.
*/
inline bool
Polyhedron::sat_c_is_up_to_date() const {
  return status.test_sat_c_up_to_date();
}

/*!
  Returns <CODE>true</CODE> if \p sat_g is up-to-date.
*/
inline bool
Polyhedron::sat_g_is_up_to_date() const {
  return status.test_sat_g_up_to_date();
}

/*!
  Sets \p status to express that constraints are up-to-date.
*/
inline void
Polyhedron::set_constraints_up_to_date() {
  status.set_c_up_to_date();
}


/*!
  Sets \p status to express that generators are up-to-date.
*/
inline void
Polyhedron::set_generators_up_to_date() {
  status.set_g_up_to_date();
}

/*!
  Sets \p status to express that constraints are minimized.
*/
inline void
Polyhedron::set_constraints_minimized() {
  set_constraints_up_to_date();
  status.set_c_minimized();
}

/*!
  Sets \p status to express that generators are minimized.
*/
inline void
Polyhedron::set_generators_minimized() {
  set_generators_up_to_date();
  status.set_g_minimized();
}

/*!
  Sets \p status to express that \p sat_c is up-to-date,
  i.e., it actually represents the relations between
  generators and constraints.
*/
inline void
Polyhedron::set_sat_c_up_to_date() {
  status.set_sat_c_up_to_date();
}


/*!
  Sets \p status to express \p sat_g is up-to-date,
  i.e., it actually represents the relations between
  constraints and generators.
*/
inline void
Polyhedron::set_sat_g_up_to_date() {
  status.set_sat_g_up_to_date();
}


/*!
  Clears the status flag indicating that the polyhedron is empty.
*/
inline void
Polyhedron::clear_empty() {
  status.reset_empty();
}


/*!
  Sets \p status to express that \p sat_c is no longer up-to-date.
*/
inline void
Polyhedron::clear_sat_c_up_to_date() {
  status.reset_sat_c_up_to_date();
  // Can get rid of sat_c here.
}


/*!
  Sets \p status to express that \p sat_g is no longer up-to-date.
*/
inline void
Polyhedron::clear_sat_g_up_to_date() {
  status.reset_sat_g_up_to_date();
  // Can get rid of sat_g here.
}

/*!
  Sets \p status to express that constraints are no longer minimized.
*/
inline void
Polyhedron::clear_constraints_minimized() {
  status.reset_c_minimized();
}

/*!
  Sets \p status to express that generators are no longer minimized.
*/
inline void
Polyhedron::clear_generators_minimized() {
  status.reset_g_minimized();
}

/*!
  Sets \p status to express that constraints are no longer up-to-date
  (thus, they are neither minimized and the saturation matrices are no
  longer meaningful).
*/
inline void
Polyhedron::clear_constraints_up_to_date() {
  clear_constraints_minimized();
  clear_sat_c_up_to_date();
  clear_sat_g_up_to_date();
  status.reset_c_up_to_date();
  // Can get rid of con_sys here.
}


/*!
  Sets \p status to express that generators are no longer up-to-date
  (thus, they are neither minimized and the saturation matrices are
  no longer meaningful).
*/
inline void
Polyhedron::clear_generators_up_to_date() {
  clear_generators_minimized();
  clear_sat_c_up_to_date();
  clear_sat_g_up_to_date();
  status.reset_g_up_to_date();
  // Can get rid of gen_sys here.
}

inline bool
operator==(const Polyhedron& x, const Polyhedron& y) {
  return x <= y && y <= x;
}

inline bool
operator!=(const Polyhedron& x, const Polyhedron& y) {
  return !(x == y);
}

inline bool
operator>=(const Polyhedron& x, const Polyhedron& y) {
  return y <= x;
}

inline bool
operator<(const Polyhedron& x, const Polyhedron& y) {
  return x <= y && !(x >= y);
}

inline bool
operator>(const Polyhedron& x, const Polyhedron& y) {
  return y < x;
}


inline size_t
Polyhedron::space_dimension() const {
  return space_dim;
}

inline bool
Polyhedron::check_empty() const {
  minimize();
  return is_empty();
}

template <class Box>
Polyhedron::Polyhedron(Topology topol, const Box& box)
  : con_sys(topol),
    gen_sys(topol),
    sat_c(),
    sat_g() {
  // Initialize the space dimension as indicated by the box.
  space_dim = box.space_dimension();

  // Check for emptyness.
  if (box.is_empty()) {
    set_empty();
    return;
  }

  // Zero-dim universe polyhedron.
  if (space_dim == 0) {
    set_zero_dim_univ();
    return;
  }

  // Insert a dummy constraint of the highest dimension to avoid the
  // need of resizing the matrix of constraints later;
  // this constraint will be removed at the end.
  con_sys.insert(Variable(space_dim - 1) > 0);

  for (size_t k = space_dim; k-- > 0; ) {
    // See if we have a valid lower bound.
    bool l_closed = false;
    Integer l_n, l_d;
    bool l_bounded = box.get_lower_bound(k, l_closed, l_n, l_d);
    if (l_bounded && topol == NECESSARILY_CLOSED && !l_closed)
      throw_generic("C_Polyhedron(const Box& box)",
		    "box has an open lower bound");
    // See if we have a valid upper bound.
    bool u_closed = false;
    Integer u_n, u_d;
    bool u_bounded = box.get_upper_bound(k, u_closed, u_n, u_d);
    if (u_bounded && topol == NECESSARILY_CLOSED && !u_closed)
      throw_generic("C_Polyhedron(const Box& box)",
		    "box has an open upper bound");

    // See if we have an implicit equality constraint.
    if (l_bounded && u_bounded
	&& l_closed && u_closed
	&& l_n == u_n && l_d == u_d) {
      // Add the constraint `l_d*v_k == l_n'.
      con_sys.insert(l_d * Variable(k) == l_n);
    }
    else {
      // Check if a lower bound constraint is required.
      if (l_bounded) {
       if (l_closed)
	 // Add the constraint `l_d*v_k >= l_n'.
	 con_sys.insert(l_d * Variable(k) >= l_n);
       else
	 // Add the constraint `l_d*v_k > l_n'.
	 con_sys.insert(l_d * Variable(k) > l_n);
      }
      // Check if an upper bound constraint is required.
      if (u_bounded) {
       if (u_closed)
	 // Add the constraint `u_d*v_k <= u_n'.
	 con_sys.insert(u_d * Variable(k) <= u_n);
       else
	 // Add the constraint `u_d*v_k < u_n'.
	 con_sys.insert(u_d * Variable(k) < u_n);
      }
    }
  }

  if (topol == NECESSARILY_CLOSED)
    // Adding the positivity constraint.
    con_sys.insert(Constraint::zero_dim_positivity());
  else {
    // Polyhedron NOT-necessarily closed:
    // adding the \epsilon dimension constraints.
    con_sys.insert(Constraint::epsilon_leq_one());
    con_sys.insert(Constraint::epsilon_geq_zero());
  }
  
  // Now removing the dummy constraint inserted before.
  size_t n_rows = con_sys.num_rows() - 1;
  con_sys[0].swap(con_sys[n_rows]);
  con_sys.erase_to_end(n_rows);

  // Constraints are up-to-date.
  set_constraints_up_to_date();
  assert(OK());
}

template <class Box>
void
Polyhedron::shrink_bounding_box(Box& box) const {
  if (check_universe())
    return;

  if (check_empty()) {
    box.set_empty();
    return;
  }

  if (space_dim == 0)
    return;

  // If finite, the maximum and minimum values
  // for each variable are used to construct constraints
  // bounding the sides of the box.

  // To find the max and min, we construct two vectors of size =
  // space_dimension of *this and whose elements are pointers to the
  // generators for *this. The vector mx records, for each variable, a
  // generator that has the maximum value for this variable, or is
  // unbounded in the positive direction. The vector mn records, for
  // each variable, a generator that has the minimum value for this
  // variable, or is unbounded in the negative direction.
  std::vector<GenSys::const_iterator> mx(space_dim);
  std::vector<GenSys::const_iterator> mn(space_dim);

  // Get the generators for *this
  const GenSys& gs = generators();
  const GenSys::const_iterator gs_begin = gs.begin();
  const GenSys::const_iterator gs_end = gs.end();

  // We first need a point in *this so as to initialize the maximum and
  // minimum to the index of a point.
  for (GenSys::const_iterator i = gs_begin; i != gs_end; ++i) {
    if (i->is_point()) {
      // Initialize the maximum and minimum vectors to the current point.
      for (size_t j = space_dim; j-- > 0; )
	mn[j] = mx[j] = i;
      break;
    }
  }

  // Now go through all the generators and record those with the max
  // and min values of each coefficient divided by its divisor.
  for (GenSys::const_iterator i = gs_begin; i != gs_end; ++i) {
    const Generator& generator = *i;
    if (generator.is_closure_point()) {
      // compare the max and min with the current values
      const Integer& div = generator.divisor();
      for (size_t j = space_dim; j-- > 0; ) {
        Variable v(j);
        const Integer& coeff = generator.coefficient(v);
        const Generator& jmx_generator = *(mx[j]);
        const Generator& jmn_generator = *(mn[j]);
        // if jmx_generator is a ray or line, then the upper bound
        // remains infinite and we do nothing.
        if ((jmx_generator.is_point() || jmx_generator.is_closure_point())  &&
	    // Compare the product of the coefficient of the generator at
	    // max/min index and the divisor of current generator with the
	    // product of the coefficient of current generator and the
	    // divisor of the generator at max/min index.
	    coeff * jmx_generator.divisor()
	    > jmx_generator.coefficient(v) * div)
	  mx[j] = i;
        // if jmn_generator is a ray or line, then the lower bound
        // remains infinite and we do nothing.
        if ((jmn_generator.is_point() || jmn_generator.is_closure_point()) &&
	    // Compare the product of the coefficient of the generator at
	    // max/min index and the divisor of current generator with the
	    // product of the coefficient of current generator and the
	    // divisor of the generator at max/min index.
	    coeff * jmn_generator.divisor()
	    < jmn_generator.coefficient(v) * div)
	  mn[j] = i;
      }
    }
    else if (generator.is_point()) {
      // compare the max and min with the current values
      const Integer& div = generator.divisor();
      for (size_t j = space_dim; j-- > 0; ) {
        Variable v(j);
        const Integer& coeff = generator.coefficient(v);
        const Generator& jmx_generator = *(mx[j]);
        const Generator& jmn_generator = *(mn[j]);
        // if jmx_generator is a ray or line, then the upper bound
        // remains infinite and we do nothing.
        if ((jmx_generator.is_point() || jmx_generator.is_closure_point()) &&
	    // Compare the product of the coefficient of the generator at
	    // max/min index and the divisor of current generator with the
	    // product of the coefficient of current generator and the
	    // divisor of the generator at max/min index.
	    coeff * jmx_generator.divisor()
	    >= jmx_generator.coefficient(v) * div)
	  mx[j] = i;
        // if jmn_generator is a ray or line, then the lower bound
        // remains infinite and we do nothing.
        if ((jmn_generator.is_point() || jmn_generator.is_closure_point()) &&
	    // Compare the product of the coefficient of the generator at
	    // max/min index and the divisor of current generator with the
	    // product of the coefficient of current generator and the
	    // divisor of the generator at max/min index.
	    coeff * jmn_generator.divisor()
	    <= jmn_generator.coefficient(v) * div)
	  mn[j] = i;
      }
    }
    else if (generator.is_ray()) {
      // In this case, we consider any axes j in which the coefficient
      // is non-zero: if the coefficient for j is positive, then the
      // j'th element of the mx vector is updated and, if it is
      // negative, the j'th element of the mn vector is updated
      // with the current generator.
      for (size_t j = space_dim; j-- > 0; ) {
        Variable v(j);
        const Integer& coeff = generator.coefficient(v);
        if (coeff > 0)
	  mx[j] = i;
        else if (coeff < 0)
	  mn[j] = i;
      }
    }
    else {
      assert(generator.is_line());
      // In this case, we consider any axes j in which the coefficient
      // is non-zero: the j'th element of the mx vector and the
      // mn vector are updated with the current generator.
      for (size_t j = space_dim; j-- > 0; ) {
        Variable v(j);
        if (generator.coefficient(v) != 0) {
          mx[j] = i;
          mn[j] = i;
	}
      }
    }
  }
  
  // To construct the bounding box.
  // Now adjust the constraints bounding the sides of the box.
  for (size_t j  = space_dim; j-- > 0; ) {
    Variable v(j);
    const Generator& jmx_generator = *(mx[j]);
    const Generator& jmn_generator = *(mn[j]);
    if (jmx_generator.is_closure_point()) {
      box.lower_upper_bound(j, false,
			    jmx_generator.coefficient(v),
			    jmx_generator.divisor());
    }
    else if (jmx_generator.is_point()) {  
      box.lower_upper_bound(j, true,
			    jmx_generator.coefficient(v),
			    jmx_generator.divisor());
    } 
    if (jmn_generator.is_closure_point()) {
      box.raise_lower_bound(j, false,
			    jmn_generator.coefficient(v),
			    jmn_generator.divisor());
    }
    else if (jmn_generator.is_point()) {
      box.raise_lower_bound(j, true,
			    jmn_generator.coefficient(v),
			    jmn_generator.divisor());
    }
  }
}

} // namespace Parma_Polyhedra_Library


/*!
  Specializes <CODE>std::swap</CODE> to use the fast swap that is provided
  as a member function instead of using the default algorithm
  (which creates a temporary and uses assignment).
*/
inline void
std::swap(Parma_Polyhedra_Library::Polyhedron& x,
	  Parma_Polyhedra_Library::Polyhedron& y) {
  x.swap(y);
}


#endif
