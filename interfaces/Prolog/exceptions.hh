/* Exceptions used internally by the Prolog interfaces.
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

class internal_exception {
private:
  Prolog_term_ref tr;

public:
  explicit internal_exception(Prolog_term_ref t)
    : tr(t) {
  }

  virtual ~internal_exception() {
  }

  virtual Prolog_term_ref term() const {
    return tr;
  }
};

class integer_out_of_range : public internal_exception {
public:
  explicit integer_out_of_range(Prolog_term_ref t)
    : internal_exception(t) {
  }
};

class non_linear : public internal_exception {
private:
  const char* w;

public:
  explicit non_linear(const char* s, Prolog_term_ref t)
    : internal_exception(t), w(s) {
  }

  const char* who() const {
    return w;
  }
};

class not_an_integer : public internal_exception {
public:
  explicit not_an_integer(Prolog_term_ref t)
    : internal_exception(t) {
  }
};

class not_unsigned_int : public internal_exception {
public:
  explicit not_unsigned_int(Prolog_term_ref t)
    : internal_exception(t) {
  }
};

class not_a_variable : public internal_exception {
public:
  explicit not_a_variable(Prolog_term_ref t)
    : internal_exception(t) {
  }
};

class not_a_polyhedron_kind : public internal_exception {
public:
  explicit not_a_polyhedron_kind(Prolog_term_ref t)
    : internal_exception(t) {
  }
};

class not_a_polyhedron_handle : public internal_exception {
public:
  explicit not_a_polyhedron_handle(Prolog_term_ref t)
    : internal_exception(t) {
  }
};

class unknown_interface_error {
private:
  const char* w;

public:
  unknown_interface_error(const char* s)
    : w(s) {
  }

  const char* where() const {
    return w;
  }
};

#if 0
static void
throw_integer_out_of_range(Prolog_term_ref t) {
  throw integer_out_of_range(t);
}
#endif

static void
throw_unknown_interface_error(const char* s) {
  throw unknown_interface_error(s);
}
