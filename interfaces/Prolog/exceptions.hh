
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

static void
throw_integer_out_of_range(Prolog_term_ref t) {
  throw integer_out_of_range(t);
}

static void
throw_unknown_interface_error(const char* s) {
  throw unknown_interface_error(s);
}
