#ifndef PPL_linearize_int_hh
#define PPL_linearize_int_hh 1

#include "Linear_Form.defs.hh"

namespace Parma_Polyhedra_Library {


template <typename Target, typename Intero_Interval>
static bool
add_linearize_int(const Binary_Operator<Target>& bop_expr,
              const Oracle<Target,Intero_Interval>& oracle,
              const std::map<dimension_type, Linear_Form<Intero_Interval> >& lf_store,
              Linear_Form<Intero_Interval>& result) {
  PPL_ASSERT(bop_expr.binary_operator() == Binary_Operator<Target>::ADD);

  typedef Linear_Form<Intero_Interval> Integer_Linear_Form;

  if (!linearize_int(*(bop_expr.left_hand_side()), oracle, lf_store, result))
    return false;

  Integer_Linear_Form linearized_second_operand;

  if (!linearize_int(*(bop_expr.right_hand_side()), oracle, lf_store,
                 linearized_second_operand))
    return false;

  result += linearized_second_operand;
  return true;
}

template <typename Target, typename Intero_Interval>
static bool
sub_linearize_int(const Binary_Operator<Target>& bop_expr,
              const Oracle<Target,Intero_Interval>& oracle,
              const std::map<dimension_type, Linear_Form<Intero_Interval> >& lf_store,
              Linear_Form<Intero_Interval>& result) {
  PPL_ASSERT(bop_expr.binary_operator() == Binary_Operator<Target>::SUB);
  
  typedef Linear_Form<Intero_Interval> Integer_Linear_Form;

  if (!linearize_int(*(bop_expr.left_hand_side()), oracle, lf_store, result))
    return false;

  Integer_Linear_Form linearized_second_operand;
  if (!linearize_int(*(bop_expr.right_hand_side()), oracle, lf_store,
                 linearized_second_operand))
    return false;

  result -= linearized_second_operand;
  return true;
}

template <typename Target, typename Intero_Interval>
static bool
mul_linearize_int(const Binary_Operator<Target>& bop_expr,
              const Oracle<Target,Intero_Interval>& oracle,
              const std::map<dimension_type, Linear_Form<Intero_Interval> >& lf_store,
              Linear_Form<Intero_Interval>& result) {
  PPL_ASSERT(bop_expr.binary_operator() == Binary_Operator<Target>::MUL);

  typedef typename Intero_Interval::boundary_type analyzer_format;
  typedef Linear_Form<Intero_Interval> Integer_Linear_Form;
  
  /*
    FIXME: We currently adopt the "Interval-Size Local" strategy in order to
    decide which of the two linear forms must be intervalized, as described
    in Section 6.2.4 ("Multiplication Strategies") of Antoine Mine's Ph.D.
    thesis "Weakly Relational Numerical Abstract Domains".
    In this Section are also described other multiplication strategies, such
    as All-Cases, Relative-Size Local, Simplification-Driven Global and
    Homogeneity Global.
  */

  // Here we choose which of the two linear forms must be intervalized.

  // true if we intervalize the first form, false if we intervalize the second.
  bool intervalize_first;
  Integer_Linear_Form linearized_first_operand;
  if (!linearize_int(*(bop_expr.left_hand_side()), oracle, lf_store,
                 linearized_first_operand))
    return false;
  Intero_Interval intervalized_first_operand;
  if (!linearized_first_operand.intervalize(oracle, intervalized_first_operand))
    return false;

  Integer_Linear_Form linearized_second_operand;
  if (!linearize_int(*(bop_expr.right_hand_side()), oracle, lf_store,
                 linearized_second_operand))
    return false;
  Intero_Interval intervalized_second_operand;
  if (!linearized_second_operand.intervalize(oracle,
                                             intervalized_second_operand))
    return false;
   
  analyzer_format first_interval_size, second_interval_size;

  // FIXME: we are not sure that what we do here is policy-proof.
  if (intervalized_first_operand.is_bounded()) {
    if (intervalized_second_operand.is_bounded()) {
      first_interval_size = intervalized_first_operand.upper() -
                            intervalized_first_operand.lower();
      second_interval_size = intervalized_second_operand.upper() -
                             intervalized_second_operand.lower();
      if (first_interval_size <= second_interval_size)
        intervalize_first = true;
      else
        intervalize_first = false;
    }
    else
      intervalize_first = true;
  }
  else {
    if (intervalized_second_operand.is_bounded())
      intervalize_first = false;
    else
      return false;
  }

  // Here we do the actual computation.
  // For optimizing, we store the relative error directly into result.
  
  if (intervalize_first) {
    linearized_second_operand *= intervalized_first_operand;
    result *= intervalized_first_operand;
    result += linearized_second_operand;
  }
  else {
    linearized_first_operand *= intervalized_second_operand;
    result *= intervalized_second_operand;
    result += linearized_first_operand;
  }
  return true;
}

template <typename Target, typename Intero_Interval>
static bool
div_linearize_int(const Binary_Operator<Target>& bop_expr,
              const Oracle<Target,Intero_Interval>& oracle,
              const std::map<dimension_type, Linear_Form<Intero_Interval> >& lf_store,
              Linear_Form<Intero_Interval>& result) {
  PPL_ASSERT(bop_expr.binary_operator() == Binary_Operator<Target>::DIV);

  typedef Linear_Form<Intero_Interval> Integer_Linear_Form;

  Integer_Linear_Form linearized_second_operand;
  if (!linearize_int(*(bop_expr.right_hand_side()), oracle, lf_store,
                 linearized_second_operand))
    return false;
  Intero_Interval intervalized_second_operand;
  if (!linearized_second_operand.intervalize(oracle,intervalized_second_operand))
    return false;

  // Check if we may divide by zero.
  if ((intervalized_second_operand.lower_is_boundary_infinity() ||
       intervalized_second_operand.lower() <= 0) &&
      (intervalized_second_operand.upper_is_boundary_infinity() ||
       intervalized_second_operand.upper() >= 0))
    return false;

  if (!linearize_int(*(bop_expr.left_hand_side()), oracle, lf_store, result))
    return false;
  
  result /= intervalized_second_operand;
  return true;
}

template <typename Target, typename Intero_Interval>
static bool
or_linearize_int(const Binary_Operator<Target>& bop_expr,
              const Oracle<Target,Intero_Interval>& oracle,
              const std::map<dimension_type, Linear_Form<Intero_Interval> >& lf_store,
              Linear_Form<Intero_Interval>& result) {
  PPL_ASSERT(bop_expr.binary_operator() == Binary_Operator<Target>::BOR);

  typedef Linear_Form<Intero_Interval> Integer_Linear_Form;
  
  if (!linearize_int(*(bop_expr.left_hand_side()), oracle, lf_store,result))
    return false;
  Integer_Linear_Form linearized_second_operand;

  if (!linearize_int(*(bop_expr.right_hand_side()), oracle, lf_store, linearized_second_operand))
    return false;

  result |= linearized_second_operand;

  return !result.overflows();
}
      
template <typename Target, typename Intero_Interval>
static bool
and_linearize_int(const Binary_Operator<Target>& bop_expr,

              const Oracle<Target,Intero_Interval>& oracle,
              const std::map<dimension_type, Linear_Form<Intero_Interval> >& lf_store,
              Linear_Form<Intero_Interval>& result) {
  PPL_ASSERT(bop_expr.binary_operator() == Binary_Operator<Target>::BAND);

  typedef Linear_Form<Intero_Interval> Integer_Linear_Form;

  Integer_Linear_Form linearized_first_operand;
  if (!linearize_int(*(bop_expr.left_hand_side()), oracle, lf_store,
       result))
    return false;
  
  Integer_Linear_Form linearized_second_operand;
  if (!linearize_int(*(bop_expr.right_hand_side()), oracle, lf_store,
       linearized_second_operand))
    return false;

  result &= linearized_second_operand;

  return !result.overflows();

}

template <typename Target, typename Intero_Interval>
static bool
xor_linearize_int(const Binary_Operator<Target>& bop_expr,

              const Oracle<Target,Intero_Interval>& oracle,
              const std::map<dimension_type, Linear_Form<Intero_Interval> >& lf_store,
              Linear_Form<Intero_Interval>& result) {
  PPL_ASSERT(bop_expr.binary_operator() == Binary_Operator<Target>::BXOR);

  typedef Linear_Form<Intero_Interval> Integer_Linear_Form;

  if (!linearize_int(*(bop_expr.left_hand_side()), oracle, lf_store,
       result))
    return false;

  Integer_Linear_Form linearized_second_operand;
  if (!linearize_int(*(bop_expr.right_hand_side()), oracle, lf_store,
       linearized_second_operand))
    return false;
  
  result ^= linearized_second_operand;

  return !result.overflows();

}

template <typename Target, typename Intero_Interval>
static bool
lshift_linearize_int(const Binary_Operator<Target>& bop_expr,

              const Oracle<Target,Intero_Interval>& oracle,
              const std::map<dimension_type, Linear_Form<Intero_Interval> >& lf_store,
              Linear_Form<Intero_Interval>& result) {
  PPL_ASSERT(bop_expr.binary_operator() == Binary_Operator<Target>::LSHIFT);

  typedef Linear_Form<Intero_Interval> Integer_Linear_Form;

  if (!linearize_int(*(bop_expr.left_hand_side()), oracle, lf_store, result))
    return false;

  Integer_Linear_Form linearized_second_operand;

  if (!linearize_int(*(bop_expr.right_hand_side()), oracle, lf_store,
                 linearized_second_operand))
    return false;

  result << linearized_second_operand;
  return true;
}

template <typename Target, typename Intero_Interval>
static bool
rshift_linearize_int(const Binary_Operator<Target>& bop_expr,

              const Oracle<Target,Intero_Interval>& oracle,
              const std::map<dimension_type, Linear_Form<Intero_Interval> >& lf_store,
              Linear_Form<Intero_Interval>& result) {
  PPL_ASSERT(bop_expr.binary_operator() == Binary_Operator<Target>::RSHIFT);

  typedef Linear_Form<Intero_Interval> Integer_Linear_Form;

  if (!linearize_int(*(bop_expr.left_hand_side()), oracle, lf_store, result))
    return false;

  Integer_Linear_Form linearized_second_operand;

  if (!linearize_int(*(bop_expr.right_hand_side()), oracle, lf_store,
                 linearized_second_operand))
    return false;
  
  result >> linearized_second_operand;
  return true;
}

template <typename Target, typename Interval_Type>
bool
linearize_int(const Concrete_Expression<Target>& expr,
          const Oracle<Target,Interval_Type>& oracle,
          const std::map<dimension_type, Linear_Form<Interval_Type> >& lf_store,
          Linear_Form<Interval_Type>& result) {
  
  typedef Linear_Form<Interval_Type> Linear_Form;
  typedef std::map<dimension_type, Linear_Form> Linear_Form_Abstract_Store;

  PPL_ASSERT(expr.type().is_bounded_integer());
  switch(expr.kind()){
    case Int_Constant<Target>::KIND: {
      const Int_Constant<Target>* intc_expr =
        expr.template as<Int_Constant>();
      Interval_Type constant_value;
        if (!oracle.get_int_constant_value(*intc_expr, constant_value))
          return false;
        result = Linear_Form(constant_value);
          return true;
      break;
    }
    case Binary_Operator<Target>::KIND:{
      const Binary_Operator<Target>* bop_expr =
        expr.template as<Binary_Operator>();
      switch (bop_expr->binary_operator()) {
        case Binary_Operator<Target>::ADD:
          return add_linearize_int(*bop_expr, oracle, lf_store, result);
        case Binary_Operator<Target>::SUB:
          return sub_linearize_int(*bop_expr, oracle, lf_store, result);
        case Binary_Operator<Target>::MUL:
          return mul_linearize_int(*bop_expr, oracle, lf_store, result);
        case Binary_Operator<Target>::DIV:
          return div_linearize_int(*bop_expr, oracle, lf_store, result);
        case Binary_Operator<Target>::BOR:
          return or_linearize_int(*bop_expr, oracle, lf_store, result);
          break;
        case Binary_Operator<Target>::BAND:
          return and_linearize_int(*bop_expr, oracle, lf_store, result);
          break;
        case Binary_Operator<Target>::BXOR:
          return xor_linearize_int(*bop_expr, oracle, lf_store, result);
          break;
        case Binary_Operator<Target>::LSHIFT:
          return lshift_linearize_int(*bop_expr, oracle, lf_store, result);
          break;
        case Binary_Operator<Target>::RSHIFT:
          return rshift_linearize_int(*bop_expr, oracle, lf_store, result);
          break;
        default:
    	      throw std::runtime_error("PPL internal error: unreachable");
      }
      break;
    }
    case Approximable_Reference<Target>::KIND: {
      const Approximable_Reference<Target>* ref_expr =
        expr.template as<Approximable_Reference>();
      std::set<dimension_type> associated_dimensions;
        if (!oracle.get_associated_dimensions(*ref_expr, associated_dimensions)
          || associated_dimensions.empty())
          return false;
      
      if (associated_dimensions.size() == 1) {
        dimension_type variable_index = *associated_dimensions.begin();
        PPL_ASSERT(variable_index != not_a_dimension());
  
        typename Linear_Form_Abstract_Store::const_iterator
                 variable_value = lf_store.find(variable_index);
        if (variable_value == lf_store.end()) {
          result = Linear_Form(Variable(variable_index));
          return true;
        }
        /*
        result = Linear_Form(variable_value->second);
  
        return !result.overflows();*/
      }
  /*
      PPL_ASSERT(associated_dimensions.size() > 1);
      std::set<dimension_type>::const_iterator i = associated_dimensions.begin();
      std::set<dimension_type>::const_iterator i_end =
        associated_dimensions.end();
      Interval_Type lub(EMPTY);
      for (; i != i_end; ++i) {
        Interval_Type curr_int;
        PPL_ASSERT(*i != not_a_dimension());
        if (!oracle.get_interval(*i, curr_int))
          return false;

        lub.join_assign(curr_int);
      }
      result = Linear_Form(lub);
      return !result.overflows();
    */
      break;
    }//end case Approx_Reference<Target>::KIND
    default:
      throw std::runtime_error("PPL internal error");
  }

  PPL_ASSERT(false);

}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_linearize_integer_hh)
