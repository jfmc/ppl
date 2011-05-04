#ifndef PPL_linearize_int_hh
#define PPL_linearize_int_hh 1

#include "Linear_Form.defs.hh"
namespace Parma_Polyhedra_Library {


template <typename Target, typename Integer_Interval>
static bool
and_linearize_int(const Binary_Operator<Target>& bop_expr,

              const Oracle<Target,Integer_Interval>& oracle,
              const std::map<dimension_type, Linear_Form<Integer_Interval> >& lf_store,
              Linear_Form<Integer_Interval>& result) {
  PPL_ASSERT(bop_expr.binary_operator() == Binary_Operator<Target>::ADD);

  typedef Linear_Form<Integer_Interval> Integer_Linear_Form;
  typedef Box<Integer_Interval> Integer_Interval_Abstract_Store;
  typedef std::map<dimension_type, Integer_Linear_Form> Integer_Linear_Form_Abstract_Store;

  if (!linearize_int(*(bop_expr.left_hand_side()), oracle, lf_store, result))
    return false;

  Integer_Linear_Form linearized_second_operand;

  if (!linearize_int(*(bop_expr.right_hand_side()), oracle, lf_store,
                 linearized_second_operand))
    return false;

  result &= linearized_second_operand;
  return true;
}

template <typename Target, typename Integer_Interval>
static bool
or_linearize_int(const Binary_Operator<Target>& bop_expr,

              const Oracle<Target,Integer_Interval>& oracle,
              const std::map<dimension_type, Linear_Form<Integer_Interval> >& lf_store,
              Linear_Form<Integer_Interval>& result) {
  PPL_ASSERT(bop_expr.binary_operator() == Binary_Operator<Target>::ADD);

  typedef Linear_Form<Integer_Interval> Integer_Linear_Form;
  typedef Box<Integer_Interval> Integer_Interval_Abstract_Store;
  typedef std::map<dimension_type, Integer_Linear_Form> Integer_Linear_Form_Abstract_Store;
  
  if (!linearize_int(*(bop_expr.left_hand_side()), oracle, lf_store, result))
    return false;

  Integer_Linear_Form linearized_second_operand;
  
  if (!linearize_int(*(bop_expr.right_hand_side()), oracle, lf_store,
                 linearized_second_operand))
    return false;

  result |= linearized_second_operand;
  return true;
}

template <typename Target, typename Interval_Type>
bool
linearize_int(const Concrete_Expression<Target>& expr,
          const Oracle<Target,Interval_Type>& oracle,
          const std::map<dimension_type, Linear_Form<Interval_Type> >& lf_store,
          Linear_Form<Interval_Type>& result) {

  typedef typename Interval_Type::boundary_type analyzer_format;
  typedef Linear_Form<Interval_Type> Linear_Form;
  typedef Box<Interval_Type> Interval_Abstract_Store;
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
         case Binary_Operator<Target>::BOR:
           return or_linearize_int(*bop_expr, oracle, lf_store, result);
           break;
         case Binary_Operator<Target>::BAND:
           return and_linearize_int(*bop_expr, oracle, lf_store, result);
           break;

       }
    }
  }
}
} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_linearize_integer_hh)
