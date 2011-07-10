#ifndef PPL_linearize_integer_hh
#define PPL_linearize_integer_hh 1

#include "Linear_Form.defs.hh"

namespace Parma_Polyhedra_Library {

/*! \brief
  Helper function used by <CODE>linearize_int</CODE> to linearize a
  sum of integer expressions.

  \par Template type parameters

  - The class template parameter \p Target specifies the implementation
  of Concrete_Expression to be used.
  - The class template parameter \p Integer_Int_Interval represents the type
  of the intervals used in the abstract domain. The interval bounds
  should have a integer type.

  Makes \p result become the linearization of \p *this in the given
  composite abstract store.

  \param bop_expr The binary operator concrete expression to linearize.
  Its binary operator type must be <CODE>ADD</CODE>.
  \param oracle The Oracle to be queried.
  \param lf_store The linear form abstract store.
  \param result The modified linear form.

  \return <CODE>true</CODE> if the linearization succeeded,
  <CODE>false</CODE> otherwise.

  \par Linearization of sum integer expressions

  Let \f$i + \sum_{v \in \cV}i_{v}v \f$ and
  \f$i' + \sum_{v \in \cV}i'_{v}v \f$
  be two linear forms and \f$\aslf\f$ a sound abstract operator on linear
  forms such that:

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \aslf
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)
  =
  \left(i \asifp i'\right) + \sum_{v \in \cV}\left(i_{v} \asifp i'_{v} \right)v.
  \f]

  Given an expression \f$e_{1} \oplus e_{2}\f$ and a composite
  abstract store \f$\left \llbracket \rho^{\#}, \rho^{\#}_l \right
  \rrbracket\f$, we construct the interval linear form
  \f$\linexprenv{e_{1} \oplus e_{2}}{\rho^{\#}}{\rho^{\#}_l}\f$
  as follows:
  \f[
  \linexprenv{e_{1} \oplus e_{2}}{\rho^{\#}}{\rho^{\#}_l}
  =
  \linexprenv{e_{1}}{\rho^{\#}}{\rho^{\#}_l}
  \aslf
  \linexprenv{e_{2}}{\rho^{\#}}{\rho^{\#}_l}
  \f]
*/
template <typename Target, typename Integer_Int_Interval>
static bool
add_linearize_int
(const Binary_Operator<Target>& bop_expr,
 const Oracle<Target,Integer_Int_Interval>& oracle,
 const std::map<dimension_type, Linear_Form<Integer_Int_Interval> >& lf_store,
 Linear_Form<Integer_Int_Interval>& result) {
  PPL_ASSERT(bop_expr.binary_operator() == Binary_Operator<Target>::ADD);

  typedef Linear_Form<Integer_Int_Interval> Integer_Linear_Form;

  if (!linearize_int(*(bop_expr.left_hand_side()), oracle, lf_store, result))
    return false;

  Integer_Linear_Form linearized_second_operand;
  if (!linearize_int(*(bop_expr.right_hand_side()), oracle, lf_store,
		     linearized_second_operand))
    return false;

  result += linearized_second_operand;

  return true;
}

/*! \brief
  Helper function used by <CODE>linearize_int</CODE> to linearize a
  difference of integer expressions.

  \par Template type parameters

  - The class template parameter \p Target specifies the implementation
  of Concrete_Expression to be used.
  - The class template parameter \p Integer_Int_Interval represents the type
  of the intervals used in the abstract domain. The interval bounds
  should have a integer type.

  Makes \p result become the linearization of \p *this in the given
  composite abstract store.

  \param bop_expr The binary operator concrete expression to linearize.
  Its binary operator type must be <CODE>SUB</CODE>.
  \param oracle The Oracle to be queried.
  \param lf_store The linear form abstract store.
  \param result The modified linear form.

  \return <CODE>true</CODE> if the linearization succeeded,
  <CODE>false</CODE> otherwise.

  \par Linearization of difference integer expressions

  Let \f$i + \sum_{v \in \cV}i_{v}v \f$ and
  \f$i' + \sum_{v \in \cV}i'_{v}v \f$
  be two linear forms, \f$\aslf\f$ and \f$\adlf\f$ two sound abstract
  operators on linear form such that:

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v\right)
  \aslf
  \left(i' + \sum_{v \in \cV}i'_{v}v\right)
  =
  \left(i \asifp i'\right)
  + \sum_{v \in \cV}\left(i_{v} \asifp i'_{v}\right)v,
  \f]

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v\right)
  \adlf
  \left(i' + \sum_{v \in \cV}i'_{v}v\right)
  =
  \left(i \adifp i'\right) + \sum_{v \in \cV}\left(i_{v} \adifp i'_{v}\right)v.
  \f]

  Given an expression \f$e_{1} \ominus e_{2}\f$ and a composite
  abstract store \f$\left \llbracket \rho^{\#}, \rho^{\#}_l \right
  \rrbracket\f$,  we construct the interval linear form
  \f$\linexprenv{e_{1} \ominus e_{2}}{\rho^{\#}}{\rho^{\#}_l}\f$
  on \f$\cV\f$ as follows:
  \f[
  \linexprenv{e_{1} \ominus e_{2}}{\rho^{\#}}{\rho^{\#}_l}
  =
  \linexprenv{e_{1}}{\rho^{\#}}{\rho^{\#}_l}
  \adlf
  \linexprenv{e_{2}}{\rho^{\#}}{\rho^{\#}_l}
  \f]
*/
template <typename Target, typename Integer_Int_Interval>
static bool
sub_linearize_int
(const Binary_Operator<Target>& bop_expr,
 const Oracle<Target,Integer_Int_Interval>& oracle,
 const std::map<dimension_type, Linear_Form<Integer_Int_Interval> >& lf_store,
 Linear_Form<Integer_Int_Interval>& result) {
  PPL_ASSERT(bop_expr.binary_operator() == Binary_Operator<Target>::SUB);

  typedef Linear_Form<Integer_Int_Interval> Integer_Linear_Form;

  if (!linearize_int(*(bop_expr.left_hand_side()), oracle, lf_store, result))
    return false;

  Integer_Linear_Form linearized_second_operand;
  if (!linearize_int(*(bop_expr.right_hand_side()), oracle, lf_store,
		     linearized_second_operand))
    return false;

  result -= linearized_second_operand;

  return true;
}

/*! \brief
  Helper function used by <CODE>linearize_int</CODE> to linearize a
  product of integer expressions.

  \par Template type parameters

  - The class template parameter \p Target specifies the implementation
  of Concrete_Expression to be used.
  - The class template parameter \p Integer_Int_Interval represents the type
  of the intervals used in the abstract domain. The interval bounds
  should have a integer type.

  Makes \p result become the linearization of \p *this in the given
  composite abstract store.

  \param bop_expr The binary operator concrete expression to linearize.
  Its binary operator type must be <CODE>MUL</CODE>.
  \param oracle The Oracle to be queried.
  \param lf_store The linear form abstract store.
  \param result The modified linear form.

  \return <CODE>true</CODE> if the linearization succeeded,
  <CODE>false</CODE> otherwise.

  \par Linearization of multiplication integer expressions

  Let \f$i + \sum_{v \in \cV}i_{v}v \f$ and
  \f$i' + \sum_{v \in \cV}i'_{v}v \f$
  be two linear forms, \f$\aslf\f$ and \f$\amlf\f$ two sound abstract
  operators on linear forms such that:

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v\right)
  \aslf
  \left(i' + \sum_{v \in \cV}i'_{v}v\right)
  =
  \left(i \asifp i'\right) + \sum_{v \in \cV}\left(i_{v} \asifp i'_{v}\right)v,
  \f]

  \f[
  i
  \amlf
  \left(i' + \sum_{v \in \cV}i'_{v}v\right)
  =
  \left(i \amifp i'\right)
  + \sum_{v \in \cV}\left(i \amifp i'_{v}\right)v.
  \f]

  Given an expression \f$[a;b] \otimes e_{2}\f$ and a composite
  abstract store \f$\left \llbracket \rho^{\#}, \rho^{\#}_l \right
  \rrbracket\f$, we construct the interval linear form
  \f$\linexprenv{[a;b] \otimes e_{2}}{\rho^{\#}}{\rho^{\#}_l}\f$
  as follows:

  \f[
  \linexprenv{[a;b] \otimes e_{2}}{\rho^{\#}}{\rho^{\#}_l}
  =
  \left([a;b]
  \amlf
  \linexprenv{e_{2}}{\rho^{\#}}{\rho^{\#}_l}\right)
  \f].

  Given an expression \f$e_{1} \otimes [a;b]\f$ and a composite
  abstract store \f$\left \llbracket \rho^{\#}, \rho^{\#}_l \right
  \rrbracket\f$, we construct the interval linear form
  \f$\linexprenv{e_{1} \otimes [a;b]}{\rho^{\#}}{\rho^{\#}_l}\f$
  as follows:

  \f[
  \linexprenv{e_{1} \otimes [a;b]}{\rho^{\#}}{\rho^{\#}_l}
  =
  \linexprenv{[a;b] \otimes e_{1}}{\rho^{\#}}{\rho^{\#}_l}.
  \f]

  Given an expression \f$e_{1} \otimes e_{2}\f$ and a composite
  abstract store \f$\left \llbracket \rho^{\#}, \rho^{\#}_l \right
  \rrbracket\f$, we construct the interval linear form
  \f$\linexprenv{e_{1} \otimes e_{2}}{\rho^{\#}}{\rho^{\#}_l}\f$
  as follows:

  \f[
  \linexprenv{e_{1} \otimes e_{2}}{\rho^{\#}}{\rho^{\#}_l}
  =
  \linexprenv{\iota\left(\linexprenv{e_{1}}{\rho^{\#}}{\rho^{\#}_l}
  \right)\rho^{\#}
  \otimes e_{2}}{\rho^{\#}}{\rho^{\#}_l},
  \f]

  Even though we intervalize the first operand in the above example, the
  actual implementation utilizes an heuristics for choosing which of the two
  operands must be intervalized in order to obtain the most precise result.
*/
template <typename Target, typename Integer_Int_Interval>
static bool
mul_linearize_int
(const Binary_Operator<Target>& bop_expr,
 const Oracle<Target,Integer_Int_Interval>& oracle,
 const std::map<dimension_type, Linear_Form<Integer_Int_Interval> >& lf_store,
 Linear_Form<Integer_Int_Interval>& result) {
  PPL_ASSERT(bop_expr.binary_operator() == Binary_Operator<Target>::MUL);

  typedef typename Integer_Int_Interval::boundary_type analyzer_format;
  typedef Linear_Form<Integer_Int_Interval> Integer_Linear_Form;

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

  Integer_Int_Interval intervalized_first_operand;
  if (!linearized_first_operand.intervalize(oracle, intervalized_first_operand))
    return false;

  Integer_Linear_Form linearized_second_operand;
  if (!linearize_int(*(bop_expr.right_hand_side()), oracle, lf_store,
		     linearized_second_operand))
    return false;

  Integer_Int_Interval intervalized_second_operand;
  if (!linearized_second_operand.intervalize(oracle,
					     intervalized_second_operand))
    return false;

  analyzer_format first_interval_size;
  analyzer_format second_interval_size;

  // FIXME: we are not sure that what we do here is policy-proof.
  if (intervalized_first_operand.is_bounded()) {
    if (intervalized_second_operand.is_bounded()) {
      first_interval_size = intervalized_first_operand.upper() -
	intervalized_first_operand.lower();
      second_interval_size = intervalized_second_operand.upper() -
	intervalized_second_operand.lower();
      intervalize_first = (first_interval_size <= second_interval_size);
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
    result = linearized_second_operand;
  }
  else {
    linearized_first_operand *= intervalized_second_operand;
    result = linearized_first_operand;
  }

  return true;
}

/*! \brief
  Helper function used by <CODE>linearize_int</CODE> to linearize a
  division of integer expressions.

  \par Template type parameters

  - The class template parameter \p Target specifies the implementation
  of Concrete_Expression to be used.
  - The class template parameter \p Integer_Int_Interval represents the type
  of the intervals used in the abstract domain. The interval bounds
  should have a integer type.

  Makes \p result become the linearization of \p *this in the given
  composite abstract store.

  \param bop_expr The binary operator concrete expression to linearize.
  Its binary operator type must be <CODE>DIV</CODE>.
  \param oracle The Oracle to be queried.
  \param lf_store The linear form abstract store.
  \param result The modified linear form.

  \return <CODE>true</CODE> if the linearization succeeded,
  <CODE>false</CODE> otherwise.

  \par Linearization of division integer expressions

  Let \f$i + \sum_{v \in \cV}i_{v}v \f$ and
  \f$i' + \sum_{v \in \cV}i'_{v}v \f$
  be two linear forms, \f$\aslf\f$ and \f$\adivlf\f$ two sound abstract
  operator on linear forms such that:

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v\right)
  \aslf
  \left(i' + \sum_{v \in \cV}i'_{v}v\right)
  =
  \left(i \asifp i'\right) + \sum_{v \in \cV}\left(i_{v} \asifp i'_{v}\right)v,
  \f]

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v\right)
  \adivlf
  i'
  =
  \left(i \adivifp i'\right) + \sum_{v \in \cV}\left(i_{v} \adivifp i'\right)v.
  \f]

  Given an expression \f$e_{1} \oslash [a;b]\f$ and a composite
  abstract store \f$\left \llbracket \rho^{\#}, \rho^{\#}_l \right
  \rrbracket\f$,
  we construct the interval linear form
  \f$
  \linexprenv{e_{1} \oslash [a;b]}{\rho^{\#}}{\rho^{\#}_l}
  \f$
  as follows:

  \f[
  \linexprenv{e_{1} \oslash [a;b]}{\rho^{\#}}{\rho^{\#}_l}
  =
  \left(\linexprenv{e_{1}}{\rho^{\#}}{\rho^{\#}_l}
  \adivlf
  [a;b]\right)
  \f]

  given an expression \f$e_{1} \oslash e_{2}\f$ and a composite
  abstract store \f$\left \llbracket \rho^{\#}, \rho^{\#}_l \right
  \rrbracket\f$, we construct the interval linear form
  \f$\linexprenv{e_{1} \oslash e_{2}}{\rho^{\#}}{\rho^{\#}_l}\f$
  as follows:

  \f[
  \linexprenv{e_{1} \oslash e_{2}}{\rho^{\#}}{\rho^{\#}_l}
  =
  \linexprenv{e_{1} \oslash \iota\left(
  \linexprenv{e_{2}}{\rho^{\#}}{\rho^{\#}_l}
  \right)\rho^{\#}}{\rho^{\#}}{\rho^{\#}_l},
  \f]
*/
template <typename Target, typename Integer_Int_Interval>
static bool
div_linearize_int
(const Binary_Operator<Target>& bop_expr,
 const Oracle<Target,Integer_Int_Interval>& oracle,
 const std::map<dimension_type, Linear_Form<Integer_Int_Interval> >& lf_store,
 Linear_Form<Integer_Int_Interval>& result) {
  PPL_ASSERT(bop_expr.binary_operator() == Binary_Operator<Target>::DIV);

  typedef Linear_Form<Integer_Int_Interval> Integer_Linear_Form;

  Integer_Linear_Form linearized_second_operand;
  if (!linearize_int(*(bop_expr.right_hand_side()), oracle, lf_store,
		     linearized_second_operand))
    return false;

  Integer_Int_Interval intervalized_second_operand;
  if (!linearized_second_operand.intervalize(oracle,
					     intervalized_second_operand))
    return false;

  // Check if we may divide by zero.
  if ((intervalized_second_operand.lower_is_boundary_infinity()
       || intervalized_second_operand.lower() <= 0)
      &&
      (intervalized_second_operand.upper_is_boundary_infinity()
       || intervalized_second_operand.upper() >= 0))
    return false;

  if (!linearize_int(*(bop_expr.left_hand_side()), oracle, lf_store, result))
    return false;

  result /= intervalized_second_operand;

  return true;
}

/*! \brief
  Helper function used by <CODE>linearize_int</CODE> to linearize a
  \f$or\f$ of integer expressions.

  \par Template type parameters

  - The class template parameter \p Target specifies the implementation
  of Concrete_Expression to be used.
  - The class template parameter \p Integer_Int_Interval represents the type
  of the intervals used in the abstract domain. The interval bounds
  should have a integer type.

  Makes \p result become the linearization of \p *this in the given
  composite abstract store.

  \param bop_expr The binary operator concrete expression to linearize.
  Its binary operator type must be <CODE>BOR</CODE>.
  \param oracle The Oracle to be queried.
  \param lf_store The linear form abstract store.
  \param result The modified linear form.

  \return <CODE>true</CODE> if the linearization succeeded,
  <CODE>false</CODE> otherwise.

  \par Linearization of or integer expressions

  Let \f$x\f$ and \f$y\f$ be two integer constants both greater than or equal
  zero, then \f$x \mathrel{\mid} y\f$ is defined as follows:

  \f[
  \max\left(x, y \right)
  \leq
  x \mathrel{\mid} y
  \leq
  x + y
  \f]

  Then let \f$i + \sum_{v \in \cV}i_{v}v \f$ and
  \f$i' + \sum_{v \in \cV}i'_{v}v \f$
  be two linear forms and \f$\borlf\f$ a sound abstract operator on linear
  forms such that:

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \borlf
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)
  =
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \f]

  where:

  \f[
  \max\left(\left(i + \sum_{v \in \cV}i_{v}v \right),
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)\right)
  \leq
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \leq
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \aslf
  \left(i' + \sum_{V \in \cV}i'_{v}v \right).
  \f]

  then:

  \f[
  \max\left(\left(i + \sum_{v \in \cV}i_{v}v \right),
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)\right)
  \leq
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \leq
  \left(i \asifp i' \right)
  + \sum_{v \in \cV}\left(i_{v} \asifp i'_{v} \right)v.
  \f]

  and we can define the elments of the linear form
  \f$k + \sum_{v \in \cV}k_{v}v\f$, as follows, \f$\forall v \in \cV\f$:

  \f[
  \max\left(i, i' \right)
  \leq
  k
  \leq
  \left(i \asifp i' \right)
  \f]

  \f[
  \max\left(i_{v},i'_{v} \right)
  \leq
  k_{v}
  \leq
  \left(i_{v} \asifp i'_{v} \right)
  \f]

  then

  \f[
  k
  \approx
  \max\left(i, i' \right)
  \bowtie
  \left(i \asifp i'\right)
  \f]

  \f[
  k_{v}
  \approx
  \max\left(i_{v}, i'_{v} \right)
  \bowtie
  \left(i_{v} \asifp i'_{v} \right)
  \f]

  If instead, let \f$x\f$ and \f$y\f$ be two integer constants with discordant
  sign, then \f$x \mathrel{\mid} y\f$ is defined as follows:

  \f[
  \min\left(x, y \right)
  \leq
  x \mathrel{\mid} y
  \leq
  -1
  \f]

  Then let \f$i + \sum_{v \in \cV}i_{v}v \f$ and
  \f$i' + \sum_{v \in \cV}i'_{v}v \f$
  be two linear forms and \f$\borlf\f$ a sound abstract operator on linear
  forms such that:

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \borlf
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)
  =
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \f]

  where

  \f[
  \min\left(\left(i + \sum_{v \in \cV}i_{v}v \right),
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)\right)
  \leq
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \leq
  -1
  \f]

  and we can define the elments of the linear form
  \f$k + \sum_{v \in \cV}k_{v}v\f$, as follows, \f$\forall v \in \cV\f$:

  \f[
  \min\left(i, i' \right)
  \leq
  k
  \leq
  -1
  \f]

  \f[
  \min\left(i_{v}, i'_{v} \right)
  \leq
  k_{v}
  \leq
  -1
  \f]

  then

  \f[
  k
  \approx
  \min\left(i, i' \right)
  \bowtie
  -1
  \f]

  \f[
  k_{v}
  \approx
  \min\left(i_{v}, i'_{v} \right)
  \bowtie
  -1
  \f]

  Finally, let \f$x\f$ and \f$y\f$ be two integer constants both less then
  zero, then \f$x \mathrel{\mid} y\f$ is defined as follows:

  \f[
  \max\left(x, y \right)
  \leq
  x \mathrel{\mid} y
  \leq
  -1
  \f]

  Then let \f$i + \sum_{v \in \cV}i_{v}v \f$ and
  \f$i' + \sum_{v \in \cV}i'_{v}v \f$
  be two linear forms and \f$\borlf\f$ a sound abstract operator on linear
  forms such that:

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \borlf
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)
  =
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \f]

  where

  \f[
  \max\left(\left(i + \sum_{v \in \cV}i_{v}v \right),
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)\right)
  \leq
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \leq
  -1
  \f]

  and we can define the elments of the linear form
  \f$k + \sum_{v \in \cV}k_{v}v\f$, as follows, \f$\forall v \in \cV\f$:

  \f[
  \max\left(i, i' \right)
  \leq
  k
  \leq
  -1
  \f]

  \f[
  \max\left(i_{v}, i'_{v} \right)
  \leq
  k_{v}
  \leq
  -1
  \f]

  then

  \f[
  k
  \approx
  \max\left(i, i' \right)
  \bowtie
  -1
  \f]

  \f[
  k_{v}
  \approx
  \max\left(i_{v}, i'_{v} \right)
  \bowtie
  -1
  \f]

  Given an expression \f$e_{1} \ovee e_{2}\f$ and a composite
  abstract store \f$\left \llbracket \rho^{\#}, \rho^{\#}_l \right
  \rrbracket\f$, we construct the interval linear form
  \f$\linexprenv{e_{1} \ovee e_{2}}{\rho^{\#}}{\rho^{\#}_l}\f$
  as follows:
  \f[
  \linexprenv{e_{1} \ovee e_{2}}{\rho^{\#}}{\rho^{\#}_l}
  =
  \linexprenv{e_{1}}{\rho^{\#}}{\rho^{\#}_l}
  \borlf
  \linexprenv{e_{2}}{\rho^{\#}}{\rho^{\#}_l}
  \f]
*/
template <typename Target, typename Integer_Int_Interval>
static bool
or_linearize_int
(const Binary_Operator<Target>& bop_expr,
 const Oracle<Target,Integer_Int_Interval>& oracle,
 const std::map<dimension_type, Linear_Form<Integer_Int_Interval> >& lf_store,
 Linear_Form<Integer_Int_Interval>& result) {
  PPL_ASSERT(bop_expr.binary_operator() == Binary_Operator<Target>::BOR);

  typedef Linear_Form<Integer_Int_Interval> Integer_Linear_Form;

  if (!linearize_int(*(bop_expr.left_hand_side()), oracle, lf_store,result))
    return false;

  Integer_Linear_Form linearized_second_operand;
  if (!linearize_int(*(bop_expr.right_hand_side()), oracle, lf_store,
		     linearized_second_operand))
    return false;

  result |= linearized_second_operand;

  return !result.overflows();
}

/*! \brief
  Helper function used by <CODE>linearize_int</CODE> to linearize a
  \f$and\f$ of integer expressions.

  \par Template type parameters

  - The class template parameter \p Target specifies the implementation
  of Concrete_Expression to be used.
  - The class template parameter \p Integer_Int_Interval represents the type
  of the intervals used in the abstract domain. The interval bounds
  should have a integer type.

  Makes \p result become the linearization of \p *this in the given
  composite abstract store.

  \param bop_expr The binary operator concrete expression to linearize.
  Its binary operator type must be <CODE>BAND</CODE>.
  \param oracle The Oracle to be queried.
  \param lf_store The linear form abstract store.
  \param result The modified linear form.

  \return <CODE>true</CODE> if the linearization succeeded,
  <CODE>false</CODE> otherwise.

  \par Linearization of and integer expressions

  Let \f$x\f$ and \f$y\f$ be two integer constants both greater than
  or equal zero, then \f$x \mathrel{\&} y\f$ is defined as follows:

  \f[
  0
  \leq
  x \mathrel{\&} y
  \leq
  \min\left(x, y \right)
  \f]

  Then let \f$i + \sum_{v \in \cV}i_{v}v \f$ and
  \f$i' + \sum_{v \in \cV}i'_{v}v \f$
  be two linear forms and \f$\bandlf\f$ a sound abstract operator on linear
  forms such that:

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \bandlf
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)
  =
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \f]

  where

  \f[
  0
  \leq
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \leq
  \min\left(\left(i + \sum_{v \in \cV}i_{v}v \right),
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)\right)
  \f]

  and we can define the elments of the linear form
  \f$k + \sum_{v \in \cV}k_{v}v\f$, as follows, \f$\forall v \in \cV\f$:

  \f[
  0
  \leq
  k
  \leq
  \min\left(i, i' \right)
  \f]

  \f[
  0
  \leq
  k_{v}
  \leq
  \min\left(i_{v}, i'_{v} \right)
  \f]

  then

  \f[
  k
  \approx
  0
  \bowtie
  \min\left(i, i' \right)
  \f]

  \f[
  k_{v}
  \approx
  0
  \bowtie
  \min\left(i_{v}, i'_{v} \right)
  \f]

  If instead, let \f$x\f$ and \f$y\f$ be two integer constants with discordant
  sign, then \f$x \mathrel{\&} y\f$ is defined as follows:

  \f[
  0
  \leq
  x \mathrel{\&} y
  \leq
  \max\left(x, y \right)
  \f]

  Then let \f$i + \sum_{v \in \cV}i_{v}v \f$ and
  \f$i' + \sum_{v \in \cV}i'_{v}v \f$
  be two linear forms and \f$\bandlf\f$ a sound abstract operator on linear
  forms such that:

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \bandlf
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)
  =
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \f]

  where

  \f[
  0
  \leq
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \leq
  \max\left(\left(i + \sum_{v \in \cV}i_{v}v \right),
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)\right)
  \f]

  and we can define the elments of the linear form
  \f$k + \sum_{v \in \cV}k_{v}v\f$, as follows, \f$\forall v \in \cV\f$:

  \f[
  0
  \leq
  k
  \leq
  \max\left(i, i' \right)
  \f]

  \f[
  0
  \leq
  k_{v}
  \leq
  \max\left(i_{v}, i'_{v} \right)
  \f]

  then

  \f[
  k
  \approx
  0
  \bowtie
  \max\left(i, i' \right)
  \f]

  \f[
  k_{v}
  \approx
  0
  \bowtie
  \max\left(i_{v}, i'_{v} \right)
  \f]

  Finally, let \f$x\f$ and \f$y\f$ be two integer constants both less then
  zero, then \f$x \mathrel{\&} y\f$ is defined as follows:

  \f[
  x + y
  \leq
  x \mathrel{\&} y
  \leq
  \min\left(x, y \right)
  \f]

  Then let \f$i + \sum_{v \in \cV}i_{v}v \f$ and
  \f$i' + \sum_{v \in \cV}i'_{v}v \f$
  be two linear forms and \f$\bandlf\f$ a sound abstract operator on linear
  forms such that:

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \bandlf
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)
  =
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \f]

  where

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \aslf
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)
  \leq
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \leq
  \min\left(\left(i + \sum_{v \in \cV}i_{v}v \right),
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)\right)
  \f]

  then

  \f[
  \left(i \asifp i' \right) + \sum_{v \in \cV}\left(i_{v} \asifp i'_{v} \right)v
  \leq
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \leq
  \min\left(\left(i + \sum_{v \in \cV}i_{v}v \right),
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)\right)
  \f]

  and we can define the elments of the linear form
  \f$k + \sum_{v \in \cV}k_{v}v\f$, as follows, \f$\forall v \in \cV\f$:

  \f[
  \left(i \asifp i' \right)
  \leq
  k
  \leq
  \min\left(i, i' \right)
  \f]

  \f[
  \left(i_{v} \asifp i'_{v} \right)
  \leq
  k_{v}
  \leq
  \min\left(i_{v}, i'_{v} \right)
  \f]

  then

  \f[
  k
  \approx
  \left(i \asifp i' \right)
  \bowtie
  \min \left(i, i' \right)
  \f]

  \f[
  k_{v}
  \approx
  \left(i_{v} \asifp i'_{v} \right)
  \bowtie
  \min\left(i_{v}, i'_{v} \right)
  \f]

  Given an expression \f$e_{1} \owedge e_{2}\f$ and a composite
  abstract store \f$\left \llbracket \rho^{\#}, \rho^{\#}_l \right
  \rrbracket\f$, we construct the interval linear form
  \f$\linexprenv{e_{1} \owedge e_{2}}{\rho^{\#}}{\rho^{\#}_l}\f$
  as follows:

  \f[
  \linexprenv{e_{1} \owedge e_{2}}{\rho^{\#}}{\rho^{\#}_l}
  =
  \linexprenv{e_{1}}{\rho^{\#}}{\rho^{\#}_l}
  \bandlf
  \linexprenv{e_{2}}{\rho^{\#}}{\rho^{\#}_l}
  \f]
*/
template <typename Target, typename Integer_Int_Interval>
static bool
and_linearize_int
(const Binary_Operator<Target>& bop_expr,
 const Oracle<Target,Integer_Int_Interval>& oracle,
 const std::map<dimension_type, Linear_Form<Integer_Int_Interval> >& lf_store,
 Linear_Form<Integer_Int_Interval>& result) {
  PPL_ASSERT(bop_expr.binary_operator() == Binary_Operator<Target>::BAND);

  typedef Linear_Form<Integer_Int_Interval> Integer_Linear_Form;

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

/*! \brief
  Helper function used by <CODE>linearize_int</CODE> to linearize a
  \f$xor\f$ of integer expressions.

  \par Template type parameters

  - The class template parameter \p Target specifies the implementation
  of Concrete_Expression to be used.
  - The class template parameter \p Integer_Int_Interval represents the type
  of the intervals used in the abstract domain. The interval bounds
  should have a integer type.

  Makes \p result become the linearization of \p *this in the given
  composite abstract store.

  \param bop_expr The binary operator concrete expression to linearize.
  Its binary operator type must be <CODE>BXOR</CODE>.
  \param oracle The Oracle to be queried.
  \param lf_store The linear form abstract store.
  \param result The modified linear form.

  \return <CODE>true</CODE> if the linearization succeeded,
  <CODE>false</CODE> otherwise.
  \par Linearization of xor integer expressions

  Let \f$x\f$ and \f$y\f$ be two integer constants with same sign,
  then \f$x^{\land}y\f$ is defined as follows:

  \f[
  0
  \leq
  x ^{\land} y
  \leq
  \left|x + y \right|
  \f]

  Then let \f$i + \sum_{v \in \cV}i_{v}v \f$ and
  \f$i' + \sum_{v \in \cV}i'_{v}v \f$
  be two linear forms and \f$\bxorlf\f$ a sound abstract operator on linear
  forms such that:

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \bxorlf
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)
  =
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \f]

  where

  \f[
  0
  \leq
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \leq
  \left|\left(i + \sum_{v \in \cV}i_{v}v \right)
  \aslf
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)\right|
  \f]

  and we can define the elments of the linear form
  \f$k + \sum_{v \in \cV}k_{v}v\f$, as follows, \f$\forall v \in \cV\f$:

  \f[
  0
  \leq
  k
  \leq
  \left|i \asifp i' \right|
  \f]

  \f[
  0
  \leq
  k_{v}
  \leq
  \left|i_{v} \asifp i'_{v} \right|
  \f]

  then

  \f[
  k
  \approx
  0
  \bowtie
  \left|i \asifp i' \right|
  \f]

  \f[
  k_{v}
  \approx
  0
  \bowtie
  \left|i_{v} \asifp i'_{v} \right|
  \f]

  Finally, let \f$x\f$ and \f$y\f$ be two integer constants both with
  discordant signs, then \f$x ^{\land} y\f$ is defined as follows:

  \f[
  -\left(\left|x \right| + \left|y \right|\right)
  \leq
  x^{\land} y
  \leq
  -1
  \f]

  Then let \f$i + \sum_{v \in \cV}i_{v}v \f$ and
  \f$i' + \sum_{v \in \cV}i'_{v}v \f$
  be two linear forms and \f$\bxorlf\f$ a sound abstract operator on linear
  forms such that:

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \bxorlf
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)
  =
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \f]

  where

  \f[
  -\left(\left|i + \sum_{v \in \cV}i_{v}v \right|
  \aslf
  \left|i' + \sum_{v \in \cV}i'_{v}v \right|\right)
  \leq
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \leq
  -1
  \f]

  and we can define the elments of the linear form
  \f$k + \sum_{v \in \cV}k_{v}v\f$, as follows, \f$\forall v \in \cV\f$:

  \f[
  -\left(\left|i \right| \asifp \left|i' \right|\right)
  \leq
  k
  \leq
  -1
  \f]

  \f[
  -\left(\left|i_{v} \right| \asifp \left|i'_{v} \right|\right)
  \leq
  k_{v}
  \leq
  -1
  \f]

  then

  \f[
  k
  \approx
  -\left(\left|i \right| \asifp \left|i' \right|\right)
  \bowtie
  -1
  \f]

  \f[
  k_{v}
  \approx
  -\left(\left|i_{v} \right| \asifp \left|i'_{v} \right|\right)
  \bowtie
  -1
  \f]

  Given an expression \f$e_{1} \dot{\ovee} e_{2}\f$ and a composite
  abstract store \f$\left \llbracket \rho^{\#}, \rho^{\#}_l \right
  \rrbracket\f$, we construct the interval linear form
  \f$\linexprenv{e_{1} \dot{\ovee} e_{2}}{\rho^{\#}}{\rho^{\#}_l}\f$
  as follows:

  \f[
  \linexprenv{e_{1} \dot{\ovee} e_{2}}{\rho^{\#}}{\rho^{\#}_l}
  =
  \linexprenv{e_{1}}{\rho^{\#}}{\rho^{\#}_l}
  \bxorlf
  \linexprenv{e_{2}}{\rho^{\#}}{\rho^{\#}_l}
  \f]
*/
template <typename Target, typename Integer_Int_Interval>
static bool
xor_linearize_int
(const Binary_Operator<Target>& bop_expr,
 const Oracle<Target,Integer_Int_Interval>& oracle,
 const std::map<dimension_type, Linear_Form<Integer_Int_Interval> >& lf_store,
 Linear_Form<Integer_Int_Interval>& result) {
  PPL_ASSERT(bop_expr.binary_operator() == Binary_Operator<Target>::BXOR);

  typedef Linear_Form<Integer_Int_Interval> Integer_Linear_Form;

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

/*! \brief
  Helper function used by <CODE>linearize_int</CODE> to linearize a
  \f$left\f$ \f$shift\f$ of integer expressions.

  \par Template type parameters

  - The class template parameter \p Target specifies the implementation
  of Concrete_Expression to be used.
  - The class template parameter \p Integer_Int_Interval represents the type
  of the intervals used in the abstract domain. The interval bounds
  should have a integer type.

  Makes \p result become the linearization of \p *this in the given
  composite abstract store.

  \param bop_expr The binary operator concrete expression to linearize.
  Its binary operator type must be <CODE>LSHIFT</CODE>.
  \param oracle The Oracle to be queried.
  \param lf_store The linear form abstract store.
  \param result The modified linear form.

  \return <CODE>true</CODE> if the linearization succeeded,
  <CODE>false</CODE> otherwise.

  \par Linearization of left shift integer expressions

  Let \f$x\f$ and \f$y\f$ be two integer constants both greater than
  or equal zero, then \f$x \ll y\f$ is defined as follows:

  \f[
  0
  \leq
  x \ll y
  \leq
  \left(x \times 2^{y} \right)
  \f]

  Then let \f$i + \sum_{v \in \cV}i_{v}v \f$ and
  \f$i' + \sum_{v \in \cV}i'_{v}v \f$
  be two linear forms and \f$\blshiftlf\f$ a sound abstract operator on linear
  forms such that:

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \blshiftlf
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)
  =
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \f]

  where

  \f[
  0
  \leq
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \leq
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \amlf
  2^{\left(i' + \sum_{v \in \cV}i'_{v}v \right)}
  \f]

  and we can define the elments of the linear form
  \f$k + \sum_{v \in \cV}k_{v}v\f$, as follows, \f$\forall v \in \cV\f$:

  \f[
  0
  \leq
  k
  \leq
  \left(i \amifp 2^{i'} \right)
  \f]

  \f[
  0
  \leq
  k_{v}
  \leq
  \left(i_{v} \amifp 2^{i'_{v}} \right)
  \f]

  then

  \f[
  k
  \approx
  0
  \bowtie
  \left(i \amifp 2^{i'} \right)
  \f]

  \f[
  k_{v}
  \approx
  0
  \bowtie
  \left(i_{v} \amifp 2^{i'_v} \right)
  \f]

  If instead, let \f$x\f$ and \f$y\f$ be two integer constants with \f$x\f$
  greater than or equal zero and \f$y\f$ less than zero,
  then \f$x \ll y\f$ is defined as follows:

  \f[
  0
  \leq
  x \ll y
  \leq
  \left(x \slash 2^{y} \right)
  \f]

  Then let \f$i + \sum_{v \in \cV}i_{v}v \f$ and
  \f$i' + \sum_{v \in \cV}i'_{v}v \f$
  be two linear forms and \f$\blshiftlf\f$ a sound abstract operator on linear
  forms such that:

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \blshiftlf
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)
  =
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \f]

  where

  \f[
  0
  \leq
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \leq
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \adivlf
  2^{\left(i' + \sum_{v \in \cV}i'_{v}v \right)}
  \f]

  and we can define the elments of the linear form
  \f$k + \sum_{v \in \cV}k_{v}v\f$, as follows, \f$\forall v \in \cV\f$:

  \f[
  0
  \leq
  k
  \leq
  \left(i \adivifp 2^{i'} \right)
  \f]

  \f[
  0
  \leq
  k_{v}
  \leq
  \left(i_{v} \adivifp 2^{i'_{v}} \right)
  \f]

  then

  \f[
  k
  \approx
  0
  \bowtie
  \left(i \adivifp 2^{i'} \right)
  \f]

  \f[
  k_{v}
  \approx
  0
  \bowtie
  \left(i_{v} \adivifp 2^{i'_{v}} \right)
  \f]

  If instead, let \f$x\f$ and \f$y\f$ be two integer constants with \f$x\f$
  less than zero and \f$y\f$ greater than or equal zero,
  then \f$x \ll y\f$ is defined as follows:

  \f[
  \left(x \times 2^{y} \right)
  \leq
  x \ll y
  \leq
  0
  \f]

  Then let \f$i + \sum_{v \in \cV}i_{v}v \f$ and
  \f$i' + \sum_{v \in \cV}i'_{v}v \f$
  be two linear forms and \f$\blshiftlf\f$ a sound abstract operator on linear
  forms such that:

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \blshiftlf
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)
  =
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \f]

  where

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \amlf
  2^{\left(i' + \sum_{v \in \cV}i'_{v}v \right)}
  \leq
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \leq
  0
  \f]

  and we can define the elments of the linear form
  \f$k + \sum_{v \in \cV}k_{v}v\f$, as follows, \f$\forall v \in \cV\f$:

  \f[
  \left(i \amifp 2^{i'} \right)
  \leq
  k
  \leq
  0
  \f]

  \f[
  \left(i_{v} \amifp 2^{i'_{v}} \right)
  \leq
  k_{v}
  \leq
  0
  \f]

  then

  \f[
  k
  \approx
  \left(i \amifp 2^{i'} \right)
  \bowtie
  0
  \f]

  \f[
  k_{v}
  \approx
  \left(i_{v} \amifp 2^{i'_{v}} \right)
  \bowtie
  0
  \f]

  Finally, let \f$x\f$ and \f$y\f$ be two integer constants both less then zero,
  then \f$x \ll y\f$ is defined as follows:

  \f[
  \left(x \slash 2^{y} \right)
  \leq
  x \ll y
  \leq
  0
  \f]

  Then let \f$i + \sum_{v \in \cV}i_{v}v \f$ and
  \f$i' + \sum_{v \in \cV}i'_{v}v \f$
  be two linear forms and \f$\blshiftlf\f$ a sound abstract operator on linear
  forms such that:

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \blshiftlf
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)
  =
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \f]

  where

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \adivlf
  2^{\left(i' + \sum_{v \in \cV}i'_{v}v \right)}
  \leq
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \leq
  0
  \f]

  and we can define the elments of the linear form
  \f$k + \sum_{v \in \cV}k_{v}v\f$, as follows, \f$\forall v \in \cV\f$:

  \f[
  \left(i \adivifp 2^{i'} \right)
  \leq
  k
  \leq
  0
  \f]

  \f[
  \left(i_{v} \adivifp 2^{i'_{v}} \right)
  \leq
  k_{v}
  \leq
  0
  \f]

  then

  \f[
  k
  \approx
  \left(i \adivifp 2^{i'} \right)
  \bowtie
  0
  \f]

  \f[
  k_{v}
  \approx
  \left(i_{v} \adivifp 2^{i'_{v}} \right)
  \bowtie
  0
  \f]

  Given an expression \f$e_{1} \leftslice e_{2}\f$ and a composite
  abstract store \f$\left \llbracket \rho^{\#}, \rho^{\#}_l \right
  \rrbracket\f$, we construct the interval linear form
  \f$\linexprenv{e_{1} \leftslice e_{2}}{\rho^{\#}}{\rho^{\#}_l}\f$
  as follows:

  \f[
  \linexprenv{e_{1} \leftslice e_{2}}{\rho^{\#}}{\rho^{\#}_l}
  =
  \linexprenv{e_{1}}{\rho^{\#}}{\rho^{\#}_l}
  \blshiftlf
  \linexprenv{e_{2}}{\rho^{\#}}{\rho^{\#}_l}
  \f]
*/
template <typename Target, typename Integer_Int_Interval>
static bool
lshift_linearize_int
(const Binary_Operator<Target>& bop_expr,
 const Oracle<Target,Integer_Int_Interval>& oracle,
 const std::map<dimension_type, Linear_Form<Integer_Int_Interval> >& lf_store,
 Linear_Form<Integer_Int_Interval>& result) {
  PPL_ASSERT(bop_expr.binary_operator() == Binary_Operator<Target>::LSHIFT);

  typedef Linear_Form<Integer_Int_Interval> Integer_Linear_Form;

  if (!linearize_int(*(bop_expr.left_hand_side()), oracle, lf_store, result))
    return false;

  Integer_Linear_Form linearized_second_operand;
  if (!linearize_int(*(bop_expr.right_hand_side()), oracle, lf_store,
		     linearized_second_operand))
    return false;

  result << linearized_second_operand;

  return true;
}

/*! \brief
  Helper function used by <CODE>linearize_int</CODE> to linearize a
  \f$right\f$ \f$shift\f$ of integer expressions.

  \par Template type parameters

  - The class template parameter \p Target specifies the implementation
  of Concrete_Expression to be used.
  - The class template parameter \p Integer_Int_Interval represents the type
  of the intervals used in the abstract domain. The interval bounds
  should have a integer type.

  Makes \p result become the linearization of \p *this in the given
  composite abstract store.

  \param bop_expr The binary operator concrete expression to linearize.
  Its binary operator type must be <CODE>RSHIFT</CODE>.
  \param oracle The Oracle to be queried.
  \param lf_store The linear form abstract store.
  \param result The modified linear form.

  \return <CODE>true</CODE> if the linearization succeeded,
  <CODE>false</CODE> otherwise.

  \par Linearization of right shift integer expressions

  Let \f$x\f$ and \f$y\f$ be two integer constants both greater than
  or equal zero, then \f$x \gg y\f$ is defined as follows:

  \f[
  0
  \leq
  x \gg y
  \leq
  \left(x \slash 2^{y} \right)
  \f]

  Then let \f$i + \sum_{v \in \cV}i_{v}v \f$ and
  \f$i' + \sum_{v \in \cV}i'_{v}v \f$
  be two linear forms and \f$\brshiftlf\f$ a sound abstract operator on linear
  forms such that:

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \brshiftlf
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)
  =
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \f]

  where

  \f[
  0
  \leq
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \leq
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \adivlf
  2^{\left(i' + \sum_{v \in \cV}i'_{v}v \right)}
  \f]

  and we can define the elments of the linear form
  \f$k + \sum_{v \in \cV}k_{v}v\f$, as follows, \f$\forall v \in \cV\f$:

  \f[
  0
  \leq
  k
  \leq
  \left(i \adivifp 2^{i'} \right)
  \f]

  \f[
  0
  \leq
  k_{v}
  \leq
  \left(i_{v} \adivifp 2^{i'_{v}} \right)
  \f]

  then

  \f[
  k
  \approx
  0
  \bowtie
  \left(i \adivifp 2^{i'} \right)
  \f]

  \f[
  k_{v}
  \approx
  0
  \bowtie
  \left(i_{v} \adivifp 2^{i'_v} \right)
  \f]

  If instead, let \f$x\f$ and \f$y\f$ be two integer constants with \f$x\f$
  greater than or equal zero and \f$y\f$ less than zero,
  then \f$x \gg y\f$ is defined as follows:

  \f[
  0
  \leq
  x \gg y
  \leq
  \left(x \times 2^{y} \right)
  \f]

  Then let \f$i + \sum_{v \in \cV}i_{v}v \f$ and
  \f$i' + \sum_{v \in \cV}i'_{v}v \f$
  be two linear forms and \f$\brshiftlf\f$ a sound abstract operator on linear
  forms such that:

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \brshiftlf
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)
  =
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \f]

  where

  \f[
  0
  \leq
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \leq
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \amlf
  2^{\left(i' + \sum_{v \in \cV}i'_{v}v \right)}
  \f]

  and we can define the elments of the linear form
  \f$k + \sum_{v \in \cV}k_{v}v\f$, as follows, \f$\forall v \in \cV\f$:

  \f[
  0
  \leq
  k
  \leq
  \left(i \amifp 2^{i'} \right)
  \f]

  \f[
  0
  \leq
  k_{v}
  \leq
  \left(i_{v} \amifp 2^{i'_{v}} \right)
  \f]

  then

  \f[
  k
  \approx
  0
  \bowtie
  \left(i \amifp 2^{i'} \right)
  \f]

  \f[
  k_{v}
  \approx
  0
  \bowtie
  \left(i_{v} \amifp 2^{i'_{v}} \right)
  \f]

  If instead, let \f$x\f$ and \f$y\f$ be two integer constants with \f$x\f$
  less than zero and \f$y\f$ greater than or equal zero,
  then \f$x \gg y\f$ is defined as follows:

  \f[
  \left(x \slash 2^{y} \right)
  \leq
  x \gg y
  \leq
  0
  \f]

  Then let \f$i + \sum_{v \in \cV}i_{v}v \f$ and
  \f$i' + \sum_{v \in \cV}i'_{v}v \f$
  be two linear forms and \f$\brshiftlf\f$ a sound abstract operator on linear
  forms such that:

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \brshiftlf
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)
  =
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \f]

  where

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \adivlf
  2^{\left(i' + \sum_{v \in \cV}i'_{v}v \right)}
  \leq
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \leq
  0
  \f]

  and we can define the elments of the linear form
  \f$k + \sum_{v \in \cV}k_{v}v\f$, as follows, \f$\forall v \in \cV\f$:

  \f[
  \left(i \adivifp 2^{i'} \right)
  \leq
  k
  \leq
  0
  \f]

  \f[
  \left(i_{v} \adivifp 2^{i'_{v}} \right)
  \leq
  k_{v}
  \leq
  0
  \f]

  then

  \f[
  k
  \approx
  \left(i \adivifp 2^{i'} \right)
  \bowtie
  0
  \f]

  \f[
  k_{v}
  \approx
  \left(i_{v} \adivifp 2^{i'_{v}} \right)
  \bowtie
  0
  \f]

  Finally, let \f$x\f$ and \f$y\f$ be two integer constants both less
  then zero, then \f$x \gg y\f$ is defined as follows:

  \f[
  \left(x \times 2^{y} \right)
  \leq
  x \gg y
  \leq
  0
  \f]

  Then let \f$i + \sum_{v \in \cV}i_{v}v \f$ and
  \f$i' + \sum_{v \in \cV}i'_{v}v \f$
  be two linear forms and \f$\brshiftlf\f$ a sound abstract operator on linear
  forms such that:

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \brshiftlf
  \left(i' + \sum_{v \in \cV}i'_{v}v \right)
  =
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \f]

  where

  \f[
  \left(i + \sum_{v \in \cV}i_{v}v \right)
  \amlf
  2^{\left(i' + \sum_{v \in \cV}i'_{v}v \right)}
  \leq
  \left(k + \sum_{v \in \cV}k_{v}v \right)
  \leq
  0
  \f]

  and we can define the elments of the linear form
  \f$k + \sum_{v \in \cV}k_{v}v\f$, as follows, \f$\forall v \in \cV\f$:

  \f[
  \left(i \amifp 2^{i'} \right)
  \leq
  k
  \leq
  0
  \f]

  \f[
  \left(i_{v} \amifp 2^{i'_{v}} \right)
  \leq
  k_{v}
  \leq
  0
  \f]

  then

  \f[
  k
  \approx
  \left(i \amifp 2^{i'} \right)
  \bowtie
  0
  \f]

  \f[
  k_{v}
  \approx
  \left(i_{v} \amifp 2^{i'_{v}} \right)
  \bowtie
  0
  \f]

  Given an expression \f$e_{1} \rightslice e_{2}\f$ and a composite
  abstract store \f$\left \llbracket \rho^{\#}, \rho^{\#}_l \right
  \rrbracket\f$, we construct the interval linear form
  \f$\linexprenv{e_{1} \rightslice e_{2}}{\rho^{\#}}{\rho^{\#}_l}\f$
  as follows:

  \f[
  \linexprenv{e_{1} \rightslice e_{2}}{\rho^{\#}}{\rho^{\#}_l}
  =
  \linexprenv{e_{1}}{\rho^{\#}}{\rho^{\#}_l}
  \brshiftlf
  \linexprenv{e_{2}}{\rho^{\#}}{\rho^{\#}_l}
  \f]
*/
template <typename Target, typename Integer_Int_Interval>
static bool
rshift_linearize_int
(const Binary_Operator<Target>& bop_expr,
 const Oracle<Target,Integer_Int_Interval>& oracle,
 const std::map<dimension_type, Linear_Form<Integer_Int_Interval> >& lf_store,
 Linear_Form<Integer_Int_Interval>& result) {
  PPL_ASSERT(bop_expr.binary_operator() == Binary_Operator<Target>::RSHIFT);

  typedef Linear_Form<Integer_Int_Interval> Integer_Linear_Form;

  if (!linearize_int(*(bop_expr.left_hand_side()), oracle, lf_store, result))
    return false;

  Integer_Linear_Form linearized_second_operand;
  if (!linearize_int(*(bop_expr.right_hand_side()), oracle, lf_store,
		     linearized_second_operand))
    return false;

  result >> linearized_second_operand;

  return true;
}

//! Linearizes a integer expression.
/*! \brief
  Makes \p result become a linear form that correctly approximates the
  value of \p expr in the given composite abstract store.

  \par Template type parameters

  - The class template parameter \p Target specifies the implementation
  of Concrete_Expression to be used.
  - The class template parameter \p FP_Interval_Type represents the type
  of the intervals used in the abstract domain. The interval bounds
  should have a integer type.

  \param expr The concrete expression to linearize.
  \param oracle The Oracle to be queried.
  \param lf_store The linear form abstract store.
  \param result Becomes the linearized expression.

  \return <CODE>true</CODE> if the linearization succeeded,
  <CODE>false</CODE> otherwise.

  Formally, if \p expr represents the expression \f$e\f$ and
  \p lf_store represents the linear form abstract store \f$\rho^{\#}_l\f$,
  then \p result will become \f$\linexprenv{e}{\rho^{\#}}{\rho^{\#}_l}\f$
  if the linearization succeeds.
*/

template <typename Target, typename Interval_Type>
bool
linearize_int
(const Concrete_Expression<Target>& expr,
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
    case Binary_Operator<Target>::BAND:
      return and_linearize_int(*bop_expr, oracle, lf_store, result);
    case Binary_Operator<Target>::BXOR:
      return xor_linearize_int(*bop_expr, oracle, lf_store, result);
    case Binary_Operator<Target>::LSHIFT:
      return lshift_linearize_int(*bop_expr, oracle, lf_store, result);
    case Binary_Operator<Target>::RSHIFT:
      return rshift_linearize_int(*bop_expr, oracle, lf_store, result);
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

      result = Linear_Form(variable_value->second);

      return !result.overflows();
    }
    break;
  }// end case Approx_Reference<Target>::KIND
  default:
    throw std::runtime_error("PPL internal error");
  }

  // PPL internal error: unreachable code
  PPL_ASSERT(false);
}

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_linearize_integer_hh)
