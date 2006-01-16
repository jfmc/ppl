/* Test LP_Problem class.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

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
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://www.cs.unipr.it/ppl/ . */

#include "ppl_test.hh"

using namespace Parma_Polyhedra_Library::IO_Operators;

int
main() TRY {
  // Variable declaration.
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Constraint_System cs;
  cs.insert(Coefficient("2251799813685248")*A
	    >= Coefficient("-5895288448651847"));
  cs.insert(Coefficient("5895288437392848")*A
	    + Coefficient("3643488632714799")*B
	    - Coefficient("2251799813685248")*C
	    >= Coefficient("-19077554137963492"));
  cs.insert(Coefficient("5895288437392848")*A +
	    Coefficient("3643488632714799")*B
	    + Coefficient("2251799813685248")*C >=
	    Coefficient("-19077554137963492"));
  cs.insert(Coefficient("11790576874785696")*A
	    + Coefficient("4503599627370496")*B
	    + Coefficient("7286977274436797")*D
	    >= Coefficient("-38155108284934184"));
  cs.insert(Coefficient("11790576874785696")*A
	    + Coefficient("4503599627370496")*B
	    - Coefficient("7286977274436797")*D
	    >= Coefficient("-38155108284934184"));
  cs.insert(Coefficient("11790576879289294")*A
	    + Coefficient("7286977274436797")*C
	    + Coefficient("4503599627370496")*D
	    >= Coefficient("-38155108289437784"));
  cs.insert(Coefficient("11790576879289294")*A
	    + Coefficient("7286977274436797")*C
	    - Coefficient("4503599627370496")*D
	    >= Coefficient("-38155108289437784"));
  cs.insert(Coefficient("11790576879289294")*A
	    - Coefficient("7286977274436797")*C
	    + Coefficient("4503599627370496")*D
	    >= Coefficient("-38155108289437784"));
  cs.insert(Coefficient("11790576879289294")*A
	    - Coefficient("7286977274436797")*C
	    - Coefficient("4503599627370496")*D
	    >= Coefficient("-38155108289437784"));
  cs.insert(Coefficient("2947644225451823")*A
	    - Coefficient("1125899906842624")*B
	    + Coefficient("1821744319735099")*D
	    >= Coefficient("-9538777088122044"));
  cs.insert(Coefficient("11790576892800094")*A
	    - Coefficient("4503599627370496")*B
	    - Coefficient("7286977274436797")*D
	    >= Coefficient("-38155108325466584"));
  cs.insert(Coefficient("5895288437392848")*A
	    - Coefficient("3643488630462999")*B
	    + Coefficient("2251799813685248")*C
	    >= Coefficient("-19077554133459892"));
  cs.insert(Coefficient("2947644218696424")*A
	    - Coefficient("1821744320860999")*B
	    - Coefficient("1125899906842624")*C
	    >= Coefficient("-9538777072359446"));
  cs.insert(Coefficient("7286977269933197")*A
	    + Coefficient("11790576924325290")*B
	    + Coefficient("4503599627370496")*D
	    >= Coefficient("-38155108379509776"));
  cs.insert(Coefficient("7286977269933197")*A
	    + Coefficient("11790576924325290")*B
	    - Coefficient("4503599627370496")*D
	    >= Coefficient("-38155108379509776"));
  cs.insert(Coefficient("562949953421312")*A
	    + Coefficient("562949953421312")*B
	    + Coefficient("562949953421312")*C
	    + Coefficient("562949953421312")*D
	    >= Coefficient("-2947644226577723"));
  cs.insert(Coefficient("562949953421312")*A
	    + Coefficient("562949953421312")*B
	    + Coefficient("562949953421312")*C
	    - Coefficient("562949953421312")*D
	    >= Coefficient("-2947644226577723"));
  cs.insert(Coefficient("562949953421312")*A
	    + Coefficient("562949953421312")*B
	    - Coefficient("562949953421312")*C
	    + Coefficient("562949953421312")*D
	    >= Coefficient("-2947644225451823"));
  cs.insert(Coefficient("562949953421312")*A
	    + Coefficient("562949953421312")*B
	    - Coefficient("562949953421312")*C
	    - Coefficient("562949953421312")*D
	    >= Coefficient("-2947644225451823"));
  cs.insert(Coefficient("7286977269933197")*A
	    + Coefficient("4503599627370496")*B
	    + Coefficient("11790576865778496")*C
	    >= Coefficient("-38155108266919784"));
  cs.insert(Coefficient("7286977251918799")*A
	    + Coefficient("4503599627370496")*B
	    - Coefficient("11790576870282096")*C
	    >= Coefficient("-38155108244401792"));
  cs.insert(Coefficient("1821744320860999")*A
	    + Coefficient("1125899906842624")*C
	    + Coefficient("2947644226577723")*D
	    >= Coefficient("-9538777093751544"));
  cs.insert(Coefficient("1821744320860999")*A
	    + Coefficient("1125899906842624")*C
	    - Coefficient("2947644226577723")*
	    D >= Coefficient("-9538777093751544"));
  cs.insert(Coefficient("1821744320860999")*A
	    - Coefficient("1125899906842624")*C
	    + Coefficient("2947644228829523")*D
	    >= Coefficient("-9538777096003344"));
  cs.insert(Coefficient("1821744320860999")*A
	    - Coefficient("1125899906842624")*C
	    - Coefficient("2947644228829523")*D
	    >= Coefficient("-9538777096003344"));
  cs.insert(Coefficient("3643488664239996")*A
	    - Coefficient("2251799813685248")*B
	    + Coefficient("5895288468918045")*C
	    >= Coefficient("-19077554257308884"));
  cs.insert(Coefficient("3643488652980997")*A
	    - Coefficient("2251799813685248")*B
	    - Coefficient("5895288468918045")*C
	    >= Coefficient("-19077554232539084"));
  cs.insert(Coefficient("562949953421312")*A
	    - Coefficient("562949953421312")*B
	    + Coefficient("562949953421312")*C
	    + Coefficient("562949953421312")*D
	    >= Coefficient("-2947644226577723"));
  cs.insert(Coefficient("562949953421312")*A
	    - Coefficient("562949953421312")*B
	    + Coefficient("562949953421312")*C
	    - Coefficient("562949953421312")*D
	    >= Coefficient("-2947644229392473"));
  cs.insert(Coefficient("562949953421312")*A
	    - Coefficient("562949953421312")*B
	    - Coefficient("562949953421312")*C
	    + Coefficient("562949953421312")*D
	    >= Coefficient("-2947644227140673"));
  cs.insert(Coefficient("562949953421312")*A
	    - Coefficient("562949953421312")*B
	    - Coefficient("562949953421312")*C
	    - Coefficient("562949953421312")*D
	    >= Coefficient("-2947644227703623"));
  cs.insert(Coefficient("7286977314969193")*A
	    - Coefficient("11790576906310892")*B
	    + Coefficient("4503599627370496")*D
	    >= Coefficient("-38155108447063768"));
  cs.insert(Coefficient("3643488655232797")*A
	    - Coefficient("5895288446400047")*B
	    - Coefficient("2251799813685248")*D
	    >= Coefficient("-19077554203265688"));
  cs.insert(Coefficient("4503599627370496")*A
	    + Coefficient("11790576753188506")*B
	    + Coefficient("7286977179861205")*C
	    >= Coefficient("-38155107920142616"));
  cs.insert(Coefficient("4503599627370496")*A
	    + Coefficient("11790576766699304")*B
	    - Coefficient("7286977179861205")*C
	    >= Coefficient("-38155107965178608"));
  cs.insert(Coefficient("4503599627370496")*A
	    + Coefficient("7286977157343207")*B
	    + Coefficient("11790576712656108")*D
	    >= Coefficient("-38155107816559824"));
  cs.insert(Coefficient("2251799813685248")*A
	    + Coefficient("3643488592182402")*B
	    - Coefficient("5895288374342453")*D
	    >= Coefficient("-19077553960071308"));
  cs.insert(Coefficient("4503599627370496")*A
	    + Coefficient("11790576753188506")*C
	    + Coefficient("7286977175357605")*D
	    >= Coefficient("-38155107924646216"));
  cs.insert(Coefficient("2251799813685248")*A
	    + Coefficient("5895288390105051")*C
	    - Coefficient("3643488594434202")*D
	    >= Coefficient("-19077553996100104"));
  cs.insert(Coefficient("2251799813685248")*A
	    - Coefficient("5895288421630249")*C
	    + Coefficient("3643488619204000")*D
	    >= Coefficient("-19077554088423896"));
  cs.insert(Coefficient("4503599627370496")*A
	    - Coefficient("11790576865778496")*C
	    - Coefficient("7286977247415199")*D
	    >= Coefficient("-38155108244401792"));
  cs.insert(Coefficient("4503599627370496")*A
	    - Coefficient("7286977247415199")*B
	    + Coefficient("11790576888296494")*D
	    >= Coefficient("-38155108307452184"));
  cs.insert(Coefficient("2251799813685248")*A
	    - Coefficient("3643488639470198")*B
	    - Coefficient("5895288464414445")*D
	    >= Coefficient("-19077554210021088"));
  cs.insert(Coefficient("2251799813685248")*A
	    - Coefficient("5895288428385648")*B
	    + Coefficient("3643488630462999")*C
	    >= Coefficient("-19077554131208092"));
  cs.insert(Coefficient("4503599627370496")*A
	    - Coefficient("11790576843260498")*B
	    - Coefficient("7286977224897201")*C
	    >= Coefficient("-38155108163336992"));
  cs.insert(Coefficient("1125899906842624")*B
	    >= Coefficient("-2947644227703623"));
  cs.insert(Coefficient("5895288459910846")*B
	    + Coefficient("2251799813685248")*C
	    + Coefficient("3643488630462999")*D
	    >= Coefficient("-19077554198762088"));
  cs.insert(Coefficient("5895288457659046")*B
	    + Coefficient("2251799813685248")*C
	    - Coefficient("3643488628211199")*D
	    >= Coefficient("-19077554189754888"));
  cs.insert(Coefficient("11790576915318092")*B
	    - Coefficient("4503599627370496")*C
	    + Coefficient("7286977269933197")*D
	    >= Coefficient("-38155108393020576"));
  cs.insert(Coefficient("5895288457659046")*B
	    - Coefficient("2251799813685248")*C
	    - Coefficient("3643488632714799")*D
	    >= Coefficient("-19077554187503088"));
  cs.insert(Coefficient("7286977292451195")*B
	    + Coefficient("11790576919821692")*C
	    + Coefficient("4503599627370496")*D
	    >= Coefficient("-38155108433552976"));
  cs.insert(Coefficient("3643488664239996")*B
	    + Coefficient("5895288486932443")*C
	    - Coefficient("2251799813685248")*D
	    >= Coefficient("-19077554304596680"));
  cs.insert(Coefficient("3643488643973798")*B
	    - Coefficient("5895288446400047")*C
	    + Coefficient("2251799813685248")*D
	    >= Coefficient("-19077554180747688"));
  cs.insert(Coefficient("7286977314969193")*B
	    - Coefficient("11790576937836090")*C
	    - Coefficient("4503599627370496")*D
	    >= Coefficient("-38155108510114168"));
  cs.insert(Coefficient("4503599627370496")*B
	    + Coefficient("7286977247415199")*C
	    + Coefficient("11790576883792894")*D
	    >= Coefficient("-38155108289437784"));
  cs.insert(Coefficient("4503599627370496")*B
	    + Coefficient("7286977251918799")*C
	    - Coefficient("11790576883792894")*D
	    >= Coefficient("-38155108280430584"));
  cs.insert(Coefficient("4503599627370496")*B
	    - Coefficient("7286977229400801")*C
	    + Coefficient("11790576852267696")*D
	    >= Coefficient("-38155108181351392"));
  cs.insert(Coefficient("1125899906842624")*D
	    >= Coefficient("-2947644225451823"));
  cs.insert(Coefficient("4503599627370496")*B
	    - Coefficient("7286977229400801")*C
	    - Coefficient("11790576852267696")*D
	    >= Coefficient("-38155108167840592"));
  cs.insert(Coefficient("-2251799813685248")*D
	    >= Coefficient("-5895288448651847"));
  cs.insert(Coefficient("2251799813685248")*C
	    >= Coefficient("-5895288446400047"));
  cs.insert(Coefficient("-2251799813685248")*C
	    >= Coefficient("-5895288444148247"));
  cs.insert(Coefficient("-1125899906842624")*B
	    + Coefficient("1821744321986899")*C
	    + Coefficient("2947644226577723")*D
	    >= Coefficient("-9538777088122044"));
  cs.insert(Coefficient("-3643488607945001")*B
	    + Coefficient("5895288414874849")*C
	    + Coefficient("2251799813685248")*D
	    >= Coefficient("-19077554059150500"));
  cs.insert(Coefficient("-4503599627370496")*B
	    + Coefficient("7286977292451195")*C
	    - Coefficient("11790576906310892")*D
	    >= Coefficient("-38155108343480984"));
  cs.insert(Coefficient("-7286977220393601")*B
	    + Coefficient("11790576829749698")*C
	    - Coefficient("4503599627370496")*D
	    >= Coefficient("-38155108086775800"));
  cs.insert(Coefficient("-4503599627370496")*B
	    - Coefficient("7286977274436797")*C
	    + Coefficient("11790576901807292")*D
	    >= Coefficient("-38155108325466584"));
  cs.insert(Coefficient("-3643488605693201")*B
	    - Coefficient("5895288414874849")*C
	    + Coefficient("2251799813685248")*D
	    >= Coefficient("-19077554059150500"));
  cs.insert(Coefficient("-1125899906842624")*B
	    - Coefficient("1821744319735099")*C
	    - Coefficient("2947644225451823")*D
	    >= Coefficient("-9538777079114846"));
  cs.insert(Coefficient("-7286977220393601")*B
	    - Coefficient("11790576834253298")*C
	    - Coefficient("4503599627370496")*D
	    >= Coefficient("-38155108113797400"));
  cs.insert(Coefficient("-5895288462162645")*B
	    + Coefficient("2251799813685248")*C
	    + Coefficient("3643488639470198")*D
	    >= Coefficient("-19077554144718892"));
  cs.insert(Coefficient("-11790576924325290")*B
	    - Coefficient("4503599627370496")*C
	    + Coefficient("7286977292451195")*D
	    >= Coefficient("-38155108320962984"));
  cs.insert(Coefficient("-5895288468918045")*B
	    + Coefficient("2251799813685248")*C
	    - Coefficient("3643488641721998")*D
	    >= Coefficient("-19077554160481492"));
  cs.insert(Coefficient("-11790576928828890")*B
	    - Coefficient("4503599627370496")*C
	    - Coefficient("7286977292451195")*D
	    >= Coefficient("-38155108329970184"));
  cs.insert(Coefficient("-281474976710656")*B
	    >= Coefficient("-736911053829681"));
  cs.insert(Coefficient("-4503599627370496")*A
	    + Coefficient("11790576658612912")*B
	    + Coefficient("7286977125818009")*C
	    >= Coefficient("-38155107627408640"));
  cs.insert(Coefficient("-2251799813685248")*A
	    + Coefficient("5895288336061856")*B
	    - Coefficient("3643488560657205")*C
	    >= Coefficient("-19077553829466920"));
  cs.insert(Coefficient("-2251799813685248")*A
	    + Coefficient("3643488535887407")*B
	    + Coefficient("5895288288774060")*D
	    >= Coefficient("-19077553683099932"));
  cs.insert(Coefficient("-7286977274436797")*A
	    + Coefficient("11790576766699304")*B
	    + Coefficient("4503599627370496")*D
	    >= Coefficient("-38155108032732608"));
  cs.insert(Coefficient("-4503599627370496")*A
	    + Coefficient("7286977098796411")*B
	    - Coefficient("11790576609073318")*D
	    >= Coefficient("-38155107483293448"));
  cs.insert(Coefficient("-7286977301458395")*A
	    + Coefficient("11790576735174106")*B
	    - Coefficient("4503599627370496")*D
	    >= Coefficient("-38155107983193008"));
  cs.insert(Coefficient("-4503599627370496")*A
	    + Coefficient("11790576708152508")*C
	    + Coefficient("7286977148336007")*D
	    >= Coefficient("-38155107771523824"));
  cs.insert(Coefficient("-281474976710656")*A
	    + Coefficient("281474976710656")*B
	    + Coefficient("281474976710656")*C
	    + Coefficient("281474976710656")*D
	    >= Coefficient("-1473822119481311"));
  cs.insert(Coefficient("-1125899906842624")*A
	    + Coefficient("2947644178164027")*C
	    - Coefficient("1821744285958102")*D
	    >= Coefficient("-9538776941755056"));
  cs.insert(Coefficient("-1125899906842624")*A
	    + Coefficient("1125899906842624")*B
	    + Coefficient("1125899906842624")*C
	    - Coefficient("1125899906842624")*D
	    >= Coefficient("-5895288471169845"));
  cs.insert(Coefficient("-4503599627370496")*A
	    - Coefficient("11790576856771296")*C
	    + Coefficient("7286977247415199")*D
	    >= Coefficient("-38155108221883792"));
  cs.insert(Coefficient("-1125899906842624")*A
	    + Coefficient("1125899906842624")*B
	    - Coefficient("1125899906842624")*C
	    + Coefficient("1125899906842624")*D
	    >= Coefficient("-5895288471169845"));
  cs.insert(Coefficient("-140737488355328")*A
	    - Coefficient("368455526774103")*C
	    - Coefficient("227718038700250")*D
	    >= Coefficient("-1192347131793131"));
  cs.insert(Coefficient("-1125899906842624")*A
	    + Coefficient("1125899906842624")*B
	    - Coefficient("1125899906842624")*C
	    - Coefficient("1125899906842624")*D
	    >= Coefficient("-5895288464414445"));
  cs.insert(Coefficient("-3643488643973798")*A
	    + Coefficient("2251799813685248")*B
	    + Coefficient("5895288441896447")*C
	    >= Coefficient("-19077554158229692"));
  cs.insert(Coefficient("-7286977296954795")*A
	    + Coefficient("4503599627370496")*B
	    - Coefficient("11790576892800094")*C
	    >= Coefficient("-38155108352488176"));
  cs.insert(Coefficient("-4503599627370496")*A
	    - Coefficient("7286977269933197")*B
	    + Coefficient("11790576924325290")*D
	    >= Coefficient("-38155108411034976"));
  cs.insert(Coefficient("-3643488639470198")*A
	    + Coefficient("2251799813685248")*C
	    + Coefficient("5895288466666245")*D
	    >= Coefficient("-19077554219028288"));
  cs.insert(Coefficient("-4503599627370496")*A
	    - Coefficient("7286977296954795")*B
	    - Coefficient("11790576955850488")*D
	    >= Coefficient("-38155108514617768"));
  cs.insert(Coefficient("-7286977251918799")*A
	    + Coefficient("4503599627370496")*C
	    - Coefficient("11790576892800094")*D
	    >= Coefficient("-38155108311955784"));
  cs.insert(Coefficient("-3643488655232797")*A
	    - Coefficient("2251799813685248")*C
	    + Coefficient("5895288480177044")*D
	    >= Coefficient("-19077554264064284"));
  cs.insert(Coefficient("-1821744320860999")*A
	    - Coefficient("1125899906842624")*C
	    - Coefficient("2947644229955423")*D
	    >= Coefficient("-9538777099381044"));
  cs.insert(Coefficient("-4503599627370496")*A
	    - Coefficient("11790576874785696")*B
	    + Coefficient("7286977269933197")*C
	    >= Coefficient("-38155108302948584"));
  cs.insert(Coefficient("-7286977274436797")*A
	    - Coefficient("4503599627370496")*B
	    + Coefficient("11790576937836090")*C
	    >= Coefficient("-38155108424545776"));
  cs.insert(Coefficient("-4503599627370496")*A
	    - Coefficient("11790576802728102")*B
	    - Coefficient("7286977197875603")*C
	    >= Coefficient("-38155108019221808"));
  cs.insert(Coefficient("-3643488664239996")*A
	    - Coefficient("2251799813685248")*B
	    - Coefficient("5895288493687843")*C
	    >= Coefficient("-19077554284330480"));
  cs.insert(Coefficient("-562949953421312")*A
	    - Coefficient("562949953421312")*B
	    + Coefficient("562949953421312")*C
	    + Coefficient("562949953421312")*D
	    >= Coefficient("-2947644250784571"));
  cs.insert(Coefficient("-281474976710656")*A
	    - Coefficient("281474976710656")*B
	    + Coefficient("281474976710656")*C
	    - Coefficient("281474976710656")*D
	    >= Coefficient("-1473822131021785"));
  cs.insert(Coefficient("-1125899906842624")*A
	    - Coefficient("1125899906842624")*B
	    - Coefficient("1125899906842624")*C
	    + Coefficient("1125899906842624")*D
	    >= Coefficient("-5895288464414445"));
  cs.insert(Coefficient("-1125899906842624")*A
	    - Coefficient("1125899906842624")*B
	    - Coefficient("1125899906842624")*C
	    - Coefficient("1125899906842624")*D
	    >= Coefficient("-5895288468918045"));
  cs.insert(Coefficient("-3643488412038417")*A
	    - Coefficient("5895288318047457")*B
	    + Coefficient("2251799813685248")*D
	    >= Coefficient("-19077553665085532"));
  cs.insert(Coefficient("-1821744199263809")*A
	    - Coefficient("2947644153394229")*B
	    - Coefficient("1125899906842624")*D
	    >= Coefficient("-9538776813402468"));
  cs.insert(Coefficient("-5895288378846052")*A
	    + Coefficient("3643488632714799")*B
	    + Coefficient("2251799813685248")*C
	    >= Coefficient("-19077554023121704"));
  cs.insert(Coefficient("-11790576834253298")*A
	    + Coefficient("7286977314969193")*B
	    - Coefficient("4503599627370496")*C
	    >= Coefficient("-38155108302948584"));
  cs.insert(Coefficient("-736911041726257")*A
	    + Coefficient("281474976710656")*B
	    + Coefficient("455436077400500")*D
	    >= Coefficient("-2384694241068264"));
  cs.insert(Coefficient("-5895288347320855")*A
	    + Coefficient("2251799813685248")*B
	    - Coefficient("3643488616952200")*D
	    >= Coefficient("-19077553951064108"));
  cs.insert(Coefficient("-2947644201807925")*A
	    + Coefficient("1821744319735099")*C
	    + Coefficient("1125899906842624")*D
	    >= Coefficient("-9538777048715548"));
  cs.insert(Coefficient("-11790576820742500")*A
	    + Coefficient("7286977296954795")*C
	    - Coefficient("4503599627370496")*D
	    >= Coefficient("-38155108248905384"));
  cs.insert(Coefficient("-11790576996382886")*A
	    - Coefficient("7286977251918799")*C
	    + Coefficient("4503599627370496")*D
	    >= Coefficient("-38155108523624968"));
  cs.insert(Coefficient("-5895288507198642")*A
	    - Coefficient("3643488632714799")*C
	    - Coefficient("2251799813685248")*D
	    >= Coefficient("-19077554291085880"));
  cs.insert(Coefficient("-11790577113476476")*A
	    - Coefficient("4503599627370496")*B
	    + Coefficient("7286977319472793")*D
	    >= Coefficient("-38155108861394936"));
  cs.insert(Coefficient("-5895288572500836")*A
	    - Coefficient("2251799813685248")*B
	    - Coefficient("3643488652980997")*D
	    >= Coefficient("-19077554450963668"));
  cs.insert(Coefficient("-5895288484680644")*A
	    - Coefficient("3643488607945001")*B
	    + Coefficient("2251799813685248")*C
	    >= Coefficient("-19077554212272888"));
  cs.insert(Coefficient("-2947644274991419")*A
	    - Coefficient("1821744320860999")*B
	    - Coefficient("1125899906842624")*C
	    >= Coefficient("-9538777190578936"));
  cs.insert(Coefficient("-2251799813685248")*A
	    >= Coefficient("-5895288448651847"));

  // Cost function
  Linear_Expression cost(10*A + 21*B + 31*C + 45*D);

  LP_Problem lp = LP_Problem(cs, cost, MAXIMIZATION);

  Generator pg = lp.optimizing_point();
  nout << "Optimizing point obtained by simplex:\n";
  print_generator(pg);

  Coefficient num;
  Coefficient den;
  lp.evaluate_objective_function(pg, num, den);
  nout << "\nOptimum value = " << num << "/" << den << endl;

  C_Polyhedron ph(cs);
  Coefficient num1;
  Coefficient den1;
  bool maximum;
  Generator pg1 = point();
  ph.maximize(cost, num1, den1, maximum, pg1);

  nout << "\nOptimizing point obtained by enumeration:\n";
  print_generator(pg1);
  nout << "\nOptimum value = " << num1 << "/" << den1 << endl;

  return (maximum && num == num1 && den == den1 && pg == pg1) ? 0 : 1;
}
CATCH

