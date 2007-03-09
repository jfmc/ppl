/* Test Box::refine(const Constraint_System&) with instances that may
   require a watchdog timer.
   Copyright (C) 2001-2007 Roberto Bagnara <bagnara@cs.unipr.it>

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
#include "pwl.hh"

namespace {

class Timeout : virtual public std::exception,
		public Parma_Polyhedra_Library::Throwable {
public:
  const char* what() const throw() {
    return "Timeout in refine1.cc";
  }

  void throw_me() const {
    throw *this;
  }

  int priority() const {
    return 0;
  }

  Timeout() {
  }

  ~Timeout() throw() {
  }
};

Timeout t;

bool
test01() {
  Variable A(0);
  Variable B(1);

  Constraint_System cs;
  cs.insert(A >= -5);
  cs.insert(A <= 5);
  cs.insert(A == B);
  cs.insert(A == 2*B);
  print_constraints(cs, "*** cs ***");

  Rational_Box known_result(2);
  known_result.add_constraint(A == 0);
  known_result.add_constraint(B == 0);
  print_constraints(known_result, "*** known_result ***");

  TBox box(2);

  bool ok = false;

  typedef TBox::interval_type::boundary_type boundary_type;
  if (std::numeric_limits<boundary_type>::is_exact
      && !std::numeric_limits<boundary_type>::is_integer) {
    // With interval boundaries made of rational numbers, this
    // refinement instance does not terminate: we use a watchdog timer.
    try {
      // Set a 0.1 seconds timeout.
      Parma_Watchdog_Library::Watchdog
	w(10, abandon_expensive_computations, t);

      box.refine(cs);

      // We should never get here.
      abandon_expensive_computations = 0;
      nout << "unexpected termination" << endl;
      ok = false;
    }
    catch (const Timeout&) {
      abandon_expensive_computations = 0;
      nout << "timeout, as expected" << endl;

      // The box will have been shrunk, nonetheless.
      ok = check_result(box, known_result, "1.0e-6", "1.0e-6", "1.0e-6");
    }
    catch (...) {
      nout << "unexpected exception" << endl;
      ok = false;
    }
  }
  else {
    // With interval boundaries other than rational numbers, this instance
    // of refinement terminates rather quickly: no timer is necessary.
    box.refine(cs);

    ok = check_result(box, known_result, "5.61e-45", "2.81e-45", "1.41e-45");
  }

  print_constraints(box, "*** box.refine(cs) ***");

  return ok;
}

bool
test02() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);
  Variable F(5);
  Variable G(6);
  Variable H(7);
  Variable I(8);
  Variable J(9);
  Variable K(10);
  Variable L(11);
  Variable M(12);
  Variable N(13);
  Variable O(14);
  Variable P(15);
  Variable Q(16);
  Variable R(17);
  Variable S(18);
  Variable T(19);
  Variable U(20);
  Variable V(21);
  Variable W(22);
  Variable X(23);
  Variable Y(24);
  Variable Z(25);
  Variable A1(26);
  Variable B1(27);
  Variable C1(28);
  Variable D1(29);
  Variable E1(30);
  Variable F1(31);
  Variable G1(32);
  Variable H1(33);

  Constraint_System cs;
  cs.insert(A + C - Q == 130);
  cs.insert(B + D - R == 190);
  cs.insert(-450*A1 - 45*B1 >= -4);
  cs.insert(-90*C1 - 9*D1 >= -8000);
  cs.insert(-30*E1 - 3*F1 >= -3500);
  cs.insert(-30*G1 - 3*H1 >= -3500);
  cs.insert(-E - G >= -1000);
  cs.insert(-F - H >= -1000);
  cs.insert(E + I - 6*A1 == 0);
  cs.insert(G + K - 6*E1 == 0);
  cs.insert(F + J - 6*C1 == 0);
  cs.insert(H + L - 6*G1 == 0);
  cs.insert(A + W - A1 == 5);
  cs.insert(B - W + X - C1 == 0);
  cs.insert(C + Y - E1 == 2);
  cs.insert(D - Y + Z - G1 == 0);
  cs.insert(M + S + A1 - B1 == 6);
  cs.insert(N - O - S + T + C1 - D1 == 0);
  cs.insert(O + U + E1 - F1 == 4);
  cs.insert(M - P + U - V - G1 + H1 == 0);
  cs.insert(A >= 0);
  cs.insert(B >= 0);
  cs.insert(C >= 0);
  cs.insert(D >= 0);
  cs.insert(E >= 0);
  cs.insert(F >= 0);
  cs.insert(G >= 0);
  cs.insert(H >= 0);
  cs.insert(I >= 0);
  cs.insert(J >= 0);
  cs.insert(K >= 0);
  cs.insert(L >= 0);
  cs.insert(M >= 0);
  cs.insert(N >= 0);
  cs.insert(O >= 0);
  cs.insert(P >= 0);
  cs.insert(Q >= 0);
  cs.insert(R >= 0);
  cs.insert(S >= 0);
  cs.insert(T >= 0);
  cs.insert(U >= 0);
  cs.insert(V >= 0);
  cs.insert(W >= 0);
  cs.insert(X >= 0);
  cs.insert(Y >= 0);
  cs.insert(Z >= 0);
  cs.insert(A1 >= 0);
  cs.insert(B1 >= 0);
  cs.insert(C1 >= 0);
  cs.insert(D1 >= 0);
  cs.insert(E1 >= 0);
  cs.insert(F1 >= 0);
  cs.insert(G1 >= 0);
  cs.insert(H1 >= 0);
  print_constraints(cs, "*** cs ***");

  TBox box(cs.space_dimension());
  box.refine(cs);

  bool ok = box.is_empty();

  print_constraints(box, "*** box.refine(cs) ***");

  return ok;
}

bool
test03() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);
  Variable F(5);
  Variable G(6);
  Variable H(7);
  Variable I(8);
  Variable J(9);
  Variable K(10);
  Variable L(11);
  Variable M(12);
  Variable N(13);
  Variable O(14);
  Variable P(15);
  Variable Q(16);
  Variable R(17);
  Variable S(18);
  Variable T(19);
  Variable U(20);
  Variable V(21);
  Variable W(22);
  Variable X(23);
  Variable Y(24);
  Variable Z(25);
  Variable A1(26);
  Variable B1(27);
  Variable C1(28);
  Variable D1(29);
  Variable E1(30);
  Variable F1(31);
  Variable G1(32);
  Variable H1(33);
  Variable I1(34);
  Variable J1(35);
  Variable K1(36);
  Variable L1(37);
  Variable M1(38);
  Variable N1(39);
  Variable O1(40);
  Variable P1(41);
  Variable Q1(42);
  Variable R1(43);
  Variable S1(44);
  Variable T1(45);
  Variable U1(46);
  Variable V1(47);
  Variable W1(48);
  Variable X1(49);
  Variable Y1(50);
  Variable Z1(51);
  Variable A2(52);
  Variable B2(53);
  Variable C2(54);
  Variable D2(55);
  Variable E2(56);
  Variable F2(57);
  Variable G2(58);
  Variable H2(59);
  Variable I2(60);
  Variable J2(61);
  Variable K2(62);
  Variable L2(63);
  Variable M2(64);
  Variable N2(65);
  Variable O2(66);
  Variable P2(67);
  Variable Q2(68);
  Variable R2(69);
  Variable S2(70);
  Variable T2(71);
  Variable U2(72);
  Variable V2(73);
  Variable W2(74);
  Variable X2(75);
  Variable Y2(76);
  Variable Z2(77);
  Variable A3(78);
  Variable B3(79);
  Variable C3(80);
  Variable D3(81);
  Variable E3(82);
  Variable F3(83);
  Variable G3(84);
  Variable H3(85);
  Variable I3(86);
  Variable J3(87);
  Variable K3(88);

  Constraint_System cs;
  cs.insert(A + F - M1 - O1 - Q1 - S1 == 0);
  cs.insert(S - U1 - D2 - P2 - V2 == 0);
  cs.insert(B + G - U1 - X1 - A2 == 0);
  cs.insert(T - M1 - H2 - R2 - Z2 == 0);
  cs.insert(C + H - D2 - H2 - L2 == 0);
  cs.insert(U - O1 - X1 - T2 - D3 == 0);
  cs.insert(D - P2 - R2 - T2 == 0);
  cs.insert(V - Q1 - A2 - L2 - H3 == 0);
  cs.insert(E - V2 - Z2 - D3 - H3 == 0);
  cs.insert(W - S1 == 0);
  cs.insert(X - V1 - E2 - W2 == 0);
  cs.insert(I - V1 - Y1 - B2 == 0);
  cs.insert(Y - I2 - A3 == 0);
  cs.insert(J - E2 - I2 - M2 == 0);
  cs.insert(Z - Y1 - E3 == 0);
  cs.insert(A1 - B2 - M2 - I3 == 0);
  cs.insert(K - W2 - A3 - E3 - I3 == 0);
  cs.insert(B1 == 0);
  cs.insert(L - N1 - P1 - R1 - T1 == 0);
  cs.insert(C1 - W1 - F2 - Q2 - X2 == 0);
  cs.insert(M - W1 - Z1 - C2 == 0);
  cs.insert(D1 - N1 - J2 - S2 - B3 == 0);
  cs.insert(N - F2 - J2 - N2 == 0);
  cs.insert(E1 - P1 - Z1 - U2 - F3 == 0);
  cs.insert(O - Q2 - S2 - U2 == 0);
  cs.insert(F1 - R1 - C2 - N2 - J3 == 0);
  cs.insert(P - X2 - B3 - F3 - J3 == 0);
  cs.insert(G1 - T1 == 0);
  cs.insert(H1 - G2 - Y2 == 0);
  cs.insert(I1 - K2 - C3 == 0);
  cs.insert(Q - G2 - K2 - O2 == 0);
  cs.insert(J1 - G3 == 0);
  cs.insert(K1 - O2 - K3 == 0);
  cs.insert(R - Y2 - C3 - G3 - K3 == 0);
  cs.insert(L1 == 0);
  cs.insert(A >= 0);
  cs.insert(-A >= -100);
  cs.insert(B >= 0);
  cs.insert(-B >= -100);
  cs.insert(C >= 0);
  cs.insert(-C >= -90);
  cs.insert(D >= 0);
  cs.insert(-D >= -50);
  cs.insert(E >= 0);
  cs.insert(-E >= -10);
  cs.insert(F >= 0);
  cs.insert(-F >= -200);
  cs.insert(G >= 0);
  cs.insert(-G >= -100);
  cs.insert(H >= 0);
  cs.insert(I >= 0);
  cs.insert(-I >= -25);
  cs.insert(J >= 0);
  cs.insert(-J >= -10);
  cs.insert(K >= 0);
  cs.insert(L >= 0);
  cs.insert(-L >= -50);
  cs.insert(M >= 0);
  cs.insert(-M >= -40);
  cs.insert(N >= 0);
  cs.insert(-N >= -20);
  cs.insert(O >= 0);
  cs.insert(-O >= -5);
  cs.insert(P >= 0);
  cs.insert(Q >= 0);
  cs.insert(-Q >= -30);
  cs.insert(R >= 0);
  cs.insert(S >= 100);
  cs.insert(T >= 100);
  cs.insert(U >= 90);
  cs.insert(V >= 50);
  cs.insert(W >= 10);
  cs.insert(X >= 20);
  cs.insert(Y >= 25);
  cs.insert(Z >= 10);
  cs.insert(A1 >= 15);
  cs.insert(B1 >= 5);
  cs.insert(C1 >= 50);
  cs.insert(D1 >= 40);
  cs.insert(E1 >= 20);
  cs.insert(F1 >= 5);
  cs.insert(G1 >= 15);
  cs.insert(H1 >= 20);
  cs.insert(I1 >= 25);
  cs.insert(J1 >= 30);
  cs.insert(K1 >= 20);
  cs.insert(L1 >= 10);
  cs.insert(M1 >= 0);
  cs.insert(N1 >= 0);
  cs.insert(O1 >= 0);
  cs.insert(P1 >= 0);
  cs.insert(Q1 >= 0);
  cs.insert(R1 >= 0);
  cs.insert(S1 >= 0);
  cs.insert(T1 >= 0);
  cs.insert(U1 >= 0);
  cs.insert(V1 >= 0);
  cs.insert(W1 >= 0);
  cs.insert(X1 >= 0);
  cs.insert(Y1 >= 0);
  cs.insert(Z1 >= 0);
  cs.insert(A2 >= 0);
  cs.insert(B2 >= 0);
  cs.insert(C2 >= 0);
  cs.insert(D2 >= 0);
  cs.insert(E2 >= 0);
  cs.insert(F2 >= 0);
  cs.insert(G2 >= 0);
  cs.insert(H2 >= 0);
  cs.insert(I2 >= 0);
  cs.insert(J2 >= 0);
  cs.insert(K2 >= 0);
  cs.insert(L2 >= 0);
  cs.insert(M2 >= 0);
  cs.insert(N2 >= 0);
  cs.insert(O2 >= 0);
  cs.insert(P2 >= 0);
  cs.insert(Q2 >= 0);
  cs.insert(R2 >= 0);
  cs.insert(S2 >= 0);
  cs.insert(T2 >= 0);
  cs.insert(U2 >= 0);
  cs.insert(V2 >= 0);
  cs.insert(W2 >= 0);
  cs.insert(X2 >= 0);
  cs.insert(Y2 >= 0);
  cs.insert(Z2 >= 0);
  cs.insert(A3 >= 0);
  cs.insert(B3 >= 0);
  cs.insert(C3 >= 0);
  cs.insert(D3 >= 0);
  cs.insert(E3 >= 0);
  cs.insert(F3 >= 0);
  cs.insert(G3 >= 0);
  cs.insert(H3 >= 0);
  cs.insert(I3 >= 0);
  cs.insert(J3 >= 0);
  cs.insert(K3 >= 0);
  print_constraints(cs, "*** cs ***");

  TBox box(cs.space_dimension());
  box.refine(cs);

  bool ok = box.is_empty();

  print_constraints(box, "*** box.refine(cs) ***");

  return ok;
}

bool
test04() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);
  Variable F(5);
  Variable G(6);
  Variable H(7);

  Constraint_System cs;
  cs.insert(-A >= -20);
  cs.insert(-B - C >= -20);
  cs.insert(-D >= -20);
  cs.insert(A + B - E - F == 0);
  cs.insert(C + D - G - H == 0);
  cs.insert(E >= 10);
  cs.insert(F + G >= 20);
  cs.insert(H >= 30);
  cs.insert(A >= 0);
  cs.insert(-A >= -30);
  cs.insert(B >= 0);
  cs.insert(-B >= -20);
  cs.insert(C >= 0);
  cs.insert(-C >= -10);
  cs.insert(D >= 0);
  cs.insert(-D >= -10);
  cs.insert(E >= 0);
  cs.insert(-E >= -10);
  cs.insert(F >= 0);
  cs.insert(-F >= -2);
  cs.insert(G >= 0);
  cs.insert(-G >= -20);
  cs.insert(H >= 0);
  cs.insert(-H >= -30);
  print_constraints(cs, "*** cs ***");

  TBox box(cs.space_dimension());
  box.refine(cs);

  bool ok = box.is_empty();

  print_constraints(box, "*** box.refine(cs) ***");

  return ok;
}

bool
test05() {
  Variable A(0);
  Variable B(1);
  Variable C(2);
  Variable D(3);
  Variable E(4);
  Variable F(5);
  Variable G(6);
  Variable H(7);
  Variable I(8);
  Variable J(9);
  Variable K(10);
  Variable L(11);
  Variable M(12);
  Variable N(13);
  Variable O(14);
  Variable P(15);
  Variable Q(16);
  Variable R(17);
  Variable S(18);
  Variable T(19);
  Variable U(20);
  Variable V(21);
  Variable W(22);
  Variable X(23);
  Variable Y(24);
  Variable Z(25);
  Variable A1(26);
  Variable B1(27);
  Variable C1(28);
  Variable D1(29);
  Variable E1(30);
  Variable F1(31);
  Variable G1(32);
  Variable H1(33);
  Variable I1(34);
  Variable J1(35);
  Variable K1(36);
  Variable L1(37);
  Variable M1(38);
  Variable N1(39);
  Variable O1(40);
  Variable P1(41);
  Variable Q1(42);
  Variable R1(43);
  Variable S1(44);
  Variable T1(45);
  Variable U1(46);
  Variable V1(47);
  Variable W1(48);
  Variable X1(49);
  Variable Y1(50);
  Variable Z1(51);
  Variable A2(52);
  Variable B2(53);
  Variable C2(54);
  Variable D2(55);
  Variable E2(56);
  Variable F2(57);
  Variable G2(58);
  Variable H2(59);
  Variable I2(60);
  Variable J2(61);
  Variable K2(62);
  Variable L2(63);
  Variable M2(64);
  Variable N2(65);
  Variable O2(66);
  Variable P2(67);
  Variable Q2(68);
  Variable R2(69);
  Variable S2(70);
  Variable T2(71);
  Variable U2(72);
  Variable V2(73);
  Variable W2(74);
  Variable X2(75);
  Variable Y2(76);
  Variable Z2(77);
  Variable A3(78);

  Constraint_System cs;
  cs.insert(Coefficient("-292733975779082240")*A
	    - Coefficient("24769797950537728")*B
	    - Coefficient("3602879701896397")*C
	    - Coefficient("20266198323167232")*D
	    - Coefficient("6755399441055744")*E
	    - Coefficient("27021597764222976")*F
	    - Coefficient("14861878770322636")*G
	    - Coefficient("26120877838748876")*H
	    + Coefficient("49539595901075456")*J >= 0);
  cs.insert(Coefficient("-18014398509481984")*A
	    - Coefficient("12249790986447750")*B
	    - Coefficient("4863887597560136")*D
	    - Coefficient("2161727821137838")*E
- Coefficient("3422735716801577")*F
	    - Coefficient("1261007895663739")*G
	    - Coefficient("9007199254740992")*H
	    + Coefficient("9007199254740992")*J >= 0);
  cs.insert(Coefficient("18014398509481984")*A
	    + Coefficient("17293822569102704")*B
	    + Coefficient("17473966554197524")*D
	    + Coefficient("6485183463413514")*E
	    + Coefficient("6305039478318694")*F
	    + Coefficient("5224175567749775")*G
	    + Coefficient("11168927075878830")*H
	    - Coefficient("9007199254740992")*J >= 0);
  cs.insert(Coefficient("9007199254740992")*A
	    + Coefficient("9007199254740992")*B
	    + Coefficient("7025615418697974")*C
	    + Coefficient("9007199254740992")*D
	    + Coefficient("8556839292003942")*E
	    + Coefficient("8016407336719483")*F
	    + Coefficient("8736983277098762")*G
	    + Coefficient("8827055269646172")*H
	    - Coefficient("8106479329266893")*J >= 0);
  cs.insert(Coefficient("219325301852943168")*A
	    + Coefficient("189151184349560832")*B
	    + Coefficient("197933203622933312")*C
	    + Coefficient("220451201759785792")*D
	    + Coefficient("136459068709326032")*E
	    + Coefficient("213470622337361504")*F
	    + Coefficient("220451201759785792")*G
	    + Coefficient("217298682020626432")*H
	    + Coefficient("4728779608739021")*I
	    - Coefficient("225179981368524800")*J >= 0);
  cs.insert(Coefficient("449909602774312576")*A
	    + Coefficient("396767127171340672")*B
	    + Coefficient("373348409109014144")*C
	    + Coefficient("451711042625260736")*D
	    + Coefficient("343624651568368832")*E
	    + Coefficient("435047724003989888")*F
	    + Coefficient("451711042625260736")*G
	    + Coefficient("441803123445045632")*H
	    + Coefficient("3152519739159347")*I
	    - Coefficient("450359962737049600")*J >= 0);
  cs.insert(Coefficient("94237822202727632")*A
	    + Coefficient("89396452603304352")*B
	    + Coefficient("83992133050459744")*C
	    + Coefficient("106960491150049280")*D
	    + Coefficient("77236733609404000")*E
	    + Coefficient("94350412193411888")*F
	    + Coefficient("106960491150049280")*G
	    + Coefficient("90972712472884016")*H
	    + Coefficient("2589569785738035")*I
	    - Coefficient("101330991615836160")*J >= 0);
  cs.insert(Coefficient("5981343255101440")*A
	    + Coefficient("5854679515581645")*B
	    + Coefficient("5671720780719718")*C
	    + Coefficient("6896136929411072")*D
	    + Coefficient("5404319552844595")*E
	    + Coefficient("6108006994621235")*F
	    + Coefficient("6896136929411072")*G
	    + Coefficient("5735052650479616")*H
	    + Coefficient("70368744177664")*I
	    - Coefficient("6333186975989760")*J >= 0);
  cs.insert(-I + 3*J >= 0);
  cs.insert(A + B + C + D + E + F + G + H - J == 0);
  cs.insert(Coefficient("-292733975779082240")*M
	    - Coefficient("24769797950537728")*N
	    - Coefficient("3602879701896397")*O
	    - Coefficient("6755399441055744")*P
	    - Coefficient("12159718993900340")*Q
	    - Coefficient("26120877838748876")*R
	    + Coefficient("49539595901075456")*T >= 0);
  cs.insert(Coefficient("-18014398509481984")*M
	    - Coefficient("10268207150404730")*N
	    - Coefficient("2161727821137838")*P
	    - Coefficient("2341871806232658")*Q
	    - Coefficient("8286623314361713")*R
	    + Coefficient("9007199254740992")*T >= 0);
  cs.insert(Coefficient("576460752303423488")*M
	    + Coefficient("576460752303423488")*N
	    + Coefficient("5764607523034235")*O
	    + Coefficient("207525870829232448")*P
	    + Coefficient("161409010644958592")*Q
	    + Coefficient("386228704043293760")*R
	    - Coefficient("288230376151711744")*T >= 0);
  cs.insert(Coefficient("9007199254740992")*M
	    + Coefficient("9007199254740992")*N
	    + Coefficient("8827055269646172")*O
	    + Coefficient("8556839292003942")*P
	    + Coefficient("7115687411245384")*Q
	    + Coefficient("9007199254740992")*R
	    - Coefficient("8106479329266893")*T >= 0);
  cs.insert(Coefficient("13707831365808948")*M
	    + Coefficient("11821949021847552")*N
	    + Coefficient("12370825226433332")*O
	    + Coefficient("8528691794332877")*P
	    + Coefficient("10963450342880052")*Q
	    + Coefficient("13581167626289152")*R
	    + Coefficient("492581209243648")*S
	    - Coefficient("12525636463624192")*T >= 0);
  cs.insert(Coefficient("224954801387156288")*M
	    + Coefficient("198383563585670336")*N
	    + Coefficient("186674204554507072")*O
	    + Coefficient("171812325784184416")*P
	    + Coefficient("183296504833979200")*Q
	    + Coefficient("220901561722522816")*R
	    + Coefficient("4278419646001971")*S
	    - Coefficient("200410183417987072")*T >= 0);
  cs.insert(Coefficient("188475644405455264")*M
	    + Coefficient("178792905206608704")*N
	    + Coefficient("167984266100919488")*O
	    + Coefficient("154473467218808000")*P
	    + Coefficient("158977066846178496")*Q
	    + Coefficient("181945424945768032")*R
	    + Coefficient("7656119366529843")*S
	    - Coefficient("184647584722190336")*T >= 0);
  cs.insert(Coefficient("382805968326492160")*M
	    + Coefficient("374699488997225280")*N
	    + Coefficient("362990129966061952")*O
	    + Coefficient("345876451382054080")*P
	    + Coefficient("333266372425416704")*Q
	    + Coefficient("367043369630695424")*R
	    + Coefficient("8106479329266893")*S
	    - Coefficient("369295169444380672")*T >= 0);
  cs.insert(-S + 3*T >= 0);
  cs.insert(M + N + O + P + Q + R - T == 0);
  cs.insert(Coefficient("-78812993478983680")*U
	    - Coefficient("10696049115004928")*V
	    - Coefficient("3039929748475085")*W
	    - Coefficient("12159718993900340")*X
	    - Coefficient("1688849860263936")*Y
	    - Coefficient("6980579422424269")*Z
	    - Coefficient("6755399441055744")*A1
	    - Coefficient("5066549580791808")*B1
	    - Coefficient("6192449487634432")*C1
	    - Coefficient("7318349394477056")*D1
	    + Coefficient("12384898975268864")*F1 >= 0);
  cs.insert(Coefficient("-18014398509481984")*U
	    - Coefficient("12610078956637388")*V
	    - Coefficient("2341871806232658")*W
	    - Coefficient("17473966554197524")*X
	    - Coefficient("2161727821137838")*Y
	    - Coefficient("3422735716801577")*Z
	    - Coefficient("3422735716801577")*A1
	    - Coefficient("4863887597560136")*B1
	    - Coefficient("12249790986447750")*C1
	    - Coefficient("8646911284551352")*D1
	    + Coefficient("9007199254740992")*F1 >= 0);
  cs.insert(Coefficient("9007199254740992")*U
	    + Coefficient("7475975381435023")*V
	    + Coefficient("2522015791327478")*W
	    + Coefficient("9007199254740992")*X
	    + Coefficient("3242591731706757")*Y
	    + Coefficient("3152519739159347")*Z
	    + Coefficient("3152519739159347")*A1
	    + Coefficient("8736983277098762")*B1
	    + Coefficient("8646911284551352")*C1
	    + Coefficient("5044031582654956")*D1
	    - Coefficient("4503599627370496")*F1 >= 0);
  cs.insert(Coefficient("9007199254740992")*U
	    + Coefficient("9007199254740992")*V
	    + Coefficient("7115687411245384")*W
	    + Coefficient("9007199254740992")*X
	    + Coefficient("8556839292003942")*Y
	    + Coefficient("8016407336719483")*Z
	    + Coefficient("8016407336719483")*A1
	    + Coefficient("9007199254740992")*B1
	    + Coefficient("9007199254740992")*C1
	    + Coefficient("8736983277098762")*D1
	    - Coefficient("8106479329266893")*F1 >= 0);
  cs.insert(Coefficient("220226021778417248")*U
	    + Coefficient("200635363399355584")*V
	    + Coefficient("175415205486080832")*W
	    + Coefficient("190502264237771968")*X
	    + Coefficient("136459068709326032")*Y
	    + Coefficient("215947602132415296")*Z
	    + Coefficient("213470622337361504")*A1
	    + Coefficient("220451201759785792")*B1
	    + Coefficient("189151184349560832")*C1
	    + Coefficient("217298682020626432")*D1
	    + Coefficient("4278419646001971")*E1
	    - Coefficient("225179981368524800")*F1 >= 0);
  cs.insert(Coefficient("921436483760003456")*U
	    + Coefficient("828662331436171264")*V
	    + Coefficient("733186019335916800")*W
	    + Coefficient("807945773150267008")*X
	    + Coefficient("687249303136737664")*Y
	    + Coefficient("879102647262720768")*Z
	    + Coefficient("870095448007979776")*A1
	    + Coefficient("903422085250521472")*B1
	    + Coefficient("793534254342681344")*C1
	    + Coefficient("874599047635350272")*D1
	    + Coefficient("8106479329266893")*E1
	    - Coefficient("900719925474099200")*F1 >= 0);
  cs.insert(Coefficient("213470622337361504")*U
	    + Coefficient("174289305579238208")*V
	    + Coefficient("158977066846178496")*W
	    + Coefficient("188250464424086720")*X
	    + Coefficient("154473467218808000")*Y
	    + Coefficient("192303704088720192")*Z
	    + Coefficient("188700824386823776")*A1
	    + Coefficient("213920982300098560")*B1
	    + Coefficient("178792905206608704")*C1
	    + Coefficient("185097944684927392")*D1
	    + Coefficient("5404319552844595")*E1
	    - Coefficient("202661983231672320")*F1 >= 0);
  cs.insert(Coefficient("898918485623150976")*U
	    + Coefficient("721476660304753408")*V
	    + Coefficient("666532744850833408")*W
	    + Coefficient("805243613373844736")*X
	    + Coefficient("691752902764108160")*Y
	    + Coefficient("790832094566259072")*Z
	    + Coefficient("781824895311518080")*A1
	    + Coefficient("882705526964617216")*B1
	    + Coefficient("749398977994450560")*C1
	    + Coefficient("750299697919924608")*D1
	    + Coefficient("8106479329266893")*E1
	    - Coefficient("810647932926689280")*F1 >= 0);
  cs.insert(-E1 + 3*F1 >= 0);
  cs.insert(U + V + W + X + Y + Z + A1 + B1 + C1 + D1 - F1 == 0);
  cs.insert(Coefficient("-78812993478983680")*H1
	    - Coefficient("10696049115004928")*I1
	    - Coefficient("3039929748475085")*J1
	    - Coefficient("12159718993900340")*K1
	    - Coefficient("6980579422424269")*L1
	    - Coefficient("7318349394477056")*M1
	    - Coefficient("1688849860263936")*N1
	    - Coefficient("6192449487634432")*O1
	    + Coefficient("12384898975268864")*Q1 >= 0);
  cs.insert(Coefficient("-36028797018963968")*H1
	    - Coefficient("24499581972895500")*I1
	    - Coefficient("3242591731706757")*J1
	    - Coefficient("33506781227636492")*K1
	    - Coefficient("5404319552844595")*L1
	    - Coefficient("16212958658533786")*M1
	    - Coefficient("4323455642275676")*N1
	    - Coefficient("24499581972895500")*O1
	    + Coefficient("18014398509481984")*Q1 >= 0);
  cs.insert(Coefficient("18014398509481984")*H1
	    + Coefficient("16212958658533786")*I1
	    + Coefficient("6665327448508334")*J1
	    + Coefficient("18014398509481984")*K1
	    + Coefficient("8106479329266893")*L1
	    + Coefficient("11349071060973650")*M1
	    + Coefficient("6485183463413514")*N1
	    + Coefficient("17293822569102704")*O1
	    - Coefficient("9007199254740992")*Q1 >= 0);
  cs.insert(Coefficient("9007199254740992")*H1
	    + Coefficient("9007199254740992")*I1
	    + Coefficient("8286623314361713")*J1
	    + Coefficient("9007199254740992")*K1
	    + Coefficient("8827055269646172")*L1
	    + Coefficient("9007199254740992")*M1
	    + Coefficient("8556839292003942")*N1
	    + Coefficient("9007199254740992")*O1
	    - Coefficient("8106479329266893")*Q1 >= 0);
  cs.insert(Coefficient("220226021778417248")*H1
	    + Coefficient("200635363399355584")*I1
	    + Coefficient("175415205486080832")*J1
	    + Coefficient("190502264237771968")*K1
	    + Coefficient("215947602132415296")*L1
	    + Coefficient("217298682020626432")*M1
	    + Coefficient("136459068709326032")*N1
	    + Coefficient("189151184349560832")*O1
	    + Coefficient("8782019273372467")*P1
	    - Coefficient("200410183417987072")*Q1 >= 0);
  cs.insert(Coefficient("230359120940000864")*H1
	    + Coefficient("207165582859042816")*I1
	    + Coefficient("183296504833979200")*J1
	    + Coefficient("201986443287566752")*K1
	    + Coefficient("219775661815680192")*L1
	    + Coefficient("218649761908837568")*M1
	    + Coefficient("171812325784184416")*N1
	    + Coefficient("198383563585670336")*O1
	    + Coefficient("3152519739159347")*P1
	    - Coefficient("200410183417987072")*Q1 >= 0);
  cs.insert(Coefficient("6670956948042547")*H1
	    + Coefficient("5446540799351194")*I1
	    + Coefficient("4968033338943078")*J1
	    + Coefficient("5882827013252710")*K1
	    + Coefficient("6009490752772506")*L1
	    + Coefficient("5784310771403981")*M1
	    + Coefficient("4827295850587750")*N1
	    + Coefficient("5587278287706522")*O1
	    + Coefficient("246290604621824")*P1
	    - Coefficient("5770237022568448")*Q1 >= 0);
  cs.insert(Coefficient("449459242811575488")*H1
	    + Coefficient("360738330152376704")*I1
	    + Coefficient("333266372425416704")*J1
	    + Coefficient("402621806686922368")*K1
	    + Coefficient("395416047283129536")*L1
	    + Coefficient("375149848959962304")*M1
	    + Coefficient("345876451382054080")*N1
	    + Coefficient("374699488997225280")*O1
	    + Coefficient("5854679515581645")*P1
	    - Coefficient("369295169444380672")*Q1 >= 0);
  cs.insert(-P1 + 3*Q1 >= 0);
  cs.insert(H1 + I1 + J1 + K1 + L1 + M1 + N1 + O1 - Q1 == 0);
  cs.insert(Coefficient("-252201579132747776")*S1
	    - Coefficient("8106479329266893")*T1
	    - Coefficient("6305039478318694")*U1
	    - Coefficient("47738156050127256")*V1
	    - Coefficient("11258999068426240")*W1
	    - Coefficient("51791395714760704")*X1
	    - Coefficient("6755399441055744")*Y1
	    - Coefficient("18915118434956084")*Z1
	    - Coefficient("19365478397693132")*A2
	    + Coefficient("45035996273704960")*C2 >= 0);
  cs.insert(Coefficient("-72057594037927936")*S1
	    - Coefficient("48999163945791000")*V1
	    - Coefficient("55484347409204512")*X1
	    - Coefficient("8646911284551352")*Y1
	    - Coefficient("5764607523034235")*Z1
	    - Coefficient("5044031582654956")*A2
	    + Coefficient("36028797018963968")*C2 >= 0);
  cs.insert(Coefficient("18014398509481984")*S1
	    + Coefficient("15672526703249326")*V1
	    + Coefficient("16753390613818246")*X1
	    + Coefficient("6485183463413514")*Y1
	    + Coefficient("5404319552844595")*Z1
	    + Coefficient("4863887597560136")*A2
	    - Coefficient("9007199254740992")*C2 >= 0);
  cs.insert(Coefficient("9007199254740992")*S1
	    + Coefficient("9007199254740992")*T1
	    + Coefficient("4863887597560136")*U1
	    + Coefficient("9007199254740992")*V1
	    + Coefficient("5854679515581645")*W1
	    + Coefficient("9007199254740992")*X1
	    + Coefficient("8556839292003942")*Y1
	    + Coefficient("8196551321814303")*Z1
	    + Coefficient("8106479329266893")*A2
	    - Coefficient("8106479329266893")*C2 >= 0);
  cs.insert(Coefficient("223828901480313664")*S1
	    + Coefficient("197933203622933312")*T1
	    + Coefficient("194105143939668384")*U1
	    + Coefficient("224954801387156288")*V1
	    + Coefficient("201761263306198208")*W1
	    + Coefficient("179018085187977216")*X1
	    + Coefficient("136459068709326032")*Y1
	    + Coefficient("224054081461682176")*Z1
	    + Coefficient("228332501107684160")*A2
	    + Coefficient("3602879701896397")*B2
	    - Coefficient("227431781182210048")*C2 >= 0);
  cs.insert(Coefficient("463870761619161088")*S1
	    + Coefficient("412529725867137408")*T1
	    + Coefficient("405323966463344640")*U1
	    + Coefficient("452161402587997824")*V1
	    + Coefficient("412980085829874496")*W1
	    + Coefficient("383256328289229184")*X1
	    + Coefficient("343624651568368832")*Y1
	    + Coefficient("449909602774312576")*Z1
	    + Coefficient("457115362178105344")*A2
	    + Coefficient("3602879701896397")*B2
	    - Coefficient("454863562364420096")*C2 >= 0);
  cs.insert(Coefficient("6804657561980109")*S1
	    + Coefficient("6199486362052198")*T1
	    + Coefficient("6192449487634432")*U1
	    + Coefficient("5685794529555251")*V1
	    + Coefficient("5580241413288755")*W1
	    + Coefficient("5643573283048653")*X1
	    + Coefficient("4827295850587750")*Y1
	    + Coefficient("6164301989963366")*Z1
	    + Coefficient("6262818231812096")*A2
	    + Coefficient("140737488355328")*B2
	    - Coefficient("6403555720167424")*C2 >= 0);
  cs.insert(Coefficient("455764282289894208")*S1
	    + Coefficient("414331165718085632")*T1
	    + Coefficient("411178645978926272")*U1
	    + Coefficient("367944089556169536")*V1
	    + Coefficient("369745529407117696")*W1
	    + Coefficient("388210287879336768")*X1
	    + Coefficient("345876451382054080")*Y1
	    + Coefficient("400820366835974144")*Z1
	    + Coefficient("406224686388818752")*A2
	    + Coefficient("3602879701896397")*B2
	    - Coefficient("409827566090715136")*C2 >= 0);
  cs.insert(-B2 + 3*C2 >= 0);
  cs.insert(S1 + T1 + U1 + V1 + W1 + X1 + Y1 + Z1 + A2 - C2 == 0);
  cs.insert(Coefficient("-126100789566373888")*D2
	    - Coefficient("23869078025063628")*E2
	    - Coefficient("5629499534213120")*F2
	    - Coefficient("8106479329266893")*G2
	    - Coefficient("25895697857380352")*H2
	    - Coefficient("4503599627370496")*I2
	    - Coefficient("32876277279804620")*J2
	    - Coefficient("13735978863480012")*K2
	    - Coefficient("9457559217478042")*L2
	    + Coefficient("24769797950537728")*N2 >= 0);
  cs.insert(Coefficient("-144115188075855872")*D2
	    - Coefficient("56204923349583792")*E2
	    - Coefficient("38911100780481088")*G2
	    - Coefficient("105204087295374784")*H2
	    - Coefficient("144115188075855872")*J2
	    - Coefficient("47558012065032440")*K2
	    - Coefficient("5764607523034235")*L2
	    + Coefficient("72057594037927936")*N2 >= 0);
  cs.insert(Coefficient("9007199254740992")*D2
	    + Coefficient("9007199254740992")*E2
	    + Coefficient("9007199254740992")*G2
	    + Coefficient("8827055269646172")*H2
	    + Coefficient("3422735716801577")*I2
	    + Coefficient("9007199254740992")*J2
	    + Coefficient("5854679515581645")*K2
	    + Coefficient("3242591731706757")*L2
	    - Coefficient("4503599627370496")*N2 >= 0);
  cs.insert(Coefficient("9007199254740992")*D2
	    + Coefficient("9007199254740992")*E2
	    + Coefficient("7836263351624663")*F2
	    + Coefficient("9007199254740992")*G2
	    + Coefficient("9007199254740992")*H2
	    + Coefficient("9007199254740992")*I2
	    + Coefficient("9007199254740992")*J2
	    + Coefficient("9007199254740992")*K2
	    + Coefficient("8827055269646172")*L2
	    - Coefficient("8106479329266893")*N2 >= 0);
  cs.insert(Coefficient("110000420898524368")*D2
	    + Coefficient("110563370851945680")*E2
	    + Coefficient("98966601811466656")*F2
	    + Coefficient("88720912659198768")*G2
	    + Coefficient("88045372715093200")*H2
	    + Coefficient("75322703767771552")*I2
	    + Coefficient("84104723041144016")*J2
	    + Coefficient("79601123413773520")*K2
	    + Coefficient("110113010889208624")*L2
	    + Coefficient("3715469692580659")*M2
	    - Coefficient("100205091708993536")*N2 >= 0);
  cs.insert(Coefficient("226531061256735936")*D2
	    + Coefficient("220676381741154304")*E2
	    + Coefficient("201085723362092640")*F2
	    + Coefficient("186223844591770016")*G2
	    + Coefficient("186223844591770016")*H2
	    + Coefficient("160778506697126720")*I2
	    + Coefficient("179693625132082784")*J2
	    + Coefficient("170911605858710336")*K2
	    + Coefficient("219550481834311680")*L2
	    + Coefficient("3602879701896397")*M2
	    - Coefficient("200410183417987072")*N2 >= 0);
  cs.insert(Coefficient("106397541196627968")*D2
	    + Coefficient("88495732677830240")*E2
	    + Coefficient("86806882817566304")*F2
	    + Coefficient("84555083003881056")*G2
	    + Coefficient("87820192733724672")*H2
	    + Coefficient("76110833702561376")*I2
	    + Coefficient("87032062798934832")*J2
	    + Coefficient("78362633516246624")*K2
	    + Coefficient("96151852044360096")*L2
	    + Coefficient("4728779608739021")*M2
	    - Coefficient("92323792361095168")*N2 >= 0);
  cs.insert(Coefficient("443604563295993856")*D2
	    + Coefficient("355784370562269184")*E2
	    + Coefficient("357585810413217408")*F2
	    + Coefficient("362539770003324928")*G2
	    + Coefficient("376050568885436416")*H2
	    + Coefficient("332365652499942592")*I2
	    + Coefficient("373798769071751168")*J2
	    + Coefficient("339121051940998336")*K2
	    + Coefficient("388660647842073792")*L2
	    + Coefficient("7656119366529843")*M2
	    - Coefficient("369295169444380672")*N2 >= 0);
  cs.insert(-M2 + 3*N2 >= 0);
  cs.insert(D2 + E2 + F2 + G2 + H2 + I2 + J2 + K2 + L2 - N2 == 0);
  cs.insert(-B - N - C1 - O1 >= -7);
  cs.insert(-C - O >= -7);
  cs.insert(Coefficient("-4503599627370496")*E
	    - Coefficient("4953959590107546")*F
	    - Coefficient("5404319552844595")*G
	    - Coefficient("4503599627370496")*P
	    >= Coefficient("-31525197391593472"));
  cs.insert(-H - R - D1 - M1 >= -21);
  cs.insert(-V - I1 >= -3);
  cs.insert(-Q - W - J1 >= -3);
  cs.insert(-X - K1 >= -3);
  cs.insert(Coefficient("-4503599627370496")*Y
	    - Coefficient("5404319552844595")*Z
	    - Coefficient("4953959590107546")*A1
	    - Coefficient("4953959590107546")*L1
	    - Coefficient("4503599627370496")*N1
	    >= Coefficient("-31525197391593472"));
  cs.insert(-2*T1 >= -3);
  cs.insert(-2*U1 >= -3);
  cs.insert(-V1 - E2 - R2 >= -10);
  cs.insert(-W1 - F2 - S2 >= -10);
  cs.insert(-2*X1 - 2*H2 - 2*U2 >= -17);
  cs.insert(Coefficient("-4503599627370496")*Y1
	    - Coefficient("4953959590107546")*Z1
	    - Coefficient("5404319552844595")*A2
	    - Coefficient("4953959590107546")*L2
	    - Coefficient("4953959590107546")*X2
	    - Coefficient("5404319552844595")*Y2
	    >= Coefficient("-58546795155816448"));
  cs.insert(-2*G2 - 2*T2 >= -3);
  cs.insert(-2*I2 - 2*V2 >= -3);
  cs.insert(-J2 - W2 >= -1);
  cs.insert(-K2 >= -1);
  cs.insert(J - K - L == 15);
  cs.insert(K + F1 - G1 == 15);
  cs.insert(L + G1 + C2 == 20);
  cs.insert(T + R1 + O2 == 20);
  cs.insert(Q1 - R1 + P2 == 15);
  cs.insert(N2 - O2 - P2 == 0);
  cs.insert(-D - B1 >= -1);
  cs.insert(T - 3*R1 - 3*O2 >= 0);
  cs.insert(Coefficient("-126100789566373888")*Q2
	    - Coefficient("23869078025063628")*R2
	    - Coefficient("5629499534213120")*S2
	    - Coefficient("8106479329266893")*T2
	    - Coefficient("25895697857380352")*U2
	    - Coefficient("4503599627370496")*V2
	    - Coefficient("32876277279804620")*W2
	    - Coefficient("9457559217478042")*X2
	    - Coefficient("9682739198846566")*Y2
	    + Coefficient("22517998136852480")*A3 >= 0);
  cs.insert(Coefficient("-144115188075855872")*Q2
	    - Coefficient("56204923349583792")*R2
	    - Coefficient("38911100780481088")*T2
	    - Coefficient("105204087295374784")*U2
	    - Coefficient("144115188075855872")*W2
	    - Coefficient("5764607523034235")*X2
	    - Coefficient("10088063165309912")*Y2
	    + Coefficient("72057594037927936")*A3 >= 0);
  cs.insert(Coefficient("9007199254740992")*Q2
	    + Coefficient("9007199254740992")*R2
	    + Coefficient("9007199254740992")*T2
	    + Coefficient("8827055269646172")*U2
	    + Coefficient("3422735716801577")*V2
	    + Coefficient("9007199254740992")*W2
	    + Coefficient("3242591731706757")*X2
	    + Coefficient("2431943798780068")*Y2
	    - Coefficient("4503599627370496")*A3 >= 0);
  cs.insert(Coefficient("9007199254740992")*Q2
	    + Coefficient("9007199254740992")*R2
	    + Coefficient("7836263351624663")*S2
	    + Coefficient("9007199254740992")*T2
	    + Coefficient("9007199254740992")*U2
	    + Coefficient("9007199254740992")*V2
	    + Coefficient("9007199254740992")*W2
	    + Coefficient("8827055269646172")*X2
	    + Coefficient("8106479329266893")*Y2
	    - Coefficient("8106479329266893")*A3 >= 0);
  cs.insert(Coefficient("110000420898524368")*Q2
	    + Coefficient("110563370851945680")*R2
	    + Coefficient("98966601811466656")*S2
	    + Coefficient("88720912659198768")*T2
	    + Coefficient("88045372715093200")*U2
	    + Coefficient("75322703767771552")*V2
	    + Coefficient("84104723041144016")*W2
	    + Coefficient("110113010889208624")*X2
	    + Coefficient("114166250553842080")*Y2
	    + Coefficient("3715469692580659")*Z2
	    - Coefficient("101330991615836160")*A3 >= 0);
  cs.insert(Coefficient("226531061256735936")*Q2
	    + Coefficient("220676381741154304")*R2
	    + Coefficient("201085723362092640")*S2
	    + Coefficient("186223844591770016")*T2
	    + Coefficient("186223844591770016")*U2
	    + Coefficient("160778506697126720")*V2
	    + Coefficient("179693625132082784")*W2
	    + Coefficient("219550481834311680")*X2
	    + Coefficient("228557681089052672")*Y2
	    + Coefficient("3602879701896397")*Z2
	    - Coefficient("202661983231672320")*A3 >= 0);
  cs.insert(Coefficient("106397541196627968")*Q2
	    + Coefficient("88495732677830240")*R2
	    + Coefficient("86806882817566304")*S2
	    + Coefficient("84555083003881056")*T2
	    + Coefficient("87820192733724672")*U2
	    + Coefficient("76110833702561376")*V2
	    + Coefficient("87032062798934832")*W2
	    + Coefficient("96151852044360096")*X2
	    + Coefficient("100205091708993536")*Y2
	    + Coefficient("4728779608739021")*Z2
	    - Coefficient("93449692267937792")*A3 >= 0);
  cs.insert(Coefficient("443604563295993856")*Q2
	    + Coefficient("355784370562269184")*R2
	    + Coefficient("357585810413217408")*S2
	    + Coefficient("362539770003324928")*T2
	    + Coefficient("376050568885436416")*U2
	    + Coefficient("332365652499942592")*V2
	    + Coefficient("373798769071751168")*W2
	    + Coefficient("388660647842073792")*X2
	    + Coefficient("406224686388818752")*Y2
	    + Coefficient("7656119366529843")*Z2
	    - Coefficient("373798769071751168")*A3 >= 0);
  cs.insert(-Z2 + 3*A3 >= 0);
  cs.insert(Q2 + R2 + S2 + T2 + U2 + V2 + W2 + X2 + Y2 - A3 == 0);
  cs.insert(A >= 0);
  cs.insert(B >= 0);
  cs.insert(C >= 0);
  cs.insert(D >= 0);
  cs.insert(E >= 0);
  cs.insert(F >= 0);
  cs.insert(G >= 0);
  cs.insert(H >= 0);
  cs.insert(I >= 0);
  cs.insert(J >= 0);
  cs.insert(K >= 0);
  cs.insert(L >= 0);
  cs.insert(M >= 0);
  cs.insert(N >= 0);
  cs.insert(O >= 0);
  cs.insert(P >= 0);
  cs.insert(Q >= 0);
  cs.insert(R >= 0);
  cs.insert(S >= 0);
  cs.insert(T >= 0);
  cs.insert(U >= 0);
  cs.insert(V >= 0);
  cs.insert(W >= 0);
  cs.insert(X >= 0);
  cs.insert(Y >= 0);
  cs.insert(Z >= 0);
  cs.insert(A1 >= 0);
  cs.insert(B1 >= 0);
  cs.insert(C1 >= 0);
  cs.insert(D1 >= 0);
  cs.insert(E1 >= 0);
  cs.insert(F1 >= 0);
  cs.insert(G1 >= 0);
  cs.insert(H1 >= 0);
  cs.insert(I1 >= 0);
  cs.insert(J1 >= 0);
  cs.insert(K1 >= 0);
  cs.insert(L1 >= 0);
  cs.insert(M1 >= 0);
  cs.insert(N1 >= 0);
  cs.insert(O1 >= 0);
  cs.insert(P1 >= 0);
  cs.insert(Q1 >= 0);
  cs.insert(R1 >= 0);
  cs.insert(S1 >= 0);
  cs.insert(T1 >= 0);
  cs.insert(U1 >= 0);
  cs.insert(V1 >= 0);
  cs.insert(W1 >= 0);
  cs.insert(X1 >= 0);
  cs.insert(Y1 >= 0);
  cs.insert(Z1 >= 0);
  cs.insert(A2 >= 0);
  cs.insert(B2 >= 0);
  cs.insert(C2 >= 0);
  cs.insert(D2 >= 0);
  cs.insert(E2 >= 0);
  cs.insert(F2 >= 0);
  cs.insert(G2 >= 0);
  cs.insert(H2 >= 0);
  cs.insert(I2 >= 0);
  cs.insert(J2 >= 0);
  cs.insert(K2 >= 0);
  cs.insert(L2 >= 0);
  cs.insert(M2 >= 0);
  cs.insert(N2 >= 0);
  cs.insert(O2 >= 0);
  cs.insert(P2 >= 0);
  cs.insert(Q2 >= 0);
  cs.insert(R2 >= 0);
  cs.insert(S2 >= 0);
  cs.insert(T2 >= 0);
  cs.insert(U2 >= 0);
  cs.insert(V2 >= 0);
  cs.insert(W2 >= 0);
  cs.insert(X2 >= 0);
  cs.insert(Y2 >= 0);
  cs.insert(Z2 >= 0);
  cs.insert(A3 >= 0);
  print_constraints(cs, "*** cs ***");

  Rational_Box known_result(cs.space_dimension());
  known_result.add_constraint(A >= 0);
  known_result.add_constraint(Coefficient("-1152921504606846976")*A
			      > Coefficient("-12264691776765966787"));
  known_result.add_constraint(B >= 0);
  known_result.add_constraint(-B >= -7);
  known_result.add_constraint(C >= 0);
  known_result.add_constraint(-C >= -7);
  known_result.add_constraint(D >= 0);
  known_result.add_constraint(-D >= -1);
  known_result.add_constraint(E >= 0);
  known_result.add_constraint(-E >= -7);
  known_result.add_constraint(F >= 0);
  known_result.add_constraint(Coefficient("-288230376151711744")*F
			      > Coefficient("-1834193302783620041"));
  known_result.add_constraint(G >= 0);
  known_result.add_constraint(Coefficient("-2305843009213693952")*G
			      > Coefficient("-13450750887079881885"));
  known_result.add_constraint(H >= 0);
  known_result.add_constraint(-H >= -21);
  known_result.add_constraint(I >= 0);
  known_result.add_constraint(Coefficient("-72057594037927936")*I
			      > Coefficient("-13588721002666838201"));
  known_result.add_constraint(J >= 15);
  known_result.add_constraint(Coefficient("-144115188075855872")*J
			      > Coefficient("-9059147335111225467"));
  known_result.add_constraint(K >= 0);
  known_result.add_constraint(-K >= -35);
  known_result.add_constraint(L >= 0);
  known_result.add_constraint(-L >= -20);
  known_result.add_constraint(M >= 0);
  known_result.add_constraint(Coefficient("-2305843009213693952")*M
			      > Coefficient("-7804391723492502607"));
  known_result.add_constraint(N >= 0);
  known_result.add_constraint(-N >= -7);
  known_result.add_constraint(O >= 0);
  known_result.add_constraint(-O >= -7);
  known_result.add_constraint(P >= 0);
  known_result.add_constraint(-P >= -7);
  known_result.add_constraint(Q >= 0);
  known_result.add_constraint(-Q >= -3);
  known_result.add_constraint(R >= 0);
  known_result.add_constraint(-R >= -20);
  known_result.add_constraint(S >= 0);
  known_result.add_constraint(-S >= -60);
  known_result.add_constraint(Coefficient("576460752303423488")*T
			      > Coefficient("3843071682022823253"));
  known_result.add_constraint(-T >= -20);
  known_result.add_constraint(U >= 0);
  known_result.add_constraint(-2*U >= -11);
  known_result.add_constraint(V >= 0);
  known_result.add_constraint(-V >= -3);
  known_result.add_constraint(W >= 0);
  known_result.add_constraint(-W >= -3);
  known_result.add_constraint(X >= 0);
  known_result.add_constraint(-X >= -3);
  known_result.add_constraint(Y >= 0);
  known_result.add_constraint(-Y >= -7);
  known_result.add_constraint(Z >= 0);
  known_result.add_constraint(Coefficient("-2305843009213693952")*Z
			      > Coefficient("-13450750887079881885"));
  known_result.add_constraint(A1 >= 0);
  known_result.add_constraint(Coefficient("-288230376151711744")*A1
			      > Coefficient("-1834193302783620041"));
  known_result.add_constraint(B1 >= 0);
  known_result.add_constraint(-B1 >= -1);
  known_result.add_constraint(C1 >= 0);
  known_result.add_constraint(-C1 >= -7);
  known_result.add_constraint(D1 >= 0);
  known_result.add_constraint(-D1 >= -21);
  known_result.add_constraint(E1 >= 0);
  known_result.add_constraint(-E1 >= -105);
  known_result.add_constraint(F1 >= 0);
  known_result.add_constraint(-F1 >= -35);
  known_result.add_constraint(G1 >= 0);
  known_result.add_constraint(-G1 >= -20);
  known_result.add_constraint(H1 >= 0);
  known_result.add_constraint(Coefficient("-2305843009213693952")*H1
			      > Coefficient("-7850846436132338933"));
  known_result.add_constraint(I1 >= 0);
  known_result.add_constraint(-I1 >= -3);
  known_result.add_constraint(J1 >= 0);
  known_result.add_constraint(-J1 >= -3);
  known_result.add_constraint(K1 >= 0);
  known_result.add_constraint(-K1 >= -3);
  known_result.add_constraint(L1 >= 0);
  known_result.add_constraint(Coefficient("-288230376151711744")*L1
			      > Coefficient("-1834193302783620041"));
  known_result.add_constraint(M1 >= 0);
  known_result.add_constraint(-M1 >= -21);
  known_result.add_constraint(N1 >= 0);
  known_result.add_constraint(-N1 >= -7);
  known_result.add_constraint(O1 >= 0);
  known_result.add_constraint(-O1 >= -7);
  known_result.add_constraint(P1 >= 0);
  known_result.add_constraint(Coefficient("-144115188075855872")*P1
			      > Coefficient("-9367487224930631681"));
  known_result.add_constraint(Q1 >= 0);
  known_result.add_constraint(Coefficient("-288230376151711744")*Q1
			      > Coefficient("-6244991483287087787"));
  known_result.add_constraint(R1 >= 0);
  known_result.add_constraint(Coefficient("-1152921504606846976")*R1
			      > Coefficient("-7686143364045646507"));
  known_result.add_constraint(S1 >= 0);
  known_result.add_constraint(Coefficient("-4611686018427387904")*S1
			      > Coefficient("-16470307208669242515"));
  known_result.add_constraint(T1 >= 0);
  known_result.add_constraint(-2*T1 >= -3);
  known_result.add_constraint(U1 >= 0);
  known_result.add_constraint(-2*U1 >= -3);
  known_result.add_constraint(V1 >= 0);
  known_result.add_constraint(-V1 >= -10);
  known_result.add_constraint(W1 >= 0);
  known_result.add_constraint(-W1 >= -10);
  known_result.add_constraint(X1 >= 0);
  known_result.add_constraint(-2*X1 >= -17);
  known_result.add_constraint(Y1 >= 0);
  known_result.add_constraint(-Y1 >= -13);
  known_result.add_constraint(Z1 >= 0);
  known_result.add_constraint(Coefficient("-576460752303423488")*Z1
			      > Coefficient("-6812717981767731581"));
  known_result.add_constraint(A2 >= 0);
  known_result.add_constraint(Coefficient("-288230376151711744")*A2
			      > Coefficient("-3122495741643544009"));
  known_result.add_constraint(B2 >= 0);
  known_result.add_constraint(-B2 >= -60);
  known_result.add_constraint(C2 >= 0);
  known_result.add_constraint(-C2 >= -20);
  known_result.add_constraint(D2 >= 0);
  known_result.add_constraint(Coefficient("-2305843009213693952")*D2
			      > Coefficient("-12833114366754784795"));
  known_result.add_constraint(E2 >= 0);
  known_result.add_constraint(-E2 >= -10);
  known_result.add_constraint(F2 >= 0);
  known_result.add_constraint(-F2 >= -10);
  known_result.add_constraint(G2 >= 0);
  known_result.add_constraint(-2*G2 >= -3);
  known_result.add_constraint(H2 >= 0);
  known_result.add_constraint(-2*H2 >= -17);
  known_result.add_constraint(I2 >= 0);
  known_result.add_constraint(-2*I2 >= -3);
  known_result.add_constraint(J2 >= 0);
  known_result.add_constraint(-J2 >= -1);
  known_result.add_constraint(K2 >= 0);
  known_result.add_constraint(-K2 >= -1);
  known_result.add_constraint(L2 >= 0);
  known_result.add_constraint(Coefficient("-576460752303423488")*L2
			      > Coefficient("-6812717981767731581"));
  known_result.add_constraint(M2 >= 0);
  known_result.add_constraint(Coefficient("-144115188075855872")*M2
			      > Coefficient("-12249790986447749121"));
  known_result.add_constraint(N2 >= 0);
  known_result.add_constraint(Coefficient("-144115188075855872")*N2
			      > Coefficient("-4083263662149249707"));
  known_result.add_constraint(O2 >= 0);
  known_result.add_constraint(Coefficient("-1152921504606846976")*O2
			      > Coefficient("-7686143364045646507"));
  known_result.add_constraint(P2 >= 0);
  known_result.add_constraint(Coefficient("-288230376151711744")*P2
			      > Coefficient("-6244991483287087787"));
  known_result.add_constraint(Q2 >= 0);
  known_result.add_constraint(R2 >= 0);
  known_result.add_constraint(-R2 >= -10);
  known_result.add_constraint(S2 >= 0);
  known_result.add_constraint(-S2 >= -10);
  known_result.add_constraint(T2 >= 0);
  known_result.add_constraint(-2*T2 >= -3);
  known_result.add_constraint(U2 >= 0);
  known_result.add_constraint(-2*U2 >= -17);
  known_result.add_constraint(V2 >= 0);
  known_result.add_constraint(-2*V2 >= -3);
  known_result.add_constraint(W2 >= 0);
  known_result.add_constraint(-W2 >= -1);
  known_result.add_constraint(X2 >= 0);
  known_result.add_constraint(Coefficient("-576460752303423488")*X2
			      > Coefficient("-6812717981767731581"));
  known_result.add_constraint(Y2 >= 0);
  known_result.add_constraint(Coefficient("-288230376151711744")*Y2
			      > Coefficient("-3122495741643544009"));
  known_result.add_constraint(Z2 >= 0);
  known_result.add_constraint(A3 >= 0);
  print_constraints(known_result, "*** known_result ***");

  TBox box(cs.space_dimension());

  bool ok = false;

  typedef TBox::interval_type::boundary_type boundary_type;
  if (std::numeric_limits<boundary_type>::is_exact
      && !std::numeric_limits<boundary_type>::is_integer) {
    // With interval boundaries made of rational numbers, this
    // refinement instance either does not terminate or terminates
    // very slowly: we use a watchdog timer.
    try {
      // Set a 0.5 seconds timeout.
      Parma_Watchdog_Library::Watchdog
	w(50, abandon_expensive_computations, t);

      box.refine(cs);

      // We should never get here.
      abandon_expensive_computations = 0;
      nout << "unexpected termination" << endl;
      ok = false;
    }
    catch (const Timeout&) {
      abandon_expensive_computations = 0;
      nout << "timeout, as expected" << endl;

      // The box will have been shrunk, nonetheless.
      ok = check_result(box, known_result, "1.0e-6", "1.0e-6", "1.0e-6");
    }
    catch (...) {
      nout << "unexpected exception" << endl;
      ok = false;
    }
  }
  else {
    // With interval boundaries other than rational numbers, this instance
    // of refinement terminates rather quickly: no timer is necessary.
    box.refine(cs);

    ok = check_result(box, known_result, "1.42e-4", "8.43e-5", "8.01e-5");
  }

  print_constraints(box, "*** box.refine(cs) ***");

  return ok;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  //DO_TEST(test05);
END_MAIN
