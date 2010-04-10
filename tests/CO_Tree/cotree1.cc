/* Test the CO_Tree class.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>

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

#include "ppl_test.hh"

#include <vector>
#include <algorithm>
#include <set>

namespace {

bool
test01() {

  CO_Tree tree(15);

  CO_Tree::inorder_iterator itr = tree.before_begin();
  CO_Tree::inorder_iterator end = tree.end();

  ++itr;

  for ( ; itr != end; ++itr)
    ;

  return true;
}

void
test02_helper(CO_Tree::inorder_iterator& itr) {
  if (!itr.is_leaf()) {
    itr.get_left_child();
    test02_helper(itr);
    itr.get_parent();
  }
  if (!itr.is_leaf()) {
    itr.get_right_child();
    test02_helper(itr);
    itr.get_parent();
  }
}

bool
test02() {

  CO_Tree tree(30);

  CO_Tree::inorder_iterator itr(&tree);

  test02_helper(itr);

  return true;
}

bool
test03() {

  // Sequential insertion, sequential erase.

  CO_Tree tree(1);

  for (unsigned n = 0; n < 500; ++n)
    tree.insert(n, n);

  for (unsigned n = 0; n < 500; ++n)
    if (!tree.erase(n))
      return false;

  return true;
}

bool
test04() {

  // Sequential insertion, backwards erase.

  CO_Tree tree(1);

  for (unsigned n = 0; n < 500; ++n)
    tree.insert(n, n);

  for (unsigned n = 500; n-- > 0; )
    if (!tree.erase(n))
      return false;

  return true;
}

bool
test05() {

  // Backwards insertion, sequential erase.

  CO_Tree tree(1);

  for (unsigned n = 500; n-- > 0; )
    tree.insert(n, n);

  for (unsigned n = 0; n < 500; ++n)
    if (!tree.erase(n))
      return false;

  return true;
}

bool
test06() {

  // Backwards insertion, backwards erase.

  CO_Tree tree(1);

  for (unsigned n = 500; --n > 0; )
    tree.insert(n, n);

  for (unsigned n = 500; --n > 0; )
    if (!tree.erase(n))
      return false;

  return true;
}

bool
test07() {

  // Sequential insertion, pseudo-random erase.

  CO_Tree tree(1);

  for (unsigned n = 0; n < 500; ++n)
    tree.insert(n, n);

  if (!tree.erase(110))
    return false;
  if (!tree.erase(290))
    return false;
  if (!tree.erase(11))
    return false;
  if (!tree.erase(69))
    return false;
  if (!tree.erase(209))
    return false;
  if (!tree.erase(468))
    return false;
  if (!tree.erase(17))
    return false;
  if (!tree.erase(293))
    return false;
  if (!tree.erase(164))
    return false;
  if (!tree.erase(350))
    return false;
  if (!tree.erase(115))
    return false;
  if (!tree.erase(322))
    return false;
  if (!tree.erase(361))
    return false;
  if (!tree.erase(2))
    return false;
  if (!tree.erase(446))
    return false;
  if (!tree.erase(281))
    return false;
  if (!tree.erase(400))
    return false;
  if (!tree.erase(375))
    return false;
  if (!tree.erase(153))
    return false;
  if (!tree.erase(116))
    return false;
  if (!tree.erase(143))
    return false;
  if (!tree.erase(329))
    return false;
  if (!tree.erase(420))
    return false;
  if (!tree.erase(133))
    return false;
  if (!tree.erase(363))
    return false;
  if (!tree.erase(192))
    return false;
  if (!tree.erase(27))
    return false;
  if (!tree.erase(275))
    return false;
  if (!tree.erase(104))
    return false;
  if (!tree.erase(213))
    return false;
  if (!tree.erase(352))
    return false;
  if (!tree.erase(427))
    return false;
  if (!tree.erase(273))
    return false;
  if (!tree.erase(288))
    return false;
  if (!tree.erase(396))
    return false;
  if (!tree.erase(114))
    return false;
  if (!tree.erase(341))
    return false;
  if (!tree.erase(413))
    return false;
  if (!tree.erase(201))
    return false;
  if (!tree.erase(280))
    return false;
  if (!tree.erase(66))
    return false;
  if (!tree.erase(227))
    return false;
  if (!tree.erase(406))
    return false;
  if (!tree.erase(79))
    return false;
  if (!tree.erase(259))
    return false;
  if (!tree.erase(176))
    return false;
  if (!tree.erase(106))
    return false;
  if (!tree.erase(0))
    return false;
  if (!tree.erase(328))
    return false;
  if (!tree.erase(270))
    return false;
  if (!tree.erase(449))
    return false;
  if (!tree.erase(165))
    return false;
  if (!tree.erase(163))
    return false;
  if (!tree.erase(43))
    return false;
  if (!tree.erase(391))
    return false;
  if (!tree.erase(202))
    return false;
  if (!tree.erase(49))
    return false;
  if (!tree.erase(105))
    return false;
  if (!tree.erase(149))
    return false;
  if (!tree.erase(318))
    return false;
  if (!tree.erase(387))
    return false;
  if (!tree.erase(389))
    return false;
  if (!tree.erase(141))
    return false;
  if (!tree.erase(408))
    return false;
  if (!tree.erase(486))
    return false;
  if (!tree.erase(354))
    return false;
  if (!tree.erase(8))
    return false;
  if (!tree.erase(33))
    return false;
  if (!tree.erase(421))
    return false;
  if (!tree.erase(385))
    return false;
  if (!tree.erase(25))
    return false;
  if (!tree.erase(485))
    return false;
  if (!tree.erase(196))
    return false;
  if (!tree.erase(31))
    return false;
  if (!tree.erase(82))
    return false;
  if (!tree.erase(434))
    return false;
  if (!tree.erase(423))
    return false;
  if (!tree.erase(358))
    return false;
  if (!tree.erase(255))
    return false;
  if (!tree.erase(287))
    return false;
  if (!tree.erase(23))
    return false;
  if (!tree.erase(122))
    return false;
  if (!tree.erase(489))
    return false;
  if (!tree.erase(19))
    return false;
  if (!tree.erase(126))
    return false;
  if (!tree.erase(44))
    return false;
  if (!tree.erase(120))
    return false;
  if (!tree.erase(131))
    return false;
  if (!tree.erase(332))
    return false;
  if (!tree.erase(448))
    return false;
  if (!tree.erase(238))
    return false;
  if (!tree.erase(264))
    return false;
  if (!tree.erase(454))
    return false;
  if (!tree.erase(218))
    return false;
  if (!tree.erase(157))
    return false;
  if (!tree.erase(436))
    return false;
  if (!tree.erase(225))
    return false;
  if (!tree.erase(437))
    return false;
  if (!tree.erase(443))
    return false;
  if (!tree.erase(179))
    return false;
  if (!tree.erase(265))
    return false;
  if (!tree.erase(475))
    return false;
  if (!tree.erase(180))
    return false;
  if (!tree.erase(487))
    return false;
  if (!tree.erase(339))
    return false;
  if (!tree.erase(492))
    return false;
  if (!tree.erase(395))
    return false;
  if (!tree.erase(491))
    return false;
  if (!tree.erase(223))
    return false;
  if (!tree.erase(113))
    return false;
  if (!tree.erase(92))
    return false;
  if (!tree.erase(48))
    return false;
  if (!tree.erase(61))
    return false;
  if (!tree.erase(127))
    return false;
  if (!tree.erase(190))
    return false;
  if (!tree.erase(67))
    return false;
  if (!tree.erase(484))
    return false;
  if (!tree.erase(439))
    return false;
  if (!tree.erase(355))
    return false;
  if (!tree.erase(243))
    return false;
  if (!tree.erase(392))
    return false;
  if (!tree.erase(159))
    return false;
  if (!tree.erase(74))
    return false;
  if (!tree.erase(337))
    return false;
  if (!tree.erase(151))
    return false;
  if (!tree.erase(458))
    return false;
  if (!tree.erase(480))
    return false;
  if (!tree.erase(334))
    return false;
  if (!tree.erase(419))
    return false;
  if (!tree.erase(309))
    return false;
  if (!tree.erase(301))
    return false;
  if (!tree.erase(125))
    return false;
  if (!tree.erase(407))
    return false;
  if (!tree.erase(496))
    return false;
  if (!tree.erase(187))
    return false;
  if (!tree.erase(50))
    return false;
  if (!tree.erase(368))
    return false;
  if (!tree.erase(283))
    return false;
  if (!tree.erase(244))
    return false;
  if (!tree.erase(170))
    return false;
  if (!tree.erase(118))
    return false;
  if (!tree.erase(457))
    return false;
  if (!tree.erase(181))
    return false;
  if (!tree.erase(479))
    return false;
  if (!tree.erase(401))
    return false;
  if (!tree.erase(494))
    return false;
  if (!tree.erase(99))
    return false;
  if (!tree.erase(236))
    return false;
  if (!tree.erase(240))
    return false;
  if (!tree.erase(147))
    return false;
  if (!tree.erase(233))
    return false;
  if (!tree.erase(172))
    return false;
  if (!tree.erase(266))
    return false;
  if (!tree.erase(32))
    return false;
  if (!tree.erase(210))
    return false;
  if (!tree.erase(161))
    return false;
  if (!tree.erase(156))
    return false;
  if (!tree.erase(178))
    return false;
  if (!tree.erase(221))
    return false;
  if (!tree.erase(78))
    return false;
  if (!tree.erase(85))
    return false;
  if (!tree.erase(135))
    return false;
  if (!tree.erase(145))
    return false;
  if (!tree.erase(356))
    return false;
  if (!tree.erase(397))
    return false;
  if (!tree.erase(450))
    return false;
  if (!tree.erase(276))
    return false;
  if (!tree.erase(41))
    return false;
  if (!tree.erase(414))
    return false;
  if (!tree.erase(14))
    return false;
  if (!tree.erase(22))
    return false;
  if (!tree.erase(29))
    return false;
  if (!tree.erase(34))
    return false;
  if (!tree.erase(498))
    return false;
  if (!tree.erase(250))
    return false;
  if (!tree.erase(36))
    return false;
  if (!tree.erase(320))
    return false;
  if (!tree.erase(268))
    return false;
  if (!tree.erase(195))
    return false;
  if (!tree.erase(382))
    return false;
  if (!tree.erase(441))
    return false;
  if (!tree.erase(235))
    return false;
  if (!tree.erase(346))
    return false;
  if (!tree.erase(476))
    return false;
  if (!tree.erase(217))
    return false;
  if (!tree.erase(335))
    return false;
  if (!tree.erase(121))
    return false;
  if (!tree.erase(94))
    return false;
  if (!tree.erase(278))
    return false;
  if (!tree.erase(272))
    return false;
  if (!tree.erase(207))
    return false;
  if (!tree.erase(463))
    return false;
  if (!tree.erase(150))
    return false;
  if (!tree.erase(432))
    return false;
  if (!tree.erase(410))
    return false;
  if (!tree.erase(208))
    return false;
  if (!tree.erase(70))
    return false;
  if (!tree.erase(84))
    return false;
  if (!tree.erase(186))
    return false;
  if (!tree.erase(6))
    return false;
  if (!tree.erase(224))
    return false;
  if (!tree.erase(9))
    return false;
  if (!tree.erase(60))
    return false;
  if (!tree.erase(175))
    return false;
  if (!tree.erase(430))
    return false;
  if (!tree.erase(128))
    return false;
  if (!tree.erase(129))
    return false;
  if (!tree.erase(465))
    return false;
  if (!tree.erase(459))
    return false;
  if (!tree.erase(289))
    return false;
  if (!tree.erase(261))
    return false;
  if (!tree.erase(26))
    return false;
  if (!tree.erase(461))
    return false;
  if (!tree.erase(279))
    return false;
  if (!tree.erase(245))
    return false;
  if (!tree.erase(478))
    return false;
  if (!tree.erase(403))
    return false;
  if (!tree.erase(45))
    return false;
  if (!tree.erase(359))
    return false;
  if (!tree.erase(327))
    return false;
  if (!tree.erase(393))
    return false;
  if (!tree.erase(373))
    return false;
  if (!tree.erase(304))
    return false;
  if (!tree.erase(83))
    return false;
  if (!tree.erase(160))
    return false;
  if (!tree.erase(198))
    return false;
  if (!tree.erase(103))
    return false;
  if (!tree.erase(367))
    return false;
  if (!tree.erase(76))
    return false;
  if (!tree.erase(73))
    return false;
  if (!tree.erase(167))
    return false;
  if (!tree.erase(291))
    return false;
  if (!tree.erase(215))
    return false;
  if (!tree.erase(219))
    return false;
  if (!tree.erase(119))
    return false;
  if (!tree.erase(456))
    return false;
  if (!tree.erase(197))
    return false;
  if (!tree.erase(477))
    return false;
  if (!tree.erase(222))
    return false;
  if (!tree.erase(174))
    return false;
  if (!tree.erase(451))
    return false;
  if (!tree.erase(214))
    return false;
  if (!tree.erase(112))
    return false;
  if (!tree.erase(464))
    return false;
  if (!tree.erase(262))
    return false;
  if (!tree.erase(47))
    return false;
  if (!tree.erase(347))
    return false;
  if (!tree.erase(111))
    return false;
  if (!tree.erase(148))
    return false;
  if (!tree.erase(308))
    return false;
  if (!tree.erase(340))
    return false;
  if (!tree.erase(100))
    return false;
  if (!tree.erase(130))
    return false;
  if (!tree.erase(323))
    return false;
  if (!tree.erase(312))
    return false;
  if (!tree.erase(292))
    return false;
  if (!tree.erase(35))
    return false;
  if (!tree.erase(306))
    return false;
  if (!tree.erase(58))
    return false;
  if (!tree.erase(353))
    return false;
  if (!tree.erase(452))
    return false;
  if (!tree.erase(91))
    return false;
  if (!tree.erase(319))
    return false;
  if (!tree.erase(330))
    return false;
  if (!tree.erase(473))
    return false;
  if (!tree.erase(488))
    return false;
  if (!tree.erase(134))
    return false;
  if (!tree.erase(315))
    return false;
  if (!tree.erase(253))
    return false;
  if (!tree.erase(374))
    return false;
  if (!tree.erase(384))
    return false;
  if (!tree.erase(95))
    return false;
  if (!tree.erase(370))
    return false;
  if (!tree.erase(13))
    return false;
  if (!tree.erase(183))
    return false;
  if (!tree.erase(136))
    return false;
  if (!tree.erase(313))
    return false;
  if (!tree.erase(307))
    return false;
  if (!tree.erase(239))
    return false;
  if (!tree.erase(258))
    return false;
  if (!tree.erase(405))
    return false;
  if (!tree.erase(56))
    return false;
  if (!tree.erase(228))
    return false;
  if (!tree.erase(455))
    return false;
  if (!tree.erase(317))
    return false;
  if (!tree.erase(497))
    return false;
  if (!tree.erase(102))
    return false;
  if (!tree.erase(117))
    return false;
  if (!tree.erase(68))
    return false;
  if (!tree.erase(234))
    return false;
  if (!tree.erase(51))
    return false;
  if (!tree.erase(107))
    return false;
  if (!tree.erase(349))
    return false;
  if (!tree.erase(348))
    return false;
  if (!tree.erase(416))
    return false;
  if (!tree.erase(88))
    return false;
  if (!tree.erase(89))
    return false;
  if (!tree.erase(366))
    return false;
  if (!tree.erase(109))
    return false;
  if (!tree.erase(189))
    return false;
  if (!tree.erase(333))
    return false;
  if (!tree.erase(3))
    return false;
  if (!tree.erase(394))
    return false;
  if (!tree.erase(267))
    return false;
  if (!tree.erase(269))
    return false;
  if (!tree.erase(246))
    return false;
  if (!tree.erase(152))
    return false;
  if (!tree.erase(173))
    return false;
  if (!tree.erase(438))
    return false;
  if (!tree.erase(24))
    return false;
  if (!tree.erase(15))
    return false;
  if (!tree.erase(390))
    return false;
  if (!tree.erase(284))
    return false;
  if (!tree.erase(360))
    return false;
  if (!tree.erase(371))
    return false;
  if (!tree.erase(81))
    return false;
  if (!tree.erase(65))
    return false;
  if (!tree.erase(299))
    return false;
  if (!tree.erase(132))
    return false;
  if (!tree.erase(98))
    return false;
  if (!tree.erase(303))
    return false;
  if (!tree.erase(139))
    return false;
  if (!tree.erase(453))
    return false;
  if (!tree.erase(402))
    return false;
  if (!tree.erase(20))
    return false;
  if (!tree.erase(54))
    return false;
  if (!tree.erase(499))
    return false;
  if (!tree.erase(260))
    return false;
  if (!tree.erase(285))
    return false;
  if (!tree.erase(381))
    return false;
  if (!tree.erase(357))
    return false;
  if (!tree.erase(248))
    return false;
  if (!tree.erase(362))
    return false;
  if (!tree.erase(62))
    return false;
  if (!tree.erase(203))
    return false;
  if (!tree.erase(411))
    return false;
  if (!tree.erase(444))
    return false;
  if (!tree.erase(388))
    return false;
  if (!tree.erase(10))
    return false;
  if (!tree.erase(342))
    return false;
  if (!tree.erase(229))
    return false;
  if (!tree.erase(481))
    return false;
  if (!tree.erase(369))
    return false;
  if (!tree.erase(378))
    return false;
  if (!tree.erase(38))
    return false;
  if (!tree.erase(77))
    return false;
  if (!tree.erase(415))
    return false;
  if (!tree.erase(466))
    return false;
  if (!tree.erase(404))
    return false;
  if (!tree.erase(90))
    return false;
  if (!tree.erase(101))
    return false;
  if (!tree.erase(169))
    return false;
  if (!tree.erase(435))
    return false;
  if (!tree.erase(296))
    return false;
  if (!tree.erase(282))
    return false;
  if (!tree.erase(63))
    return false;
  if (!tree.erase(52))
    return false;
  if (!tree.erase(40))
    return false;
  if (!tree.erase(231))
    return false;
  if (!tree.erase(302))
    return false;
  if (!tree.erase(18))
    return false;
  if (!tree.erase(383))
    return false;
  if (!tree.erase(194))
    return false;
  if (!tree.erase(351))
    return false;
  if (!tree.erase(254))
    return false;
  if (!tree.erase(431))
    return false;
  if (!tree.erase(199))
    return false;
  if (!tree.erase(80))
    return false;
  if (!tree.erase(300))
    return false;
  if (!tree.erase(140))
    return false;
  if (!tree.erase(324))
    return false;
  if (!tree.erase(286))
    return false;
  if (!tree.erase(188))
    return false;
  if (!tree.erase(386))
    return false;
  if (!tree.erase(344))
    return false;
  if (!tree.erase(166))
    return false;
  if (!tree.erase(4))
    return false;
  if (!tree.erase(226))
    return false;
  if (!tree.erase(316))
    return false;
  if (!tree.erase(158))
    return false;
  if (!tree.erase(447))
    return false;
  if (!tree.erase(86))
    return false;
  if (!tree.erase(398))
    return false;
  if (!tree.erase(108))
    return false;
  if (!tree.erase(230))
    return false;
  if (!tree.erase(310))
    return false;
  if (!tree.erase(495))
    return false;
  if (!tree.erase(171))
    return false;
  if (!tree.erase(380))
    return false;
  if (!tree.erase(249))
    return false;
  if (!tree.erase(433))
    return false;
  if (!tree.erase(16))
    return false;
  if (!tree.erase(470))
    return false;
  if (!tree.erase(277))
    return false;
  if (!tree.erase(21))
    return false;
  if (!tree.erase(372))
    return false;
  if (!tree.erase(252))
    return false;
  if (!tree.erase(424))
    return false;
  if (!tree.erase(144))
    return false;
  if (!tree.erase(377))
    return false;
  if (!tree.erase(59))
    return false;
  if (!tree.erase(46))
    return false;
  if (!tree.erase(55))
    return false;
  if (!tree.erase(429))
    return false;
  if (!tree.erase(474))
    return false;
  if (!tree.erase(321))
    return false;
  if (!tree.erase(399))
    return false;
  if (!tree.erase(471))
    return false;
  if (!tree.erase(237))
    return false;
  if (!tree.erase(442))
    return false;
  if (!tree.erase(97))
    return false;
  if (!tree.erase(220))
    return false;
  if (!tree.erase(445))
    return false;
  if (!tree.erase(326))
    return false;
  if (!tree.erase(37))
    return false;
  if (!tree.erase(336))
    return false;
  if (!tree.erase(343))
    return false;
  if (!tree.erase(412))
    return false;
  if (!tree.erase(409))
    return false;
  if (!tree.erase(460))
    return false;
  if (!tree.erase(57))
    return false;
  if (!tree.erase(168))
    return false;
  if (!tree.erase(295))
    return false;
  if (!tree.erase(247))
    return false;
  if (!tree.erase(482))
    return false;
  if (!tree.erase(425))
    return false;
  if (!tree.erase(256))
    return false;
  if (!tree.erase(96))
    return false;
  if (!tree.erase(53))
    return false;
  if (!tree.erase(469))
    return false;
  if (!tree.erase(162))
    return false;
  if (!tree.erase(493))
    return false;
  if (!tree.erase(294))
    return false;
  if (!tree.erase(177))
    return false;
  if (!tree.erase(212))
    return false;
  if (!tree.erase(30))
    return false;
  if (!tree.erase(5))
    return false;
  if (!tree.erase(193))
    return false;
  if (!tree.erase(483))
    return false;
  if (!tree.erase(124))
    return false;
  if (!tree.erase(87))
    return false;
  if (!tree.erase(64))
    return false;
  if (!tree.erase(490))
    return false;
  if (!tree.erase(155))
    return false;
  if (!tree.erase(422))
    return false;
  if (!tree.erase(191))
    return false;
  if (!tree.erase(75))
    return false;
  if (!tree.erase(325))
    return false;
  if (!tree.erase(1))
    return false;
  if (!tree.erase(182))
    return false;
  if (!tree.erase(28))
    return false;
  if (!tree.erase(364))
    return false;
  if (!tree.erase(42))
    return false;
  if (!tree.erase(39))
    return false;
  if (!tree.erase(376))
    return false;
  if (!tree.erase(467))
    return false;
  if (!tree.erase(426))
    return false;
  if (!tree.erase(205))
    return false;
  if (!tree.erase(365))
    return false;
  if (!tree.erase(137))
    return false;
  if (!tree.erase(297))
    return false;
  if (!tree.erase(462))
    return false;
  if (!tree.erase(241))
    return false;
  if (!tree.erase(123))
    return false;
  if (!tree.erase(206))
    return false;
  if (!tree.erase(440))
    return false;
  if (!tree.erase(216))
    return false;
  if (!tree.erase(146))
    return false;
  if (!tree.erase(142))
    return false;
  if (!tree.erase(72))
    return false;
  if (!tree.erase(379))
    return false;
  if (!tree.erase(472))
    return false;
  if (!tree.erase(305))
    return false;
  if (!tree.erase(271))
    return false;
  if (!tree.erase(298))
    return false;
  if (!tree.erase(232))
    return false;
  if (!tree.erase(242))
    return false;
  if (!tree.erase(184))
    return false;
  if (!tree.erase(138))
    return false;
  if (!tree.erase(154))
    return false;
  if (!tree.erase(200))
    return false;
  if (!tree.erase(71))
    return false;
  if (!tree.erase(211))
    return false;
  if (!tree.erase(274))
    return false;
  if (!tree.erase(263))
    return false;
  if (!tree.erase(311))
    return false;
  if (!tree.erase(428))
    return false;
  if (!tree.erase(331))
    return false;
  if (!tree.erase(7))
    return false;
  if (!tree.erase(345))
    return false;
  if (!tree.erase(185))
    return false;
  if (!tree.erase(338))
    return false;
  if (!tree.erase(251))
    return false;
  if (!tree.erase(417))
    return false;
  if (!tree.erase(12))
    return false;
  if (!tree.erase(93))
    return false;
  if (!tree.erase(204))
    return false;
  if (!tree.erase(257))
    return false;
  if (!tree.erase(418))
    return false;
  if (!tree.erase(314))
    return false;

  return true;
}

void
test08_helper(CO_Tree::inorder_iterator& itr) {
  if (itr.get_left_child_value()) {
    test08_helper(itr);
    itr.get_parent();
  }
  if (itr.get_right_child_value()) {
    test08_helper(itr);
    itr.get_parent();
  }
}

bool test08() {

  static const dimension_type num_tested_elements = 500;

  CO_Tree tree(num_tested_elements);

  CO_Tree::inorder_iterator itr(&tree);

  test08_helper(itr);

  return true;
}

bool test09() {

  // Pseudo-random insertion, pseudo-random erase (in the same order).

  CO_Tree tree(1);

  tree.insert(110, 110);
  tree.insert(290, 290);
  tree.insert(11, 11);
  tree.insert(69, 69);
  tree.insert(209, 209);
  tree.insert(468, 468);
  tree.insert(17, 17);
  tree.insert(293, 293);
  tree.insert(164, 164);
  tree.insert(350, 350);
  tree.insert(115, 115);
  tree.insert(322, 322);
  tree.insert(361, 361);
  tree.insert(2, 2);
  tree.insert(446, 446);
  tree.insert(281, 281);
  tree.insert(400, 400);
  tree.insert(375, 375);
  tree.insert(153, 153);
  tree.insert(116, 116);
  tree.insert(143, 143);
  tree.insert(329, 329);
  tree.insert(420, 420);
  tree.insert(133, 133);
  tree.insert(363, 363);
  tree.insert(192, 192);
  tree.insert(27, 27);
  tree.insert(275, 275);
  tree.insert(104, 104);
  tree.insert(213, 213);
  tree.insert(352, 352);
  tree.insert(427, 427);
  tree.insert(273, 273);
  tree.insert(288, 288);
  tree.insert(396, 396);
  tree.insert(114, 114);
  tree.insert(341, 341);
  tree.insert(413, 413);
  tree.insert(201, 201);
  tree.insert(280, 280);
  tree.insert(66, 66);
  tree.insert(227, 227);
  tree.insert(406, 406);
  tree.insert(79, 79);
  tree.insert(259, 259);
  tree.insert(176, 176);
  tree.insert(106, 106);
  tree.insert(0, 0);
  tree.insert(328, 328);
  tree.insert(270, 270);
  tree.insert(449, 449);
  tree.insert(165, 165);
  tree.insert(163, 163);
  tree.insert(43, 43);
  tree.insert(391, 391);
  tree.insert(202, 202);
  tree.insert(49, 49);
  tree.insert(105, 105);
  tree.insert(149, 149);
  tree.insert(318, 318);
  tree.insert(387, 387);
  tree.insert(389, 389);
  tree.insert(141, 141);
  tree.insert(408, 408);
  tree.insert(486, 486);
  tree.insert(354, 354);
  tree.insert(8, 8);
  tree.insert(33, 33);
  tree.insert(421, 421);
  tree.insert(385, 385);
  tree.insert(25, 25);
  tree.insert(485, 485);
  tree.insert(196, 196);
  tree.insert(31, 31);
  tree.insert(82, 82);
  tree.insert(434, 434);
  tree.insert(423, 423);
  tree.insert(358, 358);
  tree.insert(255, 255);
  tree.insert(287, 287);
  tree.insert(23, 23);
  tree.insert(122, 122);
  tree.insert(489, 489);
  tree.insert(19, 19);
  tree.insert(126, 126);
  tree.insert(44, 44);
  tree.insert(120, 120);
  tree.insert(131, 131);
  tree.insert(332, 332);
  tree.insert(448, 448);
  tree.insert(238, 238);
  tree.insert(264, 264);
  tree.insert(454, 454);
  tree.insert(218, 218);
  tree.insert(157, 157);
  tree.insert(436, 436);
  tree.insert(225, 225);
  tree.insert(437, 437);
  tree.insert(443, 443);
  tree.insert(179, 179);
  tree.insert(265, 265);
  tree.insert(475, 475);
  tree.insert(180, 180);
  tree.insert(487, 487);
  tree.insert(339, 339);
  tree.insert(492, 492);
  tree.insert(395, 395);
  tree.insert(491, 491);
  tree.insert(223, 223);
  tree.insert(113, 113);
  tree.insert(92, 92);
  tree.insert(48, 48);
  tree.insert(61, 61);
  tree.insert(127, 127);
  tree.insert(190, 190);
  tree.insert(67, 67);
  tree.insert(484, 484);
  tree.insert(439, 439);
  tree.insert(355, 355);
  tree.insert(243, 243);
  tree.insert(392, 392);
  tree.insert(159, 159);
  tree.insert(74, 74);
  tree.insert(337, 337);
  tree.insert(151, 151);
  tree.insert(458, 458);
  tree.insert(480, 480);
  tree.insert(334, 334);
  tree.insert(419, 419);
  tree.insert(309, 309);
  tree.insert(301, 301);
  tree.insert(125, 125);
  tree.insert(407, 407);
  tree.insert(496, 496);
  tree.insert(187, 187);
  tree.insert(50, 50);
  tree.insert(368, 368);
  tree.insert(283, 283);
  tree.insert(244, 244);
  tree.insert(170, 170);
  tree.insert(118, 118);
  tree.insert(457, 457);
  tree.insert(181, 181);
  tree.insert(479, 479);
  tree.insert(401, 401);
  tree.insert(494, 494);
  tree.insert(99, 99);
  tree.insert(236, 236);
  tree.insert(240, 240);
  tree.insert(147, 147);
  tree.insert(233, 233);
  tree.insert(172, 172);
  tree.insert(266, 266);
  tree.insert(32, 32);
  tree.insert(210, 210);
  tree.insert(161, 161);
  tree.insert(156, 156);
  tree.insert(178, 178);
  tree.insert(221, 221);
  tree.insert(78, 78);
  tree.insert(85, 85);
  tree.insert(135, 135);
  tree.insert(145, 145);
  tree.insert(356, 356);
  tree.insert(397, 397);
  tree.insert(450, 450);
  tree.insert(276, 276);
  tree.insert(41, 41);
  tree.insert(414, 414);
  tree.insert(14, 14);
  tree.insert(22, 22);
  tree.insert(29, 29);
  tree.insert(34, 34);
  tree.insert(498, 498);
  tree.insert(250, 250);
  tree.insert(36, 36);
  tree.insert(320, 320);
  tree.insert(268, 268);
  tree.insert(195, 195);
  tree.insert(382, 382);
  tree.insert(441, 441);
  tree.insert(235, 235);
  tree.insert(346, 346);
  tree.insert(476, 476);
  tree.insert(217, 217);
  tree.insert(335, 335);
  tree.insert(121, 121);
  tree.insert(94, 94);
  tree.insert(278, 278);
  tree.insert(272, 272);
  tree.insert(207, 207);
  tree.insert(463, 463);
  tree.insert(150, 150);
  tree.insert(432, 432);
  tree.insert(410, 410);
  tree.insert(208, 208);
  tree.insert(70, 70);
  tree.insert(84, 84);
  tree.insert(186, 186);
  tree.insert(6, 6);
  tree.insert(224, 224);
  tree.insert(9, 9);
  tree.insert(60, 60);
  tree.insert(175, 175);
  tree.insert(430, 430);
  tree.insert(128, 128);
  tree.insert(129, 129);
  tree.insert(465, 465);
  tree.insert(459, 459);
  tree.insert(289, 289);
  tree.insert(261, 261);
  tree.insert(26, 26);
  tree.insert(461, 461);
  tree.insert(279, 279);
  tree.insert(245, 245);
  tree.insert(478, 478);
  tree.insert(403, 403);
  tree.insert(45, 45);
  tree.insert(359, 359);
  tree.insert(327, 327);
  tree.insert(393, 393);
  tree.insert(373, 373);
  tree.insert(304, 304);
  tree.insert(83, 83);
  tree.insert(160, 160);
  tree.insert(198, 198);
  tree.insert(103, 103);
  tree.insert(367, 367);
  tree.insert(76, 76);
  tree.insert(73, 73);
  tree.insert(167, 167);
  tree.insert(291, 291);
  tree.insert(215, 215);
  tree.insert(219, 219);
  tree.insert(119, 119);
  tree.insert(456, 456);
  tree.insert(197, 197);
  tree.insert(477, 477);
  tree.insert(222, 222);
  tree.insert(174, 174);
  tree.insert(451, 451);
  tree.insert(214, 214);
  tree.insert(112, 112);
  tree.insert(464, 464);
  tree.insert(262, 262);
  tree.insert(47, 47);
  tree.insert(347, 347);
  tree.insert(111, 111);
  tree.insert(148, 148);
  tree.insert(308, 308);
  tree.insert(340, 340);
  tree.insert(100, 100);
  tree.insert(130, 130);
  tree.insert(323, 323);
  tree.insert(312, 312);
  tree.insert(292, 292);
  tree.insert(35, 35);
  tree.insert(306, 306);
  tree.insert(58, 58);
  tree.insert(353, 353);
  tree.insert(452, 452);
  tree.insert(91, 91);
  tree.insert(319, 319);
  tree.insert(330, 330);
  tree.insert(473, 473);
  tree.insert(488, 488);
  tree.insert(134, 134);
  tree.insert(315, 315);
  tree.insert(253, 253);
  tree.insert(374, 374);
  tree.insert(384, 384);
  tree.insert(95, 95);
  tree.insert(370, 370);
  tree.insert(13, 13);
  tree.insert(183, 183);
  tree.insert(136, 136);
  tree.insert(313, 313);
  tree.insert(307, 307);
  tree.insert(239, 239);
  tree.insert(258, 258);
  tree.insert(405, 405);
  tree.insert(56, 56);
  tree.insert(228, 228);
  tree.insert(455, 455);
  tree.insert(317, 317);
  tree.insert(497, 497);
  tree.insert(102, 102);
  tree.insert(117, 117);
  tree.insert(68, 68);
  tree.insert(234, 234);
  tree.insert(51, 51);
  tree.insert(107, 107);
  tree.insert(349, 349);
  tree.insert(348, 348);
  tree.insert(416, 416);
  tree.insert(88, 88);
  tree.insert(89, 89);
  tree.insert(366, 366);
  tree.insert(109, 109);
  tree.insert(189, 189);
  tree.insert(333, 333);
  tree.insert(3, 3);
  tree.insert(394, 394);
  tree.insert(267, 267);
  tree.insert(269, 269);
  tree.insert(246, 246);
  tree.insert(152, 152);
  tree.insert(173, 173);
  tree.insert(438, 438);
  tree.insert(24, 24);
  tree.insert(15, 15);
  tree.insert(390, 390);
  tree.insert(284, 284);
  tree.insert(360, 360);
  tree.insert(371, 371);
  tree.insert(81, 81);
  tree.insert(65, 65);
  tree.insert(299, 299);
  tree.insert(132, 132);
  tree.insert(98, 98);
  tree.insert(303, 303);
  tree.insert(139, 139);
  tree.insert(453, 453);
  tree.insert(402, 402);
  tree.insert(20, 20);
  tree.insert(54, 54);
  tree.insert(499, 499);
  tree.insert(260, 260);
  tree.insert(285, 285);
  tree.insert(381, 381);
  tree.insert(357, 357);
  tree.insert(248, 248);
  tree.insert(362, 362);
  tree.insert(62, 62);
  tree.insert(203, 203);
  tree.insert(411, 411);
  tree.insert(444, 444);
  tree.insert(388, 388);
  tree.insert(10, 10);
  tree.insert(342, 342);
  tree.insert(229, 229);
  tree.insert(481, 481);
  tree.insert(369, 369);
  tree.insert(378, 378);
  tree.insert(38, 38);
  tree.insert(77, 77);
  tree.insert(415, 415);
  tree.insert(466, 466);
  tree.insert(404, 404);
  tree.insert(90, 90);
  tree.insert(101, 101);
  tree.insert(169, 169);
  tree.insert(435, 435);
  tree.insert(296, 296);
  tree.insert(282, 282);
  tree.insert(63, 63);
  tree.insert(52, 52);
  tree.insert(40, 40);
  tree.insert(231, 231);
  tree.insert(302, 302);
  tree.insert(18, 18);
  tree.insert(383, 383);
  tree.insert(194, 194);
  tree.insert(351, 351);
  tree.insert(254, 254);
  tree.insert(431, 431);
  tree.insert(199, 199);
  tree.insert(80, 80);
  tree.insert(300, 300);
  tree.insert(140, 140);
  tree.insert(324, 324);
  tree.insert(286, 286);
  tree.insert(188, 188);
  tree.insert(386, 386);
  tree.insert(344, 344);
  tree.insert(166, 166);
  tree.insert(4, 4);
  tree.insert(226, 226);
  tree.insert(316, 316);
  tree.insert(158, 158);
  tree.insert(447, 447);
  tree.insert(86, 86);
  tree.insert(398, 398);
  tree.insert(108, 108);
  tree.insert(230, 230);
  tree.insert(310, 310);
  tree.insert(495, 495);
  tree.insert(171, 171);
  tree.insert(380, 380);
  tree.insert(249, 249);
  tree.insert(433, 433);
  tree.insert(16, 16);
  tree.insert(470, 470);
  tree.insert(277, 277);
  tree.insert(21, 21);
  tree.insert(372, 372);
  tree.insert(252, 252);
  tree.insert(424, 424);
  tree.insert(144, 144);
  tree.insert(377, 377);
  tree.insert(59, 59);
  tree.insert(46, 46);
  tree.insert(55, 55);
  tree.insert(429, 429);
  tree.insert(474, 474);
  tree.insert(321, 321);
  tree.insert(399, 399);
  tree.insert(471, 471);
  tree.insert(237, 237);
  tree.insert(442, 442);
  tree.insert(97, 97);
  tree.insert(220, 220);
  tree.insert(445, 445);
  tree.insert(326, 326);
  tree.insert(37, 37);
  tree.insert(336, 336);
  tree.insert(343, 343);
  tree.insert(412, 412);
  tree.insert(409, 409);
  tree.insert(460, 460);
  tree.insert(57, 57);
  tree.insert(168, 168);
  tree.insert(295, 295);
  tree.insert(247, 247);
  tree.insert(482, 482);
  tree.insert(425, 425);
  tree.insert(256, 256);
  tree.insert(96, 96);
  tree.insert(53, 53);
  tree.insert(469, 469);
  tree.insert(162, 162);
  tree.insert(493, 493);
  tree.insert(294, 294);
  tree.insert(177, 177);
  tree.insert(212, 212);
  tree.insert(30, 30);
  tree.insert(5, 5);
  tree.insert(193, 193);
  tree.insert(483, 483);
  tree.insert(124, 124);
  tree.insert(87, 87);
  tree.insert(64, 64);
  tree.insert(490, 490);
  tree.insert(155, 155);
  tree.insert(422, 422);
  tree.insert(191, 191);
  tree.insert(75, 75);
  tree.insert(325, 325);
  tree.insert(1, 1);
  tree.insert(182, 182);
  tree.insert(28, 28);
  tree.insert(364, 364);
  tree.insert(42, 42);
  tree.insert(39, 39);
  tree.insert(376, 376);
  tree.insert(467, 467);
  tree.insert(426, 426);
  tree.insert(205, 205);
  tree.insert(365, 365);
  tree.insert(137, 137);
  tree.insert(297, 297);
  tree.insert(462, 462);
  tree.insert(241, 241);
  tree.insert(123, 123);
  tree.insert(206, 206);
  tree.insert(440, 440);
  tree.insert(216, 216);
  tree.insert(146, 146);
  tree.insert(142, 142);
  tree.insert(72, 72);
  tree.insert(379, 379);
  tree.insert(472, 472);
  tree.insert(305, 305);
  tree.insert(271, 271);
  tree.insert(298, 298);
  tree.insert(232, 232);
  tree.insert(242, 242);
  tree.insert(184, 184);
  tree.insert(138, 138);
  tree.insert(154, 154);
  tree.insert(200, 200);
  tree.insert(71, 71);
  tree.insert(211, 211);
  tree.insert(274, 274);
  tree.insert(263, 263);
  tree.insert(311, 311);
  tree.insert(428, 428);
  tree.insert(331, 331);
  tree.insert(7, 7);
  tree.insert(345, 345);
  tree.insert(185, 185);
  tree.insert(338, 338);
  tree.insert(251, 251);
  tree.insert(417, 417);
  tree.insert(12, 12);
  tree.insert(93, 93);
  tree.insert(204, 204);
  tree.insert(257, 257);
  tree.insert(418, 418);
  tree.insert(314, 314);

  if (!tree.erase(110))
    return false;
  if (!tree.erase(290))
    return false;
  if (!tree.erase(11))
    return false;
  if (!tree.erase(69))
    return false;
  if (!tree.erase(209))
    return false;
  if (!tree.erase(468))
    return false;
  if (!tree.erase(17))
    return false;
  if (!tree.erase(293))
    return false;
  if (!tree.erase(164))
    return false;
  if (!tree.erase(350))
    return false;
  if (!tree.erase(115))
    return false;
  if (!tree.erase(322))
    return false;
  if (!tree.erase(361))
    return false;
  if (!tree.erase(2))
    return false;
  if (!tree.erase(446))
    return false;
  if (!tree.erase(281))
    return false;
  if (!tree.erase(400))
    return false;
  if (!tree.erase(375))
    return false;
  if (!tree.erase(153))
    return false;
  if (!tree.erase(116))
    return false;
  if (!tree.erase(143))
    return false;
  if (!tree.erase(329))
    return false;
  if (!tree.erase(420))
    return false;
  if (!tree.erase(133))
    return false;
  if (!tree.erase(363))
    return false;
  if (!tree.erase(192))
    return false;
  if (!tree.erase(27))
    return false;
  if (!tree.erase(275))
    return false;
  if (!tree.erase(104))
    return false;
  if (!tree.erase(213))
    return false;
  if (!tree.erase(352))
    return false;
  if (!tree.erase(427))
    return false;
  if (!tree.erase(273))
    return false;
  if (!tree.erase(288))
    return false;
  if (!tree.erase(396))
    return false;
  if (!tree.erase(114))
    return false;
  if (!tree.erase(341))
    return false;
  if (!tree.erase(413))
    return false;
  if (!tree.erase(201))
    return false;
  if (!tree.erase(280))
    return false;
  if (!tree.erase(66))
    return false;
  if (!tree.erase(227))
    return false;
  if (!tree.erase(406))
    return false;
  if (!tree.erase(79))
    return false;
  if (!tree.erase(259))
    return false;
  if (!tree.erase(176))
    return false;
  if (!tree.erase(106))
    return false;
  if (!tree.erase(0))
    return false;
  if (!tree.erase(328))
    return false;
  if (!tree.erase(270))
    return false;
  if (!tree.erase(449))
    return false;
  if (!tree.erase(165))
    return false;
  if (!tree.erase(163))
    return false;
  if (!tree.erase(43))
    return false;
  if (!tree.erase(391))
    return false;
  if (!tree.erase(202))
    return false;
  if (!tree.erase(49))
    return false;
  if (!tree.erase(105))
    return false;
  if (!tree.erase(149))
    return false;
  if (!tree.erase(318))
    return false;
  if (!tree.erase(387))
    return false;
  if (!tree.erase(389))
    return false;
  if (!tree.erase(141))
    return false;
  if (!tree.erase(408))
    return false;
  if (!tree.erase(486))
    return false;
  if (!tree.erase(354))
    return false;
  if (!tree.erase(8))
    return false;
  if (!tree.erase(33))
    return false;
  if (!tree.erase(421))
    return false;
  if (!tree.erase(385))
    return false;
  if (!tree.erase(25))
    return false;
  if (!tree.erase(485))
    return false;
  if (!tree.erase(196))
    return false;
  if (!tree.erase(31))
    return false;
  if (!tree.erase(82))
    return false;
  if (!tree.erase(434))
    return false;
  if (!tree.erase(423))
    return false;
  if (!tree.erase(358))
    return false;
  if (!tree.erase(255))
    return false;
  if (!tree.erase(287))
    return false;
  if (!tree.erase(23))
    return false;
  if (!tree.erase(122))
    return false;
  if (!tree.erase(489))
    return false;
  if (!tree.erase(19))
    return false;
  if (!tree.erase(126))
    return false;
  if (!tree.erase(44))
    return false;
  if (!tree.erase(120))
    return false;
  if (!tree.erase(131))
    return false;
  if (!tree.erase(332))
    return false;
  if (!tree.erase(448))
    return false;
  if (!tree.erase(238))
    return false;
  if (!tree.erase(264))
    return false;
  if (!tree.erase(454))
    return false;
  if (!tree.erase(218))
    return false;
  if (!tree.erase(157))
    return false;
  if (!tree.erase(436))
    return false;
  if (!tree.erase(225))
    return false;
  if (!tree.erase(437))
    return false;
  if (!tree.erase(443))
    return false;
  if (!tree.erase(179))
    return false;
  if (!tree.erase(265))
    return false;
  if (!tree.erase(475))
    return false;
  if (!tree.erase(180))
    return false;
  if (!tree.erase(487))
    return false;
  if (!tree.erase(339))
    return false;
  if (!tree.erase(492))
    return false;
  if (!tree.erase(395))
    return false;
  if (!tree.erase(491))
    return false;
  if (!tree.erase(223))
    return false;
  if (!tree.erase(113))
    return false;
  if (!tree.erase(92))
    return false;
  if (!tree.erase(48))
    return false;
  if (!tree.erase(61))
    return false;
  if (!tree.erase(127))
    return false;
  if (!tree.erase(190))
    return false;
  if (!tree.erase(67))
    return false;
  if (!tree.erase(484))
    return false;
  if (!tree.erase(439))
    return false;
  if (!tree.erase(355))
    return false;
  if (!tree.erase(243))
    return false;
  if (!tree.erase(392))
    return false;
  if (!tree.erase(159))
    return false;
  if (!tree.erase(74))
    return false;
  if (!tree.erase(337))
    return false;
  if (!tree.erase(151))
    return false;
  if (!tree.erase(458))
    return false;
  if (!tree.erase(480))
    return false;
  if (!tree.erase(334))
    return false;
  if (!tree.erase(419))
    return false;
  if (!tree.erase(309))
    return false;
  if (!tree.erase(301))
    return false;
  if (!tree.erase(125))
    return false;
  if (!tree.erase(407))
    return false;
  if (!tree.erase(496))
    return false;
  if (!tree.erase(187))
    return false;
  if (!tree.erase(50))
    return false;
  if (!tree.erase(368))
    return false;
  if (!tree.erase(283))
    return false;
  if (!tree.erase(244))
    return false;
  if (!tree.erase(170))
    return false;
  if (!tree.erase(118))
    return false;
  if (!tree.erase(457))
    return false;
  if (!tree.erase(181))
    return false;
  if (!tree.erase(479))
    return false;
  if (!tree.erase(401))
    return false;
  if (!tree.erase(494))
    return false;
  if (!tree.erase(99))
    return false;
  if (!tree.erase(236))
    return false;
  if (!tree.erase(240))
    return false;
  if (!tree.erase(147))
    return false;
  if (!tree.erase(233))
    return false;
  if (!tree.erase(172))
    return false;
  if (!tree.erase(266))
    return false;
  if (!tree.erase(32))
    return false;
  if (!tree.erase(210))
    return false;
  if (!tree.erase(161))
    return false;
  if (!tree.erase(156))
    return false;
  if (!tree.erase(178))
    return false;
  if (!tree.erase(221))
    return false;
  if (!tree.erase(78))
    return false;
  if (!tree.erase(85))
    return false;
  if (!tree.erase(135))
    return false;
  if (!tree.erase(145))
    return false;
  if (!tree.erase(356))
    return false;
  if (!tree.erase(397))
    return false;
  if (!tree.erase(450))
    return false;
  if (!tree.erase(276))
    return false;
  if (!tree.erase(41))
    return false;
  if (!tree.erase(414))
    return false;
  if (!tree.erase(14))
    return false;
  if (!tree.erase(22))
    return false;
  if (!tree.erase(29))
    return false;
  if (!tree.erase(34))
    return false;
  if (!tree.erase(498))
    return false;
  if (!tree.erase(250))
    return false;
  if (!tree.erase(36))
    return false;
  if (!tree.erase(320))
    return false;
  if (!tree.erase(268))
    return false;
  if (!tree.erase(195))
    return false;
  if (!tree.erase(382))
    return false;
  if (!tree.erase(441))
    return false;
  if (!tree.erase(235))
    return false;
  if (!tree.erase(346))
    return false;
  if (!tree.erase(476))
    return false;
  if (!tree.erase(217))
    return false;
  if (!tree.erase(335))
    return false;
  if (!tree.erase(121))
    return false;
  if (!tree.erase(94))
    return false;
  if (!tree.erase(278))
    return false;
  if (!tree.erase(272))
    return false;
  if (!tree.erase(207))
    return false;
  if (!tree.erase(463))
    return false;
  if (!tree.erase(150))
    return false;
  if (!tree.erase(432))
    return false;
  if (!tree.erase(410))
    return false;
  if (!tree.erase(208))
    return false;
  if (!tree.erase(70))
    return false;
  if (!tree.erase(84))
    return false;
  if (!tree.erase(186))
    return false;
  if (!tree.erase(6))
    return false;
  if (!tree.erase(224))
    return false;
  if (!tree.erase(9))
    return false;
  if (!tree.erase(60))
    return false;
  if (!tree.erase(175))
    return false;
  if (!tree.erase(430))
    return false;
  if (!tree.erase(128))
    return false;
  if (!tree.erase(129))
    return false;
  if (!tree.erase(465))
    return false;
  if (!tree.erase(459))
    return false;
  if (!tree.erase(289))
    return false;
  if (!tree.erase(261))
    return false;
  if (!tree.erase(26))
    return false;
  if (!tree.erase(461))
    return false;
  if (!tree.erase(279))
    return false;
  if (!tree.erase(245))
    return false;
  if (!tree.erase(478))
    return false;
  if (!tree.erase(403))
    return false;
  if (!tree.erase(45))
    return false;
  if (!tree.erase(359))
    return false;
  if (!tree.erase(327))
    return false;
  if (!tree.erase(393))
    return false;
  if (!tree.erase(373))
    return false;
  if (!tree.erase(304))
    return false;
  if (!tree.erase(83))
    return false;
  if (!tree.erase(160))
    return false;
  if (!tree.erase(198))
    return false;
  if (!tree.erase(103))
    return false;
  if (!tree.erase(367))
    return false;
  if (!tree.erase(76))
    return false;
  if (!tree.erase(73))
    return false;
  if (!tree.erase(167))
    return false;
  if (!tree.erase(291))
    return false;
  if (!tree.erase(215))
    return false;
  if (!tree.erase(219))
    return false;
  if (!tree.erase(119))
    return false;
  if (!tree.erase(456))
    return false;
  if (!tree.erase(197))
    return false;
  if (!tree.erase(477))
    return false;
  if (!tree.erase(222))
    return false;
  if (!tree.erase(174))
    return false;
  if (!tree.erase(451))
    return false;
  if (!tree.erase(214))
    return false;
  if (!tree.erase(112))
    return false;
  if (!tree.erase(464))
    return false;
  if (!tree.erase(262))
    return false;
  if (!tree.erase(47))
    return false;
  if (!tree.erase(347))
    return false;
  if (!tree.erase(111))
    return false;
  if (!tree.erase(148))
    return false;
  if (!tree.erase(308))
    return false;
  if (!tree.erase(340))
    return false;
  if (!tree.erase(100))
    return false;
  if (!tree.erase(130))
    return false;
  if (!tree.erase(323))
    return false;
  if (!tree.erase(312))
    return false;
  if (!tree.erase(292))
    return false;
  if (!tree.erase(35))
    return false;
  if (!tree.erase(306))
    return false;
  if (!tree.erase(58))
    return false;
  if (!tree.erase(353))
    return false;
  if (!tree.erase(452))
    return false;
  if (!tree.erase(91))
    return false;
  if (!tree.erase(319))
    return false;
  if (!tree.erase(330))
    return false;
  if (!tree.erase(473))
    return false;
  if (!tree.erase(488))
    return false;
  if (!tree.erase(134))
    return false;
  if (!tree.erase(315))
    return false;
  if (!tree.erase(253))
    return false;
  if (!tree.erase(374))
    return false;
  if (!tree.erase(384))
    return false;
  if (!tree.erase(95))
    return false;
  if (!tree.erase(370))
    return false;
  if (!tree.erase(13))
    return false;
  if (!tree.erase(183))
    return false;
  if (!tree.erase(136))
    return false;
  if (!tree.erase(313))
    return false;
  if (!tree.erase(307))
    return false;
  if (!tree.erase(239))
    return false;
  if (!tree.erase(258))
    return false;
  if (!tree.erase(405))
    return false;
  if (!tree.erase(56))
    return false;
  if (!tree.erase(228))
    return false;
  if (!tree.erase(455))
    return false;
  if (!tree.erase(317))
    return false;
  if (!tree.erase(497))
    return false;
  if (!tree.erase(102))
    return false;
  if (!tree.erase(117))
    return false;
  if (!tree.erase(68))
    return false;
  if (!tree.erase(234))
    return false;
  if (!tree.erase(51))
    return false;
  if (!tree.erase(107))
    return false;
  if (!tree.erase(349))
    return false;
  if (!tree.erase(348))
    return false;
  if (!tree.erase(416))
    return false;
  if (!tree.erase(88))
    return false;
  if (!tree.erase(89))
    return false;
  if (!tree.erase(366))
    return false;
  if (!tree.erase(109))
    return false;
  if (!tree.erase(189))
    return false;
  if (!tree.erase(333))
    return false;
  if (!tree.erase(3))
    return false;
  if (!tree.erase(394))
    return false;
  if (!tree.erase(267))
    return false;
  if (!tree.erase(269))
    return false;
  if (!tree.erase(246))
    return false;
  if (!tree.erase(152))
    return false;
  if (!tree.erase(173))
    return false;
  if (!tree.erase(438))
    return false;
  if (!tree.erase(24))
    return false;
  if (!tree.erase(15))
    return false;
  if (!tree.erase(390))
    return false;
  if (!tree.erase(284))
    return false;
  if (!tree.erase(360))
    return false;
  if (!tree.erase(371))
    return false;
  if (!tree.erase(81))
    return false;
  if (!tree.erase(65))
    return false;
  if (!tree.erase(299))
    return false;
  if (!tree.erase(132))
    return false;
  if (!tree.erase(98))
    return false;
  if (!tree.erase(303))
    return false;
  if (!tree.erase(139))
    return false;
  if (!tree.erase(453))
    return false;
  if (!tree.erase(402))
    return false;
  if (!tree.erase(20))
    return false;
  if (!tree.erase(54))
    return false;
  if (!tree.erase(499))
    return false;
  if (!tree.erase(260))
    return false;
  if (!tree.erase(285))
    return false;
  if (!tree.erase(381))
    return false;
  if (!tree.erase(357))
    return false;
  if (!tree.erase(248))
    return false;
  if (!tree.erase(362))
    return false;
  if (!tree.erase(62))
    return false;
  if (!tree.erase(203))
    return false;
  if (!tree.erase(411))
    return false;
  if (!tree.erase(444))
    return false;
  if (!tree.erase(388))
    return false;
  if (!tree.erase(10))
    return false;
  if (!tree.erase(342))
    return false;
  if (!tree.erase(229))
    return false;
  if (!tree.erase(481))
    return false;
  if (!tree.erase(369))
    return false;
  if (!tree.erase(378))
    return false;
  if (!tree.erase(38))
    return false;
  if (!tree.erase(77))
    return false;
  if (!tree.erase(415))
    return false;
  if (!tree.erase(466))
    return false;
  if (!tree.erase(404))
    return false;
  if (!tree.erase(90))
    return false;
  if (!tree.erase(101))
    return false;
  if (!tree.erase(169))
    return false;
  if (!tree.erase(435))
    return false;
  if (!tree.erase(296))
    return false;
  if (!tree.erase(282))
    return false;
  if (!tree.erase(63))
    return false;
  if (!tree.erase(52))
    return false;
  if (!tree.erase(40))
    return false;
  if (!tree.erase(231))
    return false;
  if (!tree.erase(302))
    return false;
  if (!tree.erase(18))
    return false;
  if (!tree.erase(383))
    return false;
  if (!tree.erase(194))
    return false;
  if (!tree.erase(351))
    return false;
  if (!tree.erase(254))
    return false;
  if (!tree.erase(431))
    return false;
  if (!tree.erase(199))
    return false;
  if (!tree.erase(80))
    return false;
  if (!tree.erase(300))
    return false;
  if (!tree.erase(140))
    return false;
  if (!tree.erase(324))
    return false;
  if (!tree.erase(286))
    return false;
  if (!tree.erase(188))
    return false;
  if (!tree.erase(386))
    return false;
  if (!tree.erase(344))
    return false;
  if (!tree.erase(166))
    return false;
  if (!tree.erase(4))
    return false;
  if (!tree.erase(226))
    return false;
  if (!tree.erase(316))
    return false;
  if (!tree.erase(158))
    return false;
  if (!tree.erase(447))
    return false;
  if (!tree.erase(86))
    return false;
  if (!tree.erase(398))
    return false;
  if (!tree.erase(108))
    return false;
  if (!tree.erase(230))
    return false;
  if (!tree.erase(310))
    return false;
  if (!tree.erase(495))
    return false;
  if (!tree.erase(171))
    return false;
  if (!tree.erase(380))
    return false;
  if (!tree.erase(249))
    return false;
  if (!tree.erase(433))
    return false;
  if (!tree.erase(16))
    return false;
  if (!tree.erase(470))
    return false;
  if (!tree.erase(277))
    return false;
  if (!tree.erase(21))
    return false;
  if (!tree.erase(372))
    return false;
  if (!tree.erase(252))
    return false;
  if (!tree.erase(424))
    return false;
  if (!tree.erase(144))
    return false;
  if (!tree.erase(377))
    return false;
  if (!tree.erase(59))
    return false;
  if (!tree.erase(46))
    return false;
  if (!tree.erase(55))
    return false;
  if (!tree.erase(429))
    return false;
  if (!tree.erase(474))
    return false;
  if (!tree.erase(321))
    return false;
  if (!tree.erase(399))
    return false;
  if (!tree.erase(471))
    return false;
  if (!tree.erase(237))
    return false;
  if (!tree.erase(442))
    return false;
  if (!tree.erase(97))
    return false;
  if (!tree.erase(220))
    return false;
  if (!tree.erase(445))
    return false;
  if (!tree.erase(326))
    return false;
  if (!tree.erase(37))
    return false;
  if (!tree.erase(336))
    return false;
  if (!tree.erase(343))
    return false;
  if (!tree.erase(412))
    return false;
  if (!tree.erase(409))
    return false;
  if (!tree.erase(460))
    return false;
  if (!tree.erase(57))
    return false;
  if (!tree.erase(168))
    return false;
  if (!tree.erase(295))
    return false;
  if (!tree.erase(247))
    return false;
  if (!tree.erase(482))
    return false;
  if (!tree.erase(425))
    return false;
  if (!tree.erase(256))
    return false;
  if (!tree.erase(96))
    return false;
  if (!tree.erase(53))
    return false;
  if (!tree.erase(469))
    return false;
  if (!tree.erase(162))
    return false;
  if (!tree.erase(493))
    return false;
  if (!tree.erase(294))
    return false;
  if (!tree.erase(177))
    return false;
  if (!tree.erase(212))
    return false;
  if (!tree.erase(30))
    return false;
  if (!tree.erase(5))
    return false;
  if (!tree.erase(193))
    return false;
  if (!tree.erase(483))
    return false;
  if (!tree.erase(124))
    return false;
  if (!tree.erase(87))
    return false;
  if (!tree.erase(64))
    return false;
  if (!tree.erase(490))
    return false;
  if (!tree.erase(155))
    return false;
  if (!tree.erase(422))
    return false;
  if (!tree.erase(191))
    return false;
  if (!tree.erase(75))
    return false;
  if (!tree.erase(325))
    return false;
  if (!tree.erase(1))
    return false;
  if (!tree.erase(182))
    return false;
  if (!tree.erase(28))
    return false;
  if (!tree.erase(364))
    return false;
  if (!tree.erase(42))
    return false;
  if (!tree.erase(39))
    return false;
  if (!tree.erase(376))
    return false;
  if (!tree.erase(467))
    return false;
  if (!tree.erase(426))
    return false;
  if (!tree.erase(205))
    return false;
  if (!tree.erase(365))
    return false;
  if (!tree.erase(137))
    return false;
  if (!tree.erase(297))
    return false;
  if (!tree.erase(462))
    return false;
  if (!tree.erase(241))
    return false;
  if (!tree.erase(123))
    return false;
  if (!tree.erase(206))
    return false;
  if (!tree.erase(440))
    return false;
  if (!tree.erase(216))
    return false;
  if (!tree.erase(146))
    return false;
  if (!tree.erase(142))
    return false;
  if (!tree.erase(72))
    return false;
  if (!tree.erase(379))
    return false;
  if (!tree.erase(472))
    return false;
  if (!tree.erase(305))
    return false;
  if (!tree.erase(271))
    return false;
  if (!tree.erase(298))
    return false;
  if (!tree.erase(232))
    return false;
  if (!tree.erase(242))
    return false;
  if (!tree.erase(184))
    return false;
  if (!tree.erase(138))
    return false;
  if (!tree.erase(154))
    return false;
  if (!tree.erase(200))
    return false;
  if (!tree.erase(71))
    return false;
  if (!tree.erase(211))
    return false;
  if (!tree.erase(274))
    return false;
  if (!tree.erase(263))
    return false;
  if (!tree.erase(311))
    return false;
  if (!tree.erase(428))
    return false;
  if (!tree.erase(331))
    return false;
  if (!tree.erase(7))
    return false;
  if (!tree.erase(345))
    return false;
  if (!tree.erase(185))
    return false;
  if (!tree.erase(338))
    return false;
  if (!tree.erase(251))
    return false;
  if (!tree.erase(417))
    return false;
  if (!tree.erase(12))
    return false;
  if (!tree.erase(93))
    return false;
  if (!tree.erase(204))
    return false;
  if (!tree.erase(257))
    return false;
  if (!tree.erase(418))
    return false;
  if (!tree.erase(314))
    return false;

  return true;
}

bool
test10() {

  // Pseudo-random insertion and erases, pseudo-randomly interleaved.

  CO_Tree tree(1);

  tree.insert(172261, 5);
  tree.insert(690360, 5);
  tree.erase(228023);
  tree.erase(81);
  tree.erase(903190);
  tree.erase(618996);
  tree.erase(214677);
  tree.insert(730690, 5);
  tree.insert(764524, 5);
  tree.erase(349614);
  tree.insert(328205, 5);
  tree.insert(726312, 5);
  tree.insert(565100, 5);
  tree.insert(602726, 5);
  tree.insert(204916, 5);
  tree.insert(325578, 5);
  tree.erase(528946);
  tree.insert(302647, 5);
  tree.insert(799051, 5);
  tree.insert(799631, 5);
  tree.erase(830857);
  tree.erase(541312);
  tree.insert(439214, 5);
  tree.erase(193512);
  tree.insert(14412, 5);
  tree.erase(909610);
  tree.erase(966189);
  tree.insert(806355, 5);
  tree.erase(356620);
  tree.erase(198987);
  tree.insert(498338, 5);
  tree.insert(487770, 5);
  tree.insert(56856, 5);
  tree.erase(300606);
  tree.insert(125849, 5);
  tree.erase(107205);
  tree.erase(35217);
  tree.insert(34945, 5);
  tree.erase(436873);
  tree.insert(710873, 5);
  tree.erase(804289);
  tree.erase(826607);
  tree.insert(772757, 5);
  tree.insert(334471, 5);
  tree.erase(591100);
  tree.erase(723618);
  tree.insert(58025, 5);
  tree.insert(633074, 5);
  tree.erase(518157);
  tree.erase(3493);
  tree.insert(550270, 5);
  tree.erase(633417);
  tree.erase(275569);
  tree.erase(92622);
  tree.insert(413173, 5);
  tree.erase(196431);
  tree.insert(456682, 5);
  tree.insert(504292, 5);
  tree.erase(205057);
  tree.erase(391521);
  tree.erase(888574);
  tree.erase(401947);
  tree.erase(359231);
  tree.erase(610537);
  tree.insert(485054, 5);
  tree.insert(554098, 5);
  tree.erase(241081);
  tree.insert(653516, 5);
  tree.insert(372231, 5);
  tree.erase(261796);
  tree.insert(582338, 5);
  tree.insert(519218, 5);
  tree.erase(513970);
  tree.insert(784812, 5);
  tree.erase(894977);
  tree.erase(31536);
  tree.insert(324176, 5);
  tree.erase(279207);
  tree.erase(984857);
  tree.insert(593499, 5);
  tree.erase(20127);
  tree.insert(505236, 5);
  tree.insert(367818, 5);
  tree.erase(810563);
  tree.erase(421244);
  tree.erase(41805);
  tree.insert(563291, 5);
  tree.erase(558955);
  tree.insert(133589, 5);
  tree.insert(828993, 5);
  tree.insert(552805, 5);
  tree.insert(844822, 5);
  tree.insert(326717, 5);
  tree.insert(593093, 5);
  tree.insert(530126, 5);
  tree.erase(781486);
  tree.erase(850543);
  tree.insert(327814, 5);
  tree.erase(478179);
  tree.erase(474762);
  tree.erase(727088);
  tree.erase(935710);
  tree.insert(110294, 5);
  tree.erase(400346);
  tree.erase(871137);
  tree.erase(305153);
  tree.erase(122573);
  tree.insert(300925, 5);
  tree.erase(306710);
  tree.insert(277217, 5);
  tree.insert(596963, 5);
  tree.erase(387090);
  tree.insert(378130, 5);
  tree.insert(698571, 5);
  tree.erase(369633);
  tree.erase(304789);
  tree.erase(722604);
  tree.erase(419805);
  tree.insert(767868, 5);
  tree.erase(109485);
  tree.insert(82195, 5);
  tree.erase(62949);
  tree.insert(80967, 5);
  tree.insert(686763, 5);
  tree.erase(290596);
  tree.erase(740865);
  tree.insert(539036, 5);
  tree.erase(367770);
  tree.insert(359211, 5);
  tree.insert(322532, 5);
  tree.insert(272379, 5);
  tree.erase(858270);
  tree.insert(384172, 5);
  tree.erase(344234);
  tree.insert(647283, 5);
  tree.insert(307398, 5);
  tree.insert(901063, 5);
  tree.erase(966950);
  tree.insert(250573, 5);
  tree.insert(886059, 5);
  tree.insert(134047, 5);
  tree.insert(945082, 5);
  tree.erase(271232);
  tree.insert(622954, 5);
  tree.erase(411898);
  tree.insert(875640, 5);
  tree.insert(89159, 5);
  tree.insert(679262, 5);
  tree.erase(561041);
  tree.insert(141723, 5);
  tree.insert(26272, 5);
  tree.insert(454154, 5);
  tree.erase(335821);
  tree.erase(909365);
  tree.erase(591171);
  tree.insert(160269, 5);
  tree.insert(938701, 5);
  tree.erase(914653);
  tree.erase(450907);
  tree.erase(356728);
  tree.insert(515797, 5);
  tree.insert(547084, 5);
  tree.insert(515334, 5);
  tree.insert(110991, 5);
  tree.insert(798898, 5);
  tree.erase(801052);
  tree.erase(218189);
  tree.erase(752506);
  tree.insert(709016, 5);
  tree.insert(173109, 5);
  tree.erase(490000);
  tree.insert(58109, 5);
  tree.erase(955081);
  tree.insert(671338, 5);
  tree.erase(59426);
  tree.erase(785147);
  tree.erase(776787);
  tree.erase(696532);
  tree.insert(591281, 5);
  tree.erase(884850);
  tree.erase(576590);
  tree.insert(215350, 5);
  tree.erase(973813);
  tree.erase(381494);
  tree.erase(146081);
  tree.erase(15720);
  tree.erase(887982);
  tree.erase(97487);
  tree.erase(79296);
  tree.erase(765404);
  tree.insert(796892, 5);
  tree.erase(230297);
  tree.insert(399134, 5);
  tree.erase(898506);
  tree.erase(767057);
  tree.insert(380595, 5);
  tree.erase(501962);
  tree.erase(687483);
  tree.insert(80154, 5);
  tree.erase(191309);
  tree.erase(139932);
  tree.insert(895021, 5);
  tree.insert(313563, 5);
  tree.insert(903682, 5);
  tree.erase(277685);
  tree.insert(564285, 5);
  tree.insert(735990, 5);
  tree.erase(197314);
  tree.insert(754116, 5);
  tree.insert(641892, 5);
  tree.erase(395528);
  tree.erase(897525);
  tree.insert(651136, 5);
  tree.insert(889618, 5);
  tree.erase(170337);
  tree.insert(506582, 5);
  tree.erase(804310);
  tree.erase(370888);
  tree.erase(426815);
  tree.insert(543437, 5);
  tree.erase(460008);
  tree.insert(811783, 5);
  tree.insert(418657, 5);
  tree.erase(363827);
  tree.insert(621269, 5);
  tree.erase(726651);
  tree.erase(60910);
  tree.insert(430639, 5);
  tree.insert(241888, 5);
  tree.insert(992393, 5);
  tree.erase(433890);
  tree.insert(755199, 5);
  tree.insert(416931, 5);
  tree.erase(388777);
  tree.erase(400657);
  tree.insert(580952, 5);
  tree.erase(72641);
  tree.erase(89368);
  tree.insert(918184, 5);
  tree.erase(696776);
  tree.erase(975266);
  tree.insert(588954, 5);
  tree.insert(80308, 5);
  tree.erase(297278);
  tree.erase(372555);
  tree.insert(250774, 5);
  tree.erase(305000);
  tree.erase(560997);
  tree.erase(648412);
  tree.erase(598382);
  tree.erase(914693);
  tree.insert(942439, 5);
  tree.insert(88421, 5);
  tree.erase(994985);
  tree.erase(1354);
  tree.erase(578762);
  tree.insert(631541, 5);
  tree.insert(561852, 5);
  tree.insert(703662, 5);
  tree.insert(550399, 5);
  tree.erase(665154);
  tree.erase(399015);
  tree.insert(839851, 5);
  tree.insert(724790, 5);
  tree.erase(942491);
  tree.insert(570037, 5);
  tree.erase(18859);
  tree.insert(360871, 5);
  tree.insert(576987, 5);
  tree.insert(146590, 5);
  tree.erase(563970);
  tree.insert(587665, 5);
  tree.erase(893069);
  tree.erase(907361);
  tree.erase(41351);
  tree.insert(189300, 5);
  tree.insert(291638, 5);
  tree.erase(709364);
  tree.erase(581032);
  tree.insert(136104, 5);
  tree.erase(273679);
  tree.erase(413412);
  tree.insert(734969, 5);
  tree.insert(916170, 5);
  tree.insert(162844, 5);
  tree.insert(406649, 5);
  tree.insert(304465, 5);
  tree.insert(922326, 5);
  tree.insert(660183, 5);
  tree.erase(826969);
  tree.erase(320152);
  tree.erase(924393);
  tree.insert(637289, 5);
  tree.erase(259631);
  tree.erase(584264);
  tree.erase(774548);
  tree.erase(101877);
  tree.erase(666833);
  tree.insert(994949, 5);
  tree.erase(665155);
  tree.erase(678468);
  tree.insert(400960, 5);
  tree.erase(98823);
  tree.insert(213171, 5);
  tree.insert(185677, 5);
  tree.insert(493245, 5);
  tree.erase(572761);
  tree.insert(150323, 5);
  tree.insert(84100, 5);
  tree.insert(461075, 5);
  tree.insert(322042, 5);
  tree.insert(42659, 5);
  tree.erase(456289);
  tree.insert(293469, 5);
  tree.insert(841551, 5);
  tree.insert(125383, 5);
  tree.erase(63133);
  tree.erase(19304);
  tree.insert(365981, 5);
  tree.erase(953666);
  tree.erase(788967);
  tree.insert(90192, 5);
  tree.erase(380902);
  tree.insert(88131, 5);
  tree.insert(683174, 5);
  tree.erase(649718);
  tree.insert(301183, 5);
  tree.erase(945487);
  tree.insert(434573, 5);
  tree.erase(725062);
  tree.erase(713933);
  tree.erase(312496);
  tree.insert(893141, 5);
  tree.erase(971726);
  tree.insert(596980, 5);
  tree.erase(843485);
  tree.insert(372305, 5);
  tree.insert(264029, 5);
  tree.erase(206898);
  tree.insert(734562, 5);
  tree.insert(417719, 5);
  tree.insert(411641, 5);
  tree.insert(593010, 5);
  tree.insert(992726, 5);
  tree.erase(628789);
  tree.insert(303708, 5);
  tree.erase(600938);
  tree.erase(152493);
  tree.erase(980710);
  tree.insert(785905, 5);
  tree.insert(49613, 5);
  tree.erase(963638);
  tree.insert(79421, 5);
  tree.erase(207829);
  tree.erase(96180);
  tree.erase(209095);
  tree.erase(843024);
  tree.insert(749154, 5);
  tree.insert(10569, 5);
  tree.insert(979969, 5);
  tree.insert(492373, 5);
  tree.insert(498433, 5);
  tree.erase(932587);
  tree.erase(620094);
  tree.erase(291499);
  tree.erase(7339);
  tree.insert(551742, 5);
  tree.insert(312086, 5);
  tree.erase(231349);
  tree.erase(950186);
  tree.erase(495011);
  tree.insert(874133, 5);
  tree.erase(812722);
  tree.erase(806773);
  tree.erase(881519);
  tree.insert(495354, 5);
  tree.insert(103124, 5);
  tree.erase(16259);
  tree.erase(677418);
  tree.erase(981712);
  tree.erase(558705);
  tree.insert(342733, 5);
  tree.erase(992734);
  tree.erase(774315);
  tree.erase(691087);
  tree.erase(100669);
  tree.insert(916487, 5);
  tree.insert(556837, 5);
  tree.insert(598089, 5);
  tree.insert(585205, 5);
  tree.insert(666704, 5);
  tree.erase(402557);
  tree.erase(623403);
  tree.insert(321892, 5);
  tree.erase(571522);
  tree.erase(302443);
  tree.erase(325361);
  tree.insert(273378, 5);
  tree.erase(332700);
  tree.erase(574882);
  tree.erase(804899);
  tree.erase(242589);
  tree.insert(650353, 5);
  tree.insert(966948, 5);
  tree.insert(163036, 5);
  tree.insert(277107, 5);
  tree.insert(665417, 5);
  tree.insert(115921, 5);
  tree.insert(98480, 5);
  tree.insert(105994, 5);
  tree.insert(774123, 5);
  tree.erase(832933);
  tree.insert(86317, 5);
  tree.insert(933931, 5);
  tree.erase(186709);
  tree.erase(959156);
  tree.insert(217069, 5);
  tree.erase(712995);
  tree.insert(1171, 5);
  tree.erase(148569);
  tree.erase(264801);
  tree.insert(26652, 5);
  tree.erase(105340);
  tree.erase(251743);
  tree.insert(613091, 5);
  tree.erase(906527);
  tree.insert(798878, 5);
  tree.insert(3050, 5);
  tree.insert(362124, 5);
  tree.erase(304213);
  tree.insert(478499, 5);
  tree.insert(56794, 5);
  tree.insert(465115, 5);
  tree.erase(79342);
  tree.erase(482437);
  tree.erase(663198);
  tree.insert(169939, 5);
  tree.insert(226513, 5);
  tree.erase(865128);
  tree.erase(511804);
  tree.erase(352346);
  tree.erase(898138);
  tree.erase(190495);
  tree.insert(36421, 5);
  tree.insert(387226, 5);
  tree.insert(134158, 5);
  tree.erase(120356);
  tree.insert(77645, 5);
  tree.insert(993446, 5);
  tree.erase(568111);
  tree.erase(417603);
  tree.erase(255825);
  tree.insert(470216, 5);
  tree.erase(379174);
  tree.insert(960596, 5);
  tree.insert(846267, 5);
  tree.insert(342013, 5);
  tree.erase(980519);
  tree.insert(194650, 5);
  tree.insert(117832, 5);
  tree.insert(390279, 5);
  tree.insert(963953, 5);
  tree.erase(959295);
  tree.insert(96107, 5);
  tree.erase(714937);
  tree.insert(944976, 5);
  tree.insert(444584, 5);
  tree.erase(720083);
  tree.insert(199492, 5);
  tree.erase(766496);
  tree.insert(22939, 5);
  tree.erase(505735);
  tree.insert(389873, 5);
  tree.insert(930164, 5);
  tree.erase(52251);
  tree.erase(682751);
  tree.insert(816339, 5);
  tree.insert(953165, 5);
  tree.insert(688302, 5);
  tree.erase(761079);
  tree.erase(262547);
  tree.erase(568484);
  tree.erase(939561);
  tree.erase(621931);
  tree.erase(420528);
  tree.erase(815494);
  tree.erase(517543);
  tree.erase(841123);
  tree.insert(840187, 5);
  tree.erase(524643);
  tree.insert(851988, 5);
  tree.insert(851320, 5);
  tree.insert(854098, 5);
  tree.insert(993018, 5);
  tree.insert(886463, 5);
  tree.insert(494695, 5);
  tree.insert(976505, 5);
  tree.erase(856142);
  tree.insert(868098, 5);
  tree.erase(571472);

  return true;
}

} // namespace

BEGIN_MAIN
  DO_TEST(test01);
  DO_TEST(test02);
  DO_TEST(test03);
  DO_TEST(test04);
  DO_TEST(test05);
  DO_TEST(test06);
  DO_TEST(test07);
  DO_TEST(test08);
  DO_TEST(test09);
  DO_TEST(test10);
END_MAIN
