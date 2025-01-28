













      module mo_nln_matrix

      use shr_kind_mod, only : r8 => shr_kind_r8

      private
      public :: nlnmat

      contains

      subroutine     nlnmat01( mat, y, rxt )

      use chem_mods, only : gas_pcnst, rxntot,     nzcnt

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
      real(r8), intent(in)    ::  y(gas_pcnst)
      real(r8), intent(in)    ::  rxt(rxntot)
      real(r8), intent(inout) ::  mat(nzcnt)


!----------------------------------------------
!       ... local variables
!----------------------------------------------

!----------------------------------------------
!       ... complete matrix entries implicit species
!----------------------------------------------


         mat(262) = -(rxt(30)*y(19) + rxt(31)*y(26) + rxt(34)*y(2) + rxt(36)*y(3) &
                      + rxt(42)*y(8) + rxt(44)*y(9) + rxt(79)*y(28))
         mat(54) = -rxt(30)*y(1)
         mat(60) = -rxt(31)*y(1)
         mat(247) = -rxt(34)*y(1)
         mat(209) = -rxt(36)*y(1)
         mat(163) = -rxt(42)*y(1)
         mat(190) = -rxt(44)*y(1)
         mat(132) = -rxt(79)*y(1)

         mat(247) = mat(247) + 2.000_r8*rxt(35)*y(2)

         mat(246) = -(rxt(29)*y(19) + rxt(32)*y(26) + rxt(33)*y(5) + rxt(34)*y(1) &
                      + (4._r8*rxt(35) + 4._r8*rxt(40)) * y(2) + rxt(37)*y(3) + rxt(41) &
                      *y(4) + rxt(45)*y(10) + rxt(47)*y(13) + rxt(48)*y(12) + rxt(49) &
                      *y(9) + rxt(59)*y(7) + rxt(64)*y(23) + rxt(66)*y(24) + rxt(80) &
                      *y(28) + (rxt(87) + rxt(88)) * y(32) + rxt(89)*y(33))
         mat(53) = -rxt(29)*y(2)
         mat(59) = -rxt(32)*y(2)
         mat(82) = -rxt(33)*y(2)
         mat(261) = -rxt(34)*y(2)
         mat(208) = -rxt(37)*y(2)
         mat(33) = -rxt(41)*y(2)
         mat(221) = -rxt(45)*y(2)
         mat(69) = -rxt(47)*y(2)
         mat(64) = -rxt(48)*y(2)
         mat(189) = -rxt(49)*y(2)
         mat(74) = -rxt(59)*y(2)
         mat(79) = -rxt(64)*y(2)
         mat(140) = -rxt(66)*y(2)
         mat(131) = -rxt(80)*y(2)
         mat(42) = -(rxt(87) + rxt(88)) * y(2)
         mat(30) = -rxt(89)*y(2)

         mat(261) = mat(261) + rxt(36)*y(3) + rxt(31)*y(26)
         mat(246) = mat(246) + .300_r8*rxt(59)*y(7)
         mat(208) = mat(208) + rxt(36)*y(1) + rxt(43)*y(8) + rxt(74)*y(20)
         mat(74) = mat(74) + .300_r8*rxt(59)*y(2)
         mat(162) = rxt(43)*y(3)
         mat(100) = rxt(74)*y(3)
         mat(59) = mat(59) + rxt(31)*y(1)

         mat(206) = -(rxt(36)*y(1) + rxt(37)*y(2) + (4._r8*rxt(38) + 4._r8*rxt(39) &
                      ) * y(3) + rxt(43)*y(8) + rxt(50)*y(9) + rxt(56)*y(6) + rxt(63) &
                      *y(22) + rxt(74)*y(20) + rxt(77)*y(27) + rxt(82)*y(29))
         mat(259) = -rxt(36)*y(3)
         mat(244) = -rxt(37)*y(3)
         mat(160) = -rxt(43)*y(3)
         mat(187) = -rxt(50)*y(3)
         mat(175) = -rxt(56)*y(3)
         mat(108) = -rxt(63)*y(3)
         mat(99) = -rxt(74)*y(3)
         mat(119) = -rxt(77)*y(3)
         mat(91) = -rxt(82)*y(3)

         mat(259) = mat(259) + rxt(34)*y(2)
         mat(244) = mat(244) + rxt(34)*y(1) + rxt(41)*y(4) + rxt(33)*y(5) + rxt(45) &
                      *y(10) + .500_r8*rxt(88)*y(32)
         mat(32) = rxt(41)*y(2)
         mat(81) = rxt(33)*y(2)
         mat(175) = mat(175) + 4.000_r8*rxt(58)*y(6) + rxt(57)*y(8) + 2.000_r8*rxt(62) &
                      *y(22) + rxt(69)*y(25) + rxt(75)*y(20) + 2.000_r8*rxt(78)*y(27)
         mat(160) = mat(160) + rxt(57)*y(6) + rxt(60)*y(22)
         mat(219) = rxt(45)*y(2)
         mat(108) = mat(108) + 2.000_r8*rxt(62)*y(6) + rxt(60)*y(8) + 4.000_r8*rxt(61) &
                      *y(22)
         mat(147) = rxt(69)*y(6)
         mat(99) = mat(99) + rxt(75)*y(6)
         mat(119) = mat(119) + 2.000_r8*rxt(78)*y(6)
         mat(40) = .500_r8*rxt(88)*y(2)

         mat(31) = -(rxt(41)*y(2))
         mat(225) = -rxt(41)*y(4)

         mat(225) = mat(225) + 2.000_r8*rxt(40)*y(2)
         mat(191) = (2.000_r8*rxt(38)+2.000_r8*rxt(39))*y(3)

         mat(80) = -(rxt(33)*y(2))
         mat(233) = -rxt(33)*y(5)

         mat(250) = rxt(30)*y(19) + rxt(31)*y(26) + rxt(79)*y(28)
         mat(233) = mat(233) + .300_r8*rxt(59)*y(7)
         mat(195) = rxt(74)*y(20)
         mat(165) = 4.000_r8*rxt(58)*y(6) + rxt(57)*y(8) + rxt(62)*y(22) + rxt(69) &
                      *y(25) + 2.000_r8*rxt(75)*y(20) + 2.000_r8*rxt(78)*y(27)
         mat(71) = .300_r8*rxt(59)*y(2)
         mat(149) = rxt(57)*y(6) + rxt(73)*y(20) + rxt(76)*y(27) + rxt(81)*y(29)
         mat(102) = rxt(62)*y(6)
         mat(142) = rxt(69)*y(6)
         mat(50) = rxt(30)*y(1)
         mat(93) = rxt(74)*y(3) + 2.000_r8*rxt(75)*y(6) + rxt(73)*y(8)
         mat(56) = rxt(31)*y(1)
         mat(111) = 2.000_r8*rxt(78)*y(6) + rxt(76)*y(8)
         mat(121) = rxt(79)*y(1)
         mat(84) = rxt(81)*y(8)

         mat(173) = -(rxt(56)*y(3) + rxt(57)*y(8) + 4._r8*rxt(58)*y(6) + rxt(62)*y(22) &
                      + rxt(75)*y(20) + rxt(78)*y(27))
         mat(204) = -rxt(56)*y(6)
         mat(158) = -rxt(57)*y(6)
         mat(106) = -rxt(62)*y(6)
         mat(97) = -rxt(75)*y(6)
         mat(117) = -rxt(78)*y(6)

         mat(257) = rxt(79)*y(28)
         mat(242) = .700_r8*rxt(59)*y(7) + .500_r8*rxt(66)*y(24)
         mat(72) = .700_r8*rxt(59)*y(2)
         mat(158) = mat(158) + rxt(68)*y(25)
         mat(136) = .500_r8*rxt(66)*y(2)
         mat(145) = rxt(68)*y(8) + 4.000_r8*rxt(70)*y(25)
         mat(128) = rxt(79)*y(1)

         mat(70) = -(rxt(59)*y(2))
         mat(231) = -rxt(59)*y(7)

         mat(193) = rxt(56)*y(6)
         mat(164) = rxt(56)*y(3)

         mat(157) = -(rxt(42)*y(1) + rxt(43)*y(3) + rxt(46)*y(10) + rxt(57)*y(6) &
                      + rxt(60)*y(22) + rxt(68)*y(25) + rxt(73)*y(20) + rxt(76)*y(27) &
                      + rxt(81)*y(29))
         mat(256) = -rxt(42)*y(8)
         mat(203) = -rxt(43)*y(8)
         mat(216) = -rxt(46)*y(8)
         mat(172) = -rxt(57)*y(8)
         mat(105) = -rxt(60)*y(8)
         mat(144) = -rxt(68)*y(8)
         mat(96) = -rxt(73)*y(8)
         mat(116) = -rxt(76)*y(8)
         mat(89) = -rxt(81)*y(8)

         mat(186) = -(rxt(44)*y(1) + rxt(49)*y(2) + rxt(50)*y(3) + rxt(51)*y(10) &
                      + rxt(52)*y(25))
         mat(258) = -rxt(44)*y(9)
         mat(243) = -rxt(49)*y(9)
         mat(205) = -rxt(50)*y(9)
         mat(218) = -rxt(51)*y(9)
         mat(146) = -rxt(52)*y(9)

         mat(258) = mat(258) + rxt(42)*y(8)
         mat(243) = mat(243) + rxt(45)*y(10) + rxt(47)*y(13)
         mat(205) = mat(205) + rxt(43)*y(8)
         mat(174) = rxt(57)*y(8)
         mat(159) = rxt(42)*y(1) + rxt(43)*y(3) + rxt(57)*y(6) + 2.000_r8*rxt(46) &
                      *y(10) + rxt(60)*y(22) + rxt(68)*y(25) + rxt(73)*y(20) + rxt(76) &
                      *y(27) + rxt(81)*y(29)
         mat(218) = mat(218) + rxt(45)*y(2) + 2.000_r8*rxt(46)*y(8)
         mat(66) = rxt(47)*y(2)
         mat(107) = rxt(60)*y(8)
         mat(146) = mat(146) + rxt(68)*y(8)
         mat(98) = rxt(73)*y(8)
         mat(118) = rxt(76)*y(8)
         mat(90) = rxt(81)*y(8)

         mat(220) = -(rxt(45)*y(2) + rxt(46)*y(8) + rxt(51)*y(9) + rxt(67)*y(24) &
                      + rxt(90)*y(32))
         mat(245) = -rxt(45)*y(10)
         mat(161) = -rxt(46)*y(10)
         mat(188) = -rxt(51)*y(10)
         mat(139) = -rxt(67)*y(10)
         mat(41) = -rxt(90)*y(10)

         mat(260) = rxt(44)*y(9)
         mat(245) = mat(245) + rxt(48)*y(12)
         mat(188) = mat(188) + rxt(44)*y(1)
         mat(63) = rxt(48)*y(2)


         mat(180) = rxt(51)*y(10)
         mat(212) = rxt(51)*y(9)

         mat(61) = -(rxt(48)*y(2))
         mat(229) = -rxt(48)*y(12)

         mat(229) = mat(229) + rxt(49)*y(9)
         mat(181) = rxt(49)*y(2)
         mat(213) = rxt(67)*y(24) + rxt(90)*y(32)
         mat(133) = rxt(67)*y(10)
         mat(39) = rxt(90)*y(10)

         mat(65) = -(rxt(47)*y(2))
         mat(230) = -rxt(47)*y(13)

         mat(192) = rxt(50)*y(9)
         mat(182) = rxt(50)*y(3)


         mat(179) = rxt(52)*y(25)
         mat(141) = rxt(52)*y(9)

         mat(103) = -(rxt(60)*y(8) + 4._r8*rxt(61)*y(22) + rxt(62)*y(6) + rxt(63)*y(3))
         mat(152) = -rxt(60)*y(22)
         mat(167) = -rxt(62)*y(22)
         mat(198) = -rxt(63)*y(22)

         mat(236) = rxt(64)*y(23)
         mat(76) = rxt(64)*y(2)

         mat(75) = -((rxt(64) + rxt(65)) * y(2))
         mat(232) = -(rxt(64) + rxt(65)) * y(23)

         mat(194) = rxt(63)*y(22) + rxt(77)*y(27) + rxt(82)*y(29)
         mat(101) = rxt(63)*y(3)
         mat(110) = rxt(77)*y(3)
         mat(83) = rxt(82)*y(3)

         mat(134) = -(rxt(66)*y(2) + rxt(67)*y(10))
         mat(239) = -rxt(66)*y(24)
         mat(214) = -rxt(67)*y(24)

         mat(254) = .500_r8*rxt(30)*y(19)
         mat(239) = mat(239) + rxt(65)*y(23)
         mat(201) = rxt(74)*y(20)
         mat(170) = rxt(62)*y(22)
         mat(155) = rxt(60)*y(22) + rxt(73)*y(20)
         mat(104) = rxt(62)*y(6) + rxt(60)*y(8) + 4.000_r8*rxt(61)*y(22)
         mat(77) = rxt(65)*y(2)
         mat(52) = .500_r8*rxt(30)*y(1)
         mat(95) = rxt(74)*y(3) + rxt(73)*y(8)

         mat(143) = -(rxt(52)*y(9) + rxt(68)*y(8) + rxt(69)*y(6) + 4._r8*rxt(70)*y(25))
         mat(183) = -rxt(52)*y(25)
         mat(156) = -rxt(68)*y(25)
         mat(171) = -rxt(69)*y(25)

         mat(255) = .500_r8*rxt(79)*y(28)
         mat(240) = .500_r8*rxt(66)*y(24)
         mat(156) = mat(156) + .500_r8*rxt(81)*y(29)
         mat(215) = rxt(67)*y(24)
         mat(135) = .500_r8*rxt(66)*y(2) + rxt(67)*y(10)
         mat(126) = .500_r8*rxt(79)*y(1)
         mat(88) = .500_r8*rxt(81)*y(8)

         mat(49) = -(rxt(29)*y(2) + rxt(30)*y(1))
         mat(227) = -rxt(29)*y(19)
         mat(248) = -rxt(30)*y(19)

         mat(94) = -(rxt(73)*y(8) + rxt(74)*y(3) + rxt(75)*y(6))
         mat(151) = -rxt(73)*y(20)
         mat(197) = -rxt(74)*y(20)
         mat(166) = -rxt(75)*y(20)

         mat(235) = rxt(29)*y(19)
         mat(51) = rxt(29)*y(2)


      end subroutine     nlnmat01

      subroutine     nlnmat02( mat, y, rxt )

      use chem_mods, only : gas_pcnst, rxntot,     nzcnt

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
      real(r8), intent(in)    ::  y(gas_pcnst)
      real(r8), intent(in)    ::  rxt(rxntot)
      real(r8), intent(inout) ::  mat(nzcnt)


!----------------------------------------------
!       ... local variables
!----------------------------------------------

!----------------------------------------------
!       ... complete matrix entries implicit species
!----------------------------------------------


         mat(55) = -(rxt(31)*y(1) + rxt(32)*y(2))
         mat(249) = -rxt(31)*y(26)
         mat(228) = -rxt(32)*y(26)

         mat(113) = -(rxt(76)*y(8) + rxt(77)*y(3) + rxt(78)*y(6))
         mat(153) = -rxt(76)*y(27)
         mat(199) = -rxt(77)*y(27)
         mat(168) = -rxt(78)*y(27)

         mat(237) = rxt(32)*y(26)
         mat(57) = rxt(32)*y(2)

         mat(124) = -(rxt(79)*y(1) + rxt(80)*y(2))
         mat(253) = -rxt(79)*y(28)
         mat(238) = -rxt(80)*y(28)

         mat(253) = mat(253) + rxt(31)*y(26)
         mat(169) = rxt(78)*y(27)
         mat(154) = rxt(76)*y(27)
         mat(58) = rxt(31)*y(1)
         mat(114) = rxt(78)*y(6) + rxt(76)*y(8)

         mat(85) = -(rxt(81)*y(8) + rxt(82)*y(3))
         mat(150) = -rxt(81)*y(29)
         mat(196) = -rxt(82)*y(29)

         mat(234) = rxt(80)*y(28)
         mat(122) = rxt(80)*y(2)

         mat(38) = -((rxt(87) + rxt(88)) * y(2) + rxt(90)*y(10))
         mat(226) = -(rxt(87) + rxt(88)) * y(32)
         mat(211) = -rxt(90)*y(32)

         mat(29) = -(rxt(89)*y(2))
         mat(224) = -rxt(89)*y(33)

         mat(224) = mat(224) + (rxt(87)+.500_r8*rxt(88))*y(32)
         mat(210) = rxt(90)*y(32)
         mat(37) = (rxt(87)+.500_r8*rxt(88))*y(2) + rxt(90)*y(10)


         mat(223) = rxt(89)*y(33)
         mat(28) = rxt(89)*y(2)




























      end subroutine     nlnmat02

!PS      subroutine     nlnmat_finit( mat, lmat, dti )
      subroutine     nlnmat_finit( mat, lmat, dt )

      use chem_mods, only : gas_pcnst, rxntot,     nzcnt

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
!PS      real(r8), intent(in)    ::  dti
      real(r8), intent(in)    ::  dt
      real(r8), intent(in)    ::  lmat(nzcnt)
      real(r8), intent(inout) ::  mat(nzcnt)


!----------------------------------------------
!       ... local variables
!----------------------------------------------
!PS
      real(r8), parameter :: gamma_coeff = 1._r8+1._r8/sqrt(2._r8)

!----------------------------------------------
!       ... complete matrix entries implicit species
!----------------------------------------------


         mat(   1) = lmat(   1)
         mat(   2) = lmat(   2)
         mat(   3) = lmat(   3)
         mat(   4) = lmat(   4)
         mat(   5) = lmat(   5)
         mat(   6) = lmat(   6)
         mat(   7) = lmat(   7)
         mat(   8) = lmat(   8)
         mat(   9) = lmat(   9)
         mat(  10) = lmat(  10)
         mat(  11) = lmat(  11)
         mat(  12) = lmat(  12)
         mat(  13) = lmat(  13)
         mat(  14) = lmat(  14)
         mat(  15) = lmat(  15)
         mat(  16) = lmat(  16)
         mat(  17) = lmat(  17)
         mat(  18) = lmat(  18)
         mat(  19) = lmat(  19)
         mat(  20) = lmat(  20)
         mat(  21) = lmat(  21)
         mat(  22) = lmat(  22)
         mat(  23) = lmat(  23)
         mat(  24) = lmat(  24)
         mat(  25) = lmat(  25)
         mat(  26) = lmat(  26)
         mat(  27) = lmat(  27)
         mat(  29) = mat(  29) + lmat(  29)
         mat(  31) = mat(  31) + lmat(  31)
         mat(  33) = mat(  33) + lmat(  33)
         mat(  34) = lmat(  34)
         mat(  35) = lmat(  35)
         mat(  36) = lmat(  36)
         mat(  38) = mat(  38) + lmat(  38)
         mat(  43) = lmat(  43)
         mat(  44) = lmat(  44)
         mat(  45) = lmat(  45)
         mat(  46) = lmat(  46)
         mat(  47) = lmat(  47)
         mat(  48) = lmat(  48)
         mat(  49) = mat(  49) + lmat(  49)
         mat(  55) = mat(  55) + lmat(  55)
         mat(  61) = mat(  61) + lmat(  61)
         mat(  62) = lmat(  62)
         mat(  64) = mat(  64) + lmat(  64)
         mat(  65) = mat(  65) + lmat(  65)
         mat(  66) = mat(  66) + lmat(  66)
         mat(  67) = lmat(  67)
         mat(  68) = lmat(  68)
         mat(  69) = mat(  69) + lmat(  69)
         mat(  70) = mat(  70) + lmat(  70)
         mat(  71) = mat(  71) + lmat(  71)
         mat(  73) = lmat(  73)
         mat(  74) = mat(  74) + lmat(  74)
         mat(  75) = mat(  75) + lmat(  75)
         mat(  77) = mat(  77) + lmat(  77)
         mat(  78) = lmat(  78)
         mat(  79) = mat(  79) + lmat(  79)
         mat(  80) = mat(  80) + lmat(  80)
         mat(  81) = mat(  81) + lmat(  81)
         mat(  85) = mat(  85) + lmat(  85)
         mat(  94) = mat(  94) + lmat(  94)
         mat( 103) = mat( 103) + lmat( 103)
         mat( 113) = mat( 113) + lmat( 113)
         mat( 121) = mat( 121) + lmat( 121)
         mat( 124) = mat( 124) + lmat( 124)
         mat( 128) = mat( 128) + lmat( 128)
         mat( 130) = lmat( 130)
         mat( 134) = mat( 134) + lmat( 134)
         mat( 136) = mat( 136) + lmat( 136)
         mat( 138) = lmat( 138)
         mat( 143) = mat( 143) + lmat( 143)
         mat( 157) = mat( 157) + lmat( 157)
         mat( 173) = mat( 173) + lmat( 173)
         mat( 184) = lmat( 184)
         mat( 186) = mat( 186) + lmat( 186)
         mat( 190) = mat( 190) + lmat( 190)
         mat( 206) = mat( 206) + lmat( 206)
         mat( 213) = mat( 213) + lmat( 213)
         mat( 216) = mat( 216) + lmat( 216)
         mat( 218) = mat( 218) + lmat( 218)
         mat( 220) = mat( 220) + lmat( 220)
         mat( 222) = lmat( 222)
         mat( 233) = mat( 233) + lmat( 233)
         mat( 236) = mat( 236) + lmat( 236)
         mat( 239) = mat( 239) + lmat( 239)
         mat( 240) = mat( 240) + lmat( 240)
         mat( 242) = mat( 242) + lmat( 242)
         mat( 244) = mat( 244) + lmat( 244)
         mat( 246) = mat( 246) + lmat( 246)
         mat( 259) = mat( 259) + lmat( 259)
         mat( 261) = mat( 261) + lmat( 261)
         mat( 262) = mat( 262) + lmat( 262)
         mat(  86) = 0._r8
         mat(  87) = 0._r8
         mat(  92) = 0._r8
         mat( 109) = 0._r8
         mat( 112) = 0._r8
         mat( 115) = 0._r8
         mat( 120) = 0._r8
         mat( 123) = 0._r8
         mat( 125) = 0._r8
         mat( 127) = 0._r8
         mat( 129) = 0._r8
         mat( 137) = 0._r8
         mat( 148) = 0._r8
         mat( 176) = 0._r8
         mat( 177) = 0._r8
         mat( 178) = 0._r8
         mat( 185) = 0._r8
         mat( 200) = 0._r8
         mat( 202) = 0._r8
         mat( 207) = 0._r8
         mat( 217) = 0._r8
         mat( 241) = 0._r8
         mat( 251) = 0._r8
         mat( 252) = 0._r8

         mat(:) = -1._r8*gamma_coeff*dt*mat(:)

         mat(   1) = mat(   1) + 1._r8
         mat(   2) = mat(   2) + 1._r8
         mat(   3) = mat(   3) + 1._r8
         mat(   4) = mat(   4) + 1._r8
         mat(   5) = mat(   5) + 1._r8
         mat(   6) = mat(   6) + 1._r8
         mat(   7) = mat(   7) + 1._r8
         mat(   8) = mat(   8) + 1._r8
         mat(   9) = mat(   9) + 1._r8
         mat(  10) = mat(  10) + 1._r8
         mat(  11) = mat(  11) + 1._r8
         mat(  12) = mat(  12) + 1._r8
         mat(  13) = mat(  13) + 1._r8
         mat(  14) = mat(  14) + 1._r8
         mat(  15) = mat(  15) + 1._r8
         mat(  16) = mat(  16) + 1._r8
         mat(  17) = mat(  17) + 1._r8
         mat(  18) = mat(  18) + 1._r8
         mat(  19) = mat(  19) + 1._r8
         mat(  20) = mat(  20) + 1._r8
         mat(  21) = mat(  21) + 1._r8
         mat(  22) = mat(  22) + 1._r8
         mat(  23) = mat(  23) + 1._r8
         mat(  24) = mat(  24) + 1._r8
         mat(  25) = mat(  25) + 1._r8
         mat(  26) = mat(  26) + 1._r8
         mat(  27) = mat(  27) + 1._r8
         mat(  29) = mat(  29) + 1._r8
         mat(  31) = mat(  31) + 1._r8
         mat(  34) = mat(  34) + 1._r8
         mat(  38) = mat(  38) + 1._r8
         mat(  43) = mat(  43) + 1._r8
         mat(  49) = mat(  49) + 1._r8
         mat(  55) = mat(  55) + 1._r8
         mat(  61) = mat(  61) + 1._r8
         mat(  65) = mat(  65) + 1._r8
         mat(  70) = mat(  70) + 1._r8
         mat(  75) = mat(  75) + 1._r8
         mat(  80) = mat(  80) + 1._r8
         mat(  85) = mat(  85) + 1._r8
         mat(  94) = mat(  94) + 1._r8
         mat( 103) = mat( 103) + 1._r8
         mat( 113) = mat( 113) + 1._r8
         mat( 124) = mat( 124) + 1._r8
         mat( 134) = mat( 134) + 1._r8
         mat( 143) = mat( 143) + 1._r8
         mat( 157) = mat( 157) + 1._r8
         mat( 173) = mat( 173) + 1._r8
         mat( 186) = mat( 186) + 1._r8
         mat( 206) = mat( 206) + 1._r8
         mat( 220) = mat( 220) + 1._r8
         mat( 246) = mat( 246) + 1._r8
         mat( 262) = mat( 262) + 1._r8

      end subroutine nlnmat_finit

!PS      subroutine     nlnmat( mat, y, rxt, lmat, dti )
      subroutine     nlnmat( mat, y, rxt, lmat, dt )

      use chem_mods, only : gas_pcnst, rxntot,     nzcnt

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
!PS      real(r8), intent(in)    ::  dti
      real(r8), intent(in)    ::  dt
      real(r8), intent(in)    ::  lmat(nzcnt)
      real(r8), intent(in)    ::  y(gas_pcnst)
      real(r8), intent(in)    ::  rxt(rxntot)
      real(r8), intent(inout) ::  mat(nzcnt)

      call     nlnmat01( mat, y, rxt )
      call     nlnmat02( mat, y, rxt )
!PS      call     nlnmat_finit( mat, lmat, dti )
      call     nlnmat_finit( mat, lmat, dt )

      end subroutine nlnmat

      end module mo_nln_matrix
