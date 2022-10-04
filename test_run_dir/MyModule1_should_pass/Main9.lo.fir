circuit Main9 :
  module Main9 :
    input clock : Clock
    input reset : UInt<1>

    reg pc : SInt<32>, clock with :
      reset => (UInt<1>("h0"), pc) @[Main9.scala 6:19]
    reg i : SInt<32>, clock with :
      reset => (UInt<1>("h0"), i) @[Main9.scala 7:18]
    node _T = asUInt(asSInt(UInt<1>("h0"))) @[Main9.scala 9:14]
    node _T_1 = asUInt(pc) @[Main9.scala 9:14]
    node _T_2 = eq(_T, _T_1) @[Main9.scala 9:14]
    node _T_3 = asUInt(asSInt(UInt<2>("h1"))) @[Main9.scala 9:14]
    node _T_4 = asUInt(pc) @[Main9.scala 9:14]
    node _T_5 = eq(_T_3, _T_4) @[Main9.scala 9:14]
    node _pc_T = lt(i, asSInt(UInt<5>("ha"))) @[Main9.scala 16:19]
    node _pc_T_1 = mux(_pc_T, asSInt(UInt<3>("h2")), asSInt(UInt<3>("h3"))) @[Main9.scala 16:16]
    node _T_6 = asUInt(asSInt(UInt<3>("h2"))) @[Main9.scala 9:14]
    node _T_7 = asUInt(pc) @[Main9.scala 9:14]
    node _T_8 = eq(_T_6, _T_7) @[Main9.scala 9:14]
    node _T_9 = bits(reset, 0, 0) @[Main9.scala 19:13]
    node _T_10 = eq(_T_9, UInt<1>("h0")) @[Main9.scala 19:13]
    node _i_T = add(i, asSInt(UInt<2>("h1"))) @[Main9.scala 21:14]
    node _i_T_1 = tail(_i_T, 1) @[Main9.scala 21:14]
    node _i_T_2 = asSInt(_i_T_1) @[Main9.scala 21:14]
    node _T_11 = asUInt(asSInt(UInt<3>("h3"))) @[Main9.scala 9:14]
    node _T_12 = asUInt(pc) @[Main9.scala 9:14]
    node _T_13 = eq(_T_11, _T_12) @[Main9.scala 9:14]
    node _GEN_0 = mux(_T_13, asSInt(UInt<11>("h3e8")), pc) @[Main9.scala 26:10 9:14 6:19]
    node _GEN_1 = mux(_T_8, _i_T_2, i) @[Main9.scala 9:14 21:9 7:18]
    node _GEN_2 = mux(_T_8, asSInt(UInt<2>("h1")), _GEN_0) @[Main9.scala 22:10 9:14]
    node _GEN_3 = mux(_T_5, _pc_T_1, _GEN_2) @[Main9.scala 16:10 9:14]
    node _GEN_4 = mux(_T_5, i, _GEN_1) @[Main9.scala 9:14 7:18]
    node _GEN_5 = mux(_T_2, asSInt(UInt<2>("h1")), _GEN_3) @[Main9.scala 12:10 9:14]
    node _GEN_6 = mux(_T_2, i, _GEN_4) @[Main9.scala 9:14 7:18]
    pc <= mux(reset, asSInt(UInt<32>("h0")), _GEN_5) @[Main9.scala 6:{19,19}]
    i <= mux(reset, asSInt(UInt<32>("h0")), _GEN_6) @[Main9.scala 7:{18,18}]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T_2, UInt<1>("h0"))), eq(_T_5, UInt<1>("h0"))), _T_8), _T_10), UInt<1>("h1")), "%d\n", i) : printf @[Main9.scala 19:13]
