circuit Main :
  module Main :
    input clock : Clock
    input reset : UInt<1>
    output io_led : UInt<1>

    reg cntReg : UInt<32>, clock with :
      reset => (UInt<1>("h0"), cntReg) @[Main.scala 13:23]
    reg blkReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), blkReg) @[Main.scala 14:23]
    node _cntReg_T = add(cntReg, UInt<1>("h1")) @[Main.scala 16:20]
    node _cntReg_T_1 = tail(_cntReg_T, 1) @[Main.scala 16:20]
    node _T = eq(cntReg, UInt<16>("hc34f")) @[Main.scala 17:15]
    node _blkReg_T = not(blkReg) @[Main.scala 19:15]
    node _GEN_0 = mux(_T, UInt<1>("h0"), _cntReg_T_1) @[Main.scala 16:10 17:28 18:12]
    node _GEN_1 = mux(_T, _blkReg_T, blkReg) @[Main.scala 17:28 19:12 14:23]
    io_led <= blkReg @[Main.scala 21:10]
    cntReg <= mux(reset, UInt<32>("h0"), _GEN_0) @[Main.scala 13:{23,23}]
    blkReg <= mux(reset, UInt<1>("h0"), _GEN_1) @[Main.scala 14:{23,23}]
