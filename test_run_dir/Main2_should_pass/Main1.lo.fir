circuit Main1 :
  module Main1 :
    input clock : Clock
    input reset : UInt<1>
    input io_a : UInt<2>
    input io_b : UInt<2>
    output io_out : UInt<2>
    output io_out1data : SInt<32>
    output io_out1full : UInt<1>
    input io_out1ready : UInt<1>

    reg dataReg : SInt<32>, clock with :
      reset => (UInt<1>("h0"), dataReg) @[Main.scala 63:24]
    node _io_out_T = and(io_a, io_b) @[Main.scala 67:18]
    node _T = eq(io_out1ready, UInt<1>("h1")) @[Main.scala 69:21]
    node _GEN_0 = mux(_T, dataReg, asSInt(UInt<1>("h0"))) @[Main.scala 64:15 69:30 70:17]
    node _GEN_1 = mux(_T, UInt<1>("h1"), UInt<1>("h0")) @[Main.scala 65:15 69:30 71:17]
    node _dataReg_T = add(dataReg, asSInt(UInt<2>("h1"))) @[Main.scala 77:22]
    node _dataReg_T_1 = tail(_dataReg_T, 1) @[Main.scala 77:22]
    node _dataReg_T_2 = asSInt(_dataReg_T_1) @[Main.scala 77:22]
    io_out <= _io_out_T @[Main.scala 67:10]
    io_out1data <= _GEN_0
    io_out1full <= _GEN_1
    dataReg <= mux(reset, asSInt(UInt<32>("h2a")), _dataReg_T_2) @[Main.scala 63:{24,24} 77:11]
