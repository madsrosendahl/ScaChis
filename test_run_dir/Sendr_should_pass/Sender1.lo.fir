circuit Sender1 :
  module Sender1 :
    input clock : Clock
    input reset : UInt<1>
    input io_out1_received : UInt<1>
    output io_out1_send : UInt<1>
    output io_out1_data : SInt<32>

    reg dataReg : SInt<32>, clock with :
      reset => (UInt<1>("h0"), dataReg) @[Main.scala 10:24]
    reg sendReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), sendReg) @[Main.scala 11:24]
    node _T = eq(io_out1_received, UInt<1>("h0")) @[Main.scala 14:25]
    node _T_1 = eq(sendReg, UInt<1>("h0")) @[Main.scala 14:44]
    node _T_2 = and(_T, _T_1) @[Main.scala 14:33]
    node _dataReg_T = add(dataReg, asSInt(UInt<32>("h1"))) @[Main.scala 16:23]
    node _dataReg_T_1 = tail(_dataReg_T, 1) @[Main.scala 16:23]
    node _dataReg_T_2 = asSInt(_dataReg_T_1) @[Main.scala 16:23]
    node _T_3 = bits(reset, 0, 0) @[Main.scala 17:11]
    node _T_4 = eq(_T_3, UInt<1>("h0")) @[Main.scala 17:11]
    node _GEN_0 = mux(_T_2, UInt<1>("h1"), sendReg) @[Main.scala 14:52 15:13 11:24]
    node _GEN_1 = mux(_T_2, _dataReg_T_2, dataReg) @[Main.scala 14:52 16:13 10:24]
    node _T_5 = gt(dataReg, asSInt(UInt<3>("h3"))) @[Main.scala 19:15]
    node _GEN_2 = mux(_T_5, asSInt(UInt<32>("hffffffff")), dataReg) @[Main.scala 13:16 19:{20,34}]
    node _T_6 = eq(io_out1_received, UInt<1>("h1")) @[Main.scala 20:25]
    node _GEN_3 = mux(_T_6, UInt<1>("h0"), _GEN_0) @[Main.scala 20:{33,43}]
    node _T_7 = bits(reset, 0, 0) @[Main.scala 21:9]
    node _T_8 = eq(_T_7, UInt<1>("h0")) @[Main.scala 21:9]
    io_out1_send <= sendReg @[Main.scala 12:16]
    io_out1_data <= _GEN_2
    dataReg <= mux(reset, asSInt(UInt<32>("h0")), _GEN_1) @[Main.scala 10:{24,24}]
    sendReg <= mux(reset, UInt<1>("h1"), _GEN_3) @[Main.scala 11:{24,24}]
    printf(clock, and(and(and(UInt<1>("h1"), _T_2), _T_4), UInt<1>("h1")), "send      %d\n", dataReg) : printf @[Main.scala 17:11]
    printf(clock, and(and(UInt<1>("h1"), _T_8), UInt<1>("h1")), "Sendr1 %d %d\n", sendReg, dataReg) : printf_1 @[Main.scala 21:9]
