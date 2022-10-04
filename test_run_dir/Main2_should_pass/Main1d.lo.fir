circuit Main1d :
  module Receiver1 :
    input clock : Clock
    input reset : UInt<1>
    output io_in1_received : UInt<1>
    input io_in1_send : UInt<1>
    input io_in1_data : SInt<32>

    reg recReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), recReg) @[Main.scala 26:23]
    reg timeReg : SInt<32>, clock with :
      reset => (UInt<1>("h0"), timeReg) @[Main.scala 27:24]
    node _T = eq(io_in1_send, UInt<1>("h1")) @[Main.scala 29:20]
    node _T_1 = eq(recReg, UInt<1>("h0")) @[Main.scala 29:38]
    node _T_2 = and(_T, _T_1) @[Main.scala 29:28]
    node _T_3 = asUInt(reset) @[Main.scala 31:11]
    node _T_4 = eq(_T_3, UInt<1>("h0")) @[Main.scala 31:11]
    node _GEN_0 = mux(_T_2, UInt<1>("h1"), recReg) @[Main.scala 29:46 30:12 26:23]
    node _T_5 = eq(io_in1_send, UInt<1>("h0")) @[Main.scala 33:20]
    node _GEN_1 = mux(_T_5, UInt<1>("h0"), _GEN_0) @[Main.scala 33:{28,37}]
    node _timeReg_T = add(timeReg, asSInt(UInt<32>("h1"))) @[Main.scala 34:21]
    node _timeReg_T_1 = tail(_timeReg_T, 1) @[Main.scala 34:21]
    node _timeReg_T_2 = asSInt(_timeReg_T_1) @[Main.scala 34:21]
    node _T_6 = asUInt(reset) @[Main.scala 35:9]
    node _T_7 = eq(_T_6, UInt<1>("h0")) @[Main.scala 35:9]
    io_in1_received <= recReg @[Main.scala 28:19]
    recReg <= mux(reset, UInt<1>("h0"), _GEN_1) @[Main.scala 26:{23,23}]
    timeReg <= mux(reset, asSInt(UInt<32>("h0")), _timeReg_T_2) @[Main.scala 27:{24,24} 34:11]
    printf(clock, and(and(and(UInt<1>("h1"), _T_2), _T_4), UInt<1>("h1")), "receive   %d\n", io_in1_data) : printf @[Main.scala 31:11]
    printf(clock, and(and(UInt<1>("h1"), _T_7), UInt<1>("h1")), "Recivr %d \n", recReg) : printf_1 @[Main.scala 35:9]

  module Sender1 :
    input clock : Clock
    input reset : UInt<1>
    input io_out1_received : UInt<1>
    output io_out1_send : UInt<1>
    output io_out1_data : SInt<32>

    reg dataReg : SInt<32>, clock with :
      reset => (UInt<1>("h0"), dataReg) @[Main.scala 8:24]
    reg sendReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), sendReg) @[Main.scala 9:24]
    node _T = eq(io_out1_received, UInt<1>("h0")) @[Main.scala 12:25]
    node _T_1 = eq(sendReg, UInt<1>("h0")) @[Main.scala 12:44]
    node _T_2 = and(_T, _T_1) @[Main.scala 12:33]
    node _dataReg_T = add(dataReg, asSInt(UInt<32>("h1"))) @[Main.scala 14:23]
    node _dataReg_T_1 = tail(_dataReg_T, 1) @[Main.scala 14:23]
    node _dataReg_T_2 = asSInt(_dataReg_T_1) @[Main.scala 14:23]
    node _T_3 = asUInt(reset) @[Main.scala 15:11]
    node _T_4 = eq(_T_3, UInt<1>("h0")) @[Main.scala 15:11]
    node _GEN_0 = mux(_T_2, UInt<1>("h1"), sendReg) @[Main.scala 12:52 13:13 9:24]
    node _GEN_1 = mux(_T_2, _dataReg_T_2, dataReg) @[Main.scala 12:52 14:13 8:24]
    node _T_5 = gt(dataReg, asSInt(UInt<3>("h3"))) @[Main.scala 17:15]
    node _GEN_2 = mux(_T_5, asSInt(UInt<32>("hffffffff")), dataReg) @[Main.scala 11:16 17:{20,34}]
    node _T_6 = eq(io_out1_received, UInt<1>("h1")) @[Main.scala 18:25]
    node _GEN_3 = mux(_T_6, UInt<1>("h0"), _GEN_0) @[Main.scala 18:{33,43}]
    node _T_7 = asUInt(reset) @[Main.scala 19:9]
    node _T_8 = eq(_T_7, UInt<1>("h0")) @[Main.scala 19:9]
    io_out1_send <= sendReg @[Main.scala 10:16]
    io_out1_data <= _GEN_2
    dataReg <= mux(reset, asSInt(UInt<32>("h1")), _GEN_1) @[Main.scala 8:{24,24}]
    sendReg <= mux(reset, UInt<1>("h0"), _GEN_3) @[Main.scala 9:{24,24}]
    printf(clock, and(and(and(UInt<1>("h1"), _T_2), _T_4), UInt<1>("h1")), "send      %d\n", dataReg) : printf @[Main.scala 15:11]
    printf(clock, and(and(UInt<1>("h1"), _T_8), UInt<1>("h1")), "Sendr1 %d %d\n", sendReg, dataReg) : printf_1 @[Main.scala 19:9]

  module Main1d :
    input clock : Clock
    input reset : UInt<1>

    inst rec of Receiver1 @[Main.scala 50:19]
    inst snd of Sender1 @[Main.scala 51:19]
    reg timeReg : SInt<32>, clock with :
      reset => (UInt<1>("h0"), timeReg) @[Main.scala 52:24]
    node _timeReg_T = add(timeReg, asSInt(UInt<32>("h1"))) @[Main.scala 57:21]
    node _timeReg_T_1 = tail(_timeReg_T, 1) @[Main.scala 57:21]
    node _timeReg_T_2 = asSInt(_timeReg_T_1) @[Main.scala 57:21]
    node _T = bits(reset, 0, 0) @[Main.scala 58:9]
    node _T_1 = eq(_T, UInt<1>("h0")) @[Main.scala 58:9]
    rec.clock <= clock
    rec.reset <= reset
    rec.io_in1_send <= snd.io_out1_send @[Main.scala 55:19]
    rec.io_in1_data <= snd.io_out1_data @[Main.scala 56:19]
    snd.clock <= clock
    snd.reset <= reset
    snd.io_out1_received <= rec.io_in1_received @[Main.scala 54:24]
    timeReg <= mux(reset, asSInt(UInt<32>("h0")), _timeReg_T_2) @[Main.scala 52:{24,24} 57:11]
    printf(clock, and(and(UInt<1>("h1"), _T_1), UInt<1>("h1")), "Main1d --------%d ------------\n", timeReg) : printf @[Main.scala 58:9]
