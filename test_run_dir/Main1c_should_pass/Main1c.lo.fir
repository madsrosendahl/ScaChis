circuit Main1c :
  module Main1c :
    input clock : Clock
    input reset : UInt<1>
    input io_out1_write : UInt<1>
    output io_out1_full : UInt<1>
    input io_out1_dout : SInt<32>
    input io_in1_read : UInt<1>
    output io_in1_empty : UInt<1>
    output io_in1_din : SInt<32>

    reg dataReg : SInt<32>, clock with :
      reset => (UInt<1>("h0"), dataReg) @[Main.scala 14:24]
    node _dataReg_T = add(dataReg, asSInt(UInt<2>("h1"))) @[Main.scala 17:22]
    node _dataReg_T_1 = tail(_dataReg_T, 1) @[Main.scala 17:22]
    node _dataReg_T_2 = asSInt(_dataReg_T_1) @[Main.scala 17:22]
    io_out1_full <= UInt<1>("h0") @[Main.scala 12:16]
    io_in1_empty <= UInt<1>("h1") @[Main.scala 13:16]
    io_in1_din <= dataReg @[Main.scala 15:14]
    dataReg <= mux(reset, asSInt(UInt<32>("h2a")), _dataReg_T_2) @[Main.scala 14:{24,24} 17:11]
