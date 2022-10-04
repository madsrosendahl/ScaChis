circuit Main1b :
  module Main1b :
    input clock : Clock
    input reset : UInt<1>
    input io_bin_read : UInt<1>
    output io_bin_empty : UInt<1>
    output io_bin_din : SInt<32>

    node _io_bin_din_T = mux(io_bin_read, asSInt(UInt<32>("h1")), asSInt(UInt<32>("h2"))) @[Main.scala 86:33]
    node _io_bin_din_T_1 = add(asSInt(UInt<32>("h29")), _io_bin_din_T) @[Main.scala 86:28]
    node _io_bin_din_T_2 = tail(_io_bin_din_T_1, 1) @[Main.scala 86:28]
    node _io_bin_din_T_3 = asSInt(_io_bin_din_T_2) @[Main.scala 86:28]
    io_bin_empty <= UInt<1>("h0") @[Main.scala 87:16]
    io_bin_din <= _io_bin_din_T_3 @[Main.scala 86:14]
