circuit CRC :
  module CRC :
    input clock : Clock
    input reset : UInt<1>
    input io_en : UInt<1>
    input io_in : UInt<1>
    output io_out : UInt<1>
    output io_debug : UInt<16>

    reg lfsr_0 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_0) @[CRC.scala 61:17]
    reg lfsr_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_1) @[CRC.scala 61:17]
    reg lfsr_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_2) @[CRC.scala 61:17]
    reg lfsr_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_3) @[CRC.scala 61:17]
    reg lfsr_4 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_4) @[CRC.scala 61:17]
    reg lfsr_5 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_5) @[CRC.scala 61:17]
    reg lfsr_6 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_6) @[CRC.scala 61:17]
    reg lfsr_7 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_7) @[CRC.scala 61:17]
    reg lfsr_8 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_8) @[CRC.scala 61:17]
    reg lfsr_9 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_9) @[CRC.scala 61:17]
    reg lfsr_10 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_10) @[CRC.scala 61:17]
    reg lfsr_11 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_11) @[CRC.scala 61:17]
    reg lfsr_12 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_12) @[CRC.scala 61:17]
    reg lfsr_13 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_13) @[CRC.scala 61:17]
    reg lfsr_14 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_14) @[CRC.scala 61:17]
    reg lfsr_15 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_15) @[CRC.scala 61:17]
    node _bit_T = xor(io_in, lfsr_15) @[CRC.scala 64:30]
    node bit = mux(io_en, _bit_T, UInt<1>("h0")) @[CRC.scala 64:16]
    node _lfsr_5_T = xor(lfsr_4, bit) @[CRC.scala 72:30]
    node _lfsr_12_T = xor(lfsr_11, bit) @[CRC.scala 72:30]
    node _io_out_T = mux(io_en, io_in, lfsr_15) @[CRC.scala 78:16]
    node io_debug_lo_lo_lo = cat(lfsr_1, lfsr_0) @[CRC.scala 79:20]
    node io_debug_lo_lo_hi = cat(lfsr_3, lfsr_2) @[CRC.scala 79:20]
    node io_debug_lo_lo = cat(io_debug_lo_lo_hi, io_debug_lo_lo_lo) @[CRC.scala 79:20]
    node io_debug_lo_hi_lo = cat(lfsr_5, lfsr_4) @[CRC.scala 79:20]
    node io_debug_lo_hi_hi = cat(lfsr_7, lfsr_6) @[CRC.scala 79:20]
    node io_debug_lo_hi = cat(io_debug_lo_hi_hi, io_debug_lo_hi_lo) @[CRC.scala 79:20]
    node io_debug_lo = cat(io_debug_lo_hi, io_debug_lo_lo) @[CRC.scala 79:20]
    node io_debug_hi_lo_lo = cat(lfsr_9, lfsr_8) @[CRC.scala 79:20]
    node io_debug_hi_lo_hi = cat(lfsr_11, lfsr_10) @[CRC.scala 79:20]
    node io_debug_hi_lo = cat(io_debug_hi_lo_hi, io_debug_hi_lo_lo) @[CRC.scala 79:20]
    node io_debug_hi_hi_lo = cat(lfsr_13, lfsr_12) @[CRC.scala 79:20]
    node io_debug_hi_hi_hi = cat(lfsr_15, lfsr_14) @[CRC.scala 79:20]
    node io_debug_hi_hi = cat(io_debug_hi_hi_hi, io_debug_hi_hi_lo) @[CRC.scala 79:20]
    node io_debug_hi = cat(io_debug_hi_hi, io_debug_hi_lo) @[CRC.scala 79:20]
    node _io_debug_T = cat(io_debug_hi, io_debug_lo) @[CRC.scala 79:20]
    io_out <= _io_out_T @[CRC.scala 78:10]
    io_debug <= _io_debug_T @[CRC.scala 79:12]
    lfsr_0 <= bit @[CRC.scala 67:11]
    lfsr_1 <= lfsr_0 @[CRC.scala 74:19]
    lfsr_2 <= lfsr_1 @[CRC.scala 74:19]
    lfsr_3 <= lfsr_2 @[CRC.scala 74:19]
    lfsr_4 <= lfsr_3 @[CRC.scala 74:19]
    lfsr_5 <= _lfsr_5_T @[CRC.scala 72:19]
    lfsr_6 <= lfsr_5 @[CRC.scala 74:19]
    lfsr_7 <= lfsr_6 @[CRC.scala 74:19]
    lfsr_8 <= lfsr_7 @[CRC.scala 74:19]
    lfsr_9 <= lfsr_8 @[CRC.scala 74:19]
    lfsr_10 <= lfsr_9 @[CRC.scala 74:19]
    lfsr_11 <= lfsr_10 @[CRC.scala 74:19]
    lfsr_12 <= _lfsr_12_T @[CRC.scala 72:19]
    lfsr_13 <= lfsr_12 @[CRC.scala 74:19]
    lfsr_14 <= lfsr_13 @[CRC.scala 74:19]
    lfsr_15 <= lfsr_14 @[CRC.scala 74:19]
