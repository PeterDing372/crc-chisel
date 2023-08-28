circuit CRC_hash :
  module CRC_hash :
    input clock : Clock
    input reset : UInt<1>
    input io_en : UInt<1>
    input io_in : UInt<1>
    output io_out : UInt<1>
    output io_debug : UInt<5>

    reg lfsr_0 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_0) @[CRC.scala 180:21]
    reg lfsr_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_1) @[CRC.scala 180:21]
    reg lfsr_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_2) @[CRC.scala 180:21]
    reg lfsr_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_3) @[CRC.scala 180:21]
    reg lfsr_4 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_4) @[CRC.scala 180:21]
    node _bit_T = xor(io_in, lfsr_4) @[CRC.scala 184:30]
    node bit = mux(io_en, _bit_T, UInt<1>("h0")) @[CRC.scala 184:16]
    node sel_last = mux(io_en, lfsr_4, UInt<1>("h0")) @[CRC.scala 185:21]
    node _lfsr_2_T = xor(lfsr_1, sel_last) @[CRC.scala 196:30]
    node _io_out_T = mux(io_en, io_in, lfsr_4) @[CRC.scala 205:16]
    node io_debug_lo = cat(lfsr_1, lfsr_0) @[CRC.scala 206:20]
    node io_debug_hi_hi = cat(lfsr_4, lfsr_3) @[CRC.scala 206:20]
    node io_debug_hi = cat(io_debug_hi_hi, lfsr_2) @[CRC.scala 206:20]
    node _io_debug_T = cat(io_debug_hi, io_debug_lo) @[CRC.scala 206:20]
    node _lfsr_WIRE_0 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_1 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_2 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_3 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_4 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    io_out <= _io_out_T @[CRC.scala 205:10]
    io_debug <= _io_debug_T @[CRC.scala 206:12]
    lfsr_0 <= mux(reset, _lfsr_WIRE_0, bit) @[CRC.scala 180:{21,21} 188:11]
    lfsr_1 <= mux(reset, _lfsr_WIRE_1, lfsr_0) @[CRC.scala 180:{21,21} 199:19]
    lfsr_2 <= mux(reset, _lfsr_WIRE_2, _lfsr_2_T) @[CRC.scala 180:{21,21} 196:19]
    lfsr_3 <= mux(reset, _lfsr_WIRE_3, lfsr_2) @[CRC.scala 180:{21,21} 199:19]
    lfsr_4 <= mux(reset, _lfsr_WIRE_4, lfsr_3) @[CRC.scala 180:{21,21} 199:19]
