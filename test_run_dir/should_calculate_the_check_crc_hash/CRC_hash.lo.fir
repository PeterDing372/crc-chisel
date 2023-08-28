circuit CRC_hash :
  module CRC_hash :
    input clock : Clock
    input reset : UInt<1>
    input io_en : UInt<1>
    input io_in : UInt<1>
    output io_out : UInt<1>
    output io_debug : UInt<44>

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
    reg lfsr_5 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_5) @[CRC.scala 180:21]
    reg lfsr_6 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_6) @[CRC.scala 180:21]
    reg lfsr_7 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_7) @[CRC.scala 180:21]
    reg lfsr_8 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_8) @[CRC.scala 180:21]
    reg lfsr_9 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_9) @[CRC.scala 180:21]
    reg lfsr_10 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_10) @[CRC.scala 180:21]
    reg lfsr_11 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_11) @[CRC.scala 180:21]
    reg lfsr_12 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_12) @[CRC.scala 180:21]
    reg lfsr_13 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_13) @[CRC.scala 180:21]
    reg lfsr_14 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_14) @[CRC.scala 180:21]
    reg lfsr_15 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_15) @[CRC.scala 180:21]
    reg lfsr_16 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_16) @[CRC.scala 180:21]
    reg lfsr_17 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_17) @[CRC.scala 180:21]
    reg lfsr_18 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_18) @[CRC.scala 180:21]
    reg lfsr_19 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_19) @[CRC.scala 180:21]
    reg lfsr_20 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_20) @[CRC.scala 180:21]
    reg lfsr_21 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_21) @[CRC.scala 180:21]
    reg lfsr_22 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_22) @[CRC.scala 180:21]
    reg lfsr_23 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_23) @[CRC.scala 180:21]
    reg lfsr_24 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_24) @[CRC.scala 180:21]
    reg lfsr_25 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_25) @[CRC.scala 180:21]
    reg lfsr_26 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_26) @[CRC.scala 180:21]
    reg lfsr_27 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_27) @[CRC.scala 180:21]
    reg lfsr_28 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_28) @[CRC.scala 180:21]
    reg lfsr_29 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_29) @[CRC.scala 180:21]
    reg lfsr_30 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_30) @[CRC.scala 180:21]
    reg lfsr_31 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_31) @[CRC.scala 180:21]
    reg lfsr_32 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_32) @[CRC.scala 180:21]
    reg lfsr_33 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_33) @[CRC.scala 180:21]
    reg lfsr_34 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_34) @[CRC.scala 180:21]
    reg lfsr_35 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_35) @[CRC.scala 180:21]
    reg lfsr_36 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_36) @[CRC.scala 180:21]
    reg lfsr_37 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_37) @[CRC.scala 180:21]
    reg lfsr_38 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_38) @[CRC.scala 180:21]
    reg lfsr_39 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_39) @[CRC.scala 180:21]
    reg lfsr_40 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_40) @[CRC.scala 180:21]
    reg lfsr_41 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_41) @[CRC.scala 180:21]
    reg lfsr_42 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_42) @[CRC.scala 180:21]
    reg lfsr_43 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lfsr_43) @[CRC.scala 180:21]
    node _bit_T = xor(io_in, lfsr_43) @[CRC.scala 184:30]
    node bit = mux(io_en, _bit_T, UInt<1>("h0")) @[CRC.scala 184:16]
    node sel_last = mux(io_en, lfsr_43, UInt<1>("h0")) @[CRC.scala 185:21]
    node _lfsr_2_T = xor(lfsr_1, sel_last) @[CRC.scala 196:30]
    node _lfsr_5_T = xor(lfsr_4, sel_last) @[CRC.scala 196:30]
    node _lfsr_6_T = xor(lfsr_5, sel_last) @[CRC.scala 196:30]
    node _lfsr_31_T = xor(lfsr_30, sel_last) @[CRC.scala 196:30]
    node _lfsr_32_T = xor(lfsr_31, sel_last) @[CRC.scala 196:30]
    node _lfsr_34_T = xor(lfsr_33, sel_last) @[CRC.scala 196:30]
    node _lfsr_37_T = xor(lfsr_36, sel_last) @[CRC.scala 196:30]
    node _lfsr_38_T = xor(lfsr_37, sel_last) @[CRC.scala 196:30]
    node _io_out_T = mux(io_en, io_in, lfsr_43) @[CRC.scala 205:16]
    node io_debug_lo_lo_lo_lo = cat(lfsr_1, lfsr_0) @[CRC.scala 206:20]
    node io_debug_lo_lo_lo_hi_hi = cat(lfsr_4, lfsr_3) @[CRC.scala 206:20]
    node io_debug_lo_lo_lo_hi = cat(io_debug_lo_lo_lo_hi_hi, lfsr_2) @[CRC.scala 206:20]
    node io_debug_lo_lo_lo = cat(io_debug_lo_lo_lo_hi, io_debug_lo_lo_lo_lo) @[CRC.scala 206:20]
    node io_debug_lo_lo_hi_lo_hi = cat(lfsr_7, lfsr_6) @[CRC.scala 206:20]
    node io_debug_lo_lo_hi_lo = cat(io_debug_lo_lo_hi_lo_hi, lfsr_5) @[CRC.scala 206:20]
    node io_debug_lo_lo_hi_hi_hi = cat(lfsr_10, lfsr_9) @[CRC.scala 206:20]
    node io_debug_lo_lo_hi_hi = cat(io_debug_lo_lo_hi_hi_hi, lfsr_8) @[CRC.scala 206:20]
    node io_debug_lo_lo_hi = cat(io_debug_lo_lo_hi_hi, io_debug_lo_lo_hi_lo) @[CRC.scala 206:20]
    node io_debug_lo_lo = cat(io_debug_lo_lo_hi, io_debug_lo_lo_lo) @[CRC.scala 206:20]
    node io_debug_lo_hi_lo_lo = cat(lfsr_12, lfsr_11) @[CRC.scala 206:20]
    node io_debug_lo_hi_lo_hi_hi = cat(lfsr_15, lfsr_14) @[CRC.scala 206:20]
    node io_debug_lo_hi_lo_hi = cat(io_debug_lo_hi_lo_hi_hi, lfsr_13) @[CRC.scala 206:20]
    node io_debug_lo_hi_lo = cat(io_debug_lo_hi_lo_hi, io_debug_lo_hi_lo_lo) @[CRC.scala 206:20]
    node io_debug_lo_hi_hi_lo_hi = cat(lfsr_18, lfsr_17) @[CRC.scala 206:20]
    node io_debug_lo_hi_hi_lo = cat(io_debug_lo_hi_hi_lo_hi, lfsr_16) @[CRC.scala 206:20]
    node io_debug_lo_hi_hi_hi_hi = cat(lfsr_21, lfsr_20) @[CRC.scala 206:20]
    node io_debug_lo_hi_hi_hi = cat(io_debug_lo_hi_hi_hi_hi, lfsr_19) @[CRC.scala 206:20]
    node io_debug_lo_hi_hi = cat(io_debug_lo_hi_hi_hi, io_debug_lo_hi_hi_lo) @[CRC.scala 206:20]
    node io_debug_lo_hi = cat(io_debug_lo_hi_hi, io_debug_lo_hi_lo) @[CRC.scala 206:20]
    node io_debug_lo = cat(io_debug_lo_hi, io_debug_lo_lo) @[CRC.scala 206:20]
    node io_debug_hi_lo_lo_lo = cat(lfsr_23, lfsr_22) @[CRC.scala 206:20]
    node io_debug_hi_lo_lo_hi_hi = cat(lfsr_26, lfsr_25) @[CRC.scala 206:20]
    node io_debug_hi_lo_lo_hi = cat(io_debug_hi_lo_lo_hi_hi, lfsr_24) @[CRC.scala 206:20]
    node io_debug_hi_lo_lo = cat(io_debug_hi_lo_lo_hi, io_debug_hi_lo_lo_lo) @[CRC.scala 206:20]
    node io_debug_hi_lo_hi_lo_hi = cat(lfsr_29, lfsr_28) @[CRC.scala 206:20]
    node io_debug_hi_lo_hi_lo = cat(io_debug_hi_lo_hi_lo_hi, lfsr_27) @[CRC.scala 206:20]
    node io_debug_hi_lo_hi_hi_hi = cat(lfsr_32, lfsr_31) @[CRC.scala 206:20]
    node io_debug_hi_lo_hi_hi = cat(io_debug_hi_lo_hi_hi_hi, lfsr_30) @[CRC.scala 206:20]
    node io_debug_hi_lo_hi = cat(io_debug_hi_lo_hi_hi, io_debug_hi_lo_hi_lo) @[CRC.scala 206:20]
    node io_debug_hi_lo = cat(io_debug_hi_lo_hi, io_debug_hi_lo_lo) @[CRC.scala 206:20]
    node io_debug_hi_hi_lo_lo = cat(lfsr_34, lfsr_33) @[CRC.scala 206:20]
    node io_debug_hi_hi_lo_hi_hi = cat(lfsr_37, lfsr_36) @[CRC.scala 206:20]
    node io_debug_hi_hi_lo_hi = cat(io_debug_hi_hi_lo_hi_hi, lfsr_35) @[CRC.scala 206:20]
    node io_debug_hi_hi_lo = cat(io_debug_hi_hi_lo_hi, io_debug_hi_hi_lo_lo) @[CRC.scala 206:20]
    node io_debug_hi_hi_hi_lo_hi = cat(lfsr_40, lfsr_39) @[CRC.scala 206:20]
    node io_debug_hi_hi_hi_lo = cat(io_debug_hi_hi_hi_lo_hi, lfsr_38) @[CRC.scala 206:20]
    node io_debug_hi_hi_hi_hi_hi = cat(lfsr_43, lfsr_42) @[CRC.scala 206:20]
    node io_debug_hi_hi_hi_hi = cat(io_debug_hi_hi_hi_hi_hi, lfsr_41) @[CRC.scala 206:20]
    node io_debug_hi_hi_hi = cat(io_debug_hi_hi_hi_hi, io_debug_hi_hi_hi_lo) @[CRC.scala 206:20]
    node io_debug_hi_hi = cat(io_debug_hi_hi_hi, io_debug_hi_hi_lo) @[CRC.scala 206:20]
    node io_debug_hi = cat(io_debug_hi_hi, io_debug_hi_lo) @[CRC.scala 206:20]
    node _io_debug_T = cat(io_debug_hi, io_debug_lo) @[CRC.scala 206:20]
    node _lfsr_WIRE_0 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_1 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_2 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_3 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_4 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_5 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_6 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_7 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_8 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_9 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_10 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_11 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_12 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_13 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_14 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_15 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_16 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_17 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_18 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_19 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_20 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_21 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_22 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_23 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_24 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_25 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_26 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_27 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_28 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_29 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_30 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_31 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_32 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_33 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_34 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_35 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_36 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_37 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_38 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_39 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_40 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_41 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_42 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    node _lfsr_WIRE_43 = UInt<1>("h0") @[CRC.scala 180:{29,29}]
    io_out <= _io_out_T @[CRC.scala 205:10]
    io_debug <= _io_debug_T @[CRC.scala 206:12]
    lfsr_0 <= mux(reset, _lfsr_WIRE_0, bit) @[CRC.scala 180:{21,21} 188:11]
    lfsr_1 <= mux(reset, _lfsr_WIRE_1, lfsr_0) @[CRC.scala 180:{21,21} 199:19]
    lfsr_2 <= mux(reset, _lfsr_WIRE_2, _lfsr_2_T) @[CRC.scala 180:{21,21} 196:19]
    lfsr_3 <= mux(reset, _lfsr_WIRE_3, lfsr_2) @[CRC.scala 180:{21,21} 199:19]
    lfsr_4 <= mux(reset, _lfsr_WIRE_4, lfsr_3) @[CRC.scala 180:{21,21} 199:19]
    lfsr_5 <= mux(reset, _lfsr_WIRE_5, _lfsr_5_T) @[CRC.scala 180:{21,21} 196:19]
    lfsr_6 <= mux(reset, _lfsr_WIRE_6, _lfsr_6_T) @[CRC.scala 180:{21,21} 196:19]
    lfsr_7 <= mux(reset, _lfsr_WIRE_7, lfsr_6) @[CRC.scala 180:{21,21} 199:19]
    lfsr_8 <= mux(reset, _lfsr_WIRE_8, lfsr_7) @[CRC.scala 180:{21,21} 199:19]
    lfsr_9 <= mux(reset, _lfsr_WIRE_9, lfsr_8) @[CRC.scala 180:{21,21} 199:19]
    lfsr_10 <= mux(reset, _lfsr_WIRE_10, lfsr_9) @[CRC.scala 180:{21,21} 199:19]
    lfsr_11 <= mux(reset, _lfsr_WIRE_11, lfsr_10) @[CRC.scala 180:{21,21} 199:19]
    lfsr_12 <= mux(reset, _lfsr_WIRE_12, lfsr_11) @[CRC.scala 180:{21,21} 199:19]
    lfsr_13 <= mux(reset, _lfsr_WIRE_13, lfsr_12) @[CRC.scala 180:{21,21} 199:19]
    lfsr_14 <= mux(reset, _lfsr_WIRE_14, lfsr_13) @[CRC.scala 180:{21,21} 199:19]
    lfsr_15 <= mux(reset, _lfsr_WIRE_15, lfsr_14) @[CRC.scala 180:{21,21} 199:19]
    lfsr_16 <= mux(reset, _lfsr_WIRE_16, lfsr_15) @[CRC.scala 180:{21,21} 199:19]
    lfsr_17 <= mux(reset, _lfsr_WIRE_17, lfsr_16) @[CRC.scala 180:{21,21} 199:19]
    lfsr_18 <= mux(reset, _lfsr_WIRE_18, lfsr_17) @[CRC.scala 180:{21,21} 199:19]
    lfsr_19 <= mux(reset, _lfsr_WIRE_19, lfsr_18) @[CRC.scala 180:{21,21} 199:19]
    lfsr_20 <= mux(reset, _lfsr_WIRE_20, lfsr_19) @[CRC.scala 180:{21,21} 199:19]
    lfsr_21 <= mux(reset, _lfsr_WIRE_21, lfsr_20) @[CRC.scala 180:{21,21} 199:19]
    lfsr_22 <= mux(reset, _lfsr_WIRE_22, lfsr_21) @[CRC.scala 180:{21,21} 199:19]
    lfsr_23 <= mux(reset, _lfsr_WIRE_23, lfsr_22) @[CRC.scala 180:{21,21} 199:19]
    lfsr_24 <= mux(reset, _lfsr_WIRE_24, lfsr_23) @[CRC.scala 180:{21,21} 199:19]
    lfsr_25 <= mux(reset, _lfsr_WIRE_25, lfsr_24) @[CRC.scala 180:{21,21} 199:19]
    lfsr_26 <= mux(reset, _lfsr_WIRE_26, lfsr_25) @[CRC.scala 180:{21,21} 199:19]
    lfsr_27 <= mux(reset, _lfsr_WIRE_27, lfsr_26) @[CRC.scala 180:{21,21} 199:19]
    lfsr_28 <= mux(reset, _lfsr_WIRE_28, lfsr_27) @[CRC.scala 180:{21,21} 199:19]
    lfsr_29 <= mux(reset, _lfsr_WIRE_29, lfsr_28) @[CRC.scala 180:{21,21} 199:19]
    lfsr_30 <= mux(reset, _lfsr_WIRE_30, lfsr_29) @[CRC.scala 180:{21,21} 199:19]
    lfsr_31 <= mux(reset, _lfsr_WIRE_31, _lfsr_31_T) @[CRC.scala 180:{21,21} 196:19]
    lfsr_32 <= mux(reset, _lfsr_WIRE_32, _lfsr_32_T) @[CRC.scala 180:{21,21} 196:19]
    lfsr_33 <= mux(reset, _lfsr_WIRE_33, lfsr_32) @[CRC.scala 180:{21,21} 199:19]
    lfsr_34 <= mux(reset, _lfsr_WIRE_34, _lfsr_34_T) @[CRC.scala 180:{21,21} 196:19]
    lfsr_35 <= mux(reset, _lfsr_WIRE_35, lfsr_34) @[CRC.scala 180:{21,21} 199:19]
    lfsr_36 <= mux(reset, _lfsr_WIRE_36, lfsr_35) @[CRC.scala 180:{21,21} 199:19]
    lfsr_37 <= mux(reset, _lfsr_WIRE_37, _lfsr_37_T) @[CRC.scala 180:{21,21} 196:19]
    lfsr_38 <= mux(reset, _lfsr_WIRE_38, _lfsr_38_T) @[CRC.scala 180:{21,21} 196:19]
    lfsr_39 <= mux(reset, _lfsr_WIRE_39, lfsr_38) @[CRC.scala 180:{21,21} 199:19]
    lfsr_40 <= mux(reset, _lfsr_WIRE_40, lfsr_39) @[CRC.scala 180:{21,21} 199:19]
    lfsr_41 <= mux(reset, _lfsr_WIRE_41, lfsr_40) @[CRC.scala 180:{21,21} 199:19]
    lfsr_42 <= mux(reset, _lfsr_WIRE_42, lfsr_41) @[CRC.scala 180:{21,21} 199:19]
    lfsr_43 <= mux(reset, _lfsr_WIRE_43, lfsr_42) @[CRC.scala 180:{21,21} 199:19]
