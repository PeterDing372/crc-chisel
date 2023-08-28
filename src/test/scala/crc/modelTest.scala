/*
 *   __   __     __  __     __         __
 *  /\ "-.\ \   /\ \/\ \   /\ \       /\ \
 *  \ \ \-.  \  \ \ \_\ \  \ \ \____  \ \ \____
 *   \ \_\\"\_\  \ \_____\  \ \_____\  \ \_____\
 *    \/_/ \/_/   \/_____/   \/_____/   \/_____/
 *   ______     ______       __     ______     ______     ______
 *  /\  __ \   /\  == \     /\ \   /\  ___\   /\  ___\   /\__  _\
 *  \ \ \/\ \  \ \  __<    _\_\ \  \ \  __\   \ \ \____  \/_/\ \/
 *   \ \_____\  \ \_____\ /\_____\  \ \_____\  \ \_____\    \ \_\
 *    \/_____/   \/_____/ \/_____/   \/_____/   \/_____/     \/_/
 *
 * https://joshbassett.info
 * https://twitter.com/nullobject
 * https://github.com/nullobject
 *
 * Copyright (dut) 2021 Josh Bassett
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package crc

import chisel3._
import chiseltest._
import org.scalatest._
import flatspec.AnyFlatSpec
import matchers.should.Matchers






class modelTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  def readBits(dut: CRC_hash, n: Int) = {
    println("readbits: \n")
    var bits = 0
    for (i <- (n - 1) to 0 by -1) {
      println(s"\nthe register values: ${dut.io.debug.peek()}\n")
      val bit = dut.io.out.peek().litValue
      val binary = bit & 1
      print(binary)
      dut.clock.step()
      bits |= (bit.toInt << i)
      
    }
    println("")
    bits
   
  }

  /* 
    n: number of input bits
    d: the input bits
    dut: design under test 
   */
  def writeBits(dut: CRC_hash, d: Long, n: Int) = {
    println(cf"writeBits: 0x$d%x\n")
    for (i <- (n - 1) to 0 by -1) {
      // val bit = ((d & (1 << i)) != 0)
      val bit = ((d & (1L << i)) >> i).asUInt
      println(s"i: ${i}")
      dut.io.in.poke(bit)
      val binary = ((d & 1L << i) >> i) & 1
      print(binary)
      
      
      dut.clock.step()
      println(s"\nthe register values: ${dut.io.debug.peek()}\n")
    }
    println("")
  }


  it should "calculate the CRC_hash" in {
    
    // val rocket_model = CRCRoCC()
    test(new CRC_hash(5, 0x25)) { dut =>
      println("\n\n---------------------------- changed read sequence -----------------------")
      dut.io.en.poke(true)
      writeBits(dut, 0xab, 8)

      val result1Debug = dut.io.debug.peek()
      dut.io.en.poke(false)
      
      val result1 = readBits(dut, 5)
      
      println(cf"The crc readbits is: 0x$result1%x")
      /* 0x68d2c5 */
      println(cf"The crc debug is: $result1Debug")

    }
  }


  it should "calculate the check crc_hash" in {
    
    /* hash poly from : https://users.ece.cmu.edu/~koopman/crc/crc44.html */
    test(new CRC_hash(44, 0x100000000065L)) { dut =>
      println("\n\n---------------------------- large crc -----------------------")
      dut.io.en.poke(true)
      writeBits(dut, 0x100000000000aL, 52)
      val result1Debug = dut.io.debug.peek()
      dut.io.en.poke(false)
      val result1 = readBits(dut, 44)
      
      println(cf"The crc readbits is: 0x$result1%x")
      println(cf"The crc debug is: $result1Debug")

    }
  }

  
  
  // it should "calculate the CRC_hash" in {
  //   println("\n\n---------------------------- changed read sequence -----------------------")
  //   // val rocket_model = CRCRoCC()
  //   test(new CRC_hash(16, 0xa2eb)) { dut =>
    
  //     dut.io.en.poke(true)
  //     writeBits(dut, 0x666F6F, 24)

  //     val result1Debug = dut.io.debug.peek()
  //     dut.io.en.poke(false)
      
  //     val result1 = readBits(dut, 16)
      
  //     println(cf"The crc readbits is: $result1%x")
  //     /* 0x68d2c5 */
  //     println(cf"The crc debug is: $result1Debug%x")


  //     // dut.io.en.poke(true)
  //     // writeBits(dut, 0x6F6F66, 24)
  //     // // dut.io.debug.expect(0xaf96)
      
  //     // dut.io.en.poke(false)
  //     // val result2Debug = dut.io.debug.peek()
  //     // val result2 = readBits(dut, 16)
      
  //     // println(cf"The crc converted is: $result2%x")
  //     // println(cf"The crc debug is: $result2Debug%x")

  //   }
  // }

  // it should "calculate the CRC again" in {
  //   println("-------------------- second check ----------------")
  //   test(new CRC_hash(16, 0x1021)) { dut =>
  //     dut.io.en.poke(true)
  //     writeBits(dut, 0x666F6F, 24)
  //     dut.io.debug.expect(0x68d2c5)
  //     dut.io.en.poke(false)
  //     readBits(dut, 16) shouldBe 0x68d2c5
  //   }
  // }

  // it should "check the CRC" in {
  //   test(new CRC(16, 0x1021)) { dut =>
  //     dut.io.en.poke(true)
  //     // writeString(dut, "foo")
  //     writeBits(dut, 0xaf96, 16)
  //     dut.io.debug.expect(0)
  //     dut.io.en.poke(false)
  //     readBits(dut, 16) shouldBe 0
  //   }
  // }
}
