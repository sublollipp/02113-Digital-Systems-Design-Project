//////////////////////////////////////////////////////////////////////////////
// Authors: Luca Pezzarossa
// Copyright: Technical University of Denmark - 2025
// Comments:
// The tester for the game logic block.
//////////////////////////////////////////////////////////////////////////////

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class RNGTester extends AnyFlatSpec with ChiselScalatestTester {

  "RNGTester" should "pass" in {
    test(new RNG) { dut =>
      println("Running the RNG Tester")
        dut.clock.setTimeout(0)
        val abc = new scala.util.Random
        val counts: Array[Int] = Array(0, 0, 0, 0)
        for (i <- 1 to 10000) {
          val clockDel = abc.nextInt(100)
          dut.clock.step(clockDel + 1)
          val chosenIdx = dut.io.peek().litValue.toInt
          if (chosenIdx >= 0) {
            counts(util.log2Ceil(chosenIdx).toInt) += 1
          } else {
            print("AAAAAH DEN VAR:")
            print(chosenIdx)
          }
        }
      for(i <- 0 to 3) {
        println("Index " + i + " was chosen " + counts(i) + " times.")
      }


    }
  }
}

//////////////////////////////////////////////////////////////////////////////
// End of file
//////////////////////////////////////////////////////////////////////////////
