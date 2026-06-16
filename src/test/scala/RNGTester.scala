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
    test(new RNG(4)) { dut =>
      println("Running the RNG Tester")
        dut.clock.setTimeout(0)
        val abc = new scala.util.Random
        val counts: Array[Int] = Array(0, 0, 0, 0)
        for (i <- 1 to 1000) {
          val clockDel = abc.nextInt(100)
          dut.clock.step(clockDel + 1)
          val chosenIdx = dut.io.peek().litValue.toInt
          if (chosenIdx >= 0 && chosenIdx < counts.length) {
            counts(chosenIdx) += 1
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
