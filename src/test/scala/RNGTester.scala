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
    test(new RNG(5)) { dut =>
      println("Running the RNG Tester")
        dut.clock.setTimeout(0)
        val abc = new scala.util.Random
        val counts: Array[Int] = Array(0, 0, 0, 0, 0)
        for (i <- 1 to 10000) {
          val clockDel = abc.nextInt(100)
          for (q <- 0 to clockDel) {
            dut.io.frameUpdate.poke(true.B)
            dut.clock.step(1)

            dut.io.frameUpdate.poke(false.B)
            dut.clock.step(5)
          }
          val chosenIdx = dut.io.randomVal.peek().litValue.toInt
          if (chosenIdx >= 0) {
            counts(chosenIdx) += 1
          } else {
            print("AAAAAH DEN VAR:")
            print(chosenIdx)
          }
        }
      for(i <- 0 to 4) {
        println("Index " + i + " was chosen " + counts(i) + " times.")
      }


    }
  }
}

//////////////////////////////////////////////////////////////////////////////
// End of file
//////////////////////////////////////////////////////////////////////////////
