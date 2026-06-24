//////////////////////////////////////////////////////////////////////////////
// Authors: Luca Pezzarossa
// Copyright: Technical University of Denmark - 2025
// Comments:
// The tester for the game logic block.
//////////////////////////////////////////////////////////////////////////////

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec



class GISDTester extends AnyFlatSpec with ChiselScalatestTester {

  def stepGisd(dut: GameInputStartupDelay, reps: Int = 1) = {
    for(i <- 1 to reps) {
      dut.io.updateCounter.poke(true.B)
      dut.clock.step(1)
      dut.io.updateCounter.poke(false.B)
      dut.clock.step(1)
    }
  }

  "GISDTester" should "pass" in {
    test(new GameInputStartupDelay) { dut =>
      println("Running the Game Input Startup Delay Tester")
      dut.io.anyInput.poke(true.B)
      stepGisd(dut, 15)

      stepGisd(dut, 1)
      dut.io.anyInput.poke(false.B)
      stepGisd(dut, 5)
      dut.io.waitForButtonsReleased.poke(true.B)
      stepGisd(dut, 1)
      dut.io.waitForButtonsReleased.poke(false.B)
      stepGisd(dut, 1)
      dut.io.anyInput.poke(true.B)
      stepGisd(dut, 1)
      dut.io.anyInput.poke(false.B)
      stepGisd(dut, 5)
    }
  }
}

//////////////////////////////////////////////////////////////////////////////
// End of file
//////////////////////////////////////////////////////////////////////////////
