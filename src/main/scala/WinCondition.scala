import chisel3._
import chisel3.util._

class WinCondition extends Module {
  val io = IO(new Bundle {
    val carX = Input(SInt(12.W))
    val carY = Input(SInt(11.W))

    val gameWon = Output(Bool())
    val checkpointHit = Output(Bool())
    val finishHit = Output(Bool())

    val lap1 = Output(Bool())
    val lap2 = Output(Bool())
    val lap3 = Output(Bool())
  })

  // Zones

  val checkpointArea =
    io.carX >= 832.S &&
    io.carX <= 1120.S &&
    io.carY >= 448.S &&
    io.carY <= 576.S

  val finishLine =
    io.carX >= 48.S &&
    io.carX <= 240.S &&
    io.carY >= 368.S &&
    io.carY <= 400.S

  // Edge detection

  val checkpointPrev = RegNext(checkpointArea, false.B)
  val finishPrev     = RegNext(finishLine, false.B)

  val checkpointEnter = checkpointArea && !checkpointPrev
  val finishEnter     = finishLine && !finishPrev

  // State machine

  val sArmRace :: sWaitCheckpoint :: Nil = Enum(2)

  val state = RegInit(sArmRace)

  // Registers

  val lapCounter = RegInit(0.U(3.W))
  val checkpointTaken = RegInit(false.B)

  // State transitions

  switch(state) {
    is(sArmRace) {
      when(finishEnter) {
        state := sWaitCheckpoint
      }
    }

    is(sWaitCheckpoint) {
      when(checkpointEnter) {
        state := sArmRace
      }

      when(finishEnter && checkpointPrev) {
        state := sWaitCheckpoint
      }
    }
  }

  // Lap counting

  when(checkpointEnter) {
    checkpointTaken := true.B
  }

  when(finishEnter && checkpointTaken) {
    checkpointTaken := false.B

    when(lapCounter < 3.U) {
      lapCounter := lapCounter + 1.U
    }
  }

  // Outputs

  io.gameWon := lapCounter >= 3.U

  io.checkpointHit := checkpointArea
  io.finishHit := finishLine

  io.lap1 := lapCounter >= 1.U
  io.lap2 := lapCounter >= 2.U
  io.lap3 := lapCounter >= 3.U
}