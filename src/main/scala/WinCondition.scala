import chisel3._
import chisel3.util._

class WinCondition extends Module {
  val io = IO(new Bundle {
    val carX = Input(SInt(12.W))
    val carY = Input(SInt(11.W))

    val gameWon = Output(Bool())

    // Debug
    val checkpointHit = Output(Bool())
    val finishHit = Output(Bool())
    val lap1 = Output(Bool())
    val lap2 = Output(Bool())
    val lap3 = Output(Bool())
  })

  // -----------------------------
  // Registers
  // -----------------------------

  val checkpointTaken = RegInit(false.B)
  val lapCounter = RegInit(0.U(3.W))

  // -----------------------------
  // Checkpoint zone
  // -----------------------------

  val checkpointArea =
    io.carX >= 600.S &&
    io.carX <= 1200.S &&
    io.carY >= 600.S &&
    io.carY <= 900.S

  // -----------------------------
  // Finish line
  // -----------------------------

  val finishLine =
    io.carX >= 96.S &&
    io.carX <= 224.S &&
    io.carY >= 224.S &&
    io.carY <= 240.S

  // -----------------------------
  // Edge detection
  // -----------------------------

  val checkpointPrev = RegNext(checkpointArea, false.B)
  val finishPrev     = RegNext(finishLine, false.B)

  val checkpointEnter = checkpointArea && !checkpointPrev
  val finishEnter     = finishLine && !finishPrev

  // -----------------------------
  // Checkpoint logic
  // -----------------------------

  when(checkpointEnter) {
    checkpointTaken := true.B
  }

  // -----------------------------
  // Lap counting
  // -----------------------------

  when(finishEnter && checkpointTaken) {
    lapCounter := lapCounter + 1.U
    checkpointTaken := false.B
  }

  // -----------------------------
  // Win condition
  // -----------------------------

  io.gameWon := lapCounter >= 3.U

  // -----------------------------
  // Debug outputs
  // -----------------------------

  io.checkpointHit := checkpointArea
  io.finishHit := finishLine

  io.lap1 := lapCounter >= 1.U
  io.lap2 := lapCounter >= 2.U
  io.lap3 := lapCounter >= 3.U
}