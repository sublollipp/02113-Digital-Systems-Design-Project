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
  // Checkpoint for goal-line

  val checkpointTaken = RegInit(false.B)
  val lapCounter = RegInit(0.U(3.W))

  val checkpointArea =
    io.carX >= 0.S &&
    io.carX <= 1200.S &&
    io.carY >= 0.S &&
    io.carY <= 900.S

  val checkpointOnEdge = checkpointArea && !RegNext(checkpointArea, false.B)

  when(checkpointOnEdge) {
    checkpointTaken := true.B
  }

  // Goal-Line

  val finishLine =
    io.carX >= 96.S &&
    io.carX <= 224.S &&
    io.carY >= 224.S &&
    io.carY <= 240.S

  val finishLineOnEdge = finishLine && !RegNext(finishLine, false.B)

  when(finishLineOnEdge) {
    when(checkpointTaken) {
      when(lapCounter =/= 3.U) {
        lapCounter := lapCounter + 1.U
      }
      checkpointTaken := false.B
    }
  }

  io.gameWon := (lapCounter === 3.U)

  io.checkpointHit := checkpointTaken
  io.finishHit := finishLine

  io.lap1 := lapCounter >= 1.U
  io.lap2 := lapCounter >= 2.U
  io.lap3 := lapCounter >= 3.U
}