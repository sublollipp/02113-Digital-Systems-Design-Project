import chisel3._
import chisel3.util._

class WinCondition extends Module {
  val io = IO(new Bundle {
    val carX = Input(SInt(12.W))
    val carY = Input(SInt(11.W))

    val gameWon = Output(Bool())
  })

  // Checkpoint for goal-line

  val checkpointPassed = RegInit(false.B)
  val lapCounter = RegInit(0.U(3.W))

  val checkpointArea =
    io.carX >= 320.S &&
    io.carX <= 448.S &&
    io.carY >= 576.S &&
    io.carY <= 608.S

    val checkpointPrev = RegNext(checkpointArea, false.B)

    when(checkpointArea && !checkpointPrev) {
    checkpointPassed := true.B
    }

  // Goal-Line

  val finishLine =
    io.carX >= 96.S && 
    io.carX <= 224.S &&
    io.carY >= 224.S &&
    io.carY <= 240.S

    val finishLinePrev = RegNext(finishLine, false.B)

    when(finishLine && !finishLinePrev && checkpointPassed) {
    checkpointPassed := false.B

    when(lapCounter =/= 3.U) {
        lapCounter := lapCounter + 1.U
    }
    }
    io.gameWon := (lapCounter === 3.U)
}