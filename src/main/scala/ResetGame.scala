import chisel3._
import chisel3.util._

class ResetGame extends Module {
  val io = IO(new Bundle {
    val btnC = Input(Bool())

    val hasShell = Input(Bool())
    val hasShroom = Input(Bool())

    val resetGame = Output(Bool())
  })

  val btnPrev = RegNext(io.btnC, false.B)

  val btnPressed = io.btnC && !btnPrev

  io.resetGame :=
    btnPressed &&
    !io.hasShell &&
    !io.hasShroom
}