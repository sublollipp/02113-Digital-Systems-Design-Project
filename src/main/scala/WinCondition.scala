import chisel3._
import chisel3.util._

class WinCondition extends Module {
  val io = IO(new Bundle {
    val carX = Input(SInt(12.W))
    val carY = Input(SInt(11.W))

    val gameWon = Output(Bool())
  })

  // Målområde (ret selv koordinaterne)
  val finishLine =
    io.carX >= 96.S &&
    io.carX <= 224.S &&
    io.carY >= 224.S &&
    io.carY <= 240.S

  io.gameWon := finishLine
}