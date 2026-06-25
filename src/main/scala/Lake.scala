import chisel3._
import chisel3.util._

class Lake extends Module {
  val io = IO(new Bundle {
    val x = Input(SInt(12.W))
    val y = Input(SInt(11.W))
    val inLake = Output(Bool())
  })

  // Expanded by ~16 px so a 32x32 car collides when any side touches the lake wall.
  val withinX = io.x >= 272.S && io.x <= 400.S
  val belowBottom = io.y <= 496.S
  val aboveTopEdge = (io.y * 3.S) >= (io.x + 704.S)

  io.inLake := withinX && belowBottom && aboveTopEdge
}
