import chisel3._
import chisel3.util._

class Lake extends Module {
  val io = IO(new Bundle {
    val x = Input(SInt(12.W))
    val y = Input(SInt(11.W))
    val inLake = Output(Bool())
  })


  val withinX = io.x >= 272.S && io.x <= 384.S
  val belowBottom = io.y <= 512.S
  val aboveTopEdge = (io.y * 3.S) >= (io.x + 800.S)

  io.inLake := withinX && belowBottom && aboveTopEdge
}
