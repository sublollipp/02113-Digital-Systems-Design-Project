import chisel3._
import chisel3.util._

class Lake extends Module {
  val io = IO(new Bundle {
    val x = Input(SInt(12.W))
    val y = Input(SInt(11.W))
    val inLake = Output(Bool())
  })

  io.inLake := io.x >= 240.S && io.x <= 368.S && io.y >= 378.S && io.y <= 494.S
}
