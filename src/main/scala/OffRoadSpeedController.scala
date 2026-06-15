import chisel3._
import chisel3.util._

class OffRoadSpeedController extends Module {
  val io = IO(new Bundle {
    val speedIn = Input(SInt(10.W))
    val onRoad = Input(Bool())
    val speedOut = Output(SInt(10.W))
  })

when(io.onRoad) {
  io.speedOut := io.speedIn
}.otherwise {
  when(io.speedIn > 125.S) {
    io.speedOut := 125.S
  }.elsewhen(io.speedIn < -40.S) {
    io.speedOut := -40.S
  }.otherwise {
    io.speedOut := io.speedIn
  }
}
}