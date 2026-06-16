import chisel3._
import chisel3.util._

class OffRoadSpeedController extends Module {
  val io = IO(new Bundle {
    val speedIn = Input(SInt(11.W))
    val onRoad = Input(Bool())
    val frameUpdate = Input(Bool())
    val speedOut = Output(SInt(11.W))
  })

  val speedReg = RegInit(0.S(11.W))

  when(io.frameUpdate) {

    when(io.onRoad) {
      speedReg := io.speedIn
    }.otherwise {

      when(io.speedIn > 0.S) {
        speedReg := io.speedIn - (io.speedIn >> 5).asSInt
      }.elsewhen(io.speedIn < 0.S) {
        speedReg := io.speedIn - (io.speedIn >> 5).asSInt
      }.otherwise {
        speedReg := 0.S
      }

    }
  }

  io.speedOut := speedReg
}