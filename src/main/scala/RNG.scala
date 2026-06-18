import chisel3._
import chisel3.util._

class RNG(options: Int) extends Module {
  val io = IO(new Bundle {
    val randomVal = Output(UInt(5.W))
    val frameUpdate = Input(Bool())
  })

  val randomReg = RegInit(0.U(log2Ceil(options).W))
  when (io.frameUpdate) {
    when(randomReg === options.U) {
      randomReg := 0.U
    }.otherwise {
      randomReg := randomReg + 1.U
    }
  }
  io.randomVal := randomReg
}