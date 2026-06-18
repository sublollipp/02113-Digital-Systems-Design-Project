import chisel3._
import chisel3.util._

class RNG(options: Int) extends Module {
  val io = IO(new Bundle {
    val randomVal = Output(UInt(5.W))
    val frameUpdate = Input(Bool())
  })

  val randomReg = RegInit(0.U(1.W))
  val clockDivReg = RegInit(0.U(2.W))

  when (io.frameUpdate) {
    clockDivReg := clockDivReg + 1.U
    when (clockDivReg === 0.U) {
      when(randomReg === options.U - 1.U) {
        randomReg := 0.U
      }.otherwise {
        randomReg := randomReg + 1.U
      }
    }
  }
  io.randomVal := randomReg
}