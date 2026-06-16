import chisel3._
import chisel3.util._

class LapCounterDisplay extends Module {
  val io = IO(new Bundle {
    val lap1 = Input(Bool())
    val lap2 = Input(Bool())
    val lap3 = Input(Bool())

    val show1 = Output(Bool())
    val show2 = Output(Bool())
    val show3 = Output(Bool())
  })

  io.show1 := false.B
  io.show2 := false.B
  io.show3 := false.B

  when(!io.lap1) {
    // 1 / 3
    io.show1 := true.B
  }.elsewhen(!io.lap2) {
    // 2 / 3
    io.show2 := true.B
  }.otherwise {
    // 3 / 3
    io.show3 := true.B
  }
}