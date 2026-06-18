import chisel3._
import chisel3.util._

class RaceTimer(clockFreq: Int = 100000000) extends Module {

  val io = IO(new Bundle {
    val start = Input(Bool())
    val stop  = Input(Bool())

    val digit0 = Output(UInt(4.W)) // sekunder 1'ere
    val digit1 = Output(UInt(4.W)) // sekunder 10'ere
    val digit2 = Output(UInt(4.W)) // minutter 1'ere
    val digit3 = Output(UInt(4.W)) // minutter 10'ere
  })

  val running = RegInit(false.B)

  when(io.start) {
    running := true.B
  }

  when(io.stop) {
    running := false.B
  }

  val tickCounter =
    RegInit(0.U(log2Ceil(clockFreq).W))

  val seconds = RegInit(0.U(6.W))  // 0-59
  val minutes = RegInit(0.U(7.W))

  when(running) {

    when(tickCounter === (clockFreq - 1).U) {

      tickCounter := 0.U

      when(seconds === 59.U) {
        seconds := 0.U
        minutes := minutes + 1.U
      }.otherwise {
        seconds := seconds + 1.U
      }

    }.otherwise {
      tickCounter := tickCounter + 1.U
    }
  }

  val secOnes = seconds % 10.U
  val secTens = seconds / 10.U

  val minOnes = minutes % 10.U
  val minTens = (minutes / 10.U) % 10.U

  io.digit0 := secOnes
  io.digit1 := secTens

  io.digit2 := minOnes
  io.digit3 := minTens
}