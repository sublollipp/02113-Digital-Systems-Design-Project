import chisel3._
import chisel3.util._

class RNG extends Module {
  val io = IO(new Bundle {
    val randomBit = Output(Bool())
  })

  val lfsr = RegInit("b10101101".U(8.W))

  val feedback = lfsr(7) ^ lfsr(5) ^ lfsr(4) ^ lfsr(3)

  lfsr := Cat(lfsr(6, 0), feedback)

  io.randomBit := lfsr(0)
}