import chisel3.util._
import chisel3._
import chisel3.util.random.LFSR

class RNG(options: Int) extends Module {
  val width = log2Ceil(options)
  val io = IO(new Bundle {
    val idx = Output(UInt(width.W))
  })

  val random = LFSR(width)
  val index = random % options.U

  io.idx := index
}