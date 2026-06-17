import chisel3._
import chisel3.util._

class RNG extends Module {
  val io = IO(new Bundle {
    val output = Output(Vec(4, Bool()))
  })

  val oneHotShifter = RegInit(VecInit(false.B, false.B, false.B, true.B))

  oneHotShifter := VecInit(
    oneHotShifter(3),
    oneHotShifter(0),
    oneHotShifter(1),
    oneHotShifter(2)
  )

  io.output := oneHotShifter
}