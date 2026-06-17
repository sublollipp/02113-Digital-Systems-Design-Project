import chisel3.util._
import chisel3._

class RNG extends Module {
  val io = IO(new Bundle {
    val output = Output(Vec(4, Bool()))
  })

  val oneHotShifter = RegInit(VecInit(false.B, false.B, false.B, true.B))
  val next = Wire(Vec(4, Bool()))

  next := VecInit(Seq.fill(4)(false.B))

  oneHotShifter(0) := oneHotShifter(3)
  oneHotShifter(1) := oneHotShifter(0)
  oneHotShifter(2) := oneHotShifter(1)
  oneHotShifter(3) := oneHotShifter(2)

  oneHotShifter := next
  io.output := oneHotShifter
}