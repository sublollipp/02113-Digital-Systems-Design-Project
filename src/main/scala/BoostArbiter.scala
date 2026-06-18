import chisel3._
import chisel3.util._

class BoostArbiter extends Module {
  val io = IO(new Bundle {
    val shroomBoost = Input(Bool())
    val collisionBoost = Input(Bool())
    val boost = Output(Bool())
    val boostSpeed = Output(SInt(11.W))
    val boostFrames = Output(UInt(10.W))
  })

  val none :: shroom :: collision :: Nil = Enum(3)

  val boostType = RegInit(none)
  val boostFramesRemaining = RegInit(0.U(10.W))
}
