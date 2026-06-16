import chisel3._
import chisel3.util._

class Pocket extends Module {
  val io = IO(new Bundle {
    val useBtn = Input(Bool())
    val frameUpdate = Input(Bool())
    val carPosX = Input(SInt(12.W))
    val carPosY = Input(SInt(11.W))
    val carAngle = Input(UInt(6.W))
    val hitMysteryBox = Input(Bool())
  })

  val none :: shell :: shroom :: Nil = Enum(3)

  val item = RegInit(none)



}
