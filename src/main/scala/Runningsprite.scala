import chisel3._
import chisel3.util._

class RunningSprite extends Module {
  val io = IO(new Bundle {
    val update = Input(Bool())
    val posX = Output(SInt(12.W))
    val posY = Output(SInt(11.W))
    val flipH = Output(Bool())
    val flipV = Output(Bool())
    val shownSprite = Output(Vec(3, Bool()))
  })

  val startX = 60.S(12.W)
  val targetX = (60 + 32 * 32).S(12.W) // 32 tiles right

  val xPosReg = RegInit(startX)
  val yPosReg = RegInit(170.S(11.W))

  // Move right by 1 pixel each frame update
  when(io.update && xPosReg < targetX) {
    xPosReg := xPosReg + 1.S
  }

  io.posX := xPosReg
  io.posY := yPosReg

  io.flipH := false.B
  io.flipV := false.B

  io.shownSprite := VecInit(
    false.B,
    false.B,
    true.B
  )
}