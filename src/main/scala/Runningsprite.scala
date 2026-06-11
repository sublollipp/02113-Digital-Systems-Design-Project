import chisel3._
import chisel3.util._

class RunningSprite extends Module {
  val io = IO(new Bundle {
    val posX = Output(SInt(12.W))
    val posY = Output(SInt(11.W))
    val flipH = Output(Bool())
    val flipV = Output(Bool())
    val shownSprite = Output(Vec(3, Bool()))
  })

  // Fixed position
  val xPosReg = RegInit(60.S(12.W))
  val yPosReg = RegInit(170.S(11.W))

  // Output position
  io.posX := xPosReg
  io.posY := yPosReg

  // No flipping
  io.flipH := false.B
  io.flipV := false.B

  // Select sprite_init_3.mem
  io.shownSprite := VecInit(
    false.B,  // sprite_init_1.mem
    false.B,  // sprite_init_2.mem
    true.B    // sprite_init_3.mem
  )
}