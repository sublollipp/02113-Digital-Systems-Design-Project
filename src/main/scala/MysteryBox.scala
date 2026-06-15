import chisel3._
import chisel3.util._

class MysteryBox extends Module {
  val io = IO(new Bundle {
    val box = Input(Bool())
    val hit = Input(Bool())
    val rand = Input(UInt(2.W))
    val posX = Output(SInt(12.W))
    val posY = Output(SInt(11.W))
    val hitboxX = Output(SInt(12.W))
    val hitboxY = Output(SInt(11.W))
    val hitboxWidth = Output(UInt(6.W))
    val hitboxHeight = Output(UInt(6.W))
    val shownSprite = Output(Bool())
  })

  val xPosReg = RegInit(160.S(12.W))
  val yPosReg = RegInit(420.S(11.W))
  val hitReg = RegInit(false.B)

  when(io.hit) {
    hitReg := true.B
  }

  val hitboxOffsetX = 4.S(12.W)
  val hitboxOffsetY = 4.S(11.W)
  val hitboxWidthValue = 24.U(6.W)
  val hitboxHeightValue = 24.U(6.W)

  io.posX := xPosReg
  io.posY := yPosReg
  io.hitboxX := xPosReg + hitboxOffsetX
  io.hitboxY := yPosReg + hitboxOffsetY
  io.hitboxWidth := hitboxWidthValue
  io.hitboxHeight := hitboxHeightValue
  io.shownSprite := !hitReg
}