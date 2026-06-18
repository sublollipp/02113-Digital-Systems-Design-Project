import chisel3._
import chisel3.util._

class MysteryBox extends Module {
  val io = IO(new Bundle {
    val box = Input(Bool())
    val hit = Input(Bool())
    val rand = Input(UInt(3.W))
    val frameUpdate = Input(Bool())
    val posX = Output(SInt(12.W))
    val posY = Output(SInt(11.W))
    val hitboxX = Output(SInt(12.W))
    val hitboxY = Output(SInt(11.W))
    val hitboxWidth = Output(UInt(6.W))
    val hitboxHeight = Output(UInt(6.W))
    val shownSprite = Output(Bool())
  })

  val xPosReg = RegInit(160.S(12.W))
  val yPosReg = RegInit(460.S(11.W))
  val hitReg = RegInit(false.B)

  when(io.hit) {
    hitReg := true.B
    xPosReg := 3000.S // Removes opened box from screen
  }

  val hitboxOffsetX = 4.S(12.W)
  val hitboxOffsetY = 4.S(11.W)
  val hitboxWidthValue = 24.U(6.W)
  val hitboxHeightValue = 24.U(6.W)

  val clockDivReg = RegInit(0.U(10.W))

  val rng = Module(new RNG(5))

  rng.io.frameUpdate := io.frameUpdate

  when (io.frameUpdate) {
    when (hitReg) {
      clockDivReg := clockDivReg + 1.U
    }
    when (clockDivReg === 600.U) {
      clockDivReg := 0.U
      hitReg := false.B
      when (rng.io.randomVal === 0.U) {
        xPosReg := RegInit(32.S(12.W))
        yPosReg := RegInit(32.S(11.W))
      } .elsewhen (rng.io.randomVal === 1.U) {
        xPosReg := RegInit(64.S(12.W))
        yPosReg := RegInit(64.S(11.W))
      } .elsewhen (rng.io.randomVal === 2.U) {
        xPosReg := RegInit(96.S(12.W))
        yPosReg := RegInit(96.S(11.W))
      } .elsewhen (rng.io.randomVal === 3.U) {
        xPosReg := RegInit(128.S(12.W))
        yPosReg := RegInit(128.S(11.W))
      } .otherwise {
        xPosReg := RegInit(160.S(12.W))
        yPosReg := RegInit(160.S(11.W))
      }
    }
  }

  io.posX := xPosReg
  io.posY := yPosReg
  io.hitboxX := xPosReg + hitboxOffsetX
  io.hitboxY := yPosReg + hitboxOffsetY
  io.hitboxWidth := hitboxWidthValue
  io.hitboxHeight := hitboxHeightValue
  io.shownSprite := (io.rand === 0.U) && !hitReg
}