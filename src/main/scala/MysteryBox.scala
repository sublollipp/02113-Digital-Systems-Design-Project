import chisel3._
import chisel3.util._

// Spawning the mystery box random places around the track
class MysteryBox extends Module {
  val io = IO(new Bundle {
    val rngUpdate = Input(Bool())
    val hit = Input(Bool())
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
  val hitReg = RegInit(true.B)

  when(io.hit && !hitReg) {
    hitReg := true.B
    xPosReg := 2000.S // Removes opened box from screen
  }

  val hitboxOffsetX = 4.S(12.W)
  val hitboxOffsetY = 4.S(11.W)
  val hitboxWidthValue = 24.U(6.W)
  val hitboxHeightValue = 24.U(6.W)

  val clockDivReg = RegInit(600.U(10.W))

  val rng = Module(new RNG(5))

  rng.io.frameUpdate := io.rngUpdate

  when (io.frameUpdate) {
    when (hitReg) {
     clockDivReg := clockDivReg + 1.U
    }
    when (clockDivReg >= 600.U) {
      clockDivReg := 0.U
      hitReg := false.B
      when (rng.io.randomVal === 0.U) {
        xPosReg := 288.S
        yPosReg := 160.S
      }.elsewhen (rng.io.randomVal === 1.U) {
        xPosReg := 768.S
        yPosReg := 160.S
      }.elsewhen (rng.io.randomVal === 2.U) {
        xPosReg := 1408.S
        yPosReg := 384.S
      }.elsewhen (rng.io.randomVal === 3.U) {
        xPosReg := 640.S
        yPosReg := 800.S
      }.otherwise {
        xPosReg := 160.S
        yPosReg := 460.S
      }
    }
  }

  io.posX := xPosReg
  io.posY := yPosReg
  io.hitboxX := xPosReg + hitboxOffsetX
  io.hitboxY := yPosReg + hitboxOffsetY
  io.hitboxWidth := hitboxWidthValue
  io.hitboxHeight := hitboxHeightValue
  io.shownSprite := !hitReg
}