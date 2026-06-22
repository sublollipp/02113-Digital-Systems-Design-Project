import chisel3._
import chisel3.util._

class ThirdrunningSprite extends Module {
  val io = IO(new Bundle {
    val update = Input(Bool())
    val hit = Input(Bool())
    val posX = Output(SInt(12.W))
    val posY = Output(SInt(11.W))
    val hitboxX = Output(SInt(12.W))
    val hitboxY = Output(SInt(11.W))
    val hitboxWidth = Output(UInt(6.W))
    val hitboxHeight = Output(UInt(6.W))
    val flipH = Output(Bool())
    val flipV = Output(Bool())
    val shownSprite = Output(Vec(4, Bool()))
  })

  val startX = 992.S(12.W)
  val startY = 256.S(11.W)
  val targetY = 384.S(11.W)

  val xPosReg = RegInit(startX)
  val yPosReg = RegInit(startY)
  val movingDown = RegInit(true.B)
  val hitReg = RegInit(false.B)
  val hitboxGone = RegInit(false.B)
  val hiddenReg = RegInit(false.B)
  val hitToggleCounter = RegInit(119.U(7.W))
  val hitSpriteToggle = RegInit(false.B)

  when(io.hit && !hitReg && !hiddenReg) {
    hitReg := true.B
    hitboxGone := true.B
    hitToggleCounter := 119.U
    hitSpriteToggle := false.B
  }

  when(io.update) {
    when(!hiddenReg) {
      when(hitReg) {
        when(hitToggleCounter === 0.U) {
          hitSpriteToggle := ~hitSpriteToggle
          hitToggleCounter := 119.U
        }.otherwise {
          hitToggleCounter := hitToggleCounter - 1.U
        }
      }

      when(!hitReg) {
        when(movingDown) {
          when(yPosReg < targetY) {
            yPosReg := yPosReg + 1.S
          }.otherwise {
            movingDown := false.B
          }
        }.otherwise {
          when(yPosReg > startY) {
            yPosReg := yPosReg - 1.S
          }.otherwise {
            movingDown := true.B
          }
        }
      }
    }
  }

  val hitboxOffsetX = 4.S(12.W)
  val hitboxOffsetY = 4.S(11.W)
  val hitboxWidthValue = Mux(hitboxGone, 0.U(6.W), 24.U(6.W)) // If the hitbox is gone, set width to 0, otherwise 24
  val hitboxHeightValue = Mux(hitboxGone, 0.U(6.W), 24.U(6.W)) // If the hitbox is gone, set height to 0, otherwise 24

  io.posX := xPosReg
  io.posY := yPosReg
  io.hitboxX := xPosReg + hitboxOffsetX
  io.hitboxY := yPosReg + hitboxOffsetY
  io.hitboxWidth := hitboxWidthValue
  io.hitboxHeight := hitboxHeightValue

  io.flipH := false.B
  io.flipV := false.B

  io.shownSprite := Mux(hiddenReg,
    VecInit(false.B, false.B, false.B, false.B),
    Mux(hitReg,
      Mux(hitSpriteToggle,
        VecInit(false.B, true.B, false.B, false.B),
        VecInit(true.B, false.B, false.B, false.B)
      ),
      VecInit(false.B, false.B, true.B, false.B)
    )
  )
}
