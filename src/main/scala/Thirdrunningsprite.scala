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

  val startX = 64.S(12.W)
  val targetX = 928.S(12.W) // run right until x = 928

  val xPosReg = RegInit(startX)
  val yPosReg = RegInit(864.S(11.W))
  val movingRight = RegInit(true.B)
  val hitReg = RegInit(false.B)
  val hitboxGone = RegInit(false.B)
  val hiddenReg = RegInit(false.B)

  when(io.update) {
    when(io.hit && !hitReg && !hiddenReg) {
      hitReg := true.B
      hitboxGone := true.B
    }

    when(!hiddenReg) {
      when(hitboxGone) {
        when(yPosReg > (-32).S(11.W)) {
          yPosReg := yPosReg - 1.S
        }.otherwise {
          hiddenReg := true.B
        }
      }.otherwise {
        when(movingRight) {
          when(xPosReg < targetX) {
            xPosReg := xPosReg + 1.S
          }.otherwise {
            movingRight := false.B
          }
        }.otherwise {
          when(xPosReg > startX) {
            xPosReg := xPosReg - 1.S
          }.otherwise {
            movingRight := true.B
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

  io.flipH := !movingRight
  io.flipV := false.B

  io.shownSprite := Mux(hiddenReg,
    VecInit(false.B, false.B, false.B, false.B),
    Mux(hitReg,
      VecInit(false.B, false.B, false.B, true.B),
      VecInit(false.B, false.B, true.B, false.B)
    )
  )
}
