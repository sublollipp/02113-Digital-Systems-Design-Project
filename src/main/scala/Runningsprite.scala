import chisel3._
import chisel3.util._

class RunningSprite extends Module {
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

  val startX = 60.S(12.W)
  val targetX = (60 + 32 * 32).S(12.W) // 32 tiles right

  val xPosReg = RegInit(200.S(12.W))
  val yPosReg = RegInit(170.S(11.W))
  val movingRight = RegInit(true.B)
  val hitReg = RegInit(false.B)

  val stopped = io.hit || hitReg

  when(io.update) {
    when(io.hit) {
      hitReg := true.B
    }

    when(!stopped) {
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

  val hitboxOffsetX = 4.S(12.W)
  val hitboxOffsetY = 4.S(11.W)
  val hitboxWidthValue = Mux(hitReg, 0.U(6.W), 24.U(6.W))
  val hitboxHeightValue = Mux(hitReg, 0.U(6.W), 24.U(6.W))

  io.posX := xPosReg
  io.posY := yPosReg
  io.hitboxX := xPosReg + hitboxOffsetX
  io.hitboxY := yPosReg + hitboxOffsetY
  io.hitboxWidth := hitboxWidthValue
  io.hitboxHeight := hitboxHeightValue

  io.flipH := !movingRight
  io.flipV := false.B

  io.shownSprite := Mux(hitReg,
    VecInit(false.B, false.B, false.B, true.B),
    VecInit(false.B, false.B, true.B, false.B)
  )
}
