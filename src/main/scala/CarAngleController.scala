import chisel3._
import chisel3.util._

// Controlling the angle of the player-controlled car
class CarAngleController(framesPerAngleChange: Int) extends Module{
  val io = IO(new Bundle {
    val btnLeft = Input(Bool())
    val btnRight = Input(Bool())
    val frameUpdate = Input(Bool())
    val angle = Output(UInt(6.W))
    val updateDone = Output(Bool())
  })

  val angle = RegInit(48.U(6.W))

  val clockDivReg = RegInit(0.U(6.W))

  val idle :: changeAngle :: Nil = Enum(2)
  val state = RegInit(idle)

  io.updateDone := state === idle

  switch (state) {
    is (idle) {
      when (io.frameUpdate) {
        clockDivReg := clockDivReg + 1.U
      }
      when (clockDivReg === framesPerAngleChange.U) {
        state := changeAngle
      }
    }
    is (changeAngle) {
      state := idle
      clockDivReg := 0.U
      when(io.btnLeft && !io.btnRight) {
        angle := angle - 1.U
      }.elsewhen(!io.btnLeft && io.btnRight) {
        angle := angle + 1.U
      }
    }
  }
  io.angle := angle
}
