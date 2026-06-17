import chisel3._
import chisel3.util._

class CarAngleController(framesPerAngleChange: Int) extends Module{
  val io = IO(new Bundle {
    val btnLeft = Input(Bool())
    val btnRight = Input(Bool())
    val frameUpdate = Input(Bool())
    val angle = Output(UInt(6.W))
  })

  val angle = RegInit(48.U(6.W))

  val clockDivReg = RegInit(0.U(6.W))

  val idle :: changeAngle :: Nil = Enum(2)
  val state = RegInit(idle)

  when (io.frameUpdate) {
    switch (state) {
      is (idle) {
        clockDivReg := clockDivReg + 1.U
        when (clockDivReg === framesPerAngleChange.U) {
          state := changeAngle
        }
      }
      is (changeAngle) {
        state := idle
        clockDivReg := 0.U
        when (io.btnLeft && !io.btnRight) {
          angle := angle - 1.U
        }.elsewhen(!io.btnLeft && io.btnRight) {
          angle := angle + 1.U
        }
      }
    }
  }
  io.angle := angle
}
