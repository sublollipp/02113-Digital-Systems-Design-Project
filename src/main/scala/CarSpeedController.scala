import chisel3._
import chisel3.util._

class CarSpeedController(framesPerAcceleration: UInt, maxSpeed: SInt, minSpeed: SInt) extends Module {
  val io = IO(new Bundle {
    val btnFwd = Input(Bool())
    val btnBckwd = Input(Bool())
    val frameUpdate = Input(Bool())
    val speed = Output(SInt(9.W))
  })

  val speed = RegInit(0.S(9.W))

  val idle :: accel :: Nil = Enum(2)

  val state = RegInit(idle)

  val clockDivCounter = RegInit(0.U((log2Ceil(60)).W))

  when (io.frameUpdate) {
    switch (state) {
      is (idle) {
        clockDivCounter := clockDivCounter + 1.U
        when (clockDivCounter === framesPerAcceleration) {
          state := accel
        }
      }
      is (accel) {
        state := idle
        clockDivCounter := 0.U
        when(io.btnFwd && !io.btnBckwd) {
          when (speed =/= maxSpeed) {
            speed := speed + 1.S
          }
        }.elsewhen(!io.btnFwd && io.btnBckwd) {
          when (speed <= minSpeed) {
            speed := minSpeed
          }.otherwise {
            speed := speed - 2.S
          }
        }.otherwise {
          when (speed <= minSpeed) {
            speed := minSpeed
          }.otherwise {
            speed := speed - 1.S
          }
        }
      }
    }
  }

  io.speed := speed
}
