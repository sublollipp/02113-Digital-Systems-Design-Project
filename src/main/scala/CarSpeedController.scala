import chisel3._
import chisel3.util._

class CarSpeedController(framesPerAcceleration: UInt, accelerationMultiplier: SInt, maxSpeed: SInt, minSpeed: SInt, frictionCoef: SInt) extends Module {
  val io = IO(new Bundle {
    val btnFwd = Input(Bool())
    val btnBckwd = Input(Bool())
    val frameUpdate = Input(Bool())
    val speed = Output(SInt(10.W))
  })

  val speed = RegInit(0.S(10.W))

  val idle :: accel :: Nil = Enum(2)

  val state = RegInit(idle)

  val clockDivCounter = RegInit(0.U(6.W))

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
          when (speed < maxSpeed) {
            speed := speed + 1.S * accelerationMultiplier
          }.otherwise {
            speed := maxSpeed
          }
        }.elsewhen(!io.btnFwd && io.btnBckwd) {
          when (speed <= minSpeed) {
            speed := minSpeed
          }.otherwise {
            speed := speed - (2.S * frictionCoef)
          }
        }.otherwise {
          when (speed < 0.S) {
            when (speed + frictionCoef > 0.S) {
              speed := 0.S
            }.otherwise {
              speed := speed + (1.S * frictionCoef)
            }
          }.elsewhen (speed > 0.S) {
            when (speed - frictionCoef < 0.S) {
              speed := 0.S
            }.otherwise {
              speed := speed - (1.S * frictionCoef)
            }
          }.otherwise {
            speed := 0.S
          }
        }
      }
    }
  }

  io.speed := speed
}
