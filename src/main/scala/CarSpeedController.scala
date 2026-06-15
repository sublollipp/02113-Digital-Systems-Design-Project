import chisel3._
import chisel3.util._

class CarSpeedController(framesPerAcceleration: Int, accelerationMultiplier: Int, maxSpeed: Int, minSpeed: Int, offRoadMaxSpeed: Int, frictionCoef: Int) extends Module {
  val io = IO(new Bundle {
    val btnFwd = Input(Bool())
    val btnBckwd = Input(Bool())
    val frameUpdate = Input(Bool())
    val offRoad = Input(Bool())
    val speed = Output(SInt(10.W))
  })

  val speed = RegInit(0.S(10.W))

  val idle :: accel :: Nil = Enum(2)

  val state = RegInit(idle)

  val clockDivCounter = RegInit(0.U(6.W))

  switch (state) {
    is (idle) {
      when(io.frameUpdate) {
        clockDivCounter := clockDivCounter + 1.U
        when(clockDivCounter === framesPerAcceleration.U) {
          state := accel
        }
      }
    }
    is (accel) {
      state := idle
      clockDivCounter := 0.U
      when(io.btnFwd && !io.btnBckwd) {
        when ((speed < maxSpeed.S && !io.offRoad) || (speed < offRoadMaxSpeed.S && io.offRoad)) {
          speed := speed + 1.S * accelerationMultiplier.S
        }.otherwise {
          speed := Mux(io.offRoad, offRoadMaxSpeed.S, maxSpeed.S)
        }
      }.elsewhen(!io.btnFwd && io.btnBckwd) {
        when (speed <= minSpeed.S) {
          speed := minSpeed.S
        }.otherwise {
          speed := speed - (2.S * frictionCoef.S)
        }
      }.otherwise {
        when (speed < 0.S) {
          when (speed + frictionCoef.S > 0.S) {
            speed := 0.S
          }.otherwise {
            speed := speed + (1.S * frictionCoef.S)
          }
        }.elsewhen (speed > 0.S) {
          when (speed - frictionCoef.S < 0.S) {
            speed := 0.S
          }.otherwise {
            speed := speed - (1.S * frictionCoef.S)
          }
        }.otherwise {
          speed := 0.S
        }
      }
    }
  }

  io.speed := speed
}
