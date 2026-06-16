import chisel3._
import chisel3.util._

class CarSpeedController(framesPerAcceleration: Int, accelerationMultiplier: Int, maxSpeed: Int, minSpeed: Int, offRoadMaxSpeed: Int, frictionCoef: Int) extends Module {
  val io = IO(new Bundle {
    val btnFwd = Input(Bool())
    val btnBckwd = Input(Bool())
    val frameUpdate = Input(Bool())
    val offRoad = Input(Bool())
    val boost = Input(Bool())
    val boostSpeed = Input(SInt(11.W))
    val boostFrames = Input(UInt(10.W))
    val speed = Output(SInt(11.W))
    val debugLed = Output(Bool())
  })

  val speed = RegInit(0.S(11.W))

  val idle :: accel :: boostInit :: boosting :: Nil = Enum(4)

  val state = RegInit(idle)

  val clockDivCounter = RegInit(0.U(6.W))

  val boostFrameCount = RegInit(0.U(8.W))
  val boostSpeed = RegInit(0.S(11.W))

  val debugLed = RegInit(false.B)

  io.debugLed := debugLed

  switch (state) {
    is (idle) {
      when(io.boost) {
        state := boostInit
      }.elsewhen(clockDivCounter === framesPerAcceleration.U) {
        state := accel
      }
      when(io.frameUpdate) {
        clockDivCounter := clockDivCounter + 1.U
      }
    }
    is (accel) {
      state := Mux(io.boost, boostInit, idle)
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
    is (boostInit) {
      debugLed := true.B
      boostSpeed := io.boostSpeed
      boostFrameCount := io.boostFrames
      state := boosting
    }
    is (boosting) {
      speed := boostSpeed
      when(io.frameUpdate) {
        boostFrameCount := boostFrameCount - 1.U
        when(io.boost) {
          state := boostInit
        }.elsewhen(boostFrameCount === 0.U) {
          state := idle
        }
      }
    }
  }

  io.speed := speed
}
