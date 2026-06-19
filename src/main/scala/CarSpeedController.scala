import chisel3._
import chisel3.util._

class CarSpeedController(framesPerAcceleration: Int, accelerationMultiplier: Int, maxSpeed: Int, minSpeed: Int, offRoadMaxSpeed: Int, frictionCoef: Int) extends Module {
  val io = IO(new Bundle {
    val btnFwd = Input(Bool())
    val btnBckwd = Input(Bool())
    val frameUpdate = Input(Bool())
    val resetSpeed = Input(Bool())
    val offRoad = Input(Bool())
    val shroomBoost = Input(Bool())
    val colBoost = Input(Bool())
    val boostSpeed = Input(SInt(11.W))
    val boostFrames = Input(UInt(10.W))
    val speed = Output(SInt(11.W))
    val debugLed = Output(Bool())
    val updateDone = Output(Bool())
  })

  val boost = io.colBoost || io.shroomBoost

  val speed = RegInit(0.S(11.W))

  val idle :: accel :: boostInit :: boosting :: collided :: colInit :: Nil = Enum(6)

  val state = RegInit(idle)

    when(io.resetSpeed) {
    speed := 0.S
    state := idle
  }
  
  val clockDivCounter = RegInit(0.U(6.W))

  val boostFrameCount = RegInit(0.U(8.W))
  val boostSpeed = RegInit(0.S(11.W))

  val debugLed = RegInit(false.B)

  io.debugLed := debugLed

  val boostLogicDone = WireDefault(false.B)

  io.updateDone := boostLogicDone

  switch (state) {
    is (idle) {
      boostLogicDone := true.B
      when (io.shroomBoost) {
        state := boosting
      }.elsewhen(io.colBoost) {
        state := collided
      }.elsewhen(clockDivCounter === framesPerAcceleration.U) {
        state := accel
      }
      when(io.frameUpdate) {
        clockDivCounter := clockDivCounter + 1.U
      }
      boostSpeed := io.boostSpeed
      boostFrameCount := io.boostFrames
    }
    is (accel) {
      boostSpeed := io.boostSpeed
      boostFrameCount := io.boostFrames
      when (io.shroomBoost) {
        state := boosting
      }.elsewhen(io.colBoost) {
        state := collided
      }.otherwise {
        state := idle
      }
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
      boostSpeed := io.boostSpeed
      boostFrameCount := io.boostFrames
      state := boosting
    }
    is (boosting) {
      speed := boostSpeed
      boostLogicDone := true.B
      when(io.frameUpdate) {
        boostFrameCount := boostFrameCount - 1.U
        when(io.shroomBoost) {
          state := boostInit
        }.elsewhen(boostFrameCount === 0.U) {
          state := idle
        }
      }
    }
    is (colInit) {
      debugLed := true.B
      boostSpeed := io.boostSpeed
      boostFrameCount := io.boostFrames
      state := boosting
    }
    is (collided) {
      speed := boostSpeed
      boostLogicDone := true.B
      when(io.frameUpdate) {
        boostFrameCount := boostFrameCount - 1.U
        when(io.shroomBoost) {
          state := boostInit
        }.elsewhen(io.colBoost) {
          state := colInit
        }.elsewhen(boostFrameCount === 0.U) {
          state := idle
        }
      }
    }
  }

  io.speed := speed
}
