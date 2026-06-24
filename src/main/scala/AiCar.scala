import chisel3._
import chisel3.util._

class AiCar extends Module{
  val io = IO(new Bundle {
    val updateFrame = Input(Bool())
    val updateRNG = Input(Bool())
    val resetSpeed = Input(Bool())
    val posX = Output(SInt(12.W))
    val posY = Output(SInt(11.W))
    val flipH = Output(Bool())
    val flipV = Output(Bool())
    val spriteOH_UDR = Output(Vec(5, Bool()))
  })

  val route = RegInit(0.U(2.W))

  val checkpointX1 = VecInit(
    140.S, 140.S, 145.S, 150.S, 155.S, 160.S,

    170.S, 220.S, 320.S, 400.S, 480.S, 560.S,
    640.S, 720.S, 800.S, 880.S, 960.S, 1020.S, 1060.S,

    1080.S, 1085.S, 1090.S, 1090.S, 1090.S,

    1080.S, 1060.S, 1040.S, 1020.S, 1000.S,

    980.S, 960.S, 940.S, 920.S, 900.S,

    890.S, 885.S, 880.S, 875.S,

    860.S, 820.S, 760.S, 680.S, 600.S,
    520.S, 440.S, 360.S, 280.S, 220.S,

    180.S, 160.S, 145.S
  )

  val checkpointY1 = VecInit(
    800.S, 720.S, 580.S, 360.S, 240.S, 200.S,

    200.S, 180.S, 140.S, 130.S, 120.S, 110.S,
    100.S, 120.S, 130.S, 140.S, 160.S, 185.S, 195.S,

    240.S, 300.S, 360.S, 420.S, 480.S,

    500.S, 500.S, 500.S, 500.S, 500.S,

    520.S, 560.S, 620.S, 680.S, 740.S,

    790.S, 810.S, 825.S, 835.S,

    840.S, 840.S, 840.S, 840.S, 840.S,
    840.S, 840.S, 840.S, 835.S, 825.S,

    810.S, 800.S, 800.S
  )

  val checkpointX2 = VecInit(
  170.S, 170.S, 175.S, 180.S, 185.S, 190.S,

  200.S, 250.S, 350.S, 430.S, 510.S, 590.S,
  670.S, 750.S, 830.S, 910.S, 990.S, 1050.S, 1090.S,

  1110.S, 1115.S, 1120.S, 1120.S, 1120.S,

  1110.S, 1090.S, 1070.S, 1050.S, 1030.S,

  1010.S, 990.S, 970.S, 950.S, 930.S,

  920.S, 915.S, 910.S, 905.S,

  890.S, 850.S, 790.S, 710.S, 630.S,
  550.S, 470.S, 390.S, 310.S, 250.S,

  210.S, 190.S, 175.S
)

val checkpointY2 = VecInit(
    800.S, 720.S, 580.S, 360.S, 240.S, 200.S,

    200.S, 180.S, 140.S, 130.S, 120.S, 110.S,
    100.S, 120.S, 130.S, 140.S, 160.S, 185.S, 195.S,

    240.S, 300.S, 360.S, 420.S, 480.S,

    500.S, 500.S, 500.S, 500.S, 500.S,

    520.S, 560.S, 620.S, 680.S, 740.S,

    790.S, 810.S, 825.S, 835.S,

    840.S, 840.S, 840.S, 840.S, 840.S,
    840.S, 840.S, 840.S, 835.S, 825.S,

    810.S, 800.S, 800.S
  )

val checkpointX3 = VecInit(
  110.S, 110.S, 115.S, 120.S, 125.S, 130.S,

  140.S, 190.S, 290.S, 370.S, 450.S, 530.S,
  610.S, 690.S, 770.S, 850.S, 930.S, 990.S, 1030.S,

  1050.S, 1055.S, 1060.S, 1060.S, 1060.S,

  1050.S, 1030.S, 1010.S, 990.S, 970.S,

  950.S, 930.S, 910.S, 890.S, 870.S,

  860.S, 855.S, 850.S, 845.S,

  830.S, 790.S, 730.S, 650.S, 570.S,
  490.S, 410.S, 330.S, 250.S, 190.S,

  150.S, 130.S, 115.S
)

val checkpointY3 = VecInit(
    800.S, 720.S, 580.S, 360.S, 240.S, 200.S,

    200.S, 180.S, 140.S, 130.S, 120.S, 110.S,
    100.S, 120.S, 130.S, 140.S, 160.S, 185.S, 195.S,

    240.S, 300.S, 360.S, 420.S, 480.S,

    500.S, 500.S, 500.S, 500.S, 500.S,

    520.S, 560.S, 620.S, 680.S, 740.S,

    790.S, 810.S, 825.S, 835.S,

    840.S, 840.S, 840.S, 840.S, 840.S,
    840.S, 840.S, 840.S, 835.S, 825.S,

    810.S, 800.S, 800.S
  )

  val currentCheckpoint = RegInit(0.U(6.W))

  val aiRouteRng = Module(new RNG(3))

  val routeSelect = aiRouteRng.io.randomVal(1,0)

  aiRouteRng.io.frameUpdate := io.updateRNG

  val lookAhead = Mux(currentCheckpoint >= 47.U, currentCheckpoint + 4.U - 52.U, currentCheckpoint + 4.U)

  val aiX = RegInit(100.S(12.W))
  val aiY = RegInit(420.S(11.W))

  io.posX := aiX
  io.posY := aiY

  val aiAngle = RegInit(48.U(6.W))
  val aiSpeed = RegInit(0.S(10.W))

    when(io.resetSpeed) {
    aiSpeed := 0.S
  }

  val targetX = Wire(SInt(12.W))
  val targetY = Wire(SInt(11.W))

  targetX := checkpointX1(lookAhead)
  targetY := checkpointY1(lookAhead)

  switch(route) {
    is(0.U) {
      targetX := checkpointX1(lookAhead)
      targetY := checkpointY1(lookAhead)
    }
    is(1.U) {
      targetX := checkpointX2(lookAhead)
      targetY := checkpointY2(lookAhead)
    }
    is(2.U) {
      targetX := checkpointX3(lookAhead)
      targetY := checkpointY3(lookAhead)
    }
  }

  val racingOffset = WireDefault(0.S(12.W))

  val targetXReg = RegInit(0.S(12.W))
  val targetYReg = RegInit(0.S(11.W))

  targetXReg := targetX
  targetYReg := targetY

  val dx = targetXReg - aiX
  val dy = targetYReg - aiY

  val adjustedDx = (targetXReg + racingOffset) - aiX
  val adjustedDy = targetYReg - aiY


  val aiFlipH = WireDefault(false.B)
  val aiFlipV = WireDefault(false.B)

  // Compute a raw desired angle based on target direction, then apply
  // a 3-sample majority vote to avoid rapid sprite changes when turning.
  val newDesiredRaw = Wire(UInt(6.W))
  newDesiredRaw := aiAngle

  val aiVel = Module(new CarVelocityController)

  val absDx = Mux(adjustedDx < 0.S, -adjustedDx, adjustedDx)
  val absDy = Mux(adjustedDy < 0.S, -adjustedDy, adjustedDy)

  aiVel.io.oldXPos := aiX
  aiVel.io.oldYPos := aiY
  aiVel.io.ang := aiAngle
  aiVel.io.speed := aiSpeed
  aiVel.io.frameUpdate := io.updateFrame

  val spriteController = Module(new RotatingSpriteController)

  val desiredAngleReg = RegInit(48.U(6.W))

  when(io.updateFrame) {
    desiredAngleReg := newDesiredRaw
  }

  // History of last 3 requested angles
  val angleHist = RegInit(VecInit(Seq.fill(3)(48.U(6.W))))

  // Shift history on frame updateFrame so we sample once per frame
  when(io.updateFrame) {
    angleHist(2) := angleHist(1)
    angleHist(1) := angleHist(0)
    angleHist(0) := desiredAngleReg
  }

  // Majority vote among the three history entries. If no majority, keep previous angle.
  val votedAngle = Wire(UInt(6.W))
  when(angleHist(0) === angleHist(1) || angleHist(0) === angleHist(2)) {
    votedAngle := angleHist(0)
  }.elsewhen(angleHist(1) === angleHist(2)) {
    votedAngle := angleHist(1)
  }.otherwise {
    votedAngle := aiAngle
  }

  spriteController.io.angle := votedAngle
  io.flipV := spriteController.io.flipV
  io.flipH := spriteController.io.flipH

  io.spriteOH_UDR := spriteController.io.spriteOH_UDR

  when(currentCheckpoint === 0.U) {
    racingOffset := 20.S
  }.elsewhen(currentCheckpoint === 6.U) {
    racingOffset := (-15).S
  }.elsewhen(currentCheckpoint === 12.U) {
    racingOffset := 25.S
  }.elsewhen(currentCheckpoint === 18.U) {
    racingOffset := (-20).S
  }

  aiAngle := votedAngle

  // Determine raw desired angle based on direction to the target
  when(absDx > (absDy << 1).asSInt) {
    when(adjustedDx > 0.S) {
      newDesiredRaw := 0.U
    }.otherwise {
      newDesiredRaw := 32.U
    }
  }.elsewhen(absDy > (absDx << 1).asSInt) {
    when(adjustedDy > 0.S) {
      newDesiredRaw := 16.U
    }.otherwise {
      newDesiredRaw := 48.U
    }
  }.otherwise {
    when(adjustedDx > 0.S && adjustedDy > 0.S) {
      newDesiredRaw := 8.U
    }.elsewhen(adjustedDx < 0.S && adjustedDy > 0.S) {
      newDesiredRaw := 24.U
    }.elsewhen(adjustedDx < 0.S && adjustedDy < 0.S) {
      newDesiredRaw := 40.U
    }.otherwise {
      newDesiredRaw := 56.U
    }
  }

  // Acceleration and speed control

  when(io.updateFrame) {
    when(aiSpeed < 400.S) {
      aiSpeed := aiSpeed + 3.S
    }
  }

  // Move the AI car
  aiX := aiVel.io.newXPos
  aiY := aiVel.io.newYPos

  // Change waypoint 

  when(
    (adjustedDx < 48.S && adjustedDx > (-48).S) &&
      (adjustedDy < 48.S && adjustedDy > (-48).S)
  ) {
    when(currentCheckpoint === 51.U) {

      currentCheckpoint := 0.U

      route := routeSelect

    }.otherwise {
      currentCheckpoint := currentCheckpoint + 1.U
    }
  }
  }
