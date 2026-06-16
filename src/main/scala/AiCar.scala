import chisel3._
import chisel3.util._

class AiCar extends Module{
  val io = IO(new Bundle {
    val update = Input(Bool())
    val posX = Output(SInt(12.W))
    val posY = Output(SInt(11.W))
    val flipH = Output(Bool())
    val flipV = Output(Bool())
    val spriteOH_UDR = Output(Vec(3, Bool()))
  })

  val checkpointX = VecInit(
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

  val checkpointY = VecInit(
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


  val lookAhead = Mux(currentCheckpoint >= 47.U, currentCheckpoint + 4.U - 52.U, currentCheckpoint + 4.U)

  val aiX = RegInit(100.S(12.W))
  val aiY = RegInit(420.S(11.W))

  io.posX := aiX
  io.posY := aiY

  val aiAngle = RegInit(48.U(6.W))
  val aiSpeed = RegInit(0.S(10.W))

  val targetX = checkpointX(lookAhead)
  val targetY = checkpointY(lookAhead)

  val dx = targetX - aiX
  val dy = targetY - aiY

  val racingOffset = WireDefault(0.S(8.W))

  val aiFlipH = WireDefault(false.B)
  val aiFlipV = WireDefault(false.B)

  // Compute a raw desired angle based on target direction, then apply
  // a 3-sample majority vote to avoid rapid sprite changes when turning.
  val newDesiredRaw = Wire(UInt(6.W))
  newDesiredRaw := aiAngle

  val aiVel = Module(new CarVelocityController)

  val adjustedDx = (targetX + racingOffset) - aiX
  val adjustedDy = targetY - aiY

  val absDx = Mux(adjustedDx < 0.S, -adjustedDx, adjustedDx)
  val absDy = Mux(adjustedDy < 0.S, -adjustedDy, adjustedDy)

  aiVel.io.oldXPos := aiX
  aiVel.io.oldYPos := aiY
  aiVel.io.ang := aiAngle
  aiVel.io.speed := aiSpeed
  aiVel.io.frameUpdate := io.update

  val spriteController = Module(new RotatingSpriteController(Array(63, 0, 1, 15, 17, 31, 33, 47, 49)))

  // History of last 3 requested angles (most recent at index 0)
  val angleHist = RegInit(VecInit(Seq.fill(3)(48.U(6.W))))

  // Shift history on frame update so we sample once per frame
  when(io.update) {
    angleHist(2) := angleHist(1)
    angleHist(1) := angleHist(0)
    angleHist(0) := newDesiredRaw
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

  // Drej gradvist mod målet

  // Update aiAngle from votedAngle
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

  // Accelerér

  when(aiSpeed < 325.S) {
    aiSpeed := aiSpeed + 5.S
  }

  // Flyt bilen
  aiX := aiVel.io.newXPos
  aiY := aiVel.io.newYPos

  // Skift waypoint

  when(
    (adjustedDx < 48.S && adjustedDx > (-48).S) &&
      (adjustedDy < 48.S && adjustedDy > (-48).S)
  ) {
    when(currentCheckpoint === 51.U) {
      currentCheckpoint := 0.U
    }.otherwise {
      currentCheckpoint := currentCheckpoint + 1.U
    }
  }
}
