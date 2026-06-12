import chisel3._
import chisel3.util._

class GameLogic(SpriteNumber: Int, BackTileNumber: Int) extends Module {
  val io = IO(new Bundle {
    val btnC = Input(Bool())
    val btnU = Input(Bool())
    val btnL = Input(Bool())
    val btnR = Input(Bool())
    val btnD = Input(Bool())

    val sw = Input(Vec(8, Bool()))
    val led = Output(Vec(8, Bool()))

    val spriteXPosition = Output(Vec(SpriteNumber, SInt(12.W)))
    val spriteYPosition = Output(Vec(SpriteNumber, SInt(11.W)))
    val spriteVisible = Output(Vec(SpriteNumber, Bool()))
    val spriteFlipHorizontal = Output(Vec(SpriteNumber, Bool()))
    val spriteFlipVertical = Output(Vec(SpriteNumber, Bool()))

    val viewBoxX = Output(UInt(11.W))
    val viewBoxY = Output(UInt(10.W))

    val backBufferWriteData = Output(UInt(log2Up(BackTileNumber).W))
    val backBufferWriteAddress = Output(UInt(11.W))
    val backBufferWriteEnable = Output(Bool())

    val newFrame = Input(Bool())
    val frameUpdateDone = Output(Bool())
  })

  io.led := Seq.fill(8)(false.B)

  io.spriteXPosition := Seq.fill(SpriteNumber)(0.S)
  io.spriteYPosition := Seq.fill(SpriteNumber)(0.S)
  io.spriteVisible := Seq.fill(SpriteNumber)(false.B)
  io.spriteFlipHorizontal := Seq.fill(SpriteNumber)(false.B)
  io.spriteFlipVertical := Seq.fill(SpriteNumber)(false.B)

  io.backBufferWriteData := 0.U
  io.backBufferWriteAddress := 0.U
  io.backBufferWriteEnable := false.B

  io.frameUpdateDone := false.B
  val frameUpdateReg = RegNext(io.newFrame, false.B)


  // Game Logic

  val idle :: compute1 :: done :: Nil = Enum(3)
  val stateReg = RegInit(idle)

  val car = Module(new Car)

    // AI car position

  val aiX = RegInit(160.S(12.W))
  val aiY = RegInit(800.S(11.W))

  val aiAngle = RegInit(48.U(6.W))
  val aiSpeed = RegInit(0.S(10.W))

  val racingOffset = RegInit(0.S(8.W))

val desiredAngle = WireDefault(aiAngle)

val checkpointX = VecInit(
  140.S, 140.S, 145.S, 150.S, 155.S, 160.S,

  180.S, 240.S, 320.S, 400.S, 480.S, 560.S,
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
  800.S, 720.S, 640.S, 560.S, 480.S, 400.S,

  280.S, 220.S, 160.S, 140.S, 140.S, 130.S,
  120.S, 120.S, 130.S, 140.S, 160.S, 185.S, 195.S,

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

  val targetX = checkpointX(lookAhead)
  val targetY = checkpointY(lookAhead)

  io.led(0) := aiX > 300.S

  car.io.btnLeft := io.btnL
  car.io.btnUp := io.btnU
  car.io.btnRight := io.btnR
  car.io.btnDown := io.btnD


  io.spriteFlipHorizontal(0) := car.io.flipH
  io.spriteFlipHorizontal(1) := car.io.flipH
  io.spriteFlipHorizontal(2) := car.io.flipH

  io.spriteFlipVertical(0) := car.io.flipV
  io.spriteFlipVertical(1) := car.io.flipV
  io.spriteFlipVertical(2) := car.io.flipV

  // Camera Follow with map bounds

  val cameraX = Wire(SInt(11.W))
  val cameraY = Wire(SInt(10.W))

  val camera = Module(new Camera)
  camera.io.carX := car.io.posX
  camera.io.carY := car.io.posY
  cameraX := camera.io.camX
  cameraY := camera.io.camY

  val aiVel = Module(new CarVelocityController)

  aiVel.io.oldXPos := aiX
  aiVel.io.oldYPos := aiY
  aiVel.io.ang := aiAngle
  aiVel.io.speed := aiSpeed
  aiVel.io.frameUpdate := frameUpdateReg

  io.viewBoxX := cameraX.asUInt
  io.viewBoxY := cameraY.asUInt

  io.spriteXPosition(0) := car.io.posX - cameraX
  io.spriteXPosition(1) := car.io.posX - cameraX
  io.spriteXPosition(2) := car.io.posX - cameraX

  io.spriteYPosition(0) := car.io.posY - cameraY
  io.spriteYPosition(1) := car.io.posY - cameraY
  io.spriteYPosition(2) := car.io.posY - cameraY

  io.spriteVisible(0) := car.io.shownSprite(0)
  io.spriteVisible(1) := car.io.shownSprite(1)
  io.spriteVisible(2) := car.io.shownSprite(2)

  

  // AI sprite

// AI sprite

  io.spriteXPosition(4) := aiX - cameraX
  io.spriteYPosition(4) := aiY - cameraY

  io.spriteXPosition(6) := aiX - cameraX
  io.spriteYPosition(6) := aiY - cameraY

  io.spriteXPosition(7) := aiX - cameraX
  io.spriteYPosition(7) := aiY - cameraY

  io.spriteVisible(4) := true.B
  io.spriteVisible(6) := false.B
  io.spriteVisible(7) := false.B

  io.spriteFlipHorizontal(3) := false.B
  io.spriteFlipHorizontal(4) := false.B
  io.spriteFlipHorizontal(5) := false.B

  io.spriteFlipVertical(3) := false.B
  io.spriteFlipVertical(4) := false.B
  io.spriteFlipVertical(5) := false.B

  car.io.update := false.B

  // FSM

  switch(stateReg) {
    is(idle) {
      when(io.newFrame) {
        stateReg := compute1
      }
    }

  is(compute1) {

    car.io.update := true.B

    val dx = targetX - aiX
    val dy = targetY - aiY



    val racingOffset = WireDefault(0.S(8.W))

    when(currentCheckpoint === 0.U) {
      racingOffset := 20.S
    }.elsewhen(currentCheckpoint === 6.U) {
      racingOffset := (-15).S
    }.elsewhen(currentCheckpoint === 12.U) {
      racingOffset := 25.S
    }.elsewhen(currentCheckpoint === 18.U) {
      racingOffset := (-20).S
    }

    val adjustedDx = (targetX + racingOffset) - aiX
    val adjustedDy = targetY - aiY

    val absDx = Mux(adjustedDx < 0.S, -adjustedDx, adjustedDx)
    val absDy = Mux(adjustedDy < 0.S, -adjustedDy, adjustedDy)

    when(absDx > (absDy << 1)) {

      when(adjustedDx > 0.S) {
        desiredAngle := 0.U
      }.otherwise {
        desiredAngle := 32.U
      }

    }.elsewhen(absDy > (absDx << 1)) {

      when(adjustedDy > 0.S) {
        desiredAngle := 16.U
      }.otherwise {
        desiredAngle := 48.U
      }

    }.otherwise {

      when(adjustedDx > 0.S && adjustedDy > 0.S) {
        desiredAngle := 8.U
      }.elsewhen(adjustedDx < 0.S && adjustedDy > 0.S) {
        desiredAngle := 24.U
      }.elsewhen(adjustedDx < 0.S && adjustedDy < 0.S) {
        desiredAngle := 40.U
      }.otherwise {
        desiredAngle := 56.U
      }

    }

    // Drej gradvist mod målet

    aiAngle := desiredAngle

    // Accelerér

    when(aiSpeed < 275.S) {
      aiSpeed := aiSpeed + 4.S
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

    stateReg := done
  }

    is(done) {
      io.frameUpdateDone := true.B
      stateReg := idle
    }
}

//runningsprite
val runningSprite = Module(new RunningSprite)

io.spriteXPosition(3) := runningSprite.io.posX - cameraX
io.spriteYPosition(3) := runningSprite.io.posY - cameraY
io.spriteFlipHorizontal(3) := runningSprite.io.flipH
io.spriteFlipVertical(3) := runningSprite.io.flipV
io.spriteVisible(3) := runningSprite.io.shownSprite(2)
runningSprite.io.update := frameUpdateReg
}