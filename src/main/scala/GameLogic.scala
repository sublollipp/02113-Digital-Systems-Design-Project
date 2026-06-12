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

val desiredAngle = WireDefault(aiAngle)

 val checkpointX = VecInit(
  160.S, 160.S, 160.S, 160.S,
  320.S, 480.S, 640.S, 800.S, 960.S, 1056.S,
  1056.S, 1056.S, 1056.S,
  960.S, 912.S,
  912.S, 912.S, 912.S,
  800.S, 640.S, 480.S, 320.S, 160.S,
  160.S, 160.S
)

val checkpointY = VecInit(
  800.S, 600.S, 400.S, 193.S,
  193.S, 193.S, 193.S, 193.S, 193.S, 193.S,
  300.S, 400.S, 496.S,
  496.S, 496.S,
  600.S, 700.S, 800.S,
  800.S, 800.S, 800.S, 800.S, 800.S,
  700.S, 600.S
) 


val currentCheckpoint = RegInit(0.U(5.W))


  val lookAhead = Mux(currentCheckpoint >= 23.U, currentCheckpoint + 2.U - 25.U, currentCheckpoint + 2.U)

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
  aiVel.io.frameUpdate := io.newFrame

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

    // Beregn ønsket retning

    val absDx = Mux(dx < 0.S, -dx, dx)
    val absDy = Mux(dy < 0.S, -dy, dy)

    when(absDx > (absDy << 1)) {

      when(dx > 0.S) {
        desiredAngle := 0.U
      }.otherwise {
        desiredAngle := 32.U
      }

    }.elsewhen(absDy > (absDx << 1)) {

      when(dy > 0.S) {
        desiredAngle := 16.U
      }.otherwise {
        desiredAngle := 48.U
      }

    }.otherwise {

      when(dx > 0.S && dy > 0.S) {
        desiredAngle := 8.U
      }.elsewhen(dx < 0.S && dy > 0.S) {
        desiredAngle := 24.U
      }.elsewhen(dx < 0.S && dy < 0.S) {
        desiredAngle := 40.U
      }.otherwise {
        desiredAngle := 56.U
      }

    }

    // Drej gradvist mod målet

    aiAngle := desiredAngle

    // Accelerér

    when(aiSpeed < 250.S) {
      aiSpeed := aiSpeed + 4.S
    }

    // Flyt bilen
    aiX := aiVel.io.newXPos
    aiY := aiVel.io.newYPos

    // Skift waypoint

    when(
      (dx < 48.S && dx > (-48).S) &&
      (dy < 48.S && dy > (-48).S)
    ) {
      when(currentCheckpoint === 24.U) {
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