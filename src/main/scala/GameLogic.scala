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


  // Game Logic

  val idle :: compute1 :: done :: Nil = Enum(3)
  val stateReg = RegInit(idle)

  val car = Module(new Car)

    // AI car position
  val aiX = RegInit(160.S(12.W))
  val aiY = RegInit(160.S(11.W))

  val checkpointX = VecInit(
    260.S(12.W),
    1340.S(12.W),
    1300.S(12.W),
    1160.S(12.W),
    1060.S(12.W),
    864.S(12.W),
    180.S(12.W),
    160.S(12.W)
  )

  val checkpointY = VecInit(
    195.S(11.W),
    165.S(11.W),
    435.S(11.W),
    450.S(11.W),
    735.S(11.W),
    768.S(11.W),
    705.S(11.W),
    448.S(11.W)
  )



  val currentCheckpoint = RegInit(0.U(3.W))


  val targetX = checkpointX(currentCheckpoint)
  val targetY = checkpointY(currentCheckpoint)

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

      val stepX = dx >> 4
      val stepY = dy >> 4

      when(stepX > 8.S) {
        aiX := aiX + 8.S
      }.elsewhen(stepX < (-8).S) {
        aiX := aiX - 8.S
      }.otherwise {
        aiX := aiX + stepX
      }

      when(stepY > 8.S) {
        aiY := aiY + 8.S
      }.elsewhen(stepY < (-8).S) {
        aiY := aiY - 8.S
      }.otherwise {
        aiY := aiY + stepY
      }

      when(
        (dx < 32.S && dx > (-32).S) &&
        (dy < 32.S && dy > (-32).S)
      ) {
        when(currentCheckpoint === 7.U) {
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

val runningSprite = Module(new RunningSprite)

io.spriteXPosition(3) := runningSprite.io.posX - cameraX
io.spriteYPosition(3) := runningSprite.io.posY - cameraY
io.spriteFlipHorizontal(3) := runningSprite.io.flipH
io.spriteFlipVertical(3) := runningSprite.io.flipV
io.spriteVisible(3) := runningSprite.io.shownSprite(2)
}