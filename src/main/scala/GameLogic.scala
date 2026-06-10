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

    val spriteXPosition = Output(Vec(SpriteNumber, SInt(11.W)))
    val spriteYPosition = Output(Vec(SpriteNumber, SInt(10.W)))
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

  val desiredCameraX = car.io.posX - 304.S
  val desiredCameraY = car.io.posY - 224.S

  when(desiredCameraX < 0.S) {
    cameraX := 0.S
  }.elsewhen(desiredCameraX > 640.S) {
    cameraX := 640.S
  }.otherwise {
    cameraX := desiredCameraX
  }

  when(desiredCameraY < 0.S) {
    cameraY := 0.S
  }.elsewhen(desiredCameraY > 480.S) {
    cameraY := 480.S
  }.otherwise {
    cameraY := desiredCameraY
  }

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
      stateReg := done
    }

    is(done) {
      io.frameUpdateDone := true.B
      stateReg := idle
    }
  }
}