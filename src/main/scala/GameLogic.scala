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

    val viewBoxX = Output(UInt(10.W))
    val viewBoxY = Output(UInt(9.W))

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

  io.viewBoxX := 0.U
  io.viewBoxY := 0.U

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

  io.spriteXPosition(0) := car.io.posX
  io.spriteXPosition(1) := car.io.posX
  io.spriteXPosition(2) := car.io.posX

  io.spriteYPosition(0) := car.io.posY
  io.spriteYPosition(1) := car.io.posY
  io.spriteYPosition(2) := car.io.posY

  io.spriteVisible(0) := car.io.shownSprite(0)
  io.spriteVisible(1) := car.io.shownSprite(1)
  io.spriteVisible(2) := car.io.shownSprite(2)

  car.io.update := false.B

  // Camera Follow

  val WORLD_WIDTH  = 1280.S(12.W)
  val WORLD_HEIGHT = 960.S(11.W)

  val SCREEN_WIDTH  = 640.S(12.W)
  val SCREEN_HEIGHT = 480.S(11.W)

  val CAR_WIDTH  = 32.S(6.W)
  val CAR_HEIGHT = 32.S(6.W)

  val desiredViewX =
  car.io.posX - (SCREEN_WIDTH / 2.S) + 16.S

  val desiredViewY =
  car.io.posY - (SCREEN_HEIGHT / 2.S) + 16.S

  when(desiredViewX < 0.S) {
    io.viewBoxX := 0.U
  }.elsewhen(desiredViewX > (WORLD_WIDTH - SCREEN_WIDTH)) {
    io.viewBoxX := (WORLD_WIDTH - SCREEN_WIDTH).asUInt
  }.otherwise {
    io.viewBoxX := desiredViewX.asUInt
  }

  when(desiredViewY < 0.S) {
    io.viewBoxY := 0.U
  }.elsewhen(desiredViewY > (WORLD_HEIGHT - SCREEN_HEIGHT)) {
    io.viewBoxY := (WORLD_HEIGHT - SCREEN_HEIGHT).asUInt
  }.otherwise {
    io.viewBoxY := desiredViewY.asUInt
  }


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