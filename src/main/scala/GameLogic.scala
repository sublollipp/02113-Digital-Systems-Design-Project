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

  val aiUpSprite :: aiDiagSprite :: aiRightSprite :: Nil = Enum(3)

  val aiSprite = WireDefault(aiUpSprite)

  // AI car position

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

  // AI-car

  val aiCar = Module(new AiCar)

  aiCar.io.update := false.B

  io.spriteXPosition(4) := aiCar.io.posX - cameraX
  io.spriteYPosition(4) := aiCar.io.posY - cameraY

  io.spriteXPosition(6) := aiCar.io.posX - cameraX
  io.spriteYPosition(6) := aiCar.io.posY - cameraY

  io.spriteXPosition(7) := aiCar.io.posX - cameraX
  io.spriteYPosition(7) := aiCar.io.posY - cameraY

  io.spriteVisible(4) := aiCar.io.spriteOH_UDR(0)
  io.spriteVisible(6) := aiCar.io.spriteOH_UDR(1)
  io.spriteVisible(7) := aiCar.io.spriteOH_UDR(2)

  io.spriteFlipHorizontal(4) := aiCar.io.flipH
  io.spriteFlipHorizontal(6) := aiCar.io.flipH
  io.spriteFlipHorizontal(7) := aiCar.io.flipH

  io.spriteFlipVertical(4) := aiCar.io.flipV
  io.spriteFlipVertical(6) := aiCar.io.flipV
  io.spriteFlipVertical(7) := aiCar.io.flipV

  // Viewbox
  io.viewBoxX := cameraX.asUInt
  io.viewBoxY := cameraY.asUInt

  // Car
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

  switch(stateReg) {
    is(idle) {
      when(io.newFrame) {
        stateReg := compute1
      }
    }

    is(compute1) {
      car.io.update := true.B
      aiCar.io.update := true.B
      stateReg := done
    }


  is(done) {
    io.frameUpdateDone := true.B
    stateReg := idle
  }
}

// running sprite and hit-sprite wiring
val runningSprite = Module(new RunningSprite)
runningSprite.io.update := frameUpdateReg

val carWidth = 32.S(12.W)
val carHeight = 32.S(11.W)
val runningHit = (car.io.posX < runningSprite.io.hitboxX + runningSprite.io.hitboxWidth.asSInt) &&
                 (car.io.posX + carWidth > runningSprite.io.hitboxX) &&
                 (car.io.posY < runningSprite.io.hitboxY + runningSprite.io.hitboxHeight.asSInt) &&
                 (car.io.posY + carHeight > runningSprite.io.hitboxY)

runningSprite.io.hit := runningHit

io.spriteXPosition(3) := runningSprite.io.posX - cameraX
io.spriteYPosition(3) := runningSprite.io.posY - cameraY
io.spriteFlipHorizontal(3) := runningSprite.io.flipH
io.spriteFlipVertical(3) := runningSprite.io.flipV
io.spriteVisible(3) := runningSprite.io.shownSprite(2)

// slot 5 shows the hit version of the running sprite
io.spriteXPosition(5) := runningSprite.io.posX - cameraX
io.spriteYPosition(5) := runningSprite.io.posY - cameraY
io.spriteFlipHorizontal(5) := runningSprite.io.flipH
io.spriteFlipVertical(5) := runningSprite.io.flipV
io.spriteVisible(5) := runningSprite.io.shownSprite(3)

// Mystery Box
val mysteryBox = Module(new MysteryBox)
val rng = Module(new RNG(3))
mysteryBox.io.box := false.B
mysteryBox.io.hit := false.B
mysteryBox.io.rand := rng.io.idx
io.spriteXPosition(14) := mysteryBox.io.posX - cameraX
io.spriteYPosition(14) := mysteryBox.io.posY - cameraY
io.spriteFlipHorizontal(14) := false.B
io.spriteFlipVertical(14) := false.B
io.spriteVisible(14) := mysteryBox.io.shownSprite
} // # todo - er det meningen, alt dette defineres i switch statement? 