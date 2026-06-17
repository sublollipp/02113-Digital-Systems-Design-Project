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

    val seg = Output(UInt(7.W))
    val an  = Output(UInt(4.W))
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

  val pocket = Module(new Pocket)
  pocket.io.useBtn := false.B
  pocket.io.frameUpdate := false.B
  pocket.io.carPosX := 0.S
  pocket.io.carPosY := 0.S
  pocket.io.carAngle := 0.U
  pocket.io.hitMysteryBox := false.B
  pocket.io.rngInput := VecInit(Seq.fill(4)(false.B))

  val firstInput = RegInit(false.B)

  when(io.btnU || io.btnD || io.btnL || io.btnR) {
    firstInput := true.B
  }


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

  val winCondition = Module(new WinCondition)

  val raceTimer = Module(new RaceTimer)

  val startLight = Module(new RaceStartLight(redFrames = 120, yellowFrames = 120, greenFrames = 60)
  )

  startLight.io.update := frameUpdateReg

  raceTimer.io.start := firstInput
  raceTimer.io.stop := winCondition.io.gameWon

  val display = Module(new SevenSegmentDisplay)

  display.io.digit0 := raceTimer.io.digit0
  display.io.digit1 := raceTimer.io.digit1
  display.io.digit2 := raceTimer.io.digit2
  display.io.digit3 := raceTimer.io.digit3

  io.seg := display.io.seg  
  io.an := display.io.an

  val lapDisplay = Module(new LapCounterDisplay)

  lapDisplay.io.lap1 := winCondition.io.lap1
  lapDisplay.io.lap2 := winCondition.io.lap2
  lapDisplay.io.lap3 := winCondition.io.lap3

  io.spriteXPosition(11) := 8.S
  io.spriteYPosition(11) := 8.S

  io.spriteVisible(11) := lapDisplay.io.show1

  io.spriteFlipHorizontal(11) := false.B
  io.spriteFlipVertical(11) := false.B

  io.spriteXPosition(12) := 8.S
  io.spriteYPosition(12) := 8.S

  io.spriteVisible(12) := lapDisplay.io.show2

  io.spriteFlipHorizontal(12) := false.B
  io.spriteFlipVertical(12) := false.B

  io.spriteXPosition(13) := 8.S
  io.spriteYPosition(13) := 8.S

  io.spriteVisible(13) := lapDisplay.io.show3

  io.spriteFlipHorizontal(13) := false.B
  io.spriteFlipVertical(13) := false.B

  io.spriteXPosition(15) := 40.S
  io.spriteYPosition(15) := 8.S

  io.spriteVisible(15) := true.B

  io.spriteFlipHorizontal(15) := false.B
  io.spriteFlipVertical(15) := false.B

    // Fast 3-tal efter "/"
  io.spriteXPosition(16) := 72.S
  io.spriteYPosition(16) := 8.S

  io.spriteVisible(16) := true.B

  io.spriteFlipHorizontal(16) := false.B
  io.spriteFlipVertical(16) := false.B

  val startX = 272.S(12.W)
  val startY = 20.S(11.W)

// ==========================
// STARTLYS - POSITIONER
// ==========================

// Rød række
io.spriteXPosition(17) := 272.S
io.spriteYPosition(17) := 20.S

io.spriteXPosition(18) := 304.S
io.spriteYPosition(18) := 20.S

io.spriteXPosition(19) := 336.S
io.spriteYPosition(19) := 20.S

// Gul række
io.spriteXPosition(20) := 272.S
io.spriteYPosition(20) := 52.S

io.spriteXPosition(21) := 304.S
io.spriteYPosition(21) := 52.S

io.spriteXPosition(22) := 336.S
io.spriteYPosition(22) := 52.S

// Grøn række
io.spriteXPosition(23) := 272.S
io.spriteYPosition(23) := 84.S

io.spriteXPosition(24) := 304.S
io.spriteYPosition(24) := 84.S

io.spriteXPosition(25) := 336.S
io.spriteYPosition(25) := 84.S


// ==========================
// STARTLYS - SYNLIGHED
// ==========================

// Røde lamper
io.spriteVisible(17) :=
  startLight.io.visible &&
  startLight.io.showRed

io.spriteVisible(24) :=
  startLight.io.visible &&
  startLight.io.showRed

io.spriteVisible(25) :=
  startLight.io.visible &&
  startLight.io.showRed


// Gule lamper
io.spriteVisible(18) :=
  startLight.io.visible &&
  startLight.io.showYellow

io.spriteVisible(26) :=
  startLight.io.visible &&
  startLight.io.showYellow

io.spriteVisible(27) :=
  startLight.io.visible &&
  startLight.io.showYellow


// Grønne lamper
io.spriteVisible(19) :=
  startLight.io.visible &&
  startLight.io.showGreen

io.spriteVisible(28) :=
  startLight.io.visible &&
  startLight.io.showGreen

io.spriteVisible(29) :=
  startLight.io.visible &&
  startLight.io.showGreen

  val carCollision = Module(new CarCollision)

  carCollision.io.carX := car.io.posX
  carCollision.io.carY := car.io.posY

  carCollision.io.aiX := aiCar.io.posX
  carCollision.io.aiY := aiCar.io.posY

  val crashReg = RegInit(false.B)

  val crashSpriteX = RegInit(0.S(12.W))
  val crashSpriteY = RegInit(0.S(11.W))

  val crashX = WireDefault(0.S(12.W))
  val crashY = WireDefault(0.S(11.W))

  winCondition.io.carX := car.io.posX
  winCondition.io.carY := car.io.posY

  crashX := (car.io.posX + aiCar.io.posX) >> 1
  crashY := (car.io.posY + aiCar.io.posY) >> 1


  when(carCollision.io.collision && !crashReg) {
    crashReg := true.B

    crashSpriteX := ((car.io.posX+ 16.S) + (aiCar.io.posX + 16.S)) >> 1
    crashSpriteY := ((car.io.posY + 16.S) + (aiCar.io.posY + 16.S)) >> 1
}

  io.spriteXPosition(10) := crashSpriteX - 16.S - cameraX
  io.spriteYPosition(10) := crashSpriteY - 16.S - cameraY

  io.spriteFlipHorizontal(10) := false.B
  io.spriteFlipVertical(10) := false.B

  io.spriteVisible(10) := crashReg

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

  when(startLight.io.raceStarted && !winCondition.io.gameWon && !crashReg) {

    car.io.update := true.B
    aiCar.io.update := true.B

  }.otherwise {

    car.io.update := false.B
    aiCar.io.update := false.B
  }

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

val runningSprite2 = Module(new SecondrunningSprite)
runningSprite2.io.update := false.B
runningSprite2.io.hit := false.B

val carWidth = 32.S(12.W)
val carHeight = 32.S(11.W)
val runningHit = (car.io.posX < runningSprite.io.hitboxX + runningSprite.io.hitboxWidth.asSInt) &&
                 (car.io.posX + carWidth > runningSprite.io.hitboxX) &&
                 (car.io.posY < runningSprite.io.hitboxY + runningSprite.io.hitboxHeight.asSInt) &&
                 (car.io.posY + carHeight > runningSprite.io.hitboxY)
val runningHitPrev = RegNext(runningHit, false.B)
val runningHitRising = runningHit && !runningHitPrev


runningSprite.io.hit := runningHit
// Trigger car slow effect on running sprite hit
car.io.boost := runningHitRising
car.io.boostFrames := 30.U
car.io.boostSpeed := -100.S

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

// slot 20 is the second running sprite
io.spriteXPosition(20) := 96.S
io.spriteYPosition(20) := 864.S
io.spriteFlipHorizontal(20) := runningSprite2.io.flipH
io.spriteFlipVertical(20) := runningSprite2.io.flipV
io.spriteVisible(20) := runningSprite2.io.shownSprite(2)

when(carCollision.io.collision) {
  crashReg := true.B
}

// Mystery Box
val mysteryBox = Module(new MysteryBox)
val rng = Module(new RNG)

val mysteryBoxHit = (car.io.posX < mysteryBox.io.hitboxX + mysteryBox.io.hitboxWidth.asSInt) &&
                     (car.io.posX + carWidth > mysteryBox.io.hitboxX) &&
                     (car.io.posY < mysteryBox.io.hitboxY + mysteryBox.io.hitboxHeight.asSInt) &&
                     (car.io.posY + carHeight > mysteryBox.io.hitboxY)
val mysteryBoxHitPrev = RegNext(mysteryBoxHit, false.B)
val mysteryBoxHitRising = mysteryBoxHit && !mysteryBoxHitPrev

mysteryBox.io.box := false.B
mysteryBox.io.hit := mysteryBoxHit
mysteryBox.io.rand := 0.U

pocket.io.hitMysteryBox := mysteryBoxHitRising
pocket.io.rngInput := rng.io.output

io.spriteXPosition(26) := 576.S
io.spriteYPosition(26) := 16.S

io.spriteVisible(26) := pocket.io.showShell

io.spriteFlipHorizontal(26) := false.B
io.spriteFlipVertical(26) := false.B

io.spriteXPosition(27) := 576.S
io.spriteYPosition(27) := 16.S

io.spriteVisible(27) := pocket.io.showShroom

io.spriteFlipHorizontal(27) := false.B
io.spriteFlipVertical(27) := false.B

io.spriteXPosition(14) := mysteryBox.io.posX - cameraX
io.spriteYPosition(14) := mysteryBox.io.posY - cameraY
io.spriteFlipHorizontal(14) := false.B
io.spriteFlipVertical(14) := false.B
io.spriteVisible(14) := mysteryBox.io.shownSprite

io.led(0) := winCondition.io.checkpointHit
io.led(1) := winCondition.io.finishHit
io.led(2) := winCondition.io.lap1
io.led(3) := winCondition.io.lap2
io.led(4) := winCondition.io.lap3
io.led(5) := winCondition.io.gameWon
io.led(6) := crashReg
io.led(7) := lapDisplay.io.show3

} // # todo - er det meningen, alt dette defineres i switch statement? 

