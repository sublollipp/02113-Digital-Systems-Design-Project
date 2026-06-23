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
  /*
    val carUpSprite = 0
  val carUpRightSprite = 1
  val carRightSprite = 2
  val luigiSprite = 3
  val yellowCarUpSprite = 4
  val luigiDeadSprite = 5
  val yellowCarUpRightSprite = 6
  val yellowCarRightSprite = 7
  val carDownRightRightSprite = 8
  // val carUpSprite = 9 (ubrugt pt.)
  val explosionSprite = 10
  val numOneSprite = 11
  val numTwoSprite = 12
  val numThreeSpriteOne = 13
  val mysteryBoxSprite = 14
  val slashSprite = 15
  val numThreeSpriteTwo = 16
  val redLightSprite = 17
  val yellowLightSprite = 18
  val greenLightSprite = 19
  val dmitriSprite = 20
  val dmitriGone2HeavenSprite = 21
  val greenShellFrontSprite = 22
  val mushroomSprite = 23
  val greenShellFrontSpriteTwo = 24
  val ladySprite = 25
  val greenShellDisplaySprite = 26
  val carDownDownRightSprite = 27
  val yellowCarDownRightRightSprite = 28 // Er lige nu bare en farvet cirkel
  val yellowCarDownDownRightSprite = 29 // Er lige nu også bare en farvet cirkel
  val ladyDustEyesOpenSprite = 30
  val ladyDustEyesClosedSprite = 31
   */

  // Sprites
  val blackBoxSprite = 10
  val carUpSprite = 15
  val carUpRightSprite = 16
  val carRightSprite = 17
  val luigiSprite = 28
  val yellowCarUpSprite = 20
  val luigiDeadSprite = 29
  val yellowCarUpRightSprite = 21
  val yellowCarRightSprite = 22
  val carDownRightRightSprite = 18
  val explosionSprite = 14
  val numOneSprite = 3
  val numTwoSprite = 4
  val numThreeSpriteOne = 5
  val mysteryBoxSprite = 31
  val slashSprite = 6
  val numThreeSpriteTwo = 7
  val redLightSprite = 0
  val yellowLightSprite = 1
  val greenLightSprite = 2
  val dmitriSprite = 30
  val dmitriGone2HeavenSprite = 11
  val greenShellFrontSprite = 12
  val mushroomSprite = 8
  val greenShellFrontSpriteTwo = 13
  val ladySprite = 27
  val greenShellDisplaySprite = 9
  val carDownDownRightSprite = 19
  val yellowCarDownRightRightSprite = 23
  val yellowCarDownDownRightSprite = 24 // Er lige nu også bare en farvet cirkel
  val ladyDustEyesOpenSprite = 26
  val ladyDustEyesClosedSprite = 25

  io.led := Seq.fill(8)(false.B)

  io.spriteXPosition := Seq.fill(SpriteNumber)(0.S)
  io.spriteYPosition := Seq.fill(SpriteNumber)(0.S)
  io.spriteFlipHorizontal := Seq.fill(SpriteNumber)(false.B)
  io.spriteFlipVertical := Seq.fill(SpriteNumber)(false.B)

  io.backBufferWriteData := 0.U
  io.backBufferWriteAddress := 0.U
  io.backBufferWriteEnable := false.B

  val anyInput = io.btnC || io.btnU || io.btnD || io.btnL || io.btnR

  val spriteVisible = WireDefault(VecInit.fill(32)(false.B))

  val hideAllSprites = WireDefault(false.B)

  for (i <- 0 to 31) {
    io.spriteVisible(i) := spriteVisible(i) && !hideAllSprites
  }

  io.frameUpdateDone := false.B

  val updateFrame = WireDefault(false.B)
  val updateRNG = WireDefault(false.B)

  val car = Module(new Car)

  val pocket = Module(new Pocket)

  val firstInput = RegInit(false.B)

  val onSplash = RegInit(true.B)

  when(anyInput) {
    firstInput := true.B
  }

  val mysteryBox = Module(new MysteryBox)

  // AI car position

  car.io.btnLeft := io.btnL
  car.io.btnUp := io.btnU
  car.io.btnRight := io.btnR
  car.io.btnDown := io.btnD


  io.spriteFlipHorizontal(carUpSprite) := car.io.flipH
  io.spriteFlipHorizontal(carUpRightSprite) := car.io.flipH
  io.spriteFlipHorizontal(carRightSprite) := car.io.flipH
  io.spriteFlipHorizontal(carDownRightRightSprite) := car.io.flipH
  io.spriteFlipHorizontal(carDownDownRightSprite) := car.io.flipH

  io.spriteFlipVertical(carUpSprite) := car.io.flipV
  io.spriteFlipVertical(carUpRightSprite) := car.io.flipV
  io.spriteFlipVertical(carRightSprite) := car.io.flipV
  io.spriteFlipVertical(carDownRightRightSprite) := car.io.flipV
  io.spriteFlipVertical(carDownDownRightSprite) := car.io.flipV


  // Camera Follow with map bounds

  val cameraX = Wire(SInt(11.W))
  val cameraY = Wire(SInt(10.W))

  val camera = Module(new Camera)
  camera.io.carX := car.io.posX
  camera.io.carY := car.io.posY
  when (onSplash) {
    cameraX := 0.S
    cameraY := 0.S
  }.otherwise {
    cameraX := camera.io.camX
    cameraY := camera.io.camY
  }

  val aiCar = Module(new AiCar)

  val winCondition = Module(new WinCondition)

  val raceTimer = Module(new RaceTimer)

  val startLight = Module(new RaceStartLight(redFrames = 120, yellowFrames = 120, greenFrames = 60)
  )

  val shell = Module(new Shell)

  val playerHitPulse = shell.io.hitPlayer

  car.io.resetSpeed := playerHitPulse

  aiCar.io.resetSpeed := false.B

  pocket.io.useBtn := io.btnC

  shell.io.spawn := pocket.io.useShell

  val shellSpawnDistance = 48.S

  val sinValues = VecInit((0 until 64).map(i =>
    (Math.sin((3.14159 / 180) * i * 5.625) * 64).round.toInt.S(8.W)
  ))

  val cosValues = VecInit((0 until 64).map(i =>
    (Math.cos((3.14159 / 180) * i * 5.625) * 64).round.toInt.S(8.W)
  ))

  val carCenterX = car.io.posX + 8.S
  val carCenterY = car.io.posY + 8.S

  val spawnOffsetX =
    ((cosValues(car.io.angleOut) * shellSpawnDistance) >> 6).asSInt

  val spawnOffsetY =
    ((sinValues(car.io.angleOut) * shellSpawnDistance) >> 6).asSInt

  pocket.io.useBtn := false.B
  pocket.io.frameUpdate := false.B
  pocket.io.hitMysteryBox := false.B

  shell.io.startX := carCenterX + spawnOffsetX - 16.S
  shell.io.startY := carCenterY + spawnOffsetY - 16.S

  shell.io.startAngle := car.io.angleOut
  shell.io.frameUpdate := updateFrame
  pocket.io.shellOnScreen := shell.io.visible

  shell.io.playerX := car.io.posX
  shell.io.playerY := car.io.posY

  shell.io.aiX := aiCar.io.posX
  shell.io.aiY := aiCar.io.posY

  val playerStun = RegInit(0.U(8.W))

    when(shell.io.hitPlayer) {
    playerStun := 120.U
  }

    when(updateFrame && playerStun =/= 0.U) {
    playerStun := playerStun - 1.U
  }

  val aiStun = RegInit(0.U(8.W))

    when(shell.io.hitAi) {
      aiStun := 120.U
      aiCar.io.resetSpeed := true.B
    }

    when(updateFrame && aiStun =/= 0.U) {
      aiStun := aiStun - 1.U
    }

  val shellAnimCounter = RegInit(0.U(6.W))
  val shellAnimToggle = RegInit(false.B)

  when(updateFrame && shell.io.visible) {
    when(shellAnimCounter === 14.U) {
      shellAnimCounter := 0.U
      shellAnimToggle := !shellAnimToggle
    }.otherwise {
      shellAnimCounter := shellAnimCounter + 1.U
    }
  }.elsewhen(!shell.io.visible) {
    shellAnimCounter := 0.U
    shellAnimToggle := false.B
  }


  io.spriteXPosition(greenShellFrontSprite) := shell.io.posX - cameraX
  io.spriteYPosition(greenShellFrontSprite) := shell.io.posY - cameraY
  spriteVisible(greenShellFrontSprite) := shell.io.visible && !shellAnimToggle

  io.spriteFlipHorizontal(greenShellFrontSprite) := false.B
  io.spriteFlipVertical(greenShellFrontSprite) := false.B

  io.spriteXPosition(greenShellFrontSpriteTwo) := shell.io.posX - cameraX
  io.spriteYPosition(greenShellFrontSpriteTwo) := shell.io.posY - cameraY

  spriteVisible(greenShellFrontSpriteTwo) := shell.io.visible && shellAnimToggle

  io.spriteFlipHorizontal(greenShellFrontSpriteTwo) := false.B
  io.spriteFlipVertical(greenShellFrontSpriteTwo) := false.B

  startLight.io.update := updateFrame

    val carCollision = Module(new CarCollision)

  raceTimer.io.start := firstInput && startLight.io.raceStarted
  raceTimer.io.stop := winCondition.io.gameWon || carCollision.io.collision

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

  io.spriteXPosition(numOneSprite) := 536.S
  io.spriteYPosition(numOneSprite) := 8.S

  spriteVisible(numOneSprite) := lapDisplay.io.show1

  io.spriteFlipHorizontal(numOneSprite) := false.B
  io.spriteFlipVertical(numOneSprite) := false.B

  io.spriteXPosition(numTwoSprite) := 536.S
  io.spriteYPosition(numTwoSprite) := 8.S

  spriteVisible(numTwoSprite) := lapDisplay.io.show2

  io.spriteFlipHorizontal(numTwoSprite) := false.B
  io.spriteFlipVertical(numTwoSprite) := false.B

  io.spriteXPosition(numThreeSpriteOne) := 536.S
  io.spriteYPosition(numThreeSpriteOne) := 8.S

  spriteVisible(numThreeSpriteOne) := lapDisplay.io.show3

  io.spriteFlipHorizontal(numThreeSpriteOne) := false.B
  io.spriteFlipVertical(numThreeSpriteOne) := false.B

  io.spriteXPosition(slashSprite) := 568.S
  io.spriteYPosition(slashSprite) := 8.S

  spriteVisible(slashSprite) := true.B

  io.spriteFlipHorizontal(slashSprite) := false.B
  io.spriteFlipVertical(slashSprite) := false.B

    // Fast 3-tal efter "/"
  io.spriteXPosition(numThreeSpriteTwo) := 600.S
  io.spriteYPosition(numThreeSpriteTwo) := 8.S

  spriteVisible(numThreeSpriteTwo) := true.B

  io.spriteFlipHorizontal(numThreeSpriteTwo) := false.B
  io.spriteFlipVertical(numThreeSpriteTwo) := false.B

    // Startlys - rød

  io.spriteXPosition(redLightSprite) := 304.S
  io.spriteYPosition(redLightSprite) := 20.S

  spriteVisible(redLightSprite) :=
    startLight.io.visible &&
    startLight.io.showRed

  io.spriteFlipHorizontal(redLightSprite) := false.B
  io.spriteFlipVertical(redLightSprite) := false.B


  // Startlys - gul

  io.spriteXPosition(yellowLightSprite) := 304.S
  io.spriteYPosition(yellowLightSprite) := 20.S

  spriteVisible(yellowLightSprite) :=
    startLight.io.visible &&
    startLight.io.showYellow

  io.spriteFlipHorizontal(yellowLightSprite) := false.B
  io.spriteFlipVertical(yellowLightSprite) := false.B


  // Startlys - grøn

  io.spriteXPosition(greenLightSprite) := 304.S
  io.spriteYPosition(greenLightSprite) := 20.S

  spriteVisible(greenLightSprite) :=
    startLight.io.visible &&
    startLight.io.showGreen

  io.spriteFlipHorizontal(greenLightSprite) := false.B
  io.spriteFlipVertical(greenLightSprite) := false.B

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

  io.spriteXPosition(explosionSprite) := crashSpriteX - 16.S - cameraX
  io.spriteYPosition(explosionSprite) := crashSpriteY - 16.S - cameraY

  io.spriteFlipHorizontal(explosionSprite) := false.B
  io.spriteFlipVertical(explosionSprite) := false.B

  spriteVisible(explosionSprite) := crashReg

  aiCar.io.updateFrame := false.B
  aiCar.io.updateRNG := updateRNG

  io.spriteXPosition(yellowCarUpSprite) := aiCar.io.posX - cameraX
  io.spriteYPosition(yellowCarUpSprite) := aiCar.io.posY - cameraY

  io.spriteXPosition(yellowCarUpRightSprite) := aiCar.io.posX - cameraX
  io.spriteYPosition(yellowCarUpRightSprite) := aiCar.io.posY - cameraY

  io.spriteXPosition(yellowCarRightSprite) := aiCar.io.posX - cameraX
  io.spriteYPosition(yellowCarRightSprite) := aiCar.io.posY - cameraY

  io.spriteXPosition(yellowCarDownRightRightSprite) := aiCar.io.posX - cameraX
  io.spriteYPosition(yellowCarDownRightRightSprite) := aiCar.io.posY - cameraY

  io.spriteXPosition(yellowCarDownDownRightSprite) := aiCar.io.posX - cameraX
  io.spriteYPosition(yellowCarDownDownRightSprite) := aiCar.io.posY - cameraY

  spriteVisible(yellowCarUpSprite) := aiCar.io.spriteOH_UDR(0)
  spriteVisible(yellowCarUpRightSprite) := aiCar.io.spriteOH_UDR(1)
  spriteVisible(yellowCarRightSprite) := aiCar.io.spriteOH_UDR(2)
  spriteVisible(yellowCarDownRightRightSprite) := aiCar.io.spriteOH_UDR(3)
  spriteVisible(yellowCarDownDownRightSprite) := aiCar.io.spriteOH_UDR(4)

  io.spriteFlipHorizontal(yellowCarUpSprite) := aiCar.io.flipH
  io.spriteFlipHorizontal(yellowCarUpRightSprite) := aiCar.io.flipH
  io.spriteFlipHorizontal(yellowCarRightSprite) := aiCar.io.flipH
  io.spriteFlipHorizontal(yellowCarDownRightRightSprite) := aiCar.io.flipH
  io.spriteFlipHorizontal(yellowCarDownDownRightSprite) := aiCar.io.flipH

  io.spriteFlipVertical(yellowCarUpSprite) := aiCar.io.flipV
  io.spriteFlipVertical(yellowCarUpRightSprite) := aiCar.io.flipV
  io.spriteFlipVertical(yellowCarRightSprite) := aiCar.io.flipV
  io.spriteFlipVertical(yellowCarDownRightRightSprite) := aiCar.io.flipV
  io.spriteFlipVertical(yellowCarDownDownRightSprite) := aiCar.io.flipV

  // Viewbox
  io.viewBoxX := cameraX.asUInt
  io.viewBoxY := cameraY.asUInt

  // Car
  io.spriteXPosition(carUpSprite) := car.io.posX - cameraX
  io.spriteXPosition(carUpRightSprite) := car.io.posX - cameraX
  io.spriteXPosition(carRightSprite) := car.io.posX - cameraX
  io.spriteXPosition(carDownRightRightSprite) := car.io.posX - cameraX
  io.spriteXPosition(carDownDownRightSprite) := car.io.posX - cameraX

  io.spriteYPosition(carUpSprite) := car.io.posY - cameraY
  io.spriteYPosition(carUpRightSprite) := car.io.posY - cameraY
  io.spriteYPosition(carRightSprite) := car.io.posY - cameraY
  io.spriteYPosition(carDownRightRightSprite) := car.io.posY - cameraY
  io.spriteYPosition(carDownDownRightSprite) := car.io.posY - cameraY

  spriteVisible(carUpSprite) := car.io.shownSprite(0)
  spriteVisible(carUpRightSprite) := car.io.shownSprite(1)
  spriteVisible(carRightSprite) := car.io.shownSprite(2)
  spriteVisible(carDownDownRightSprite) := car.io.shownSprite(3)
  spriteVisible(carDownRightRightSprite) := car.io.shownSprite(4)

  car.io.update := false.B

  val bgController = Module(new BGController)
  bgController.io.showGame := !onSplash
  bgController.io.showSplash := onSplash
  bgController.io.doneAckn := false.B
  io.backBufferWriteData := bgController.io.backBufferWriteData
  io.backBufferWriteAddress := bgController.io.backBufferWriteAddress
  io.backBufferWriteEnable := bgController.io.backBufferWriteEnable

  val doneUpdatingBG = bgController.io.bgUpdateDone

  val startupCounter = RegInit(0.U(4.W))
  val startcountOver = RegInit(false.B)
  val readyForStartup = RegInit(false.B)

  val idle :: game :: startSplash :: splashIdle :: done :: startSplashUpdateDone :: deadOrDone :: Nil = Enum(7)
  val stateReg = RegInit(startSplash)
  /// Running Sprite Logic
val runningSprite = Module(new RunningSprite)
runningSprite.io.update := updateFrame

val runningSprite2 = Module(new SecondrunningSprite)
runningSprite2.io.update := updateFrame
runningSprite2.io.hit := false.B

val runningSprite3 = Module(new ThirdrunningSprite)
runningSprite3.io.update := updateFrame

// Shell hit detection against running sprite
val shellSize = 32.S(12.W)

val runningSpriteAlive = runningSprite.io.hitboxWidth =/= 0.U && runningSprite.io.hitboxHeight =/= 0.U
val runningSprite2Alive = runningSprite2.io.hitboxWidth =/= 0.U && runningSprite2.io.hitboxHeight =/= 0.U
val runningSprite3Alive = runningSprite3.io.hitboxWidth =/= 0.U && runningSprite3.io.hitboxHeight =/= 0.U

val shellHitsRunningSprite =
  shell.io.visible &&
  runningSpriteAlive &&
  (shell.io.posX < runningSprite.io.hitboxX + runningSprite.io.hitboxWidth.asSInt) &&
  (shell.io.posX + shellSize > runningSprite.io.hitboxX) &&
  (shell.io.posY < runningSprite.io.hitboxY + runningSprite.io.hitboxHeight.asSInt) &&
  (shell.io.posY + shellSize > runningSprite.io.hitboxY)

val shellHitsRunningSprite2 =
  shell.io.visible &&
  runningSprite2Alive &&
  (shell.io.posX < runningSprite2.io.hitboxX + runningSprite2.io.hitboxWidth.asSInt) &&
  (shell.io.posX + shellSize > runningSprite2.io.hitboxX) &&
  (shell.io.posY < runningSprite2.io.hitboxY + runningSprite2.io.hitboxHeight.asSInt) &&
  (shell.io.posY + shellSize > runningSprite2.io.hitboxY)

val shellHitsRunningSprite3 =
  shell.io.visible &&
  runningSprite3Alive &&
  (shell.io.posX < runningSprite3.io.hitboxX + runningSprite3.io.hitboxWidth.asSInt) &&
  (shell.io.posX + shellSize > runningSprite3.io.hitboxX) &&
  (shell.io.posY < runningSprite3.io.hitboxY + runningSprite3.io.hitboxHeight.asSInt) &&
  (shell.io.posY + shellSize > runningSprite3.io.hitboxY)

  shell.io.hitObstacle := shellHitsRunningSprite || shellHitsRunningSprite2 || shellHitsRunningSprite3

  // running sprite and hit-sprite wiring
val carWidth = 32.S(12.W)
val carHeight = 32.S(11.W)
val runningHit = runningSpriteAlive &&
                 (car.io.posX < runningSprite.io.hitboxX + runningSprite.io.hitboxWidth.asSInt) &&
                 (car.io.posX + carWidth > runningSprite.io.hitboxX) &&
                 (car.io.posY < runningSprite.io.hitboxY + runningSprite.io.hitboxHeight.asSInt) &&
                 (car.io.posY + carHeight > runningSprite.io.hitboxY)
val runningHitPrev = RegNext(runningHit, false.B)
val runningHitRising = runningHit && !runningHitPrev
// Latch shell hit for 1 cycle so it bridges the shell deactivation gap
val shellHitPending = RegInit(false.B)
when(shellHitsRunningSprite) {
  shellHitPending := true.B
}.otherwise {
  shellHitPending := false.B
}

val runningHit2 = runningSprite2Alive &&
                  (car.io.posX < runningSprite2.io.hitboxX + runningSprite2.io.hitboxWidth.asSInt) &&
                  (car.io.posX + carWidth > runningSprite2.io.hitboxX) &&
                  (car.io.posY < runningSprite2.io.hitboxY + runningSprite2.io.hitboxHeight.asSInt) &&
                  (car.io.posY + carHeight > runningSprite2.io.hitboxY)
val runningHit2Prev = RegNext(runningHit2, false.B)
val runningHit2Rising = runningHit2 && !runningHit2Prev
val shellHit2Pending = RegInit(false.B)
when(shellHitsRunningSprite2) {
  shellHit2Pending := true.B
}.otherwise {
  shellHit2Pending := false.B
}

val runningHit3 = runningSprite3Alive &&
                  (car.io.posX < runningSprite3.io.hitboxX + runningSprite3.io.hitboxWidth.asSInt) &&
                  (car.io.posX + carWidth > runningSprite3.io.hitboxX) &&
                  (car.io.posY < runningSprite3.io.hitboxY + runningSprite3.io.hitboxHeight.asSInt) &&
                  (car.io.posY + carHeight > runningSprite3.io.hitboxY)
val runningHit3Prev = RegNext(runningHit3, false.B)
val runningHit3Rising = runningHit3 && !runningHit3Prev
val shellHit3Pending = RegInit(false.B)
when(shellHitsRunningSprite3) {
  shellHit3Pending := true.B
}.otherwise {
  shellHit3Pending := false.B
}

runningSprite.io.hit := runningHit || shellHitsRunningSprite || shellHitPending
runningSprite2.io.hit := runningHit2 || shellHitsRunningSprite2 || shellHit2Pending
runningSprite3.io.hit := runningHit3 || shellHitsRunningSprite3 || shellHit3Pending
// Trigger car slow effect on running sprite hit
car.io.colBoost := runningHitRising || runningHit2Rising || runningHit3Rising
car.io.boostFrames := 30.U
car.io.boostSpeed := -10.S
  car.io.shroomBoost := pocket.io.useShroom
  when (pocket.io.useShroom) {
    car.io.boostFrames := 90.U
    car.io.boostSpeed := 600.S
  }

  // Alive Luigi
io.spriteXPosition(luigiSprite) := runningSprite.io.posX - cameraX
io.spriteYPosition(luigiSprite) := runningSprite.io.posY - cameraY
io.spriteFlipHorizontal(luigiSprite) := runningSprite.io.flipH
io.spriteFlipVertical(luigiSprite) := runningSprite.io.flipV
spriteVisible(luigiSprite) := runningSprite.io.shownSprite(2)

// Dead luigi
io.spriteXPosition(luigiDeadSprite) := runningSprite.io.posX - cameraX
io.spriteYPosition(luigiDeadSprite) := runningSprite.io.posY - cameraY
io.spriteFlipHorizontal(luigiDeadSprite) := runningSprite.io.flipH
io.spriteFlipVertical(luigiDeadSprite) := runningSprite.io.flipV
spriteVisible(luigiDeadSprite) := runningSprite.io.shownSprite(3)

// Dmitri is the second running sprite
io.spriteXPosition(dmitriSprite) := runningSprite2.io.posX - cameraX
io.spriteYPosition(dmitriSprite) := runningSprite2.io.posY - cameraY
io.spriteFlipHorizontal(dmitriSprite) := runningSprite2.io.flipH
io.spriteFlipVertical(dmitriSprite) := runningSprite2.io.flipV
spriteVisible(dmitriSprite) := runningSprite2.io.shownSprite(2)

// Dmitri flying to the sky
io.spriteXPosition(dmitriGone2HeavenSprite) := runningSprite2.io.posX - cameraX
io.spriteYPosition(dmitriGone2HeavenSprite) := runningSprite2.io.posY - cameraY
io.spriteFlipHorizontal(dmitriGone2HeavenSprite) := runningSprite2.io.flipH
io.spriteFlipVertical(dmitriGone2HeavenSprite) := runningSprite2.io.flipV
spriteVisible(dmitriGone2HeavenSprite) := runningSprite2.io.shownSprite(3)

// Lady sprite before being hit
io.spriteXPosition(ladySprite) := runningSprite3.io.posX - cameraX
io.spriteYPosition(ladySprite) := runningSprite3.io.posY - cameraY
io.spriteFlipHorizontal(ladySprite) := runningSprite3.io.flipH
io.spriteFlipVertical(ladySprite) := runningSprite3.io.flipV
spriteVisible(ladySprite) := runningSprite3.io.shownSprite(2)

// Lady sprite after being hit (blinking pile of dust)
io.spriteXPosition(ladyDustEyesOpenSprite) := runningSprite3.io.posX - cameraX
io.spriteYPosition(ladyDustEyesOpenSprite) := runningSprite3.io.posY - cameraY
io.spriteFlipHorizontal(ladyDustEyesOpenSprite) := runningSprite3.io.flipH
io.spriteFlipVertical(ladyDustEyesOpenSprite) := runningSprite3.io.flipV
spriteVisible(ladyDustEyesOpenSprite) := runningSprite3.io.shownSprite(0)

io.spriteXPosition(ladyDustEyesClosedSprite) := runningSprite3.io.posX - cameraX
io.spriteYPosition(ladyDustEyesClosedSprite) := runningSprite3.io.posY - cameraY
io.spriteFlipHorizontal(ladyDustEyesClosedSprite) := runningSprite3.io.flipH
io.spriteFlipVertical(ladyDustEyesClosedSprite) := runningSprite3.io.flipV
spriteVisible(ladyDustEyesClosedSprite) := runningSprite3.io.shownSprite(1)

when(carCollision.io.collision) {
  crashReg := true.B
}

  /////////
  // FSM //
  /////////
  switch(stateReg) {
    is(idle) {
      when(io.newFrame) {
        stateReg := game
      }
    }

  is(game) {

    pocket.io.frameUpdate := true.B

    updateFrame := true.B
    updateRNG := true.B

    when(startLight.io.raceStarted) {
      car.io.update := playerStun === 0.U
      aiCar.io.updateFrame := aiStun === 0.U
    }

    when(crashReg || winCondition.io.gameWon) {
      stateReg := deadOrDone
      readyForStartup := false.B
    }.otherwise {
      stateReg := done
    }
  }

    is(splashIdle) {
      hideAllSprites := true.B
      when(io.newFrame) {
        stateReg := startSplash
      }
    }

    is(startSplash) {
      hideAllSprites := true.B
      updateRNG := true.B
      when (!readyForStartup) {
        when(!startcountOver) {
          startupCounter := startupCounter + 1.U
          when(startupCounter === 10.U) {
            startcountOver := true.B
          }
        }.otherwise { // Ignoring inputs held on power-on to prevent RNG manip
          when(!anyInput) {
            readyForStartup := true.B
          }
        }
      }
      when (anyInput && readyForStartup) {
        onSplash := false.B
      }
      when (!onSplash && doneUpdatingBG) {
        stateReg := idle
        bgController.io.doneAckn := true.B
        io.frameUpdateDone := true.B
      }.otherwise {
        stateReg := startSplashUpdateDone
      }
    }

    is (startSplashUpdateDone) {
      io.frameUpdateDone := true.B
      stateReg := splashIdle
    }

  is(done) {
    when (car.io.updateDone) {
      io.frameUpdateDone := true.B
      stateReg := idle
    }
  }
    is (deadOrDone) {
      car.io.update := false.B
      aiCar.io.updateFrame := false.B
      when(!onSplash) {
        when(!anyInput) {
          readyForStartup := true.B
        }
        when(readyForStartup && anyInput) {
          readyForStartup := false.B
          onSplash := true.B
          runningSprite.reset := true.B
          runningSprite2.reset := true.B
          runningSprite3.reset := true.B
          aiCar.reset := true.B
          car.reset := true.B
          pocket.reset := true.B
          winCondition.reset := true.B
          startLight.reset := true.B
          shell.reset := true.B
          raceTimer.reset := true.B
          crashReg := false.B
        }
        // todo - refactor finish into multiple states
      }.otherwise {
        when(bgController.io.bgUpdateDone) {
          bgController.io.doneAckn := true.B
          stateReg := splashIdle
        }
      }
    }
}


// Mystery Box

val mysteryBoxHit = (car.io.posX < mysteryBox.io.hitboxX + mysteryBox.io.hitboxWidth.asSInt) &&
                     (car.io.posX + carWidth > mysteryBox.io.hitboxX) &&
                     (car.io.posY < mysteryBox.io.hitboxY + mysteryBox.io.hitboxHeight.asSInt) &&
                     (car.io.posY + carHeight > mysteryBox.io.hitboxY)
val mysteryBoxHitPrev = RegNext(mysteryBoxHit, false.B)
val mysteryBoxHitRising = mysteryBoxHit && !mysteryBoxHitPrev

mysteryBox.io.rngUpdate := updateRNG
mysteryBox.io.hit := mysteryBoxHit
mysteryBox.io.frameUpdate := updateFrame

pocket.io.hitMysteryBox := mysteryBoxHitRising

io.spriteXPosition(greenShellDisplaySprite) := 8.S
io.spriteYPosition(greenShellDisplaySprite) := 8.S

spriteVisible(greenShellDisplaySprite) := pocket.io.showShell

io.spriteFlipHorizontal(greenShellDisplaySprite) := false.B
io.spriteFlipVertical(greenShellDisplaySprite) := false.B

io.spriteXPosition(mushroomSprite) := 8.S
io.spriteYPosition(mushroomSprite) := 8.S

spriteVisible(mushroomSprite) := pocket.io.showShroom

io.spriteFlipHorizontal(mushroomSprite) := false.B
io.spriteFlipVertical(mushroomSprite) := false.B

io.spriteXPosition(mysteryBoxSprite) := mysteryBox.io.posX - cameraX
io.spriteYPosition(mysteryBoxSprite) := mysteryBox.io.posY - cameraY
io.spriteFlipHorizontal(mysteryBoxSprite) := false.B
io.spriteFlipVertical(mysteryBoxSprite) := false.B
spriteVisible(mysteryBoxSprite) := mysteryBox.io.shownSprite
}

