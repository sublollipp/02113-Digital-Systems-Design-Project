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

  val spriteVisible = WireDefault(VecInit.fill(32)(false.B))

  val hideAllSprites = WireDefault(false.B)

  io.spriteXPosition := Seq.fill(SpriteNumber)(0.S)
  io.spriteYPosition := Seq.fill(SpriteNumber)(0.S)
  io.spriteFlipHorizontal := Seq.fill(SpriteNumber)(false.B)
  io.spriteFlipVertical := Seq.fill(SpriteNumber)(false.B)

  for (i <- 0 to 31) {
    io.spriteVisible(i) := spriteVisible(i) && !hideAllSprites
  }

  io.backBufferWriteData := 0.U
  io.backBufferWriteAddress := 0.U
  io.backBufferWriteEnable := false.B

  io.frameUpdateDone := false.B
  val updateFrame = WireDefault(false.B)
  val updateRNG = WireDefault(false.B)


  // Game Logic

  val car = Module(new Car)

  val pocket = Module(new Pocket)
  pocket.io.useBtn := false.B
  pocket.io.frameUpdate := false.B
  pocket.io.carPosX := 0.S
  pocket.io.carPosY := 0.S
  pocket.io.carAngle := 0.U
  pocket.io.hitMysteryBox := false.B

  val firstInput = RegInit(false.B)

  val onSplash = RegInit(true.B)

  when(io.btnU || io.btnD || io.btnL || io.btnR) {
    firstInput := true.B
  }

  val mysteryBox = Module(new MysteryBox)

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
  when (onSplash) {
    cameraX := 0.S
    cameraY := 0.S
  }.otherwise {
    cameraX := camera.io.camX
    cameraY := camera.io.camY
  }


  // AI-car

  val aiCar = Module(new AiCar)

  val winCondition = Module(new WinCondition)

  val raceTimer = Module(new RaceTimer)

  val startLight = Module(new RaceStartLight(redFrames = 120, yellowFrames = 120, greenFrames = 60)
  )

  val shell = Module(new Shell)

  val resetGame = Module(new ResetGame)

  resetGame.io.btnC := io.btnC
  resetGame.io.hasShell := pocket.io.showShell
  resetGame.io.hasShroom := pocket.io.showShroom

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

  shell.io.startX := carCenterX + spawnOffsetX - 16.S
  shell.io.startY := carCenterY + spawnOffsetY - 16.S

  shell.io.startAngle := car.io.angleOut
  shell.io.frameUpdate := updateFrame
  pocket.io.shellOnScreen := shell.io.visible
  pocket.io.resetGame := false.B

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


  io.spriteXPosition(22) := shell.io.posX - cameraX
  io.spriteYPosition(22) := shell.io.posY - cameraY
  spriteVisible(22) := shell.io.visible && !shellAnimToggle

  io.spriteFlipHorizontal(22) := false.B
  io.spriteFlipVertical(22) := false.B

  io.spriteXPosition(24) := shell.io.posX - cameraX
  io.spriteYPosition(24) := shell.io.posY - cameraY

  spriteVisible(24) := shell.io.visible && shellAnimToggle

  io.spriteFlipHorizontal(24) := false.B
  io.spriteFlipVertical(24) := false.B

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

  io.spriteXPosition(11) := 536.S
  io.spriteYPosition(11) := 8.S

  spriteVisible(11) := lapDisplay.io.show1

  io.spriteFlipHorizontal(11) := false.B
  io.spriteFlipVertical(11) := false.B

  io.spriteXPosition(12) := 536.S
  io.spriteYPosition(12) := 8.S

  spriteVisible(12) := lapDisplay.io.show2

  io.spriteFlipHorizontal(12) := false.B
  io.spriteFlipVertical(12) := false.B

  io.spriteXPosition(13) := 536.S
  io.spriteYPosition(13) := 8.S

  spriteVisible(13) := lapDisplay.io.show3

  io.spriteFlipHorizontal(13) := false.B
  io.spriteFlipVertical(13) := false.B

  io.spriteXPosition(15) := 568.S
  io.spriteYPosition(15) := 8.S

  spriteVisible(15) := true.B

  io.spriteFlipHorizontal(15) := false.B
  io.spriteFlipVertical(15) := false.B

    // Fast 3-tal efter "/"
  io.spriteXPosition(16) := 600.S
  io.spriteYPosition(16) := 8.S

  spriteVisible(16) := true.B

  io.spriteFlipHorizontal(16) := false.B
  io.spriteFlipVertical(16) := false.B

    // Startlys - rød

  io.spriteXPosition(17) := 304.S
  io.spriteYPosition(17) := 20.S

  spriteVisible(17) :=
    startLight.io.visible &&
    startLight.io.showRed

  io.spriteFlipHorizontal(17) := false.B
  io.spriteFlipVertical(17) := false.B


  // Startlys - gul

  io.spriteXPosition(18) := 304.S
  io.spriteYPosition(18) := 20.S

  spriteVisible(18) :=
    startLight.io.visible &&
    startLight.io.showYellow

  io.spriteFlipHorizontal(18) := false.B
  io.spriteFlipVertical(18) := false.B


  // Startlys - grøn

  io.spriteXPosition(19) := 304.S
  io.spriteYPosition(19) := 20.S

  spriteVisible(19) :=
    startLight.io.visible &&
    startLight.io.showGreen

  io.spriteFlipHorizontal(19) := false.B
  io.spriteFlipVertical(19) := false.B

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

  spriteVisible(10) := crashReg

  aiCar.io.updateFrame := false.B
  aiCar.io.updateRNG := updateRNG

  io.spriteXPosition(4) := aiCar.io.posX - cameraX
  io.spriteYPosition(4) := aiCar.io.posY - cameraY

  io.spriteXPosition(6) := aiCar.io.posX - cameraX
  io.spriteYPosition(6) := aiCar.io.posY - cameraY

  io.spriteXPosition(7) := aiCar.io.posX - cameraX
  io.spriteYPosition(7) := aiCar.io.posY - cameraY

  io.spriteXPosition(28) := aiCar.io.posX - cameraX
  io.spriteYPosition(28) := aiCar.io.posY - cameraY

  io.spriteXPosition(29) := aiCar.io.posX - cameraX
  io.spriteYPosition(29) := aiCar.io.posY - cameraY

  spriteVisible(4) := aiCar.io.spriteOH_UDR(0)
  spriteVisible(6) := aiCar.io.spriteOH_UDR(1)
  spriteVisible(7) := aiCar.io.spriteOH_UDR(2)
  spriteVisible(28) := aiCar.io.spriteOH_UDR(3)
  spriteVisible(29) := aiCar.io.spriteOH_UDR(4)

  io.spriteFlipHorizontal(4) := aiCar.io.flipH
  io.spriteFlipHorizontal(6) := aiCar.io.flipH
  io.spriteFlipHorizontal(7) := aiCar.io.flipH
  io.spriteFlipHorizontal(28) := aiCar.io.flipH
  io.spriteFlipHorizontal(29) := aiCar.io.flipH

  io.spriteFlipVertical(4) := aiCar.io.flipV
  io.spriteFlipVertical(6) := aiCar.io.flipV
  io.spriteFlipVertical(7) := aiCar.io.flipV
  io.spriteFlipVertical(28) := aiCar.io.flipV
  io.spriteFlipVertical(29) := aiCar.io.flipV

  // Viewbox
  io.viewBoxX := cameraX.asUInt
  io.viewBoxY := cameraY.asUInt

  // Car
  io.spriteXPosition(0) := car.io.posX - cameraX
  io.spriteXPosition(1) := car.io.posX - cameraX
  io.spriteXPosition(2) := car.io.posX - cameraX
  io.spriteXPosition(8) := car.io.posX - cameraX
  io.spriteXPosition(27) := car.io.posX - cameraX

  io.spriteYPosition(0) := car.io.posY - cameraY
  io.spriteYPosition(1) := car.io.posY - cameraY
  io.spriteYPosition(2) := car.io.posY - cameraY
  io.spriteYPosition(8) := car.io.posY - cameraY
  io.spriteYPosition(27) := car.io.posY - cameraY

  spriteVisible(0) := car.io.shownSprite(0)
  spriteVisible(1) := car.io.shownSprite(1)
  spriteVisible(2) := car.io.shownSprite(2)
  spriteVisible(27) := car.io.shownSprite(3)
  spriteVisible(8) := car.io.shownSprite(4)

  car.io.update := false.B

  val bgController = Module(new BGController)
  bgController.io.showGame := !onSplash
  bgController.io.showSplash := onSplash
  io.backBufferWriteData := bgController.io.backBufferWriteData
  io.backBufferWriteAddress := bgController.io.backBufferWriteAddress
  io.backBufferWriteEnable := bgController.io.backBufferWriteEnable

  val doneUpdatingBG = bgController.io.bgUpdateDone

  val startupCounter = RegInit(0.U(4.W))
  val startcountOver = RegInit(false.B)
  val readyForStartup = RegInit(false.B)

  val idle :: game :: startSplash :: splashIdle :: done :: startSplashUpdateDone :: Nil = Enum(6)
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

val shellHitsRunningSprite =
  shell.io.visible &&
  (shell.io.posX < runningSprite.io.hitboxX + runningSprite.io.hitboxWidth.asSInt) &&
  (shell.io.posX + shellSize > runningSprite.io.hitboxX) &&
  (shell.io.posY < runningSprite.io.hitboxY + runningSprite.io.hitboxHeight.asSInt) &&
  (shell.io.posY + shellSize > runningSprite.io.hitboxY)

  shell.io.hitObstacle := shellHitsRunningSprite

  // running sprite and hit-sprite wiring
val carWidth = 32.S(12.W)
val carHeight = 32.S(11.W)
val runningHit = (car.io.posX < runningSprite.io.hitboxX + runningSprite.io.hitboxWidth.asSInt) &&
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

val runningHit2 = (car.io.posX < runningSprite2.io.hitboxX + runningSprite2.io.hitboxWidth.asSInt) &&
                  (car.io.posX + carWidth > runningSprite2.io.hitboxX) &&
                  (car.io.posY < runningSprite2.io.hitboxY + runningSprite2.io.hitboxHeight.asSInt) &&
                  (car.io.posY + carHeight > runningSprite2.io.hitboxY)
val runningHit2Prev = RegNext(runningHit2, false.B)
val runningHit2Rising = runningHit2 && !runningHit2Prev

val runningHit3 = (car.io.posX < runningSprite3.io.hitboxX + runningSprite3.io.hitboxWidth.asSInt) &&
                  (car.io.posX + carWidth > runningSprite3.io.hitboxX) &&
                  (car.io.posY < runningSprite3.io.hitboxY + runningSprite3.io.hitboxHeight.asSInt) &&
                  (car.io.posY + carHeight > runningSprite3.io.hitboxY)
val runningHit3Prev = RegNext(runningHit3, false.B)
val runningHit3Rising = runningHit3 && !runningHit3Prev

runningSprite.io.hit := runningHit || shellHitsRunningSprite || shellHitPending
runningSprite2.io.hit := runningHit2
runningSprite3.io.hit := runningHit3
// Trigger car slow effect on running sprite hit
car.io.colBoost := runningHitRising || runningHit2Rising || runningHit3Rising
car.io.boostFrames := 30.U
car.io.boostSpeed := -10.S
  car.io.shroomBoost := pocket.io.useShroom
  when (pocket.io.useShroom) {
    car.io.boostFrames := 90.U
    car.io.boostSpeed := 600.S
  }

io.spriteXPosition(3) := runningSprite.io.posX - cameraX
io.spriteYPosition(3) := runningSprite.io.posY - cameraY
io.spriteFlipHorizontal(3) := runningSprite.io.flipH
io.spriteFlipVertical(3) := runningSprite.io.flipV
spriteVisible(3) := runningSprite.io.shownSprite(2)

// slot 5 shows the hit version of the running sprite
io.spriteXPosition(5) := runningSprite.io.posX - cameraX
io.spriteYPosition(5) := runningSprite.io.posY - cameraY
io.spriteFlipHorizontal(5) := runningSprite.io.flipH
io.spriteFlipVertical(5) := runningSprite.io.flipV
spriteVisible(5) := runningSprite.io.shownSprite(3)

// slot 20 is the second running sprite
io.spriteXPosition(20) := runningSprite2.io.posX - cameraX
io.spriteYPosition(20) := runningSprite2.io.posY - cameraY
io.spriteFlipHorizontal(20) := runningSprite2.io.flipH
io.spriteFlipVertical(20) := runningSprite2.io.flipV
spriteVisible(20) := runningSprite2.io.shownSprite(2)

// slot 21 is the second running sprite hit version
io.spriteXPosition(21) := runningSprite2.io.posX - cameraX
io.spriteYPosition(21) := runningSprite2.io.posY - cameraY
io.spriteFlipHorizontal(21) := runningSprite2.io.flipH
io.spriteFlipVertical(21) := runningSprite2.io.flipV
spriteVisible(21) := runningSprite2.io.shownSprite(3)

// slot 25 is the third running sprite before it is hit
io.spriteXPosition(25) := runningSprite3.io.posX - cameraX
io.spriteYPosition(25) := runningSprite3.io.posY - cameraY
io.spriteFlipHorizontal(25) := runningSprite3.io.flipH
io.spriteFlipVertical(25) := runningSprite3.io.flipV
spriteVisible(25) := runningSprite3.io.shownSprite(2)

// slot 30 and 31 are the third running sprite alternates after hit
io.spriteXPosition(30) := runningSprite3.io.posX - cameraX
io.spriteYPosition(30) := runningSprite3.io.posY - cameraY
io.spriteFlipHorizontal(30) := runningSprite3.io.flipH
io.spriteFlipVertical(30) := runningSprite3.io.flipV
spriteVisible(30) := runningSprite3.io.shownSprite(0)

io.spriteXPosition(31) := runningSprite3.io.posX - cameraX
io.spriteYPosition(31) := runningSprite3.io.posY - cameraY
io.spriteFlipHorizontal(31) := runningSprite3.io.flipH
io.spriteFlipVertical(31) := runningSprite3.io.flipV
spriteVisible(31) := runningSprite3.io.shownSprite(1)

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

    when(startLight.io.raceStarted &&
        !winCondition.io.gameWon &&
        !crashReg) {

      car.io.update := playerStun === 0.U
      aiCar.io.updateFrame := aiStun === 0.U

    }.otherwise {

      car.io.update := false.B
      aiCar.io.updateFrame := false.B
    }

    stateReg := done
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
        }
        // Ignoring inputs held on power-on to prevent RNG manip
        when(startcountOver) {
          when(!io.btnU && !io.btnC && !io.btnD && !io.btnL && !io.btnR) {
            readyForStartup := true.B
          }
        }
      }
      when ((io.btnU || io.btnC || io.btnD || io.btnR || io.btnL) && readyForStartup) {
        onSplash := false.B
      }
      // todo - && doneUpdatingBG
      when (!onSplash) {
        stateReg := idle
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

io.spriteXPosition(26) := 8.S
io.spriteYPosition(26) := 8.S

spriteVisible(26) := pocket.io.showShell

io.spriteFlipHorizontal(26) := false.B
io.spriteFlipVertical(26) := false.B

io.spriteXPosition(23) := 8.S
io.spriteYPosition(23) := 8.S

spriteVisible(23) := pocket.io.showShroom

io.spriteFlipHorizontal(23) := false.B
io.spriteFlipVertical(23) := false.B

io.spriteXPosition(14) := mysteryBox.io.posX - cameraX
io.spriteYPosition(14) := mysteryBox.io.posY - cameraY
io.spriteFlipHorizontal(14) := false.B
io.spriteFlipVertical(14) := false.B
spriteVisible(14) := mysteryBox.io.shownSprite

io.led(0) := winCondition.io.checkpointHit
io.led(1) := winCondition.io.finishHit
io.led(2) := winCondition.io.lap1
io.led(3) := winCondition.io.lap2
io.led(4) := winCondition.io.lap3
io.led(5) := winCondition.io.gameWon
io.led(6) := crashReg
io.led(7) := lapDisplay.io.show3

}

