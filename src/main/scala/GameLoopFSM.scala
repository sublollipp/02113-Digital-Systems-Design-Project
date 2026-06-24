import chisel3._
import chisel3.util._

class GameLoopFSM extends Module {
  val io = IO(new Bundle {
    val startFrameUpdate = Input(Bool())
    val allUpdatesDone = Input(Bool())
    val carsCrashed = Input(Bool())
    val winCondition = Input(Bool())
    val bgDone = Input(Bool())
    val anyInput = Input(Bool())
    val frameUpdate = Output(Bool())
    val RNGUpdate = Output(Bool())
    val carUpdate = Output(Bool())
    val hideAllSprites = Output(Bool())
    val switchToSplash = Output(Bool())
    val switchToGame = Output(Bool())
    val anchorCamera = Output(Bool())
    val resetAll = Output(Bool())
    val bgAckn = Output(Bool())
    val frameUpdateDone = Output(Bool())
  })

  val switchToSplash = WireDefault(false.B)
  val switchToGame = WireDefault(false.B)
  val RNGUpdate = WireDefault(false.B)
  val frameUpdate = WireDefault(false.B)
  val hideAllSprites = WireDefault(true.B)
  val resetAll = WireDefault(false.B)
  val carUpdate = WireDefault(false.B)
  val bgAckn = WireDefault(false.B)
  val frameUpdateDone = WireDefault(false.B)
  val anchorCamera = WireDefault(true.B)

  val idle :: game :: startSplash :: splashIdle :: done :: startSplashUpdateDone :: deadOrFinished :: goingToSplash :: goingToGame :: splashInit :: gameInit :: Nil = Enum(11)
  val state = RegInit(splashIdle)

  val gameInputDelay = Module(new GameInputStartupDelay)
  gameInputDelay.io.anyInput := io.anyInput
  gameInputDelay.io.updateCounter := frameUpdateDone
  gameInputDelay.io.resetGame := false.B
  gameInputDelay.io.waitForButtonsReleased := false.B

  switch(state) {
    is(idle) {
      hideAllSprites := false.B
      anchorCamera := false.B
      when(io.startFrameUpdate) {
        state := game
      }
    }

    is(game) {
      hideAllSprites := false.B
      anchorCamera := false.B

      frameUpdate := true.B
      RNGUpdate := true.B

      carUpdate := true.B

      when(io.carsCrashed || io.winCondition) {
        state := deadOrFinished
        gameInputDelay.io.waitForButtonsReleased := true.B
      }.otherwise {
        state := done
      }
    }

    is(done) {
      anchorCamera := false.B
      hideAllSprites := false.B

      when(io.allUpdatesDone) {
        frameUpdateDone := true.B
        state := idle
      }
    }

    is(splashIdle) {
      when(io.startFrameUpdate) {
        state := startSplash
      }
    }

    is(startSplash) {
      RNGUpdate := true.B
      when(gameInputDelay.io.gameStart) {
        state := gameInit
      }.otherwise {
        state := startSplashUpdateDone
      }
    }

    is(gameInit) {
      switchToGame := true.B
      resetAll := true.B
      state := goingToGame
    }

    is(startSplashUpdateDone) {
      frameUpdateDone := true.B
      state := splashIdle
    }
    is(deadOrFinished) {
      anchorCamera := false.B
      hideAllSprites := false.B
      when(gameInputDelay.io.gameStart) {
        state := splashInit
      }
    }
    is(goingToGame) {
      when(io.bgDone) {
        state := idle
        bgAckn := true.B
        frameUpdateDone := true.B
      }
    }

    is(splashInit) {
      switchToSplash := true.B
      resetAll := true.B
      state := goingToSplash
    }

    is(goingToSplash) {
      anchorCamera := true.B
      when(io.bgDone) {
        state := splashIdle
        bgAckn := true.B
        frameUpdateDone := true.B
        gameInputDelay.io.resetGame := true.B
      }
    }
  }

  io.switchToSplash := switchToSplash
  io.switchToGame := switchToGame
  io.RNGUpdate := RNGUpdate
  io.frameUpdate := frameUpdate
  io.hideAllSprites := hideAllSprites
  io.resetAll := resetAll
  io.carUpdate := carUpdate
  io.bgAckn := bgAckn
  io.frameUpdateDone := frameUpdateDone
  io.anchorCamera := anchorCamera
}
