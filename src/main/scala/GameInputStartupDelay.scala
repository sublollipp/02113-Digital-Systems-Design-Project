import chisel3._
import chisel3.util._

class GameInputStartupDelay() extends Module {
  val io = IO(new Bundle {
    val anyInput = Input(Bool())
    val updateCounter = Input(Bool())
    val resetGame = Input(Bool())
    val waitForButtonsReleased = Input(Bool())
    val gameStart = Output(Bool())
    val countOut = Output(UInt(4.W))
  })

  val gameStart = WireDefault(false.B)

  val counterReg = RegInit(0.U(4.W))

  io.countOut := counterReg

  val doneCount :: gameReady :: counting :: startGame :: Nil = Enum(4)

  val state = RegInit(counting)

  switch (state) {
    is (counting) {
      when (io.updateCounter) {
        counterReg := counterReg + 1.U
      }
      when (counterReg === 10.U) {
        state := doneCount
      }
    }
    is(doneCount) {
      when (!io.anyInput) {
        state := gameReady
      }.elsewhen (io.resetGame) {
        counterReg := 0.U
        state := counting
      }
    }
    is (gameReady) {
      when (io.anyInput) {
        state := startGame
      }.elsewhen (io.resetGame) {
        counterReg := 0.U
        state := counting
      }
    }
    is (startGame) {
      gameStart := true.B
      when (io.waitForButtonsReleased) {
        state := doneCount
      }.elsewhen (io.resetGame) {
        counterReg := 0.U
        state := counting
      }
    }
  }

  io.gameStart := gameStart
}
