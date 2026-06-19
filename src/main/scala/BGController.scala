import chisel3._
import chisel3.util._

class BGController extends Module {
  val io = IO(new Bundle {
    val showSplash = Input(Bool()) // show___ switches background on a rising edge
    val showGame = Input(Bool())
    val anyPlayerInput = Input(Bool())
    val backBufferWriteData = Output(UInt(6.W))
    val backBufferWriteAddress = Output(UInt(11.W))
    val backBufferWriteEnable = Output(Bool())
    val bgUpdateDone = Output(Bool())
  })

  val showSplash = io.showSplash
  val showGame = io.showGame && !showSplash

  val splash :: game :: goingToSplash :: goingToGame :: Nil = Enum(4)
  val currentTile = RegInit(0.U(11.W))

  val updateTile = RegInit(false.B)

  val state = RegInit(splash)

  switch (state) {
    is (splash) {
      when (showGame) {
        currentTile := 0.U
        state := goingToGame
      }
    }
    is (game) {
      when (showSplash) {
        currentTile := 0.U
        state := goingToSplash
      }
    }
    is (goingToSplash) {
      when (updateTile) {
        currentTile := currentTile + 1.U
      }
      io.backBufferWriteAddress := currentTile
      io.backBufferWriteData
      updateTile := !updateTile
    }
  }

}
